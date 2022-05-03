// --------------------------------------------------------------------------------------
// FAKE build script
// --------------------------------------------------------------------------------------

#r "paket: groupref Build //"
#load ".fake/build.fsx/intellisense.fsx"

open Fake.Api
open Fake.Core
open Fake.Core.TargetOperators
open Fake.DotNet
open Fake.IO
open Fake.IO.FileSystemOperators
open Fake.IO.Globbing.Operators
open Fake.Tools.Git
open System.IO

Target.initEnvironment ()

// The name of the project
// (used by attributes in AssemblyInfo, name of a NuGet package and directory in 'src')
let project = "FSharpApiSearch"

// Longer description of the project
// (used as a description for NuGet package; line breaks are automatically cleaned up)
let description = "F# API search engine"

// Default target configuration
let configuration = Environment.environVarOrDefault "configuration" "Release"

// Pattern specifying assemblies to be tested using Persimmon
let testAssemblies = "tests/**/bin" </> configuration </> "*" </> "*.Tests.exe"

// Git configuration (used for publishing documentation in gh-pages branch)
// The profile where the project is posted
let gitOwner = "hafuu"
let gitHome = sprintf "%s/%s" "https://github.com" gitOwner

// The name of the project on GitHub
let gitName = "FSharpApiSearch"

// The url for the raw files hosted
let gitRaw = Environment.environVarOrDefault "gitRaw" "https://raw.githubusercontent.com/hafuu"

let outDir = "bin"

// --------------------------------------------------------------------------------------
// END TODO: The rest of the file includes standard build steps
// --------------------------------------------------------------------------------------

// Read additional information from the release notes document
let release = ReleaseNotes.load "RELEASE_NOTES.md"

// Helper active pattern for project types
let (|Fsproj|Csproj|Vbproj|Shproj|) (projFileName:string) =
    match projFileName with
    | f when f.EndsWith("fsproj") -> Fsproj
    | f when f.EndsWith("csproj") -> Csproj
    | f when f.EndsWith("vbproj") -> Vbproj
    | f when f.EndsWith("shproj") -> Shproj
    | _                           -> failwith (sprintf "Project file %s not supported. Unknown project type." projFileName)

// Generate assembly info files with the right version & up-to-date information
Target.create "AssemblyInfo" (fun _ ->
  let getAssemblyInfoAttributes projectName =
      [ AssemblyInfo.Title projectName
        AssemblyInfo.Product project
        AssemblyInfo.Description description
        AssemblyInfo.Version release.AssemblyVersion
        AssemblyInfo.FileVersion release.AssemblyVersion
        AssemblyInfo.Configuration configuration ]

  let getProjectDetails (projectPath: string) =
      let projectName = System.IO.Path.GetFileNameWithoutExtension(projectPath)
      ( projectPath,
        projectName,
        System.IO.Path.GetDirectoryName(projectPath),
        (getAssemblyInfoAttributes projectName)
      )

  !! "src/**/*.??proj"
  |> Seq.map getProjectDetails
  |> Seq.iter (fun (projFileName, _, folderName, attributes) ->
      match projFileName with
      | Fsproj -> AssemblyInfoFile.createFSharp (folderName </> "AssemblyInfo.fs") attributes
      | Csproj -> AssemblyInfoFile.createCSharp (folderName </> "Properties" </> "AssemblyInfo.cs") attributes
      | Vbproj -> AssemblyInfoFile.createVisualBasic (folderName </> "My Project" </> "AssemblyInfo.vb") attributes
      | Shproj -> ()
      )
)

// Copies binaries from default VS location to expected bin folder
// But keeps a subdirectory structure for each project in the
// src folder to support multiple project outputs
Target.create "CopyBinaries" (fun _ ->
  !! "src/**/*.??proj"
  |>  Seq.map (fun f -> ((System.IO.Path.GetDirectoryName f) @@ "bin" @@ configuration, outDir @@ (System.IO.Path.GetFileNameWithoutExtension f)))
  |>  Seq.iter (fun (fromDir, toDir) -> Shell.copyDir toDir fromDir (fun _ -> true))
)

// --------------------------------------------------------------------------------------
// Clean build results


Target.create "Clean" (fun _ ->
  !! "**/bin"
  ++ "**/obj"
  ++ outDir
  |> Shell.cleanDirs 
)

// --------------------------------------------------------------------------------------
// Build library & test project

Target.create "Build" (fun _ ->
  !! "*.sln"
  |> Seq.iter (DotNet.build (fun args ->
    { args with
        Configuration = DotNet.BuildConfiguration.fromString configuration
    }))
)

// --------------------------------------------------------------------------------------
// Run the unit tests using test runner

Target.create "RunTests" (fun _ ->
  let result =  
    !! testAssemblies
    |> Seq.map (fun testAsm ->
      Command.RawCommand(testAsm, Arguments.Empty)
      |> CreateProcess.fromCommand
      |> Proc.run
    )
    |> Seq.sumBy (fun result -> result.ExitCode)
  if result <> 0 then failwith "Some tests failed."
)

// --------------------------------------------------------------------------------------
// Build a NuGet package

Target.create "NuGet" (fun _ ->
  use trace = Trace.traceTask "PaketPack" "."
  let args =
    Arguments.OfArgs([ "pack" ])
    |> Arguments.append [ "--version"; release.NugetVersion ]
    |> Arguments.append [ "--release-notes"; System.Net.WebUtility.HtmlEncode(release.Notes |> String.concat System.Environment.NewLine) ]
    |> Arguments.append [ outDir ]
  let result = DotNet.exec id "paket" (args.ToWindowsCommandLine)
  if not result.OK then failwith "Error during packing."
  trace.MarkSuccess()
)

Target.create "PublishNuget" (fun _ ->
  let apiKey = Environment.environVar "api-key"
  if (String.isNullOrEmpty apiKey = false) then TraceSecrets.register "<api-key>" apiKey

  !! (outDir @@ "/**/*.nupkg")
  |> Seq.iter (fun package ->
    use trace = Trace.traceTask "PaketPublish" package
    let args =
      Arguments.OfArgs([ "push" ])
      |> Arguments.appendNotEmpty "--api-key" apiKey
      |> Arguments.append([ package ])
    let result = DotNet.exec id "paket" (args.ToWindowsCommandLine)
    if not result.OK then failwithf "Error during pushing %s" package
    trace.MarkSuccess()
  )
)

// --------------------------------------------------------------------------------------
// Build a zip package

let githubReleaseFilePath = outDir @@ (project + ".zip")

Target.create "PackGithubRelease" (fun _ ->
  [ "FSharpApiSearch.Console"; "FSharpApiSearch.Database" ]
  |> Seq.map (fun proj ->
    let projDir = outDir </> proj
    !! (projDir @@ "*" @@ "*.dll")
      ++ (projDir @@ "*" @@ "*.exe")
      ++ (projDir @@ "*" @@ "*.config")
      ++ (projDir @@ "*" @@ "*.json")
    |> Zip.filesAsSpecsFlatten
    |> Zip.moveToFolder project
  )
  |> Seq.concat
  |> Seq.distinctBy(fun (f, _) -> Path.GetFileName(f))
  |> Zip.zipSpec githubReleaseFilePath
)

// --------------------------------------------------------------------------------------
// Release Scripts

Target.create "Release" (fun _ ->
  Staging.stageAll ""
  Commit.exec "" (sprintf "Bump version to %s" release.NugetVersion)
  Branches.pushBranch "" "origin" "master"

  Branches.tag "" release.NugetVersion
  Branches.pushTag "" "origin" release.NugetVersion

  
  //// release on github
  //let user = Environment.environVar "github-user"
  //if String.isNullOrEmpty user then TraceSecrets.register "<github-user>" user
  //let password = Environment.environVar "github-pw"
  //if String.isNotNullOrEmpty password then TraceSecrets.register "<github-pw>" password
  //GitHub.createClient user password
  //|> GitHub.draftNewRelease gitOwner gitName release.NugetVersion (release.SemVer.PreRelease <> None) release.Notes
  //// TODO: |> uploadFile "PATH_TO_FILE"
  //|> GitHub.publishDraft
  //|> Async.RunSynchronously
)

// --------------------------------------------------------------------------------------
// Run all targets by default. Invoke 'build <Target>' to override

Target.create "All" ignore

"Clean"
  ==> "AssemblyInfo"
  ==> "Build"
  ==> "CopyBinaries"
  ==> "RunTests"
  ==> "NuGet"
  ==> "PackGithubRelease"
  ==> "All"

Target.runOrDefault "All"