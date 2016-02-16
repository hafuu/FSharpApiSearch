module FSharpApiSearch.AssemblyLoader

open Microsoft.FSharp.Compiler.SourceCodeServices
open System.IO
open System.Reflection

let syslib name =
  Path.Combine(
    System.Environment.GetFolderPath(System.Environment.SpecialFolder.ProgramFilesX86)
    , @"Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5\"
    , name)
let fscore4400 =
  Path.Combine(
    System.Environment.GetFolderPath(System.Environment.SpecialFolder.ProgramFilesX86)
    , @"Reference Assemblies\Microsoft\FSharp\.NETFramework\v4.0\4.4.0.0\FSharp.Core.dll")

let resolvePath (assemblyName: string) =
  let assemblyName = if assemblyName.EndsWith(".dll") = false then assemblyName + ".dll" else assemblyName

  if File.Exists(syslib assemblyName) then
    Some (syslib assemblyName)
  elif assemblyName = "FSharp.Core.dll" then
    Some fscore4400
  elif File.Exists(assemblyName) then
    Some (assemblyName)
  else
    None

let ignoreFSharpCompilerServiceError() =
  typeof<FSharpChecker>.Assembly.GetType("Microsoft.FSharp.Compiler.AbstractIL.Diagnostics")
  |> Option.ofObj
  |> Option.bind (fun diagMod -> diagMod.GetMember("diagnosticsLog", BindingFlags.NonPublic ||| BindingFlags.Static) |> Array.tryHead)
  |> Option.bind (tryUnbox<PropertyInfo>)
  |> Option.bind (fun x -> x.GetValue(null) |> Option.ofObj)
  |> Option.bind (tryUnbox<ref<Option<System.IO.TextWriter>>>)
  |> Option.iter (fun x -> x := None)

let load references =
  ignoreFSharpCompilerServiceError()

  let checker = FSharpChecker.Create()
  let base1 = Path.GetTempFileName()
  let fileName1 = Path.ChangeExtension(base1, ".fs")
  let projFileName = Path.ChangeExtension(base1, ".fsproj")
  let dllName = Path.ChangeExtension(base1, ".dll")
  let options =
    checker.GetProjectOptionsFromCommandLineArgs
      (projFileName,
        [|  yield "--simpleresolution" 
            yield "--noframework" 
            yield "--debug:full" 
            yield "--define:DEBUG" 
            yield "--optimize-" 
            yield "--out:" + dllName
            yield "--warn:3" 
            yield "--fullpaths" 
            yield "--flaterrors" 
            yield "--target:library" 
            yield fileName1
            for r in references |> Seq.choose resolvePath do
              yield "-r:" + r |]
      )
  let refAssemblies =
    let x = checker.ParseAndCheckProject(options) |> Async.RunSynchronously
    x.ProjectContext.GetReferencedAssemblies()
  refAssemblies