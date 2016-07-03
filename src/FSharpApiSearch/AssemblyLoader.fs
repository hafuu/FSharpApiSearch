module FSharpApiSearch.AssemblyLoader

open Microsoft.FSharp.Compiler.SourceCodeServices
open System.IO
open System.Reflection

type AssemblyResolver = {
  FSharpCore: string
  Framework: string list
  Directories: string list
}
with
  member this.Resolve(assemblyName: string) =
    let assemblyName = if assemblyName.EndsWith(".dll") = false then assemblyName + ".dll" else assemblyName
    if assemblyName = "FSharp.Core.dll" then
      Some (Path.Combine(this.FSharpCore, assemblyName))
    else
      seq { yield! this.Directories; yield! this.Framework }
      |> Seq.map (fun dir -> Path.Combine(dir, assemblyName))
      |> Seq.tryFindBack File.Exists

let internal ignoreFSharpCompilerServiceError() =
  typeof<FSharpChecker>.Assembly.GetType("Microsoft.FSharp.Compiler.AbstractIL.Diagnostics")
  |> Option.ofObj
  |> Option.bind (fun diagMod -> diagMod.GetMember("diagnosticsLog", BindingFlags.NonPublic ||| BindingFlags.Static) |> Array.tryHead)
  |> Option.bind (tryUnbox<PropertyInfo>)
  |> Option.bind (fun x -> x.GetValue(null) |> Option.ofObj)
  |> Option.bind (tryUnbox<ref<Option<System.IO.TextWriter>>>)
  |> Option.iter (fun x -> x := None)

let load (assemblyResolver: AssemblyResolver) references =
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
            for r in references |> Seq.choose assemblyResolver.Resolve do
              yield "-r:" + r |]
      )
  let refAssemblies =
    let x = checker.ParseAndCheckProject(options) |> Async.RunSynchronously
    x.ProjectContext.GetReferencedAssemblies()
  Array.ofList refAssemblies