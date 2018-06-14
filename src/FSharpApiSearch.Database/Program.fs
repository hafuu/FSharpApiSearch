module FSharpApiSearch.Database.Program

open FSharpApiSearch
open CommandLine
open System.IO
open System.Configuration

type Args = {
  AssemblyResolver : AssemblyLoader.AssemblyResolver
  References: string list
  Help: bool
}

module Args =
  let defaultArg = {
    AssemblyResolver =
      {
        FSharpCore = ConfigurationManager.AppSettings.Get("FSharpCore")
        Framework = ConfigurationManager.AppSettings.Get("Framework").Split(';') |> Array.toList |> List.rev
        Directories = []
      }
    References = []
    Help = false
  }

  let rec parse arg = function
    | KeyValue "--FSharpCore" path :: rest -> parse { arg with AssemblyResolver = { arg.AssemblyResolver with FSharpCore = path } } rest
    | KeyValue "--Framework" path :: rest -> parse { arg with AssemblyResolver = { arg.AssemblyResolver with Framework = path :: arg.AssemblyResolver.Framework } } rest
    | KeyValue "--lib" path :: rest -> parse { arg with AssemblyResolver = { arg.AssemblyResolver with Directories = path :: arg.AssemblyResolver.Directories } } rest
    | ("--help" | "-h") :: rest -> parse { arg with Help = true } rest
    | path :: rest -> parse { arg with References = path :: arg.References } rest
    | [] -> arg

let helpMessage = """usage: FSharpApiSearch.Database.exe <options> <assemblies>

assemblies: Specifies the assembly name or the assembly path.
            By default, 'mscorlib', 'System', 'System.Core' and 'FSharp.Core' are specified.
options:
  --lib:<folder-name>
      Specifies a directory to be searched for assemblies that are referenced.
  --FSharpCore:<folder-name>
      Specifies the FSharp.Core path.
      If omitted, it will use the value in 'FSharpApiSearch.Database.exe.config'.
  --Framework:<folder-name>
      Specifies a directory to be searched for .Net Framework assemblies that are references.
      If omitted, it will use the value in 'FSharpApiSearch.Database.exe.config'.
  --help, -h
      Print this message."""

let printAssemblies (assemblies: AssemblyLoader.AssemblyInfo[]) =
  printfn "Create the database of the following assemblies."
  assemblies
  |> Array.iter (fun a -> printfn "  %s" a.Path)

let printForwardingLogs (apiDict: ApiDictionary, logs: seq<ApiLoader.TypeForward>) =
  if Seq.isEmpty logs then
    ()
  else
    printfn "%s type forwarding" apiDict.AssemblyName
    logs
    |> Seq.sortBy (fun tf -> tf.Type)
    |> Seq.iter (fun tf ->
      printfn "  %s : %s -> %s" tf.Type tf.From tf.To
    )

let printApiNumber (dictionaries: ApiDictionary[]) =
  printfn "Finished."
  dictionaries
  |> Array.iter (fun apiDic ->
    printfn "  %s : %d" apiDic.AssemblyName apiDic.PublicApiNumber
  )

[<EntryPoint>]
let main argv = 
  let args = Args.parse Args.defaultArg (List.ofArray argv)
  match args with
  | { Help = true } ->
    printfn "%s" helpMessage
    0
  | _ ->
    try
      let assemblies =
        match args.References with
        | [] -> FSharpApiSearchClient.DefaultTargets
        | _ -> List.rev args.References
      let assemblies = args.AssemblyResolver.ResolveAll(assemblies)
      printAssemblies assemblies
      let databaseResult =
        AssemblyLoader.load assemblies
        |> ApiLoader.loadWithLogs
      
      databaseResult |> Array.iter printForwardingLogs

      let database =
        databaseResult
        |> Array.map fst 
        |> Database.compact assemblies

      printApiNumber database

      printfn "Saving database."
      Database.save Database.databaseName database
      0
    with ex ->
      printfn "%A" ex
      1