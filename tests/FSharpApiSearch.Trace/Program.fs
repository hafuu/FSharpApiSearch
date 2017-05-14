module FSharpApiSearch.Trace.Program

open FSharpApiSearch
open FSharpApiSearch.Printer
open FSharpApiSearch.Console
open System
open System.Diagnostics
open System.IO

let assemblyResolver: AssemblyLoader.AssemblyResolver = {
  FSharpCore = Path.Combine(System.Environment.GetFolderPath(Environment.SpecialFolder.ProgramFilesX86), @"Reference Assemblies\Microsoft\FSharp\.NETFramework\v4.0\4.4.0.0\")
  Framework = [ Path.Combine(System.Environment.GetFolderPath(Environment.SpecialFolder.ProgramFilesX86), @"Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5\") ]
  Directories = []
}

[<EntryPoint>]
let main argv =
  let args = Args.parse Args.empty (List.ofArray argv)
  let options = args.SearchOptions |> SearchOptions.Parallel.Set Disabled

  let dictionaries = ApiLoader.loadFromFile ApiLoader.databaseName
  let targets = Args.targetsOrDefault args
  let targetAssemblies = dictionaries |> Seq.filter (fun x -> targets |> Seq.exists ((=)x.AssemblyName)) |> Seq.toArray

  let apis = targetAssemblies |> Seq.collect (fun x -> x.Api) |> Seq.toArray

  use listener = new TextWriterTraceListener(System.Console.Out)
  Debug.Listeners.Add(listener) |> ignore
  Debug.IndentSize <- 2

  let rec loop() =
    try
      Console.WriteLine("input query.")
      Console.Write("> ");
      let query = Console.ReadLine()
      
      if query = "" then
        ()
      else
        Console.WriteLine("input target name.")
        Console.Write("> ");
        let targetName = Console.ReadLine()

        let target = apis |> Array.find (fun x -> FSharp.printFullName x = targetName)
        let dummyDict: ApiDictionary = { AssemblyName = "dummy"; Api = [| target |]; TypeDefinitions = dict Seq.empty; TypeAbbreviations = [||] }
        let result = Matcher.search dictionaries options [ dummyDict ] query

        printfn "Result = %b" (Seq.isEmpty result = false)

        loop()
    with
      ex ->
        printfn "%A" ex
  loop()
  0