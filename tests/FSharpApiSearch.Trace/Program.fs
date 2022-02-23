module FSharpApiSearch.Trace.Program

open FSharpApiSearch
open FSharpApiSearch.StringPrinter
open FSharpApiSearch.Console
open System
open System.Diagnostics
open System.IO

let assemblyResolver: AssemblyLoader.AssemblyResolver = {
  FSharpCore = Path.Combine(System.Environment.GetFolderPath(Environment.SpecialFolder.ProgramFilesX86), @"Reference Assemblies\Microsoft\FSharp\.NETFramework\v4.0\4.4.0.0\")
  Framework = [ Path.Combine(System.Environment.GetFolderPath(Environment.SpecialFolder.ProgramFilesX86), @"Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5\") ]
  Directories = []
}

let positionPrintVisitor printPos lowType =
  let rec print lowType =
    printPos lowType;
    match lowType with
    | Wildcard _ -> ()
    | Variable _ -> ()
    | Identifier _ -> ()
    | Arrow ((xs, x), _) -> List.iter print xs; print x
    | Tuple (tpl, _) -> List.iter print tpl.Elements
    | Generic (id, args, _) -> print id; List.iter print args
    | TypeAbbreviation (abb, _) -> print abb.Abbreviation; print abb.Original
    | Delegate (t, (xs, x), _) -> print t; List.iter print xs; print x
    | ByRef (_, t, _) -> print t
    | LowType.Subtype (t, _) -> print t
    | Choice (o, xs, _) -> print o; List.iter print xs
    | LoadingType _ -> ()

  print lowType
  lowType

let printQueryPositions (query: Query) =
  printfn "Query Positions:"
  query
  |> LowTypeVisitor.accept_Query (positionPrintVisitor (fun x ->
    match x.Position with
    | AtQuery (Some id, _) -> printfn "  %A : %A" id (x.Print())
    | _ -> ()
  ))
  |> ignore

let printSignaturePositions (api: Api) =
  printfn "Signature Positions:"
  api.Signature
  |> LowTypeVisitor.accept_ApiSignature (positionPrintVisitor (fun x ->
    match x.Position with
    | AtSignature (id) -> printfn "  %A : %A" id (x.Print())
    | _ -> ()
  ))
  |> ignore

[<EntryPoint>]
let main argv =
  let args = Args.parse Args.empty (List.ofArray argv)
  let options = args.SearchOptions |> SearchOptions.Parallel.Set Disabled

  let dictionaries = Database.loadFromFile Database.databaseName
  let targets = Args.targetsOrDefault args
  let targetAssemblies = dictionaries |> Seq.filter (fun x -> targets |> Seq.exists ((=)x.AssemblyName)) |> Seq.toArray

  let apis = targetAssemblies |> Seq.collect (fun x -> x.Api) |> Seq.toArray

  use listener = new TextWriterTraceListener(System.Console.Out)
  Trace.Listeners.Add(listener) |> ignore
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
        let resultQuery, result = Engine.search dictionaries options [ dummyDict ] query

        printfn "Result = %b" (Seq.isEmpty result = false)
        result
        |> Seq.iter (fun x ->
          printfn "Match Positions:"
          x.MatchPositions
          |> Map.iter (fun sigId queryId ->
            printfn "  %A : %A" queryId sigId
          )
        )

        printQueryPositions resultQuery
        printSignaturePositions target

        loop()
    with
      ex ->
        printfn "%A" ex
  loop()
  0