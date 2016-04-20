module FSharpApiSearch.Trace.Program

open FSharpApiSearch
open FSharpApiSearch.Console
open System
open System.Diagnostics


[<EntryPoint>]
let main argv =
  let args = Args.parse Args.empty (List.ofArray argv)
  let options = args.SearchOptions
  let targets, references = Args.targetAndReference args

  let assemblies = AssemblyLoader.load references
  let dictionaries = Seq.map ApiLoader.load assemblies |> Seq.toArray
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

        let target = apis |> Array.find (fun x -> x.Name = ReverseName.ofString targetName)
        
        let result = Matcher.search dictionaries options [ target ] query

        printfn "Result = %b" (Seq.isEmpty result = false)

        loop()
    with
      ex ->
        printfn "%A" ex
  loop()
  0