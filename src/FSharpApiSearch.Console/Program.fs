open FSharpApiSearch
open FSharpApiSearch.Types
open System.Diagnostics
open System

let searchAndShowResult (client: FSharpApiSearchClient) (query: string) =
  let sw = Stopwatch.StartNew()
  let results = client.Search(query)
  results
  |> Seq.filter (fun x -> x.Distance < 3)
  |> Seq.iter (fun x ->
    Console.Write(sprintf "%s: %s" x.Api.Name (Signature.display x.Api.Signature))
    Console.ForegroundColor <- ConsoleColor.DarkGray
    Console.WriteLine(sprintf ", distance: %d" x.Distance)
    Console.ResetColor()
  )
  sw.Stop()
  printfn "total %d [ms]" sw.ElapsedMilliseconds

[<EntryPoint>]
let main argv =
  match argv with
  | [| query |] ->
    let client = FSharpApiSearchClient()
    searchAndShowResult client query
    0
  | _ ->
    printfn "initializing"
    let client = FSharpApiSearchClient()
    let rec loop () =
      printfn "input query or #q to quit."
      printf "> "
      match System.Console.ReadLine() with
      | "#q" -> ()
      | query ->
        try searchAndShowResult client query with ex -> printfn "%A" ex
        loop()
    loop()
    0