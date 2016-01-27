open FSharpApiSearch
open FSharpApiSearch.Types
open System.Diagnostics

let rec updateSource newSource = function
  | Variable (_, name) -> Variable (newSource, name)
  | Identity _ as x -> x
  | Arrow xs -> Arrow (List.map (updateSource newSource) xs)
  | Generic (id, xs) -> Generic (id, List.map (updateSource newSource) xs)
  | Tuple xs -> Tuple (List.map (updateSource newSource) xs)
  | Unknown as x -> x

let matchAndShowResult (query: string) (target: string) =
  let query = QueryParser.parse query
  let target = { Name = "test"; Signature = QueryParser.parseSignature target |> updateSource Source.Target }
  let result = Matcher.matches query target (Matcher.Context.initialize (Matcher.Equations.strict query)) |> Matcher.MatchResult.toBool
  printfn "result: %b" result

[<EntryPoint>]
let main argv =
  Debug.Listeners.Add(new TextWriterTraceListener(System.Console.Out)) |> ignore
  Debug.IndentSize <- 2
  match argv with
  | [| query; target |] ->
    matchAndShowResult query target
    0
  | _ ->
    printfn "Please input the query and the target."
    0