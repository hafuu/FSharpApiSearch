open FSharpApiSearch
open System.Diagnostics

let rec updateSource newSource = function
  | Variable (_, name) -> Variable (newSource, name)
  | StrongVariable (_, name) -> StrongVariable (newSource, name)
  | Identity _ as x -> x
  | StrongIdentity _ as x -> x
  | Wildcard as x -> x
  | WildcardGroup _ as x -> x
  | Arrow xs -> Arrow (List.map (updateSource newSource) xs)
  | Generic (id, xs) -> Generic (updateSource newSource id, List.map (updateSource newSource) xs)
  | StaticMethod x ->
    StaticMethod {
      Arguments = List.map (updateSource newSource) x.Arguments
      ReturnType = updateSource newSource x.ReturnType
    }
  | InstanceMember x ->
    InstanceMember {
      Source = newSource
      Receiver = updateSource newSource x.Receiver
      Arguments = List.map (updateSource newSource) x.Arguments
      ReturnType = updateSource newSource x.ReturnType
    }

let matchAndShowResult (query: string) (target: string) =
  let query = QueryParser.parse query
  let target = { Name = "test"; Signature = QueryParser.parseFSharpSignature target |> updateSource Source.Target }
  let eqs = Matcher.Equations.initialize query |> Matcher.Equations.strictVariables query
  let ctx = Matcher.Context.initialize eqs
  let result = Matcher.matches query target Matcher.defaultRule ctx |> Matcher.Result.toBool
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