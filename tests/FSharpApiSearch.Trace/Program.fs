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
  | TypeAbbreviation x -> TypeAbbreviation { Abbreviation = updateSource newSource x.Abbreviation; Original = updateSource newSource x.Original }

let matchAndShowResult (query: string) (target: string) abbTable =
  let query =
    let update q = Signature.replaceAbbreviation abbTable q |> updateSource Source.Target
    match QueryParser.parse query with
    | { Method = BySignature s } as x -> { x with Method = BySignature (update s) }
    | { Method = ByName (n, SignatureQuery s) } as x -> { x with Method = ByName (n, SignatureQuery (update s)) }
    | x -> x
  let target = { Name = "test"; Signature = QueryParser.parseFSharpSignature target |> Signature.replaceAbbreviation abbTable |> updateSource Source.Target }
  let eqs = Matcher.Equations.initialize query |> Matcher.Equations.strictVariables query
  let ctx = Matcher.Context.initialize eqs
  let result = Matcher.matches query target Matcher.defaultRule ctx |> Matcher.Result.toBool
  printfn "result: %b" result

let makeAbbreviationTable (xs: string list) =
  xs
  |> List.map (fun x -> x.Split([| '=' |], 2))
  |> List.map (fun abb -> { Abbreviation = QueryParser.parseFSharpSignature abb.[0]; Original = QueryParser.parseFSharpSignature abb.[1] })

[<EntryPoint>]
let main argv =
  Debug.Listeners.Add(new TextWriterTraceListener(System.Console.Out)) |> ignore
  Debug.IndentSize <- 2
  match List.ofArray argv with
  | query :: target :: abbreviations ->
    matchAndShowResult query target (makeAbbreviationTable abbreviations)
    0
  | _ ->
    printfn "Please input the query and the target."
    0