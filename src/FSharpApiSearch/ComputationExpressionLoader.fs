module internal FSharpApiSearch.ComputationExpressionLoader

open FSharpApiSearch.MatcherTypes
open FSharpApiSearch.SpecialTypes.LowType.Patterns
open FSharp.Collections.ParallelSeq

let (|P|) (p: Parameter) =
  match p.Type with
  | TypeAbbreviation x -> x.Original
  | t -> t

module Extract =
  let bind (m: Member) = // M<'T> * ('T -> M<'U>) -> M<'U>
    match m with
    | { Parameters = [ [ P t1; P (Arrow ([ _], _)) ] ]; ReturnParameter = P t2 } -> [ t1; t2 ]
    | _ -> []

  let return' (m: Member) = // 'T -> M<'T>
    match m with
    | { Parameters = [ [ _ ] ]; ReturnParameter = P t1 } -> [ t1 ]
    | _ -> []

  let returnFrom (m: Member) = // M<'T> -> M<'T>
    match m with
    | { Parameters = [ [ P t1 ] ]; ReturnParameter = P t2 } -> [ t1; t2 ]
    | _ -> []

  let run (m: Member) = // M<'T> -> M<'T> or M<'T> -> 'T
    match m with
    | { Parameters = [ [ _ ] ]; ReturnParameter = P t1 } -> [ t1 ]
    | _ -> []

  let zero (m: Member) = // unit -> M<'T>
    match m with
    | { Parameters = [ [ P Unit ] ]; ReturnParameter = P t1 } -> [ t1 ]
    | _ -> []

  let source (m: Member) = // 'T -> M<'U>
    match m with
    | { Parameters = [ [ P t1 ] ] } -> [ t1 ]
    | _ -> []

  let customOperation (m: Member) = m.ReturnParameter.Type

module BuilderMethod =
  let bind = function
    | { Name = "Bind"; Parameters = [ [ _; P (Variable _ | Arrow ([ _ ], _)) ] ]; } -> true
    | _ -> false

  let delay = function
    | { Name = "Delay"; Parameters = [ [ P (Variable _ | Arrow ([ (Unit | Variable _) ], _)) ] ] } -> true
    | _ -> false

  let return' = function
    | { Name = "Return"; Parameters = [ [ _ ] ] } -> true
    | _ -> false

  let returnFrom = function
    | { Name = "ReturnFrom"; Parameters = [ [ _ ] ] } -> true
    | _ -> false

  let combine = function
    | { Name = "Combine"; Parameters = [ [ _; _ ] ] } -> true
    | _ -> false

  let for' = function
    | { Name = "For"; Parameters = [ [ _; P (Variable _ | Arrow ([ _ ], _)) ] ] } -> true
    | _ -> false

  let tryFinally = function
    | { Name = "TryFinally"; Parameters = [ [ _; P (Variable _ | Arrow ([ (Variable _ | Unit) ], (Variable _ | Unit))) ] ] } -> true
    | _ -> false

  let tryWith = function
    | { Name = "TryWith"; Parameters = [ [ _; P (Variable _ | Arrow ([ _ ], _)) ] ] } -> true
    | _ -> false

  let using = function
    | { Name = "Using"; Parameters = [ [ _; P (Variable _ | Arrow ([ _ ], _)) ] ] } -> true
    | _ -> false

  let while' = function
    | { Name = "While"; Parameters = [ [ P (Variable _ | Arrow ([ (Variable _ | Unit) ], (Variable _ | Boolean) )); _ ] ] } -> true
    | _ -> false

  let yield' = function
    | { Name = "Yield"; Parameters = [ [ _ ] ] } -> true
    | _ -> false

  let yieldFrom = function
    | { Name = "YieldFrom"; Parameters = [ [ _ ] ] } -> true
    | _ -> false

  let zero = function
    | { Name = "Zero"; Parameters = [ [ P (Variable _ | Unit) ] ] } -> true
    | _ -> false

let extractTypes (typeDef: FullTypeDefinition) (customOperations: seq<string * Member>) =
  seq {
    yield! typeDef.InstanceMembers
            |> Seq.collect (fun m ->
              match m with
              | { Name = "Run" } -> Extract.run m
              | { Name = "Bind" } -> Extract.bind m
              | { Name = ("Return" | "Yield") } -> Extract.return' m
              | { Name = ("ReturnFrom" | "YieldFrom") } -> Extract.returnFrom m
              | { Name = "Zero" } -> Extract.zero m
              | { Name = "Source" } -> Extract.source m
              | _ -> [] )
    yield! customOperations |> Seq.map (snd >> Extract.customOperation)
  }
  |> Seq.distinct

let hasMethod (builderTypeDef: FullTypeDefinition) f = builderTypeDef.InstanceMembers |> List.exists f

let syntaxMethods =
  [
    "let!", [ BuilderMethod.bind ]
    "yield", [ BuilderMethod.yield' ]
    "yield!", [ BuilderMethod.yieldFrom ]
    "return", [ BuilderMethod.return' ]
    "return!", [ BuilderMethod.returnFrom ]
    "use", [ BuilderMethod.using ]
    "use!", [ BuilderMethod.bind; BuilderMethod.using ]
    "if/then", [ BuilderMethod.zero ]
    "for", [ BuilderMethod.for' ]
    "while", [ BuilderMethod.while'; BuilderMethod.delay ]
    "try/with", [ BuilderMethod.tryWith; BuilderMethod.delay ]
    "try/finally", [ BuilderMethod.tryFinally; BuilderMethod.delay ]
  ]

let hasSyntax (builderTypeDef: FullTypeDefinition) (expectedMethods: (Member -> bool) list) : bool =
  expectedMethods |> List.forall (hasMethod builderTypeDef)

let extractSyntaxes (builderTypeDef: FullTypeDefinition) : Set<string> =
  syntaxMethods
  |> List.choose (fun (syntax, expectedMethod) ->
    if hasSyntax builderTypeDef expectedMethod then
      Some syntax
    else
      None
  )
  |> Set.ofList