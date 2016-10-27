module internal FSharpApiSearch.ComputationExpression

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
    | { Parameters = [ [ P t1; P (Arrow [ _; _ ]) ] ]; ReturnParameter = P t2 } -> [ t1; t2 ]
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

module BuilderMethod =
  let bind = function
    | { Name = "Bind"; Parameters = [ [ _; P (Arrow [ _; _ ]) ] ]; } -> true
    | _ -> false

  let delay = function
    | { Name = "Delay"; Parameters = [ [ P (Arrow [ Unit; _ ]) ] ] } -> true
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
    | { Name = "For"; Parameters = [ [ _; P (Arrow [ _; _ ]) ] ] } -> true
    | _ -> false

  let tryFinally = function
    | { Name = "TryFinally"; Parameters = [ [ _; P (Arrow [ Unit; Unit ]) ] ] } -> true
    | _ -> false

  let tryWith = function
    | { Name = "TryWith"; Parameters = [ [ _; P (Arrow [ _; _ ]) ] ] } -> true
    | _ -> false

  let using = function
    | { Name = "Using"; Parameters = [ [ _; P (Arrow [ _; _ ]) ] ] } -> true
    | _ -> false

  let while' = function
    | { Name = "While"; Parameters = [ [ P (Arrow [ Unit; Boolean ]); _ ] ] } -> true
    | _ -> false

  let yield' = function
    | { Name = "Yield"; Parameters = [ [ _ ] ] } -> true
    | _ -> false

  let yieldFrom = function
    | { Name = "YieldFrom"; Parameters = [ [ _ ] ] } -> true
    | _ -> false

  let zero = function
    | { Name = "Zero"; Parameters = [ [ P Unit ] ] } -> true
    | _ -> false

let extract (typeDef: FullTypeDefinition) =
  typeDef.InstanceMembers
  |> Seq.collect (fun m ->
    match m with
    | { Name = "Run" } -> Extract.run m
    | { Name = "Bind" } -> Extract.bind m
    | { Name = ("Return" | "Yield") } -> Extract.return' m
    | { Name = ("ReturnFrom" | "YieldFrom") } -> Extract.returnFrom m
    | { Name = "Zero" } -> Extract.zero m
    | { Name = "Source" } -> Extract.source m
    | _ -> [] )
  |> Seq.distinct

let isBuilder (lowTypeMatcher: ILowTypeMatcher) (compExprType: LowType) (ctx: Context) (typeDef: FullTypeDefinition) : bool =
  extract typeDef
  |> Seq.exists (fun t -> lowTypeMatcher.Test t compExprType ctx |> MatchingResult.toBool)

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
    "if", [ BuilderMethod.zero ]
    "for", [ BuilderMethod.for' ]
    "while", [ BuilderMethod.while'; BuilderMethod.delay ]
    "try-with", [ BuilderMethod.tryWith; BuilderMethod.delay ]
    "try-finally", [ BuilderMethod.tryFinally; BuilderMethod.delay ]
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

let private map (options: SearchOptions) f (xs: #seq<_>) =
  match options.Parallel with
  | Enabled -> PSeq.map f xs :> seq<_>
  | Disabled -> Seq.map f xs

let private filter (options: SearchOptions) f (xs: #seq<_>) =
  match options.Parallel with
  | Enabled -> PSeq.filter f xs :> seq<_>
  | Disabled -> Seq.filter f xs

let private collect (options: SearchOptions) f (xs: #seq<_>) =
  match options.Parallel with
  | Enabled -> PSeq.collect f xs :> seq<_>
  | Disabled -> Seq.collect f xs

let private choose (options: SearchOptions) f xs =
  match options.Parallel with
  | Enabled -> PSeq.choose f xs :> seq<_>
  | Disabled -> Seq.choose f xs

let private append options xs ys =
  match options.Parallel with
  | Enabled -> PSeq.append xs ys :> seq<_>
  | Disabled -> Seq.append xs ys

let test (lowTypeMatcher: ILowTypeMatcher) (builderTypes: LowType) (ctx: Context) (api: Api) =
  match api.Signature with
  | ApiSignature.ModuleValue (TypeAbbreviation { Original = Arrow xs }) -> lowTypeMatcher.Test builderTypes (List.last xs) ctx
  | ApiSignature.ModuleValue value -> lowTypeMatcher.Test builderTypes value ctx
  | ApiSignature.ModuleFunction xs -> lowTypeMatcher.Test builderTypes ((xs |> List.last |> List.last).Type) ctx
  | _ -> Failure

let search (options: SearchOptions) (targets: ApiDictionary seq) (lowTypeMatcher: ILowTypeMatcher) (query: ComputationExpressionQuery) (initialContext: Context) =
  let querySyntaxes = Set.ofList query.Syntaxes

  let builderTypes =
    targets
    |> collect options (fun target -> target.Api)
    |> choose options (fun api ->
      match api.Signature with
      | ApiSignature.FullTypeDefinition td -> Some (td, api.Document)
      | _ -> None)
    |> filter options (fst >> isBuilder lowTypeMatcher query.Type initialContext)
    |> map options (fun (builderTypeDef, doc) ->
      let syntaxes = extractSyntaxes builderTypeDef
      (builderTypeDef, doc, syntaxes)
    )
    |> filter options (fun (_, _, syntaxes) ->
      if Set.isEmpty querySyntaxes then
        Set.isEmpty syntaxes = false
      else
        Set.intersect syntaxes querySyntaxes = querySyntaxes
    )
    |> Seq.toList

  let builderResults =
    builderTypes
    |> Seq.map (fun (td, doc, syntaxes) ->
      let apiSig = ApiSignature.ComputationExpressionBuilder (td.LowType, Set.toList syntaxes)
      let api = { Name = DisplayName td.Name; Signature = apiSig; TypeConstraints = td.TypeConstraints; Document = doc }
      { Distance = 0; Api = api; AssemblyName = td.AssemblyName }
    )

  let builderTypes = Choice (builderTypes |> List.map (fun (td, _, _) -> td.LowType))

  let apiResults =
    seq {
      for dic in targets do
        for api in dic.Api do
          yield (dic, api)
    }
    |> choose options (fun (dic, api) ->
      match test lowTypeMatcher builderTypes initialContext api with
      | Matched ctx -> Some { Distance = ctx.Distance; Api = api; AssemblyName = dic.AssemblyName }
      | _ -> None
    )

  append options builderResults apiResults