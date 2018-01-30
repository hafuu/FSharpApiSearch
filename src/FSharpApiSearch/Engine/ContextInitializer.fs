module internal FSharpApiSearch.ContextInitializer

open EngineTypes

let collectFromSignatureQuery (|Target|_|) query =
  let results = ResizeArray()
  let add x = results.Add(x)

  let rec f = function
    | Target x -> add x
    | Identifier _ | Wildcard _ | Variable _ -> ()
    | Arrow ((ps, ret), _) -> List.iter f ps; f ret
    | Tuple ({ Elements = xs }, _) -> List.iter f xs
    | Generic (id, args, _) -> f id; List.iter f args
    | TypeAbbreviation ({ Original = o }, _) -> f o
    | ByRef (_, t, _) -> f t
    | Choice (_, xs, _) -> List.iter f xs
    | Delegate (t, _, _) -> f t
    | LowType.Subtype (t, _) -> f t
    | LoadingType _ -> Name.loadingNameError()

  match query with
  | { Query.Method = QueryMethod.ByName (_, sigQuery) }
  | { Query.Method = QueryMethod.BySignature sigQuery }
  | { Query.Method = QueryMethod.ByNameOrSignature (_, sigQuery) }->
    match sigQuery with
    | SignatureQuery.Wildcard -> ()
    | SignatureQuery.Signature lt -> f lt
  | { Query.Method = QueryMethod.ByActivePattern apQuery } ->
    match apQuery with
    | { ActivePatternQuery.Signature = ActivePatternSignature.AnyParameter (x, y) } -> f x; f y
    | { ActivePatternQuery.Signature = ActivePatternSignature.Specified x } -> f x
  | { Query.Method = QueryMethod.ByComputationExpression ceQuery } -> f ceQuery.Type

  results |> Seq.distinct |> Seq.toList
    
let collectVariables = collectFromSignatureQuery (function Variable _ as v -> Some v | _ -> None)
let collectWildcardGroups = collectFromSignatureQuery (function Wildcard (Some _, _) as w -> Some w | _ -> None)
let collectUserInputTypes = collectFromSignatureQuery (function Identifier (UserInputType id, _) -> Some id | _ -> None)

let initialEquations options query eqs =
  match options.RespectNameDifference with
  | Enabled ->
    let variables = collectVariables query
    let wildcards = collectWildcardGroups query
    let inequalities =
      [
        for x in variables do
          for y in variables do
            if x < y then yield (x, y)

        for x in wildcards do
          for y in wildcards do
            if x < y then yield (x, y)
      ]
    { eqs with Inequalities = inequalities }
  | Disabled -> eqs

let queryTypes query (dictionaries: ApiDictionary[]) =
  collectUserInputTypes query
  |> Seq.map (fun id ->
    let types =
      dictionaries
      |> Seq.collect (fun d -> d.TypeDefinitions.Values)
      |> Seq.filter (fun td -> TypeNameEquality.sameName (UserInputType id) (ConcreteType td.ConcreteType) = Ok 0)
      |> Seq.toArray
    (id, types)
  )
  |> Map.ofSeq

let initializeContext (dictionaries: ApiDictionary[]) (options: SearchOptions) (query: Query) =
  {
    Distance = 0
    Equations = Equations.empty |> initialEquations options query
    MatchPositions = Map.empty
    QueryTypes = queryTypes query dictionaries
    ApiDictionaries = dictionaries |> Seq.map (fun d -> (d.AssemblyName, d)) |> dict
    SubtypeCache = SubtypeCache.create()
  }