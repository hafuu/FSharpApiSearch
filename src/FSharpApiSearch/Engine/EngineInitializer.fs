module internal FSharpApiSearch.EngineInitializer

open System.Diagnostics
open FSharpApiSearch.EngineTypes

let buildMatchers options apiMatchers =
  let lowTypeMatcher = LowTypeMatcher.instance options
  let apiMatchers = apiMatchers |> Array.map (fun f -> f options)
  (lowTypeMatcher, apiMatchers)

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
      |> Seq.filter (fun td -> TypeNameEquality.sameName (UserInputType id) (ConcreteType td.ConcreteType) = TypeNameEquality.Result.Matched)
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

let private replaceTypeAbbreviation' nameEquality (table: TypeAbbreviation list) (query: Query) =
  let rec replace = function
    | Identifier (id, pos) as i ->
      let replacements = table |> List.filter (function { Abbreviation = Identifier (abbId, _) } -> nameEquality abbId id | _ -> false)
      match replacements with
      | [] -> i
      | _ ->
        let choices = [
          for r in replacements do
            yield TypeAbbreviation.create ({ Abbreviation = i; Original = r.Original })
          yield i
        ]
        Choice (i, choices, pos)
    | Generic (Identifier (id, _) as originalId, args, genPos) as generic ->
      let types =
        let replacedArgs = args |> List.map replace
        let idReplacements = table |> List.filter (function { Abbreviation = Generic (Identifier (abbId, _), _, _) } -> nameEquality abbId id | _ -> false)
        [
          for idRep in idReplacements do
            match idRep with
            | { Abbreviation = Generic (_, abbArgs, _); Original = original } ->
              let applyTable =
                List.zip abbArgs replacedArgs
                |> List.map (function Variable (_, v, _), arg -> (v, arg) | _ -> failwith "Parameters of generic type abbreviation should be variable.")
                |> Map.ofList
              let replacedGeneric = LowType.applyVariable VariableSource.Target applyTable original
              yield TypeAbbreviation.create { Abbreviation = generic; Original = replacedGeneric }
            | _ -> failwith "It is not a generic type abbreviation."

          if SpecialTypes.Identifier.tuples |> List.exists (fun tpl -> nameEquality tpl id) then
            yield Tuple.create { Elements = replacedArgs; IsStruct = false }

          if SpecialTypes.Identifier.valueTuples |> List.exists (fun tpl -> nameEquality tpl id) then
            yield Tuple.create { Elements = replacedArgs; IsStruct = true }

          if nameEquality SpecialTypes.Identifier.byref id then
            yield ByRef.create (false, replacedArgs.Head)

          yield Generic (originalId, replacedArgs, genPos)
        ]

      match types with
      | [ one ] -> one
      | many -> Choice (generic, many, genPos)

    | Generic (id, args, pos) ->
      let replacedArgs = args |> List.map replace
      Generic (id, replacedArgs, pos)
    | Arrow ((ps, ret), pos) -> Arrow ((List.map replace ps, replace ret), pos)
    | Tuple (x, pos) -> Tuple ({ x with Elements = List.map replace x.Elements }, pos)
    | LowType.Subtype (t, pos) -> LowType.Subtype (replace t, pos)
    | ByRef _ as x -> x
    | Choice _ as x -> x
    | Delegate _ as x -> x
    | TypeAbbreviation _ as x -> x
    | Variable _ as x -> x
    | Wildcard _ as x -> x
    | LoadingType _ -> Name.loadingNameError()
    
  let replaceSignatureQuery = function
    | SignatureQuery.Wildcard -> SignatureQuery.Wildcard
    | SignatureQuery.Signature lt -> SignatureQuery.Signature (replace lt)
  let replaceActivePatternSignature = function
    | ActivePatternSignature.AnyParameter (x, y) -> ActivePatternSignature.AnyParameter (replace x, replace y)
    | ActivePatternSignature.Specified x -> ActivePatternSignature.Specified (replace x)
  let replaceComputationExpressionQuery (ce: ComputationExpressionQuery) = { ce with Type = replace ce.Type }

  match query with
  | { Method = QueryMethod.ByName (name, sigQuery) } -> { query with Method = QueryMethod.ByName (name, replaceSignatureQuery sigQuery) }
  | { Method = QueryMethod.BySignature sigQuery } -> { query with Method = QueryMethod.BySignature (replaceSignatureQuery sigQuery) }
  | { Method = QueryMethod.ByNameOrSignature (name, sigQuery) } -> { query with Method = QueryMethod.ByNameOrSignature (name, replaceSignatureQuery sigQuery) }
  | { Method = QueryMethod.ByActivePattern apQuery } -> { query with Method = QueryMethod.ByActivePattern { apQuery with Signature = replaceActivePatternSignature apQuery.Signature } }
  | { Method = QueryMethod.ByComputationExpression ceQuery } -> { query with Method = QueryMethod.ByComputationExpression (replaceComputationExpressionQuery ceQuery) }

let replaceTypeAbbreviation (table: TypeAbbreviation list) (options: SearchOptions) (query: Query) =
  let equality x y = TypeNameEquality.equalityFromOptions options x y = TypeNameEquality.Result.Matched
  replaceTypeAbbreviation' equality table query

let typeAbbreviationTableFromApiDictionary (dictionaries: ApiDictionary seq) =
  dictionaries |> Seq.collect (fun x -> x.TypeAbbreviations) |> Seq.filter (fun t -> t.Accessibility = Public) |> Seq.map (fun t -> t.TypeAbbreviation) |> Seq.toList

let singleTypeAsNameQuery (query: Query) =
  let isSymbolName (x: NameItem) = match x.Name with SymbolName _ -> true | _ -> false
  let (|AsNameQuery|_|) (t: LowType) =
    match t with
    | Identifier (UserInputType pi, _) -> Some pi.Name
    | Generic (Identifier (UserInputType pi, _), args, _) when args |> List.forall (function Variable _ -> true | _ -> false) -> Some pi.Name
    | _ -> None
    |> Option.filter (List.forall isSymbolName)
  let method =
    match query.Method with
    | QueryMethod.BySignature (SignatureQuery.Signature (AsNameQuery name) as bySig) ->
      let expected =
        name
        |> List.map (fun n ->
          {
            Expected = match n.Name with SymbolName s -> s | _ -> failwith "It is not symbol name."
            GenericParameters = n.GenericParameters |> List.map (fun v -> v.Name)
            MatchMethod = NameMatchMethod.StringCompare
          }
        )
      QueryMethod.ByNameOrSignature (expected, bySig)
    | QueryMethod.BySignature _ as x -> x
    | QueryMethod.ByName _ as x -> x
    | QueryMethod.ByNameOrSignature _ as x -> x
    | QueryMethod.ByActivePattern _ as x -> x
    | QueryMethod.ByComputationExpression _ as x -> x
  { query with Method = method }

let shortLetterAsVariable (threshold: int) (query: Query) =
  let rec update = function
    | Identifier (UserInputType { Name = [ { Name = SymbolName name } ] }, pos) when name.Length <= threshold ->
      Variable (VariableSource.Query, { Name = name; IsSolveAtCompileTime = false }, pos)
    | Identifier _ as i -> i
    | Wildcard _ as w -> w
    | Variable _ as v -> v
    | Arrow ((ps, ret), pos) -> let arrow = List.map update ps, update ret in Arrow (arrow, pos)
    | Tuple (tpl, pos) -> Tuple ({ tpl with Elements = List.map update tpl.Elements }, pos)
    | Generic (id, ps, pos) -> Generic (update id, List.map update ps, pos)
    | TypeAbbreviation _ as t -> t
    | Delegate _ as d -> d
    | ByRef _ as b -> b
    | LowType.Subtype (Generic (id, ps, genPos), subPos) -> LowType.Subtype (Generic (id, List.map update ps, genPos), subPos)
    | LowType.Subtype _ as s -> s
    | Choice (original, xs, pos) -> Choice (original, List.map update xs, pos)
    | LoadingType _ -> Name.loadingNameError()

  LowTypeVisitor.accept_Query update query

let queryPosition query =
  let posId = ref 0
  let pos oldPos =
    match oldPos with
    | AtQuery (_, range) ->
      let newPosId = !posId
      incr posId
      AtQuery (Some (QueryId newPosId), range)
    | AtSignature _ | Unknown -> oldPos
  
  let updateTuple pos tpl = { tpl with Elements = List.map (LowType.setPosition pos) tpl.Elements }

  let update pos (lowType: LowType) =
    match lowType with
    | Tuple (tpl, tplPos) -> Tuple (updateTuple pos tpl, tplPos)
    | Arrow ((args, ret), arrowPos) ->
      let args =
        args
        |> List.map (function
          | Tuple (tpl, tplPos) -> Tuple (updateTuple pos tpl, tplPos)
          | other -> LowType.setPosition pos other)
      let ret = LowType.setPosition pos ret
      Arrow ((args, ret), arrowPos)
    | Choice (original, choices, choicePos) ->
      let position = pos choicePos
      let choices = List.map (LowType.setPosition (fun _ -> position)) choices
      Choice (original, choices, position)
    | Wildcard _ | Variable _ | Identifier _ | Generic _ | TypeAbbreviation _ | Delegate _ | ByRef _ | LowType.Subtype _ | LoadingType _ -> LowType.setPosition pos lowType
  
  LowTypeVisitor.accept_Query (update pos) query

type IInitializeStorategy =
  abstract Matchers: SearchOptions * Query -> ILowTypeMatcher * IApiMatcher[]
  abstract ParseQuery: string -> Query
  abstract InitializeQuery: Query * ApiDictionary[] * SearchOptions -> Query
  abstract InitialContext: Query * ApiDictionary[] * SearchOptions -> Context

type FSharpInitializeStorategy() =
  interface IInitializeStorategy with
    member this.Matchers(options, query) =
      [|
        match query.Method with
        | QueryMethod.ByName _ ->
          yield NameMatcher.instance
          yield SignatureMatcher.instance
        | QueryMethod.BySignature _ ->
          yield SignatureMatcher.instance
        | QueryMethod.ByNameOrSignature _ ->
          yield NameOrSignatureMatcher.instance
        | QueryMethod.ByActivePattern _ ->
          yield ActivePatternMatcher.instance
        | QueryMethod.ByComputationExpression _ ->
          yield ComputationExpressionMatcher.Filter.instance

        match options.GreedyMatching with
        | Enabled ->
          yield ConstraintSolver.instance
        | Disabled -> ()
      |]
      |> buildMatchers options
    member this.ParseQuery(queryStr) = QueryParser.FSharp.parse queryStr
    member this.InitializeQuery(query, dictionaries, options) =
      let table = typeAbbreviationTableFromApiDictionary dictionaries
      query
      |> shortLetterAsVariable options.ShortLetterAsVariable
      |> singleTypeAsNameQuery
      |> replaceTypeAbbreviation table options
      |> queryPosition
    member this.InitialContext(query, dictionaries, options) = initializeContext dictionaries options query

let csharpAliases =
  SpecialTypes.Identifier.CSharp.aliases
  |> List.map (fun (alias, original) ->
    let alias = Identifier.create (UserInputType { Name = Name.ofString alias })
    let original = Identifier.create original
    { Abbreviation = alias; Original = original })

type CSharpInitializeStorategy() =
  interface IInitializeStorategy with
    member this.Matchers(options, query) =
      [|
        yield CSharpFilter.instance

        match query.Method with
        | QueryMethod.ByName _ ->
          yield NameMatcher.instance
          yield SignatureMatcher.instance
        | QueryMethod.BySignature _ ->
          yield SignatureMatcher.instance
        | QueryMethod.ByNameOrSignature _ ->
          yield NameOrSignatureMatcher.instance
        | QueryMethod.ByActivePattern _ -> ()
        | QueryMethod.ByComputationExpression _ -> ()
      |]
      |> buildMatchers options
    member this.ParseQuery(queryStr) = QueryParser.CSharp.parse queryStr
    member this.InitializeQuery(query, _, options) =
      query
      |> shortLetterAsVariable options.ShortLetterAsVariable
      |> singleTypeAsNameQuery
      |> replaceTypeAbbreviation csharpAliases options
      |> queryPosition
    member this.InitialContext(query, dictionaries, options) = initializeContext dictionaries options query