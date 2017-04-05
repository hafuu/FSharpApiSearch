module internal FSharpApiSearch.MatcherInitializer

open System.Diagnostics
open FSharpApiSearch.MatcherTypes

let matchers options =
  let lowTypeMatcher = LowTypeMatcher.instance options
  let apiMatchers =
    [
      NameMatcher.instance
      SignatureMatcher.instance
      ActivePatternMatcher.instance
      ConstraintSolver.instance
      NonPublicFilter.instance
      ComputationExpressionMatcher.Filter.instance
    ]
    |> List.map (fun f -> f options)
  (lowTypeMatcher, apiMatchers)

let collectFromSignatureQuery getTarget query =
  let (|Target|_|) t = getTarget t
  let rec f = function
    | Target x -> Seq.singleton x
    | Arrow xs -> Seq.collect f xs
    | Tuple { Elements = xs } -> Seq.collect f xs
    | Generic (id, args) ->
      Seq.concat [
        f id
        Seq.collect f args
      ]
    | TypeAbbreviation { Original = o } -> f o
    | _ -> Seq.empty
  let results = 
    match query with
    | { Query.Method = QueryMethod.ByName (_, sigQuery) }
    | { Query.Method = QueryMethod.BySignature sigQuery } ->
      match sigQuery with
      | SignatureQuery.Wildcard -> Seq.empty
      | SignatureQuery.Signature lt -> f lt
      | SignatureQuery.InstanceMember (receiver, parameters, returnType) ->
        Seq.concat [
          f receiver
          Seq.collect f parameters
          f returnType
        ]
    | { Query.Method = QueryMethod.ByActivePattern apQuery } ->
      match apQuery with
      | { ActivePatternQuery.Signature = ActivePatternSignature.AnyParameter (x, y) } -> Seq.collect f [ x; y ]
      | { ActivePatternQuery.Signature = ActivePatternSignature.Specified x } -> f x
    | { Query.Method = QueryMethod.ByComputationExpression ceQuery } -> f ceQuery.Type

  results |> Seq.distinct |> Seq.toList
    
let collectVariables = collectFromSignatureQuery (function Variable _ as v -> Some v | _ -> None)
let collectWildcardGroups = collectFromSignatureQuery (function Wildcard (Some _) as w -> Some w | _ -> None)
let collectPartialIdentities = collectFromSignatureQuery (function Identity (PartialIdentity id) -> Some id | _ -> None)

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
  collectPartialIdentities query
  |> Seq.map (fun id ->
    let types = dictionaries |> Seq.collect (fun d -> d.TypeDefinitions) |> Seq.filter (fun td -> Identity.sameName (PartialIdentity id) (FullIdentity td.FullIdentity)) |> Seq.toArray
    (id, types)
  )
  |> Map.ofSeq

let initializeContext (dictionaries: ApiDictionary[]) (options: SearchOptions) (query: Query) =
  {
    Distance = 0
    Equations = Equations.empty |> initialEquations options query
    QueryTypes = queryTypes query dictionaries
    ApiDictionaries = dictionaries |> Seq.map (fun d -> (d.AssemblyName, d)) |> Map.ofSeq
  }

let replaceTypeAbbreviation nameEquality (dictionaries: ApiDictionary seq) (query: Query) =
  let table = dictionaries |> Seq.collect (fun x -> x.TypeAbbreviations) |> Seq.filter (fun t -> t.Accessibility = Public) |> Seq.map (fun t -> t.TypeAbbreviation) |> Seq.toList
  let rec replace = function
    | Identity id as i ->
      let replacements = table |> List.filter (function { Abbreviation = Identity abbId } -> nameEquality abbId id | _ -> false)
      match replacements with
      | [] -> i
      | _ ->
        Choice [
          for r in replacements do
            yield TypeAbbreviation { Abbreviation = i; Original = r.Original }
          yield i
        ]
    | Generic (Identity id, args) as generic ->
      let types =
        let replacedArgs = args |> List.map replace
        let idReplacements = table |> List.filter (function { Abbreviation = Generic (Identity abbId, _) } -> nameEquality abbId id | _ -> false)
        [
          for idRep in idReplacements do
            match idRep with
            | { Abbreviation = Generic (_, abbArgs); Original = original } ->
              let applyTable =
                List.zip abbArgs replacedArgs
                |> List.map (function Variable (_, v), arg -> (v, arg) | _ -> failwith "Parameters of generic type abbreviation should be variable.")
                |> Map.ofList
              let replacedGeneric = LowType.applyVariable VariableSource.Target applyTable original
              yield TypeAbbreviation { Abbreviation = generic; Original = replacedGeneric }
            | _ -> failwith "It is not a generic type abbreviation."

          if SpecialTypes.Identity.tuples |> List.exists (fun tpl -> nameEquality tpl id) then
            yield Tuple { Elements = replacedArgs; IsStruct = false }

          if SpecialTypes.Identity.valueTuples |> List.exists (fun tpl -> nameEquality tpl id) then
            yield Tuple { Elements = replacedArgs; IsStruct = true }

          yield Generic (Identity id, replacedArgs)
        ]

      match types with
      | [ one ] -> one
      | many -> Choice many

    | Generic (id, args) ->
      let replacedArgs = args |> List.map replace
      Generic (id, replacedArgs)
    | Arrow xs -> Arrow (List.map replace xs)
    | Tuple x -> Tuple { x with Elements = List.map replace x.Elements }
    | other -> other
  let replaceSignatureQuery = function
    | SignatureQuery.Wildcard -> SignatureQuery.Wildcard
    | SignatureQuery.Signature lt -> SignatureQuery.Signature (replace lt)
    | SignatureQuery.InstanceMember (receiver, args, returnType) -> SignatureQuery.InstanceMember (replace receiver, List.map replace args, replace returnType)
  let replaceActivePatternSignature = function
    | ActivePatternSignature.AnyParameter (x, y) -> ActivePatternSignature.AnyParameter (replace x, replace y)
    | ActivePatternSignature.Specified x -> ActivePatternSignature.Specified (replace x)
  let replaceComputationExpressionQuery (ce: ComputationExpressionQuery) = { ce with Type = replace ce.Type }

  match query with
  | { Method = QueryMethod.ByName (name, sigQuery) } -> { query with Method = QueryMethod.ByName (name, replaceSignatureQuery sigQuery) }
  | { Method = QueryMethod.BySignature sigQuery } -> { query with Method = QueryMethod.BySignature (replaceSignatureQuery sigQuery) }
  | { Method = QueryMethod.ByActivePattern apQuery } -> { query with Method = QueryMethod.ByActivePattern { apQuery with Signature = replaceActivePatternSignature apQuery.Signature } }
  | { Method = QueryMethod.ByComputationExpression ceQuery } -> { query with Method = QueryMethod.ByComputationExpression (replaceComputationExpressionQuery ceQuery) }
          

let initializeQuery (dictionaries: ApiDictionary seq) (options: SearchOptions) (query: Query) =
  query
  |> replaceTypeAbbreviation (Identity.equalityFromOptions options) dictionaries