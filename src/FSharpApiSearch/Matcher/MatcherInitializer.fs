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
    ]
    |> List.map (fun f -> f options)
  (lowTypeMatcher, apiMatchers)

let collectFromSignatureQuery getTarget query =
  let (|Target|_|) t = getTarget t
  let rec f = function
    | Target x -> Seq.singleton x
    | Arrow xs -> Seq.collect f xs
    | Tuple xs -> Seq.collect f xs
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

let replaceTypeAbbreviation (dictionaries: ApiDictionary seq) (query: Query) =
  let table = dictionaries |> Seq.collect (fun x -> x.TypeAbbreviations) |> Seq.filter (fun t -> t.Accessibility = Public) |> Seq.map (fun t -> t.TypeAbbreviation) |> Seq.toList
  let rec replace = function
    | Identity id as i ->
      let replacement = table |> List.tryFindBack (function { Abbreviation = Identity abbId } -> Identity.sameName abbId id | _ -> false)
      match replacement with
      | Some replacement -> TypeAbbreviation { Abbreviation = i; Original = replacement.Original }
      | None -> i
    | Generic (Identity id, args) as generic ->
      let replacedArgs = args |> List.map replace
      let idReplacement = table |> List.tryFindBack (function { Abbreviation = Generic (Identity abbId, _) } -> Identity.sameName abbId id | _ -> false)
      match idReplacement with
      | Some { Abbreviation = Generic (_, abbArgs); Original = original } ->
        let applyTable =
          List.zip abbArgs replacedArgs
          |> List.map (function Variable (_, v), arg -> (v, arg) | _ -> failwith "Parameters of generic type abbreviation should be variable.")
          |> Map.ofList
        let replacedGeneric = LowType.applyVariable VariableSource.Target applyTable original
        TypeAbbreviation { Abbreviation = generic; Original = replacedGeneric }
      | Some _ -> generic
      | None -> Generic (Identity id, replacedArgs)
    | Generic (id, args) ->
      let replacedArgs = args |> List.map replace
      Generic (id, replacedArgs)
    | Arrow xs -> Arrow (List.map replace xs)
    | Tuple xs -> Tuple (List.map replace xs)
    | other -> other
  let replaceSignatureQuery = function
    | SignatureQuery.Wildcard -> SignatureQuery.Wildcard
    | SignatureQuery.Signature lt -> SignatureQuery.Signature (replace lt)
    | SignatureQuery.InstanceMember (receiver, args, returnType) -> SignatureQuery.InstanceMember (replace receiver, List.map replace args, replace returnType)
  let replaceActivePatternSignature = function
    | ActivePatternSignature.AnyParameter (x, y) -> ActivePatternSignature.AnyParameter (replace x, replace y)
    | ActivePatternSignature.Specified x -> ActivePatternSignature.Specified (replace x)
  match query with
  | { Method = QueryMethod.ByName (name, sigQuery) } -> { query with Method = QueryMethod.ByName (name, replaceSignatureQuery sigQuery) }
  | { Method = QueryMethod.BySignature sigQuery } -> { query with Method = QueryMethod.BySignature (replaceSignatureQuery sigQuery) }
  | { Method = QueryMethod.ByActivePattern apQuery } -> { query with Method = QueryMethod.ByActivePattern { apQuery with Signature = replaceActivePatternSignature apQuery.Signature } }
          

let initializeQuery (dictionaries: ApiDictionary seq) (query: Query) =
  query
  |> replaceTypeAbbreviation dictionaries