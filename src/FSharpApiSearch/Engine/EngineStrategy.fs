namespace FSharpApiSearch

open EngineTypes

type internal EngineStrategy =
  abstract Matchers: Query -> ILowTypeMatcher * IApiMatcher[]
  abstract ParseQuery: string -> Query
  abstract InitializeQuery: Query * ApiDictionary[] -> Query
  abstract InitialContext: Query * ApiDictionary[] -> Context
  abstract SeqFunctions: SeqFunctions

module internal EngineStrategy =

  let buildMatchers options apiMatchers =
    let lowTypeMatcher = LowTypeMatcher.instance options
    let apiMatchers = apiMatchers |> Array.map (fun f -> f options)
    (lowTypeMatcher, apiMatchers)

  open QueryInitializer
  open ContextInitializer

  type FSharp(options: SearchOptions) =
    interface EngineStrategy with
      member this.Matchers(query) =
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
      member this.InitializeQuery(query, dictionaries) =
        let table = typeAbbreviationTableFromApiDictionary dictionaries
        initializeQuery options table query
      member this.InitialContext(query, dictionaries) = initializeContext dictionaries options query
      member val SeqFunctions = SeqFunctions.create options with get

  let csharpAliases =
    SpecialTypes.Identifier.CSharp.aliases
    |> List.map (fun (alias, original) ->
      let alias = Identifier.create (UserInputType { Name = Name.ofString alias })
      let original = Identifier.create original
      { Abbreviation = alias; Original = original })

  type CSharp(options: SearchOptions) =
    interface EngineStrategy with
      member this.Matchers(query) =
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
      member this.InitializeQuery(query, _) = initializeQuery options csharpAliases query
      member this.InitialContext(query, dictionaries) = initializeContext dictionaries options query
      member val SeqFunctions = SeqFunctions.create options with get

  let internal create options : EngineStrategy =
    match options.Language with
    | FSharp -> FSharp(options) :> _
    | CSharp -> CSharp(options) :> _