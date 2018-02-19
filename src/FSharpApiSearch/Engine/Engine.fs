module FSharpApiSearch.Engine

open System.Diagnostics
open FSharpApiSearch.EngineTypes
open FSharpApiSearch.Printer

let internal test (lowTypeMatcher: ILowTypeMatcher) (apiMatchers: IApiMatcher[]) (query: Query) (ctx: Context) (api: Api) =
  let mutable continue' = true
  let mutable state = ctx
  let mutable index = 0

  while continue' && index < apiMatchers.Length do
    let m = apiMatchers.[index]
    index <- index + 1

    let result = ApiMatcher.test lowTypeMatcher m query api state

    match result with
    | Matched ctx -> state <- ctx
    | _ -> continue' <- false
  
  if continue' then
    Matched state
  else
    Failure FailureInfo.None

let internal search' (seqFunc: SeqFunctions) (targets: ApiDictionary seq) (lowTypeMatcher: ILowTypeMatcher) (apiMatchers: IApiMatcher[]) (query: Query) (initialContext: Context) =
  targets
  |> Seq.collect (fun dic -> dic.Api |> Seq.map (fun api -> (dic, api)))
  |> seqFunc.Choose (fun (dic, api) ->
    match test lowTypeMatcher apiMatchers query initialContext api with
    | Matched ctx -> Some { Distance = ctx.Distance; Api = api; AssemblyName = dic.AssemblyName; MatchPositions = ctx.MatchPositions }
    | _ -> None
  )

let search (dictionaries: ApiDictionary[]) (options: SearchOptions) (targets: ApiDictionary seq) (queryStr: string) : Query * seq<Result> =
  let strategy = EngineStrategy.create options
  let query = strategy.InitializeQuery(strategy.ParseQuery(queryStr), dictionaries)
  let lowTypeMatcher, apiMatchers = strategy.Matchers(query)
  let initialContext = strategy.InitialContext(query, dictionaries)

  let results =
    match query.Method with
    | QueryMethod.ByComputationExpression ceQuery -> ComputationExpressionMatcher.search strategy.SeqFunctions targets lowTypeMatcher ceQuery initialContext
    | _ -> search' strategy.SeqFunctions targets lowTypeMatcher apiMatchers query initialContext

  (query, results)