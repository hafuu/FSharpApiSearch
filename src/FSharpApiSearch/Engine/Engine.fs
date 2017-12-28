module FSharpApiSearch.Engine

open System.Diagnostics
open FSharpApiSearch.EngineTypes
open FSharpApiSearch.Printer
open FSharp.Collections.ParallelSeq

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
    Failure

let private choose (options: SearchOptions) f xs=
  match options.Parallel with
  | Enabled -> PSeq.choose f xs :> seq<_>
  | Disabled -> Seq.choose f xs

let internal search' (targets: ApiDictionary seq) (options: SearchOptions) (lowTypeMatcher: ILowTypeMatcher) (apiMatchers: IApiMatcher[]) (query: Query) (initialContext: Context) =
  targets
  |> Seq.collect (fun dic -> dic.Api |> Seq.map (fun api -> (dic, api)))
  |> choose options (fun (dic, api) ->
    match test lowTypeMatcher apiMatchers query initialContext api with
    | Matched ctx -> Some { Distance = ctx.Distance; Api = api; AssemblyName = dic.AssemblyName; MatchPositions = ctx.MatchPositions }
    | _ -> None
  )

let internal storategy options =
  match options.Language with
  | FSharp -> EngineInitializer.FSharpInitializeStorategy() :> EngineInitializer.IInitializeStorategy
  | CSharp -> EngineInitializer.CSharpInitializeStorategy() :> EngineInitializer.IInitializeStorategy

let search (dictionaries: ApiDictionary[]) (options: SearchOptions) (targets: ApiDictionary seq) (queryStr: string) : Query * seq<Result> =
  let storategy = storategy options
  let query = storategy.InitializeQuery(storategy.ParseQuery(queryStr), dictionaries, options)
  let lowTypeMatcher, apiMatchers = storategy.Matchers(options, query)
  let initialContext = storategy.InitialContext(query, dictionaries, options)

  let results =
    match query.Method with
    | QueryMethod.ByComputationExpression ceQuery -> ComputationExpressionMatcher.search options targets lowTypeMatcher ceQuery initialContext
    | _ -> search' targets options lowTypeMatcher apiMatchers query initialContext

  (query, results)