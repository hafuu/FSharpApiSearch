module FSharpApiSearch.Matcher

open System.Diagnostics
open FSharpApiSearch.MatcherTypes
open FSharp.Collections.ParallelSeq

let internal test (lowTypeMatcher: ILowTypeMatcher) (apiMatchers: IApiMatcher list) (query: Query) (ctx: Context) (api: Api) =
  apiMatchers
  |> Seq.fold (fun state m ->
    match state with
    | Matched ctx ->
      Debug.WriteLine(sprintf "Test \"%s\" and \"%s\" by %s. Equations: %s"
        query.OriginalString
        (ApiSignature.debug api.Signature)
        m.Name
        (Equations.debug ctx.Equations))
      Debug.Indent()
      let result = m.Test lowTypeMatcher query.Method api ctx
      Debug.Unindent()
      result
    | _ -> Failure
  ) (Matched ctx)

let private choose (options: SearchOptions) f xs=
  match options.Parallel with
  | Enabled -> PSeq.choose f xs :> seq<_>
  | Disabled -> Seq.choose f xs

let internal search' (targets: ApiDictionary seq) (options: SearchOptions) (lowTypeMatcher: ILowTypeMatcher) (apiMatchers: IApiMatcher list) (query: Query) (initialContext: Context) =
  targets
  |> Seq.collect (fun dic -> dic.Api |> Seq.map (fun api -> (dic, api)))
  |> choose options (fun (dic, api) ->
    match test lowTypeMatcher apiMatchers query initialContext api with
    | Matched ctx -> Some { Distance = ctx.Distance; Api = api; AssemblyName = dic.AssemblyName }
    | _ -> None
  )

let search (dictionaries: ApiDictionary[]) (options: SearchOptions) (targets: ApiDictionary seq) (queryStr: string) =
  let lowTypeMatcher, apiMatchers = MatcherInitializer.matchers options
  let query = QueryParser.FSharp.parse queryStr |> MatcherInitializer.initializeQuery dictionaries options
  let initialContext = MatcherInitializer.initializeContext dictionaries options query

  match query.Method with
  | QueryMethod.ByComputationExpression ceQuery -> ComputationExpressionMatcher.search options targets lowTypeMatcher ceQuery initialContext
  | _ -> search' targets options lowTypeMatcher apiMatchers query initialContext