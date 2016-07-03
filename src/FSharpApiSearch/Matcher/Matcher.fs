module FSharpApiSearch.Matcher

open System.Diagnostics
open FSharpApiSearch.MatcherTypes

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

let search (dictionaries: ApiDictionary[]) (options: SearchOptions) (targets: ApiDictionary seq) (queryStr: string) =
  let lowTypeMatcher, apiMatchers = MatcherInitializer.matchers options
  let query = QueryParser.parse queryStr |> MatcherInitializer.initializeQuery dictionaries
  let initialContext = MatcherInitializer.initializeContext dictionaries options query
  seq {
    for dic in targets do
      for api in dic.Api do
        match test lowTypeMatcher apiMatchers query initialContext api with
        | Matched ctx -> yield { Distance = ctx.Distance; Api = api; AssemblyName = dic.AssemblyName }
        | _ -> ()
  }
  |> Seq.cache