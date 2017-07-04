module internal FSharpApiSearch.ActivePatternMatcher

open System.Diagnostics
open FSharpApiSearch.MatcherTypes

let testAllParameter (lowTypeMatcher: ILowTypeMatcher) activePatternType returnType (right: Function) ctx =
  let left = Arrow ([ activePatternType ], returnType)
  let right =
    let ps, ret = Function.toArrow right
    Arrow ([ List.last ps ], ret)
  lowTypeMatcher.Test left right ctx

let test (lowTypeMatcher: ILowTypeMatcher) (query: ActivePatternQuery) (api: Api) ctx =
  match api.Signature with
  | ApiSignature.ActivePatten (kind, right) when query.Kind = kind ->
    match query.Signature with
    | ActivePatternSignature.AnyParameter (activePatternType, returnType) ->
      testAllParameter lowTypeMatcher activePatternType returnType right ctx
    | ActivePatternSignature.Specified left ->
      let right = Arrow (Function.toArrow right)
      lowTypeMatcher.Test left right ctx
  | _ -> Failure

let instance (_: SearchOptions) =
  { new IApiMatcher with
      member this.Name = "Active Pattern Matcher"
      member this.Test lowTypeMatcher query api ctx =
        match query.Method with
        | QueryMethod.ByActivePattern activePatternQuery -> test lowTypeMatcher activePatternQuery api ctx
        | _ -> Matched ctx }