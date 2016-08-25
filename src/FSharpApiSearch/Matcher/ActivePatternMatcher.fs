module internal FSharpApiSearch.ActivePatternMatcher

open System.Diagnostics
open FSharpApiSearch.MatcherTypes

let testAllParameter (lowTypeMatcher: ILowTypeMatcher) activePatternType returnType (right: Function) ctx =
  let left = Arrow [ activePatternType; returnType ]
  let rightElems =
    if right.Length >= 2 then
      right.[(right.Length - 2)..(right.Length - 1)]
    else
      failwith "invalid active pattern arguments."
  let right = Function.toArrow rightElems
  lowTypeMatcher.Test left right ctx

let test (lowTypeMatcher: ILowTypeMatcher) (query: ActivePatternQuery) (api: Api) ctx =
  match api.Signature with
  | ApiSignature.ActivePatten (kind, right) when query.Kind = kind ->
    match query.Signature with
    | ActivePatternSignature.AnyParameter (activePatternType, returnType) ->
      testAllParameter lowTypeMatcher activePatternType returnType right ctx
    | ActivePatternSignature.Specified left ->
      let right = Function.toArrow right
      lowTypeMatcher.Test left right ctx
  | _ -> Failure

let instance (_: SearchOptions) =
  { new IApiMatcher with
      member this.Name = "Active Pattern Matcher"
      member this.Test lowTypeMatcher query api ctx =
        match query with
        | QueryMethod.ByActivePattern activePatternQuery -> test lowTypeMatcher activePatternQuery api ctx
        | _ -> Matched ctx }