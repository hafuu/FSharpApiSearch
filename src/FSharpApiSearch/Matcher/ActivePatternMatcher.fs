module internal FSharpApiSearch.ActivePatternMatcher

open System.Diagnostics
open FSharpApiSearch.MatcherTypes

let testAllParameter (lowTypeMatcher: ILowTypeMatcher) activePatternType returnType (right: LowType) ctx =
  let leftElems = [ activePatternType; returnType ]
  let rightElems =
    match right with
    | Arrow args when args.Length >= 2 -> args.[(args.Length - 2)..(args.Length - 1)]
    | _ -> failwith "invalid active pattern arguments."
  LowTypeMatcher.Rules.testAll lowTypeMatcher leftElems rightElems ctx

let testSpecifiedParameter (lowTypeMatcher: ILowTypeMatcher) left right ctx = lowTypeMatcher.Test left right ctx

let test (lowTypeMatcher: ILowTypeMatcher) (query: ActivePatternQuery) (api: Api) ctx =
  match api.Signature with
  | ApiSignature.ActivePatten (kind, targetSig) when query.Kind = kind ->
    match query.Signature with
    | ActivePatternSignature.AnyParameter (activePatternType, returnType) -> testAllParameter lowTypeMatcher activePatternType returnType targetSig ctx
    | ActivePatternSignature.Specified querySig' -> testSpecifiedParameter lowTypeMatcher querySig' targetSig ctx
  | _ -> Failure

let instance (_: SearchOptions) =
  { new IApiMatcher with
      member this.Name = "Active Pattern Matcher"
      member this.Test lowTypeMatcher query api ctx =
        match query with
        | QueryMethod.ByActivePattern activePatternQuery -> test lowTypeMatcher activePatternQuery api ctx
        | _ -> Matched ctx }