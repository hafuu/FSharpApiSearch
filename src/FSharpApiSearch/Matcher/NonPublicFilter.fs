module internal FSharpApiSearch.NonPublicFilter

open MatcherTypes

let test api ctx =
  match api.Signature with
  | ApiSignature.FullTypeDefinition { Accessibility = accessibility } ->
    match accessibility with
    | Accessibility.Public -> Matched ctx
    | Accessibility.Private -> Failure
  | _ -> Matched ctx

let instance (_: SearchOptions) =
  { new IApiMatcher with
      member this.Name = "Non Public Filter"
      member this.Test lowTypeMatcher query api ctx = test api ctx }