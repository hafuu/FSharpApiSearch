module internal FSharpApiSearch.NameOrSignatureMatcher

open MatcherTypes

let instance (options: SearchOptions) =
  let nameMatcher = NameMatcher.instance options
  let sigMatcher = SignatureMatcher.instance options
  { new IApiMatcher with
      member this.Name = "Name or Signature Matcher"
      member this.Test lowTypeMatcher query api ctx =
        let nameResult = ApiMatcher.test lowTypeMatcher nameMatcher query api ctx
        match nameResult with
        | Matched _ as m -> m
        | _ -> ApiMatcher.test lowTypeMatcher sigMatcher query api ctx
  }