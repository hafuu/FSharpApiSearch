module internal FSharpApiSearch.PublishedApiFilter

open System.Diagnostics
open FSharpApiSearch.MatcherTypes

let test (api: Api) ctx =
  match api.Signature with
  | ApiSignature.TypeAbbreviation _ -> Failure
  | _ -> Matched ctx

let instance (_: SearchOptions) =
  { new IApiMatcher with
      member this.Name = "Published Api Filter"
      member this.Test lowTypeMatcher query api ctx = test api ctx }