module internal FSharpApiSearch.NonPublicFilter

open EngineTypes

let testAccessibility ctx = function
  | Public -> Matched ctx
  | Private -> Failure FailureInfo.None

let test api ctx =
  match api.Signature with
  | ApiSignature.FullTypeDefinition { Accessibility = accessibility }
  | ApiSignature.TypeAbbreviation { Accessibility = accessibility }
  | ApiSignature.ModuleDefinition { Accessibility = accessibility } -> testAccessibility ctx accessibility
  | _ -> Matched ctx

let instance (_: SearchOptions) =
  { new IApiMatcher with
      member this.Name = "Non Public Filter"
      member this.Test lowTypeMatcher query api ctx = test api ctx }