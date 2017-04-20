module internal FSharpApiSearch.CSharpFilter

open MatcherTypes

let test api ctx =
  match api.Signature with
  | ApiSignature.ModuleValue _
  | ApiSignature.ModuleFunction _
  | ApiSignature.InstanceMember _
  | ApiSignature.StaticMember _
  | ApiSignature.Constructor _
  | ApiSignature.FullTypeDefinition _
  | ApiSignature.ExtensionMember _ -> Matched ctx

  | ApiSignature.ActivePatten _
  | ApiSignature.ModuleDefinition _
  | ApiSignature.TypeAbbreviation _
  | ApiSignature.TypeExtension _
  | ApiSignature.UnionCase _
  | ApiSignature.ComputationExpressionBuilder _ -> Failure

let instance (_: SearchOptions) =
  { new IApiMatcher with
      member this.Name = "CSharp Filter"
      member this.Test lowTypeMatcher query api ctx = test api ctx }