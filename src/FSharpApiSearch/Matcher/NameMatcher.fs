module internal FSharpApiSearch.NameMatcher

open System.Diagnostics
open FSharpApiSearch.MatcherTypes

let test' stringEquality expectedName actualName =
  if not (List.length expectedName <= List.length actualName) then
    false
  else
    Seq.zip expectedName actualName
    |> Seq.forall (fun (expected, actual) ->
      match expected with
      | "*" -> true // wildcard
      | _ -> stringEquality expected actual.InternalFSharpName
    )

let test stringEquality query (api: Api) ctx =
  match query with
  | QueryMethod.ByName (expectedName, _) ->
    match api.Name, api.Kind with
    | DisplayName actualName, ApiKind.Constructor ->
      let name_type = List.tail actualName
      let name_new = actualName
      let name_ctor = { FSharpName = ".ctor"; InternalFSharpName = ".ctor"; GenericParametersForDisplay = [] } :: name_type
      let ok = [ name_new; name_type; name_ctor ] |> List.exists (test' stringEquality expectedName)
      if ok then Matched ctx else Failure
    | DisplayName actualName, _ -> if test' stringEquality expectedName actualName then Matched ctx else Failure
    | _ -> Failure
  | _ -> Matched ctx
let instance (options: SearchOptions) =
  let stringEquality =
    match options.IgnoreCase with
    | Enabled -> String.equalsIgnoreCase
    | Disabled -> String.equals
  { new IApiMatcher with
      member this.Name = "Name Matcher"
      member this.Test lowTypeMatcher query api ctx = test stringEquality query api ctx }