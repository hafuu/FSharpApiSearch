module internal FSharpApiSearch.NameMatcher

open System.Diagnostics
open FSharpApiSearch.MatcherTypes
open System.Text.RegularExpressions

let test' regexOption expectedName actualName =
  if not (List.length expectedName <= List.length actualName) then
    false
  else
    Seq.zip expectedName actualName
    |> Seq.forall (fun (expected : string, actual) ->
      let pattern = expected.Replace("*",".*")
      Regex.IsMatch(actual.InternalFSharpName, pattern, regexOption)
    )

let test regexOption query (api: Api) ctx =
  match query with
  | QueryMethod.ByName (expectedName, _) ->
    match api.Name, api.Kind with
    | DisplayName actualName, ApiKind.Constructor ->
      let name_type = List.tail actualName
      let name_new = actualName
      let name_ctor = { FSharpName = ".ctor"; InternalFSharpName = ".ctor"; GenericParametersForDisplay = [] } :: name_type
      let ok = [ name_new; name_type; name_ctor ] |> List.exists (test' regexOption expectedName)
      if ok then Matched ctx else Failure
    | DisplayName actualName, _ -> if test' regexOption expectedName actualName then Matched ctx else Failure
    | _ -> Failure
  | _ -> Matched ctx
let instance (options: SearchOptions) =
  let regexOption =
    match options.IgnoreCase with
    | Enabled -> RegexOptions.CultureInvariant ||| RegexOptions.IgnoreCase
    | Disabled -> RegexOptions.CultureInvariant
  { new IApiMatcher with
      member this.Name = "Name Matcher"
      member this.Test lowTypeMatcher query api ctx = test regexOption query api ctx }