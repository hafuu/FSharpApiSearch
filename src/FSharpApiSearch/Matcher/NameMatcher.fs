module internal FSharpApiSearch.NameMatcher

open System.Diagnostics
open FSharpApiSearch.MatcherTypes
open System.Text.RegularExpressions

let testCompare ignoreCase expected actual =
  match ignoreCase with
  | Enabled -> String.equalsIgnoreCase expected actual
  | Disabled -> String.equals expected actual

let testRegex ignoreCase (expected: string) (actual: string) =
  let regexOption =
    match ignoreCase with
    | Enabled -> RegexOptions.CultureInvariant ||| RegexOptions.IgnoreCase
    | Disabled -> RegexOptions.CultureInvariant
  Regex.IsMatch(actual, expected, regexOption)

let testAny _ _ _ = true

let test' ignoreCase (expected: ByName list) (actualNames: DisplayNameItem list) =
  if not (List.length expected <= List.length actualNames) then
    false
  else
    Seq.zip expected actualNames
    |> Seq.forall (fun (byName, actual: DisplayNameItem) ->
      let cmp =
        match byName.MatchMethod with
        | NameMatchMethod.StringCompare -> testCompare
        | NameMatchMethod.Regex -> testRegex
        | NameMatchMethod.Any -> testAny
      let actualName =
        match actual.Name with
        | SymbolName n -> n
        | OperatorName (_, n) -> n
        | WithCompiledName (n, _) -> n
      cmp ignoreCase byName.Expected actualName
    )

let test ignoreCase query (api: Api) ctx =
  match query with
  | QueryMethod.ByName (expected, _) ->
    match api.Name, api.Kind with
    | DisplayName actualName, ApiKind.Constructor ->
      let name_type = List.tail actualName
      let name_new = actualName
      let name_ctor = { Name = SymbolName ".ctor"; GenericParameters = [] } :: name_type
      let ok = [ name_new; name_type; name_ctor ] |> List.exists (test' ignoreCase expected)
      if ok then Matched ctx else Failure
    | DisplayName actualName, _ -> if test' ignoreCase expected actualName then Matched ctx else Failure
    | _ -> Failure
  | _ -> Matched ctx

let instance (options: SearchOptions) =
  { new IApiMatcher with
      member this.Name = "Name Matcher"
      member this.Test lowTypeMatcher query api ctx = test options.IgnoreCase query api ctx }