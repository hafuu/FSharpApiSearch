module internal FSharpApiSearch.NameMatcher

open System.Diagnostics
open FSharpApiSearch.MatcherTypes
open System.Text.RegularExpressions
open System

type StringOptions = {
  StringComparer: StringComparer
  StringComparison: StringComparison
  RegexOptions: RegexOptions
}

let stringOptions ignoreCase =
  match ignoreCase with
  | Enabled -> { StringComparer = StringComparer.InvariantCultureIgnoreCase; StringComparison = StringComparison.InvariantCultureIgnoreCase; RegexOptions = RegexOptions.CultureInvariant ||| RegexOptions.IgnoreCase }
  | Disabled -> { StringComparer = StringComparer.InvariantCulture; StringComparison = StringComparison.InvariantCulture; RegexOptions = RegexOptions.CultureInvariant }

let test' strOpt (expected: ByName list) (actualNames: DisplayNameItem list) =
  if not (List.length expected <= List.length actualNames) then
    false
  else
    Seq.zip expected actualNames
    |> Seq.forall (fun (byName, actual: DisplayNameItem) ->
      let cmp strOpt expected actual =
        match byName.MatchMethod with
        | NameMatchMethod.StringCompare -> String.equalsWithComparer strOpt.StringComparer expected actual
        | NameMatchMethod.StartsWith -> actual.StartsWith(expected, strOpt.StringComparison)
        | NameMatchMethod.EndsWith -> actual.EndsWith(expected, strOpt.StringComparison)
        | NameMatchMethod.Contains -> actual.IndexOf(expected, strOpt.StringComparison) >= 0
        | NameMatchMethod.Regex -> Regex.IsMatch(actual, expected, strOpt.RegexOptions)
        | NameMatchMethod.Any -> true
      let actualName =
        match actual.Name with
        | SymbolName n -> n
        | OperatorName (_, n) -> n
        | WithCompiledName (n, _) -> n
      cmp strOpt byName.Expected actualName
    )

let test strOpt query (api: Api) ctx =
  match query with
  | QueryMethod.ByName (expected, _) ->
    match api.Name, api.Kind with
    | DisplayName actualName, ApiKind.Constructor ->
      let name_type = List.tail actualName
      let name_new = actualName
      let name_ctor = { Name = SymbolName ".ctor"; GenericParameters = [] } :: name_type
      let ok = [ name_new; name_type; name_ctor ] |> List.exists (test' strOpt expected)
      if ok then Matched ctx else Failure
    | DisplayName actualName, _ -> if test' strOpt expected actualName then Matched ctx else Failure
    | _ -> Failure
  | _ -> Matched ctx

let instance (options: SearchOptions) =
  let strOpt = stringOptions options.IgnoreCase
  { new IApiMatcher with
      member this.Name = "Name Matcher"
      member this.Test lowTypeMatcher query api ctx = test strOpt query api ctx }