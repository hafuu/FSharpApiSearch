module internal FSharpApiSearch.NameMatcher

open System.Diagnostics
open FSharpApiSearch.EngineTypes
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

let private cmp strOpt (byName: ByName) actual =
  let expected = byName.Expected
  match byName.MatchMethod with
  | NameMatchMethod.StringCompare -> String.equalsWithComparer strOpt.StringComparer expected actual
  | NameMatchMethod.StartsWith -> actual.StartsWith(expected, strOpt.StringComparison)
  | NameMatchMethod.EndsWith -> actual.EndsWith(expected, strOpt.StringComparison)
  | NameMatchMethod.Contains -> actual.IndexOf(expected, strOpt.StringComparison) >= 0
  | NameMatchMethod.Regex -> Regex.IsMatch(actual, expected, strOpt.RegexOptions)
  | NameMatchMethod.Any -> true

let test' strOpt (expected: ByName list) (actualNames: NameItem list) =
  if not (List.length expected <= List.length actualNames) then
    false
  else
    let rec loop (expected: ByName list) (actualNames: NameItem list) =
      match expected, actualNames with
      | byName :: expected, actual :: actualNames ->
        let actualName =
          match actual.Name with
          | SymbolName n -> n
          | OperatorName (_, n) -> n
          | WithCompiledName (n, _) -> n
        let result = cmp strOpt byName actualName && (let expectedLen = byName.GenericParameters.Length in if expectedLen > 0 then expectedLen = actual.GenericParameters.Length else true)
        if result then
          loop expected actualNames
        else
          false
      | _ -> true
    loop expected actualNames

let ctor = { Name = SymbolName ".ctor"; GenericParameters = [] }

let test strOpt query (api: Api) ctx =
  match query with
  | QueryMethod.ByName (expected, _) | QueryMethod.ByNameOrSignature (expected, _) ->
    match api.Name, api.Kind with
    | ApiName actualName, ApiKind.Constructor ->
      // TypeName or TypeName.new or TypeName..ctor
      let ok = test' strOpt expected actualName.Tail || test' strOpt expected actualName || test' strOpt expected (ctor :: actualName.Tail)
      if ok then Matched ctx else Failure
    | ApiName actualName, _ -> if test' strOpt expected actualName then Matched ctx else Failure
    | _ -> Failure
  | _ -> Matched ctx

let instance (options: SearchOptions) =
  let strOpt = stringOptions options.IgnoreCase
  { new IApiMatcher with
      member this.Name = "Name Matcher"
      member this.Test lowTypeMatcher query api ctx = test strOpt query.Method api ctx }