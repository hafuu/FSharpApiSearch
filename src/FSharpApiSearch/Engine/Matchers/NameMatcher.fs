module internal FSharpApiSearch.NameMatcher

open FSharpApiSearch.EngineTypes
open System.Text.RegularExpressions
open System

open TypeNameEquality

type StringOptions = {
  DefaultTestString: string -> string -> Result
  StringComparison: StringComparison
  RegexOptions: RegexOptions
}

let stringOptions options =
  let stringComparison, regexOptions =
    match options.IgnoreCase with
    | Enabled -> StringComparison.InvariantCultureIgnoreCase, RegexOptions.CultureInvariant ||| RegexOptions.IgnoreCase
    | Disabled -> StringComparison.InvariantCulture, RegexOptions.CultureInvariant
  let testString =
    match options.Substring with
    | Enabled -> testStringSubstring
    | Disabled -> testStringExact
  {
    DefaultTestString = testString stringComparison
    StringComparison = stringComparison
    RegexOptions = regexOptions
  }

let private cmp strOpt (byName: ByName) (actual: string) =
  let expected = byName.Expected
  let boolToResult b = if b then Ok 0 else Error FailureReason.DifferentName
  match byName.MatchMethod with
  | NameMatchMethod.Equals -> actual.Equals(expected, strOpt.StringComparison) |> boolToResult
  | NameMatchMethod.Default -> strOpt.DefaultTestString expected actual
  | NameMatchMethod.StartsWith -> actual.StartsWith(expected, strOpt.StringComparison) |> boolToResult
  | NameMatchMethod.EndsWith -> actual.EndsWith(expected, strOpt.StringComparison) |> boolToResult
  | NameMatchMethod.Contains -> actual.IndexOf(expected, strOpt.StringComparison) >= 0 |> boolToResult
  | NameMatchMethod.Regex -> Regex.IsMatch(actual, expected, strOpt.RegexOptions) |> boolToResult
  | NameMatchMethod.Any -> Ok 0

let testGenericParameterLength (byName: ByName) (actual: NameItem) =
  let expectedLen = byName.GenericParameters.Length
  if expectedLen > 0 then
    let actualLen = actual.GenericParameters.Length
    if expectedLen = actualLen then
      Ok 0
    else
      Error (FailureReason.DifferentGenericParameter (expectedLen, actualLen))
  else
    Ok 0


let testByName strOpt (expected: ByName list) (actualNames: NameItem list) =
  if not (List.length expected <= List.length actualNames) then
    Error FailureReason.DifferentName
  else
    let test' (byName: ByName) (actualName: NameItem) = test {
      do! cmp strOpt byName (testee actualName)
      do! testGenericParameterLength byName actualName
    }
    forall2 test' expected actualNames

let ctor = { Name = SymbolName ".ctor"; GenericParameters = [] }

let test strOpt query (api: Api) ctx =
  match query with
  | QueryMethod.ByName (expected, _) | QueryMethod.ByNameOrSignature (expected, _) ->
    match api.Name, api.Kind with
    | ApiName actualName, ApiKind.Constructor ->
      // {TypeName} or {TypeName}.new or {TypeName}..ctor
      let constructors = [| actualName.Tail; actualName; (ctor :: actualName.Tail) |]
      let result =
        constructors
        |> Array.tryPick (fun constructorName ->
          match testByName strOpt expected constructorName with
          | Ok distance -> Some distance
          | Error _ -> None
        )
      match result with
      | Some distance -> Matched (ctx |> Context.addDistance "name matching" distance)
      | None -> Failure FailureInfo.None
    | ApiName actualName, _ ->
      match testByName strOpt expected actualName with
      | Ok distance -> Matched (ctx |> Context.addDistance "name matching" distance)
      | Error _ -> Failure FailureInfo.None
    | _ -> Failure FailureInfo.None
  | _ -> Matched ctx

let instance (options: SearchOptions) =
  let strOpt = stringOptions options
  { new IApiMatcher with
      member this.Name = "Name Matcher"
      member this.Test lowTypeMatcher query api ctx = test strOpt query.Method api ctx }