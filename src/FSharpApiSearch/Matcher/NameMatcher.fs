module internal FSharpApiSearch.NameMatcher

open System.Diagnostics
open FSharpApiSearch.MatcherTypes

let test query (api: Api) ctx =
  match query with
  | QueryMethod.ByName (expectedName, _) ->
    match api.Name with
    | DisplayName actualName when expectedName.Length <= actualName.Length ->
      let matched =
        Seq.zip expectedName actualName
        |> Seq.forall (fun (expected, actual) ->
          match expected with
          | "*" -> true // wildcard
          | _ -> expected = actual.InternalFSharpName
        )
      if matched then Matched ctx else Failure
    | _ -> Failure
  | _ -> Matched ctx
let instance (_: SearchOptions) =
  { new IApiMatcher with
      member this.Name = "Name Matcher"
      member this.Test lowTypeMatcher query api ctx = test query api ctx }