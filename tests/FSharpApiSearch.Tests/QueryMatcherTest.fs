module QueryMatcherTest

open Persimmon
open Persimmon.Syntax.UseTestNameByReflection
open FSharpApiSearch.Types
open FSharpApiSearch

type MatchOption =
  | Strict
  | NoOption

type Expected =
  | Always of bool
  | WhenStrict of bool

let expectedValue matchOpt expected =
  match expected, matchOpt with
  | Always b, _ -> b
  | WhenStrict b, Strict -> b
  | WhenStrict b, NoOption -> not b

let strictMatchTest =
  let patterns = [
    "int", "int", Always true
    "int", "string", Always false

    "'a", "int", Always true
    "'a", "'a", Always true
    "'a", "'b", Always true

    "int -> string", "int -> string", Always true
    "int -> int", "int -> string", Always false

    "int -> int", "int -> int -> int", Always false
    "int -> int -> int", "int -> int", Always false

    "'a -> 'a", "int -> int", Always true
    "'a -> 'a", "int -> string", Always false
    "int -> int", "'a -> 'a", Always true
    "int -> string", "'a -> 'a", Always false

    "'a -> 'b", "int -> string", Always true
    "'a -> 'b", "int -> int", WhenStrict false
    "int -> string", "'a -> 'b", Always true
    "int -> int", "'a -> 'b", Always true // The target is not strict.

    "'a -> 'a", "'a -> 'a", Always true
    "'a -> 'a", "'b -> 'b", Always true
    "'a -> 'b", "'a -> 'a", WhenStrict false

    "'a -> 'a", "'a -> int", Always true
    "string -> 'a -> 'a", "'a -> int -> int", Always true

    // nested arrow
    "'a -> 'b", "('a -> 'b) -> 'c", Always true
    "('a -> 'b) -> 'c", "('a -> 'b) -> 'c", Always true
    "('a -> int -> int) -> 'b", "('a -> 'b -> 'b) -> 'b", Always true
    "('a -> int -> int) -> 'b", "('a -> 'b -> string) -> 'b", Always false
    "('a -> 'b -> int) -> 'b", "(int -> int -> 'a) -> 'a", WhenStrict false

    // generic
    "int option -> int", "int option -> int", Always true
    "'a option -> 'a", "'a -> 'b", Always true
    "'a -> 'b", "'a option -> 'a", Always true
    "int option", "int list", Always false

    // tuple
    "'a -> 'b", "int * int -> int", Always true
    "'a * 'b -> 'a", "int * int -> int", WhenStrict false
    "'a * 'b -> 'b", "int * string -> int", Always false
    "string * 'a -> int", "'a * int -> 'b", Always true
  ]
  parameterize {
    source (seq {
      for matchOpt in [ Strict; NoOption ] do
        for (q, t, expected) in patterns do
          yield (matchOpt, q, t, expectedValue matchOpt expected)
    })
    run (fun (matchOpt, query, target, expected) -> test {
      let query = QueryParser.parse query
      let target =
        let t = QueryParser.parse target
        TestHelpers.updateSource Source.Target t.Query
      let initialEquations =
        match matchOpt with
        | Strict -> Matcher.Equations.strict query
        | NoOption -> Matcher.Equations.empty 
      do! Matcher.matches query target initialEquations |> assertEquals expected
    })
  }