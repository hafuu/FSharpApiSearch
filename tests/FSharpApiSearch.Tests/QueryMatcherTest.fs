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

let signatureMatchTest =
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

    // bug #12 recursive and circular generic type
    "'a -> 'a", "nativeptr<'T> -> 'T", Always false
    "('a -> 'b) -> 'a option -> 'b option", "('a -> 'b option) -> 'a option -> 'b option", Always false
    "('a -> 'b) -> 'a 'm -> 'b 'm", "('T -> option<'U>) -> seq<'T> -> 'U", Always false
  ]
  parameterize {
    source (seq {
      for matchOpt in [ Strict; NoOption ] do
        for (q, t, expected) in patterns do
          yield (matchOpt, q, t, expectedValue matchOpt expected)
    })
    run (fun (matchOpt, query, target, expected) -> test {
      let query = QueryParser.parse query
      let targetSig = QueryParser.parseSignature target |> TestHelpers.updateSource Source.Target
      let target = { Name = "test"; Signature = targetSig }
      let initialContext =
        let eqs =
          match matchOpt with
          | Strict -> Matcher.Equations.strict query
          | NoOption -> Matcher.Equations.empty
        Matcher.Context.initialize eqs
        
      do! Matcher.matches query target initialContext |> Matcher.MatchResult.toBool |> assertEquals expected
    })
  }

let nameMatchTest = parameterize {
  source [
    "map : _", "Microsoft.FSharp.Collections.List.map", "('a -> 'b) -> 'a list -> 'b list", true
    "bind : _", "Microsoft.FSharp.Collections.List.map", "('a -> 'b) -> 'a list -> 'b list", false
    "map : ('a -> 'b) -> 'a option -> 'b option", "Microsoft.FSharp.Core.Option.map", "('a -> 'b) -> 'a option -> 'b option", true
    "map : ('a -> 'b) -> 'a option -> 'b option", "Microsoft.FSharp.Collections.List.map", "('a -> 'b) -> 'a list -> 'b list", false
  ]
  run (fun (query, targetName, targetSig, expected) -> test {
    let query = QueryParser.parse query
    let targetSig = QueryParser.parseSignature targetSig |> TestHelpers.updateSource Source.Target
    let target = { Name = targetName; Signature = targetSig }
    let actual = Matcher.matches query target (Matcher.Context.initialize (Matcher.Equations.strict query)) |> Matcher.MatchResult.toBool
    do! actual |> assertEquals expected
  })
}

let distanceTest = parameterize {
  source [
    "int", "int", 0
    "'a", "int", 1
    "int -> string", "'a -> 'b", 2
    "'a", "'b", 0
    "'a", "list<int>", 1
    "'a", "int -> string", 2
    "'a", "int -> int -> int", 3
    "'a", "(int -> int) -> int -> int", 4
    "('a -> 'b) -> 'a list -> 'b list", "('T -> 'U) -> 'T list -> 'U list", 0
    "('a -> 'b) -> 'a list -> 'b list", "('T -> 'U) -> 'T list -> 'T", 1 // 'b list ant 'T
  ]
  run (fun (query, targetSig, expected) -> test {
    let query = QueryParser.parse query
    let targetSig = QueryParser.parseSignature targetSig |> TestHelpers.updateSource Source.Target
    let target = { Name = "test"; Signature = targetSig }
    let result = Matcher.matches query target (Matcher.Context.initialize (Matcher.Equations.strict query))
    match result with
    | Matcher.Success { Distance = actual} ->
      do! actual |> assertEquals expected
    | Matcher.Failure ->
      do! fail "not matched"
  })
}