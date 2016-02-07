module MatcherTest

open Persimmon
open Persimmon.Syntax.UseTestNameByReflection
open FSharpApiSearch
open TestHelpers.DSL

let targetSugnature x = x |> QueryParser.parseFSharpSignature |> TestHelpers.updateSource Source.Target

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

let initialContext matchOpt query =
  let eqs =
    let eqs = Matcher.Equations.initialize query
    match matchOpt with
    | Strict -> Matcher.Equations.strictVariables query eqs
    | NoOption -> eqs
  Matcher.Context.initialize eqs

let nameMatchTest = parameterize {
  source [
    "map : _", "Microsoft.FSharp.Collections.List.map", "('a -> 'b) -> 'a list -> 'b list", true
    "bind : _", "Microsoft.FSharp.Collections.List.map", "('a -> 'b) -> 'a list -> 'b list", false
    "map : ('a -> 'b) -> 'a option -> 'b option", "Microsoft.FSharp.Core.Option.map", "('a -> 'b) -> 'a option -> 'b option", true
    "map : ('a -> 'b) -> 'a option -> 'b option", "Microsoft.FSharp.Collections.List.map", "('a -> 'b) -> 'a list -> 'b list", false
  ]
  run (fun (query, targetName, targetSig, expected) -> test {
    let query = QueryParser.parse query
    let targetSig = QueryParser.parseFSharpSignature targetSig |> TestHelpers.updateSource Source.Target
    let target = { Name = targetName; Signature = targetSig }
    let ctx = initialContext NoOption query
    let actual = Matcher.matches query target Matcher.defaultRule ctx |> Matcher.Result.toBool
    do! actual |> assertEquals expected
  })
}

module EquationsTest =
  let initializeTest = parameterize {
    source [
      "'a -> 'b", []
      "'a -> 'a", []
      "'a -> !'a", [ (variable "a", strongVariable "a") ]
    ]
    run (fun (query, expected) -> test {
      let query = QueryParser.parse query
      let equations = Matcher.Equations.initialize query
      do! equations.Equalities |> assertEquals expected
    })
  }

  let strictVariableTest = parameterize {
    source [
      "'a -> 'a", []
      "'a -> !'a", []
      "'a -> 'b", [ (variable "a", variable "b") ]
      "!'a -> 'b", [ (variable "b", strongVariable "a") ]
      "'a -> !'a -> 'b -> !'b", [ (variable "a", variable "b"); (variable "a", strongVariable "b"); (variable "b", strongVariable "a"); (strongVariable "a", strongVariable "b") ]
      "?a -> ?a", []
      "?a -> ?b", [ (wildcardGroup "a", wildcardGroup "b") ]
    ]
    run (fun (query, expected) -> test {
      let query = QueryParser.parse query
      let equations = Matcher.Equations.initialize query |> Matcher.Equations.strictVariables query
      do! List.sort equations.Inequalities |> assertEquals (List.sort expected)
    })
  }

module DefaultMatcherTest =
  let matchTest updateTargetSignature cases = parameterize {
    source (seq {
      for matchOpt in [ Strict; NoOption ] do
        for (q, t, expected) in cases do
          yield (matchOpt, q, t, expectedValue matchOpt expected)
    })
    run (fun (matchOpt, query, target, expected) -> test {
      let query = QueryParser.parse query
      let targetSig = targetSugnature target |> updateTargetSignature
      let target = { Name = "test"; Signature = targetSig }
      let ctx = initialContext matchOpt query
      let actual = Matcher.matches query target Matcher.defaultRule ctx |> Matcher.Result.toBool
      do! actual |> assertEquals expected
    })
  }

  let runTest xs = matchTest id xs

  let identityTest =
    runTest [
      "int", "int", Always true
      "int", "string", Always false
      "B", "A.B", Always true
      "B.C", "A.B.C", Always true
    ]

  let ``strong identity is the same behavior as identity`` =
    runTest [
      "!int", "int", Always true
      "!int", "string", Always false
    ]

  let variableTest =
    runTest [
      "'a", "'b", Always true
      "'a", "int", Always false
      "int", "'a", Always false
      "'a -> 'b", "'a -> 'a", WhenStrict false
    ]

  let ``strong variable is the same behavior as variable`` =
    runTest [
      "!'a", "'b", Always true
      "!'a", "int", Always false
      "!'a -> !'b", "'a -> 'a", WhenStrict false
    ]

  let ``variable and strong variable share the name space`` =
    runTest [
      "!'a -> 'a", "'a -> 'a", Always true
    ]

  let arrowTest =
    runTest [
      "int -> string", "int -> string", Always true
      "int -> int", "int -> string", Always false
      "int -> 'a", "int -> 'b", Always true
      "int -> 'a", "'a -> 'b", Always false

      "int -> int", "int -> int -> int", Always false
      "int -> int -> int", "int -> int", Always false
    ]

  let ``nested arrow test`` =
    runTest [
      "(int -> string) -> string", "(int -> string) -> string", Always true
      "(int -> string) -> string", "(int -> string) -> int", Always false
      "('a -> 'b) -> 'b", "(int -> string) -> string", Always false
      "(int -> string) -> string", "('a -> 'b) -> 'b", Always false
    ]

  let genericTest =
    runTest [
      "int option", "int option", Always true
      "int option", "string option", Always false
      "int option", "'a option", Always false
    ]

  let tupleTest =
    runTest [
      "int * int * int", "int * int * int", Always true
      "int * int * int", "int * string * int", Always false
      "int * int", "int * int * int", Always false
    ]

  let ``another types test`` =
    runTest [
      "int * int", "int -> int", Always false
      "int option", "int", Always false
    ]

  let wildcardTest =
    runTest [
      "?", "int", Always true
      "?", "'a", Always true
      "?", "int list", Always true
      "?", "int -> int", Always true
      "? -> ?", "int", Always false
      "? -> ?", "int -> string", Always true
      "? -> ?", "int -> int", Always true

      "?a", "int", Always true
      "?a -> ?a", "int -> int", Always true
      "?a -> ?a", "int -> string", Always false
      "?a -> ?a", "int -> 'a", Always false
      "?a -> ?b", "int -> string", Always true
      "?a -> ?b", "int -> int", WhenStrict false
    ]

  let ``higher order type test`` =
    runTest [
      "'a -> 'a ?", "'a -> 'a list", Always true
    ]

  let staticMethodMatchTest =
    let cases = [
      "int -> string", "int -> string", Always true
      "'a -> 'b -> 'c", "'a * 'b -> 'c", Always true
      "'a -> 'b -> 'c", "'a * 'b * 'c -> 'd", Always false
    ]
    matchTest TestHelpers.toStaticMethod cases

  let instanceMemberMatchTest =
    let cases = [
      "receiver => returnType", "receiver => returnType", Always true
      "a => returnType", "a => anotherReturnType", Always false
      "receiver => b", "anotherReceiver => b", Always false
      "a => b -> c", "a => b -> c", Always true

      // property or field
      "a => b", "a => unit -> b", Always true
      "receiver => b", "anotherReceiver => unit -> b", Always false
      "a => returnType", "a => unit -> anotherReturnType", Always false
      "a => unit -> b", "a => b", Always false

      // instance member and arrow
      "a => unit -> b", "a -> b", Always true
      "a => b", "a -> b", Always true
      "a => b -> c", "b -> a -> c", Always true
      "a => b -> c", "a -> b -> c", Always false
    ]
    matchTest id cases

  let typeAbbreviationTest =
    let p = QueryParser.parseFSharpSignature
    parameterize {
      source[
        (identity "A"), (abbreviation (identity "A") (identity "a")), true
        (identity "B"), (abbreviation (identity "A") (identity "a")), false
        (abbreviation (identity "A") (identity "a")), (abbreviation (identity "A") (identity "a")), true
        (abbreviation (identity "B") (identity "b")), (abbreviation (identity "A") (identity "a")), false
        (abbreviation (p "List<'a, 'a>") (p "list<'a, 'a>")), (abbreviation (p "List<int, int>") (p "list<int, int>")), false
      ]
      run (fun (query, target, expected) -> test {
        let query = { OriginalString = ""; Method = BySignature query }
        let target = { Name = "test"; Signature = TestHelpers.updateSource Source.Target target }
        let ctx = Matcher.Equations.initialize query |> Matcher.Context.initialize
        let actual = Matcher.matches query target Matcher.defaultRule ctx |> Matcher.Result.toBool
        do! actual |> assertEquals expected
      })
    }

module SimilarityMatchTest =
  let matchTest updateSource cases = parameterize {
    source (seq {
      for matchOpt in [ Strict; NoOption ] do
        for (q, t, expected) in cases do
          yield (matchOpt, q, t, expectedValue matchOpt expected)
    })
    run (fun (matchOpt, query, target, expected) -> test {
      let query = QueryParser.parse query
      let targetSig = targetSugnature target |> updateSource
      let target = { Name = "test"; Signature = targetSig }
      let ctx = initialContext matchOpt query
      let actual = Matcher.matches query target Matcher.similaritySearchingRule ctx |> Matcher.Result.toBool
      do! actual |> assertEquals expected
    })
  }

  let runTest xs = matchTest id xs

  let identityTest =
    runTest [
      "int", "int", Always true
      "int", "string", Always false
      "B", "A.B", Always true
      "B.C", "A.B.C", Always true
      "int", "'a", Always true
      "!int", "'a", Always false
    ]

  let variablesTest = 
    runTest [
      "'a", "'a", Always true
      "'a", "int", Always true
      "!'a", "'a", Always true
      "!'a", "int", Always false

      // bug #12 recursive and circular generic type
      "'a -> 'a", "nativeptr<'T> -> 'T", Always false
      "('a -> 'b) -> 'a option -> 'b option", "('a -> 'b option) -> 'a option -> 'b option", Always false
      "('a -> 'b) -> 'a 'm -> 'b 'm", "('T -> option<'U>) -> seq<'T> -> 'U", Always false
    ]

  let ``variable and strong variable share the name space`` =
    runTest [
      "!'a -> 'a", "'a -> 'a", Always true
      "!'a -> 'a", "int -> string", Always false
    ]

  let arrowTest =
    runTest [
      "'a -> 'b", "'a -> 'b", Always true
      "int -> string", "int -> string", Always true

      "'a -> 'b", "int -> string", Always true
      "'a -> 'b", "int -> int", WhenStrict false

      "int -> string", "'a -> 'b", Always true
      "int -> int", "'a -> 'b", Always true
    ]

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
      "('a -> 'b) -> 'a list -> 'b list", "('T -> 'U) -> 'T list -> 'T", 1

      "? -> ?", "'a -> 'b", 0
      "? -> int", "'a -> 'b", 1

      "a => b", "a => unit -> b", 1
      "a => unit -> b", "a -> b", 1

      // bug #16
      "('a * string) -> string", "('a * 'b) -> 'b", 1
      "('a * string) -> string", "('a * 'b) -> 'a", 2
    ]
    run (fun (query, target, expected) -> test {
      let query = QueryParser.parse query
      let targetSig = targetSugnature target |> TestHelpers.updateSource Source.Target
      let target = { Name = "test"; Signature = targetSig }
      let ctx = initialContext NoOption query
      let result = Matcher.matches query target Matcher.similaritySearchingRule ctx
      match result with
      | Matcher.Matched { Distance = actual } ->
        do! actual |> assertEquals expected
      | _ ->
        do! fail "not matched"
    })
  }

  let typeAbbreviationTest =
    let p = QueryParser.parseFSharpSignature
    parameterize {
      source[
        (identity "A"), (abbreviation (identity "A") (identity "a")), true
        (identity "B"), (abbreviation (identity "A") (identity "a")), false
        (abbreviation (identity "A") (identity "a")), (abbreviation (identity "A") (identity "a")), true
        (abbreviation (identity "B") (identity "b")), (abbreviation (identity "A") (identity "a")), false
        (variable "a"), (abbreviation (identity "A") (identity "a")), true
        (abbreviation (p "List<'a, 'a>") (p "list<'a, 'a>")), (abbreviation (p "List<int, int>") (p "list<int, int>")), true
        (abbreviation (p "List<'a, 'a>") (p "list<'a, 'a>")), (abbreviation (p "List<int, string>") (p "list<int, string>")), false
      ]
      run (fun (query, target, expected) -> test {
        let query = { OriginalString = ""; Method = BySignature query }
        let target = { Name = "test"; Signature = TestHelpers.updateSource Source.Target target }
        let ctx = Matcher.Equations.initialize query |> Matcher.Context.initialize
        let actual = Matcher.matches query target Matcher.similaritySearchingRule ctx |> Matcher.Result.toBool
        do! actual |> assertEquals expected
      })
    }
