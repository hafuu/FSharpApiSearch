module QueryParserTest

open Persimmon
open Persimmon.Syntax.UseTestNameByReflection
open FSharpApiSearch
open TestHelpers.DSL

module BySignature =
  let runParseTest (input, expectedSignature) = test {
    let actual = QueryParser.parse input
    let expected = { OriginalString = input; Method = BySignature expectedSignature }
    do! actual |> assertEquals expected
  }

  let parseTest = parameterize {
    source [
      "a", (identity "a")
      "!a", (strongIdentity "a")
      "'a", (variable "a")
      "!'a", (strongVariable "a")
    ]
    run runParseTest
  }

  let wildcardTest = parameterize {
    source [
      "?", (wildcard)
      "?a", (wildcardGroup "a")
      "? -> ?", (arrow [ wildcard; wildcard ])
      "a<?, ?b>", (generic (identity "a") [ wildcard; wildcardGroup "b" ])
    ]
    run runParseTest
  }

  let arrowParseTest = parameterize {
    source [
      "a -> a", (arrow [ identity "a"; identity "a" ])
      "('a -> 'b) -> 'c", (arrow [ (arrow [ variable "a"; variable "b" ]); variable "c" ])
      "('a -> 'b)", (arrow [ variable "a"; variable "b" ])
    ]
    run runParseTest
  }

  let dotNetGenericParseTest = parameterize {
    source [
      "a<b, c>", (generic (identity "a") [ identity "b"; identity "c" ])
      "'a -> b<c, d> -> d", (arrow [ variable "a"; generic (identity "b") [ identity "c"; identity "d" ]; identity "d" ])
      "?<b, c>", (generic (wildcard) [ identity "b"; identity "c" ])
      "?a<b, c>", (generic (WildcardGroup "a") [ identity "b"; identity "c" ])
    ]
    run runParseTest
  }

  let mlGenericParseTest = parameterize {
    source [
      "a b", (generic (identity "b") [ identity "a" ])
      "a b -> c", (arrow [ generic (identity "b") [ identity "a" ]; identity "c" ])
      "(a, b) c", (generic (identity "c") [ identity "a"; identity "b" ])
      "(a, b) c -> d", (arrow [ generic (identity "c") [ identity "a"; identity "b" ]; identity "d" ])
      "(a, b -> b) c", (generic (identity "c") [ identity "a"; arrow [ identity "b"; identity "b" ] ])
      "a<b> c", (generic (identity "c") [ generic (identity "a") [ identity "b" ] ])
      "a ?", (generic (wildcard) [ identity "a" ])
      "b ?a", (generic (wildcardGroup "a") [ identity "b" ])
    ]
    run runParseTest
  }

  let tupleParseTest = parameterize {
    source [
      "a * b", (tuple [ identity "a"; identity "b" ])
      "a * b -> b", (arrow [ tuple [ identity "a"; identity "b" ]; identity "b" ])
      "a<b * c, d>", (generic (identity "a") [ tuple [ identity "b"; identity "c" ]; identity "d" ])
      "a * b c", (tuple [ identity "a"; generic (identity "c") [ identity "b" ] ])
      "a -> (a * b)", (arrow [ identity "a"; tuple [ identity "a"; identity "b" ] ])
      "(a * b) -> a", (arrow [ tuple [ identity "a"; identity "b" ]; identity "a" ])
    ]
    run runParseTest
  }

module ByName =
  let parseTest =
    let alpha = variable "a"
    let beta = variable "b"
    let option_alpha = generic (identity "option") [ alpha ]
    let option_beta = generic (identity "option") [ beta ]
    parameterize {
      source [
        "map : _", "map", AnySignature
        "bind : ('a -> 'b option) -> 'a option -> 'b option", "bind", (SignatureQuery (arrow [ (arrow [ alpha; option_beta ]); option_alpha; option_beta ]))
      ]
      run (fun (input, expectedName, expectedSig) -> test {
        let actual = QueryParser.parse input
        let expected = { OriginalString = input; Method = ByName (expectedName, expectedSig) }
        do! actual |> assertEquals expected
      })
    }