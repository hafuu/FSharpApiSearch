module QueryParserTest

open Persimmon
open Persimmon.Syntax.UseTestNameByReflection
open FSharpApiSearch.Types
open FSharpApiSearch
open TestHelpers.DSL

let runParseTest (input, expected) = test {
  let actual = QueryParser.parse input
  do! actual.Query |> assertEquals expected
}

let parseTest = parameterize {
  source [
    "a", (identity "a")
    "'a", (variable "a")
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