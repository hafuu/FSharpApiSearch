module QueryParserTest

open Persimmon
open Persimmon.Syntax.UseTestNameByReflection
open FSharpApiSearch.Types
open FSharpApiSearch

let typeId name = Type name
let type' name = TypeIdentity (typeId name)
let variableId name = Variable (Query, name)
let variable name = TypeIdentity (variableId name)
let arrow xs = Arrow xs
let generic id xs = Generic (id, xs)
let tuple xs = Tuple xs

let runParseTest (input, expected) = test {
  let actual = QueryParser.parse input
  do! actual.Query |> assertEquals expected
}

let parseTest = parameterize {
  source [
    "a", (type' "a")
    "'a", (variable "'a")
  ]
  run runParseTest
}

let arrowParseTest = parameterize {
  source [
    "a -> a", (arrow [ type' "a"; type' "a" ])
    "('a -> 'b) -> 'c", (arrow [ (arrow [ variable "'a"; variable "'b" ]); variable "'c" ])
    "('a -> 'b)", (arrow [ variable "'a"; variable "'b" ])
  ]
  run runParseTest
}

let dotNetGenericParseTest = parameterize {
  source [
    "a<b, c>", (generic (typeId "a") [ type' "b"; type' "c" ])
    "'a -> b<c, d> -> d", (arrow [ variable "'a"; generic (typeId "b") [ type' "c"; type' "d" ]; type' "d" ])
  ]
  run runParseTest
}

let mlGenericParseTest = parameterize {
  source [
    "a b", (generic (typeId "b") [ type' "a" ])
    "a b -> c", (arrow [ generic (typeId "b") [ type' "a" ]; type' "c" ])
    "(a, b) c", (generic (typeId "c") [ type' "a"; type' "b" ])
    "(a, b) c -> d", (arrow [ generic (typeId "c") [ type' "a"; type' "b" ]; type' "d" ])
    "(a, b -> b) c", (generic (typeId "c") [ type' "a"; arrow [ type' "b"; type' "b" ] ])
    "a<b> c", (generic (typeId "c") [ generic (typeId "a") [ type' "b" ] ])
  ]
  run runParseTest
}

let tupleParseTest = parameterize {
  source [
    "a * b", (tuple [ type' "a"; type' "b" ])
    "a * b -> b", (arrow [ tuple [ type' "a"; type' "b" ]; type' "b" ])
    "a<b * c, d>", (generic (typeId "a") [ tuple [ type' "b"; type' "c" ]; type' "d" ])
    "a * b c", (tuple [ type' "a"; generic (typeId "c") [ type' "b" ] ])
    "a -> (a * b)", (arrow [ type' "a"; tuple [ type' "a"; type' "b" ] ])
    "(a * b) -> a", (arrow [ tuple [ type' "a"; type' "b" ]; type' "a" ])
  ]
  run runParseTest
}