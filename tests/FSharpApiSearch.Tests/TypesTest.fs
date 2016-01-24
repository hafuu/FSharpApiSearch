module TypesTest

open Persimmon
open Persimmon.Syntax.UseTestNameByReflection
open FSharpApiSearch.Types
open FSharpApiSearch
open TestHelpers.DSL

module SignatureTest =
  let displayTest = parameterize {
    source [
      identity "a", "a"
      variable "a", "'a"
      generic (identity "a") [ identity "b"; identity "c" ], "a<b, c>"
      arrow [ identity "a"; identity "b"; identity "c" ], "a -> b -> c"
      arrow [ identity "a"; arrow [ identity "a"; identity "b" ]; identity "b" ], "a -> (a -> b) -> b"
      tuple [ identity "a"; identity "b" ], "a * b"
      tuple [ identity "a"; tuple [ identity "a"; identity "b" ] ], "a * (a * b)"
    ]
    run (fun (input, expected) -> test {
      let actual = Signature.display input
      do! actual |> assertEquals expected
    })
  }