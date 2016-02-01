module TypesTest

open Persimmon
open Persimmon.Syntax.UseTestNameByReflection
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
      array (identity "a"), "a[]"
      array2d (identity "a"), "a[,]"
      array (array2d (identity "a")), "a[,][]"
    ]
    run (fun (input, expected) -> test {
      let actual = Signature.display input
      do! actual |> assertEquals expected
    })
  }