[<Persimmon.Category("printer")>]
module QueryPrinterTest

open Persimmon
open Persimmon.Syntax.UseTestNameByReflection
open Persimmon.MuscleAssert
open FSharpApiSearch

let querySplitTest = parameterize {
  source [
    "a -> b", [], [ "a -> b", None ]
    "a -> b", [ (QueryId 1, { Begin = 0; End = 1 }) ], [ ("a", Some (QueryId 1)); (" -> b", None) ]
    "a -> b -> c", [ (QueryId 1, { Begin = 5; End = 6 }) ], [ ("a -> ", None); ("b", Some (QueryId 1)); (" -> c", None) ]
  ]
  run (fun (query, ranges, expected) -> test {
    let actual = QueryPrinter.Impl.split query (Array.ofList ranges)
    do! actual |> assertEquals (Array.ofList expected)
  })
}