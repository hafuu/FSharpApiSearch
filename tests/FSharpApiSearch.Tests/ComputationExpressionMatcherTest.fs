module ComputationExpressionMatcherTest

open Persimmon
open Persimmon.Syntax.UseTestNameByReflection
open Persimmon.MuscleAssert
open FSharpApiSearch
open TestHelper
open TestHelper.DSL

let testComputationExpression = parameterize {
  source [
    "{ _ } : option<'a>", 2
    "{ let! } : option<'a>", 2
    "{ let!; return } : option<'a>", 2
    "{ for } : option<'a>", 0
    "{ _ } : list<'a>", 0
  ]
  run (fun (query, expected) -> test {
    let! apiDict = TestAssemblies.fsharpAssemblyApi
    let! dictionaries = TestAssemblies.apiDictionary
    let options = SearchOptions.defaultOptions |> SearchOptions.Parallel.Set Disabled
    let actual = Matcher.search dictionaries options [| apiDict |] query
    do! Seq.length actual |> assertEquals expected
  })
}