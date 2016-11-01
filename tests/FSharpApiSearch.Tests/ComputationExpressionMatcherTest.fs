module ComputationExpressionMatcherTest

open Persimmon
open Persimmon.Syntax.UseTestNameByReflection
open Persimmon.MuscleAssert
open FSharpApiSearch
open TestHelper
open TestHelper.DSL

let testComputationExpression = parameterize {
  source [
    "{ _ } : option<'a>", 1, 2
    "{ let! } : option<'a>", 1, 2
    "{ let!; return } : option<'a>", 1, 2
    "{ for } : option<'a>", 0, 0
    "{ _ } : list<'a>", 0, 0
    "{ try/finally } : TryFinallyTest", 2, 2
  ]
  run (fun (query, builderExpected, expected) -> test {
    let! apiDict = TestAssemblies.fsharpAssemblyApi
    let! dictionaries = TestAssemblies.apiDictionary
    let options = TestHelper.defaultTestOptions
    let actual = Matcher.search dictionaries options [| apiDict |] query |> Seq.cache
    let computationBuilderCount = actual |> Seq.filter (fun result -> result.Api.Kind = ApiKind.ComputationExpressionBuilder) |> Seq.length
    let apiCount = actual |> Seq.filter (fun result -> result.Api.Kind <> ApiKind.ComputationExpressionBuilder) |> Seq.length
    do! apiCount |> assertEquals expected
    do! computationBuilderCount |> assertEquals builderExpected
  })
}