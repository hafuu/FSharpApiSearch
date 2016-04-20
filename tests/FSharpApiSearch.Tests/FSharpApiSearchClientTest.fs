module FSharpApiSearchClientTest

open Persimmon
open Persimmon.Syntax.UseTestNameByReflection
open System.IO
open FSharpApiSearch
open System.Reflection

let assemblyName = @"SearchTestAssembly"
let assemblyPath =
  Path.Combine(
    Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)
    , assemblyName + ".dll")

let testClient = test {
  return FSharpApiSearchClient([ assemblyName ], assemblyPath :: FSharpApiSearchClient.DefaultReferences)
}

let searchTest = parameterize {
  source [
    "int -> int -> int", [ "TestModule.f"; "TestModule.TestClass.f" ]
    "int -> int -> ?", [ "TestModule.f"; "TestModule.h"; "TestModule.TestClass.f"; "TestModule.TestClass.h" ]
    "?a -> ?a -> ?a", [ "TestModule.f"; "TestModule.g"; "TestModule.TestClass.f"; "TestModule.TestClass.g" ]
  ]
  run (fun (query, expecteds) -> test {
    let! client = testClient
    let actual =
      client.Search(query, SearchOptions.defaultOptions)
      |> Seq.map (fun x -> ReverseName.toString x.Api.Name) |> Seq.toList |> List.sort
    do! actual |> assertEquals (List.sort expecteds)
  })
}