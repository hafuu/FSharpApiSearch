module FSharpApiSearchClientTest

open Persimmon
open Persimmon.Syntax.UseTestNameByReflection
open System.IO
open FSharpApiSearch
open System.Reflection

let assemblyName = @"SearchTestAssembly.dll"
let assemblyPath =
  Path.Combine(
    Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)
    , assemblyName)

let testClient = test {
  let assemblies = ApiLoader.loadAssembly [ System.IO.Path.GetFullPath(assemblyPath) ]
  let testAssembly = assemblies |> List.filter (fun x -> x.FileName = Some assemblyPath )
  return FSharpApiSearchClient(testAssembly)
}

let searchTest = parameterize {
  source [
    "int -> int -> int", [ "TestModule.f" ]
    "int -> int -> 'a", [ "TestModule.f"; "TestModule.h" ]
    "'a -> 'a -> 'a", [ "TestModule.f"; "TestModule.g" ]
  ]
  run (fun (query, expecteds) -> test {
    let! client = testClient
    let actual = client.Search(query) |> Seq.map (fun x -> x.Api.Name) |> Seq.toList
    do! actual |> assertEquals expecteds
  })
}