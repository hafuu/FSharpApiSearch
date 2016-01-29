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
    "int -> int -> int", [ "TestModule.f"; "TestModule.TestClass.f" ]
    "int -> int -> ?", [ "TestModule.f"; "TestModule.h"; "TestModule.TestClass.f"; "TestModule.TestClass.h" ]
    "?a -> ?a -> ?a", [ "TestModule.f"; "TestModule.g"; "TestModule.TestClass.f"; "TestModule.TestClass.g" ]
  ]
  run (fun (query, expecteds) -> test {
    let! client = testClient
    let actual = client.Search(query) |> Seq.map (fun x -> x.Api.Name) |> Seq.toList |> List.sort
    do! actual |> assertEquals (List.sort expecteds)
  })
}