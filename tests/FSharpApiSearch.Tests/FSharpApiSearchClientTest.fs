module FSharpApiSearchClientTest

open Persimmon
open Persimmon.Syntax.UseTestNameByReflection
open Persimmon.MuscleAssert
open System.IO
open FSharpApiSearch
open FSharpApiSearch.Printer
open System.Reflection

let assemblyName = @"SearchTestAssembly"
let assemblyPath =
  Path.Combine(
    Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)
    , assemblyName + ".dll")

let testClient = test {
  let assemblies = AssemblyLoader.load TestAssemblies.assemblyResolver (assemblyPath :: FSharpApiSearchClient.DefaultReferences)
  let dictionaries = ApiLoader.load assemblies
  return FSharpApiSearchClient([ assemblyName ], dictionaries)
}

let searchTest = parameterize {
  source [
    "int -> int -> int", [ "TestModule.f"; "TestModule.TestClass.f" ]
    "int * int -> ?", [ "TestModule.f"; "TestModule.h"; "TestModule.TestClass.f"; "TestModule.TestClass.h" ]
    "?a -> ?a -> ?a", [ "TestModule.f"; "TestModule.g"; "TestModule.TestClass.f"; "TestModule.TestClass.g" ]
  ]
  run (fun (query, expecteds) -> test {
    let! client = testClient
    let actual =
      client.Search(query, TestHelper.defaultTestOptions)
      |> Seq.map (fun x -> FSharp.printFullName x.Api) |> Seq.toList |> List.sort
    do! actual |> assertEquals (List.sort expecteds)
  })
}