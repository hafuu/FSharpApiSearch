module ApiLoaderTest

open Persimmon
open Persimmon.Syntax.UseTestNameByReflection

open FSharpApiSearch
open FSharpApiSearch.Types
open TestHelpers.DSL
open System.Reflection
open System.IO

let assemblyName = @"LoadTestAssembly.dll"
let assemblyPath =
  Path.Combine(
    Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)
    , assemblyName)

let loadApis = test {
  let assemblies = ApiLoader.loadAssembly [ System.IO.Path.GetFullPath(assemblyPath) ]
  return assemblies |> List.find (fun x -> x.FileName = Some assemblyPath ) |> ApiLoader.collectApi
}

let loadedApiTest = parameterize {
  source [
    "PublicModule.nonGenericFunction", "int -> int -> int"
    "PublicModule.genericFunction", "'a -> 'b -> 'b"
    "PublicModule.tupleFunction", "'a * 'b * 'c -> 'a"
    "PublicModule.value", "int"
    "PublicModule.NestedModule.publicFunction", "int -> int"
    "PublicModule.listmap", "('a -> 'b) -> 'a list -> 'b list"
    "PublicModule.partialGenericMap", "Map<int, 'a> -> 'a"
  ]
  run (fun (name, signature) -> test {
    let! apis = loadApis
    let actual = Seq.tryFind (fun x -> x.Name = name) apis |> Option.map (fun x -> x.Signature)
    let expected = QueryParser.parseSignature signature |> TestHelpers.updateSource Source.Target
    do! actual |> assertEquals (Some expected)
  })
}

let nonloadedApiTest = parameterize {
  source [
    "PublicModule.internalFunction"
    "PublicModule.privateFunction"
    "InternalModule.publicFunction"
    "PrivateModule.publicFunction"
  ]
  run (fun name -> test {
    let! apis = loadApis
    let actual = Seq.tryFind (fun x -> x.Name = name) apis
    do! actual |> assertEquals None
  })
}