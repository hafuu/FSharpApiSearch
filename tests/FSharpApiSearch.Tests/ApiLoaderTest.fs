module ApiLoaderTest

open Persimmon
open Persimmon.Syntax.UseTestNameByReflection

open FSharpApiSearch
open TestHelpers.DSL
open System.Reflection
open System.IO

let fsharpAssemblyName = @"LoadTestAssembly.dll"
let fsharpAssemblyPath =
  Path.Combine(
    Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)
    , fsharpAssemblyName)

let csharpAssemblyName = @"CSharpLoadTestAssembly.dll";
let csharpAssemblyPath =
  Path.Combine(
    Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)
    , csharpAssemblyName)

let loadAssemblies = test {
  return ApiLoader.loadAssembly [ Path.GetFullPath(fsharpAssemblyPath); Path.GetFullPath(csharpAssemblyPath) ]
}

module FSharpTest =
  let fsharpAssemblyApi = test {
    let! assemblies = loadAssemblies
    return assemblies |> List.find (fun x -> x.FileName = Some fsharpAssemblyPath ) |> ApiLoader.collectApi
  }

  let loadFSharpApiTest = parameterize {
    source [
      "PublicModule.nonGenericFunction", "int -> int -> int"
      "PublicModule.genericFunction", "'a -> 'b -> 'b"
      "PublicModule.tupleFunction", "'a * 'b * 'c -> 'a"
      "PublicModule.value", "int"
      "PublicModule.NestedModule.publicFunction", "int -> int"
      "PublicModule.listmap", "('a -> 'b) -> 'a list -> 'b list"
      "PublicModule.partialGenericMap", "Map<int, 'a> -> 'a"
      "PublicModule.floatReturnType", "int -> float"
    ]
    run (fun (name, signature) -> test {
      let! apis = fsharpAssemblyApi
      let actual = Seq.tryFind (fun x -> x.Name = name) apis |> Option.map (fun x -> x.Signature)
      let expected = QueryParser.parseFSharpSignature signature |> TestHelpers.updateSource Source.Target
      do! actual |> assertEquals (Some expected)
    })
  }

  let loadStaticMethodTest = parameterize {
    source [
      "TopLevelNamespace.StaticMemberClass.StaticMethod1", [ "unit -> int" ]
      "TopLevelNamespace.StaticMemberClass.StaticMethod2", [ "int * string -> int" ]
      "TopLevelNamespace.StaticMemberClass.FloatReturnType", [ "single * float -> float" ]
      "TopLevelNamespace.StaticMemberClass.SingleReturnType", [ "int -> single" ]
      "TopLevelNamespace.StaticMemberClass", [ "unit -> StaticMemberClass"; "int -> StaticMemberClass" ]
      "TopLevelNamespace.StaticMemberClass.OverloadMethod", [ "int -> int"; "string * int -> string" ]
      "TopLevelNamespace.StaticMemberClass.InferencedFloatType", [ "float -> float" ]
    ]
    run (fun (name, signatures) -> test {
      let! apis = fsharpAssemblyApi
      let actuals = Seq.filter (fun x -> x.Name = name) apis |> Seq.map (fun x -> x.Signature) |> Seq.toList |> List.sort
      let expecteds = signatures |> List.map (QueryParser.parseFSharpSignature >> TestHelpers.updateSource Source.Target >> TestHelpers.toStaticMethod) |> List.sort
      do! actuals |> assertEquals expecteds
    })
  }

  let nonloadedApiTest = parameterize {
    source [
      "PublicModule.internalFunction"
      "PublicModule.privateFunction"
      "InternalModule.publicFunction"
      "PrivateModule.publicFunction"
      "TopLevelNamespace.StaticMemberClass.Property"
    ]
    run (fun name -> test {
      let! apis = fsharpAssemblyApi
      let actual = Seq.tryFind (fun x -> x.Name = name) apis
      do! actual |> assertEquals None
    })
  }

module CSharpTest =
  let csharpAssemblyApi = test {
    let! assemblies = loadAssemblies
    return assemblies |> List.find (fun x -> x.FileName = Some csharpAssemblyPath ) |> ApiLoader.collectApi
  }
  let loadStaticMethodTest = parameterize {
    source [
      "CSharpLoadTestAssembly.StaticMemberClass.StaticMethod1", [ "unit -> int" ]
      "CSharpLoadTestAssembly.StaticMemberClass.StaticMethod2", [ "int * int * string -> float" ]
      "CSharpLoadTestAssembly.StaticMemberClass.StaticMethod3", [ "unit -> unit" ]
      "CSharpLoadTestAssembly.StaticMemberClass", [ "unit -> StaticMemberClass"; "string * string -> StaticMemberClass" ]
      "CSharpLoadTestAssembly.OuterClass.InnerClass.StaticMethod", [ "unit -> int" ]
      "CSharpLoadTestAssembly.OuterClass.InnerClass", [ "unit -> InnerClass" ]
    ]
    run (fun (name, signatures) -> test {
      let! apis = csharpAssemblyApi
      let actuals = Seq.filter (fun x -> x.Name = name) apis |> Seq.map (fun x -> x.Signature) |> Seq.toList |> List.sort
      let expecteds = signatures |> List.map (QueryParser.parseFSharpSignature >> TestHelpers.updateSource Source.Target >> TestHelpers.toStaticMethod) |> List.sort
      do! actuals |> assertEquals expecteds
    })
  }

  let nonloadedApiTest = parameterize {
    source [
      "CSharpLoadTestAssembly.StaticMemberClass.StaticProperty"
      "CSharpLoadTestAssembly.StaticMemberClass.StaticField"
    ]
    run (fun name -> test {
      let! apis = csharpAssemblyApi
      let actual = Seq.tryFind (fun x -> x.Name = name) apis
      do! actual |> assertEquals None
    })
  }
