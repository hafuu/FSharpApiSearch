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

  let testMember (name, signatures) = test {
    let! apis = fsharpAssemblyApi
    let actuals = Seq.filter (fun x -> x.Name = name) apis |> Seq.map (fun x -> x.Signature) |> Seq.toList |> List.sort
    let expecteds = signatures |> List.map (QueryParser.parseFSharpSignature >> TestHelpers.updateSource Source.Target) |> List.sort
    do! actuals |> assertEquals expecteds
  }

  let testStaticMethod (name, signatures) = test {
    let! apis = fsharpAssemblyApi
    let actuals = Seq.filter (fun x -> x.Name = name) apis |> Seq.map (fun x -> x.Signature) |> Seq.toList |> List.sort
    let expecteds = signatures |> List.map (QueryParser.parseFSharpSignature >> TestHelpers.updateSource Source.Target >> TestHelpers.toStaticMethod) |> List.sort
    do! actuals |> assertEquals expecteds
  }

  let loadFSharpApiTest = parameterize {
    source [
      "PublicModule.nonGenericFunction", [ "int -> int -> int" ]
      "PublicModule.genericFunction", [ "'a -> 'b -> 'b" ]
      "PublicModule.tupleFunction", [ "'a * 'b * 'c -> 'a" ]
      "PublicModule.value", [ "int" ]
      "PublicModule.NestedModule.publicFunction", [ "int -> int" ]
      "PublicModule.listmap", [ "('a -> 'b) -> 'a list -> 'b list" ]
      "PublicModule.partialGenericMap", [ "Map<int, 'a> -> 'a" ]
      "PublicModule.floatReturnType", [ "int -> float" ]
      "PublicModule.array", [ "int[]" ]
      "PublicModule.array2d", [ "int[,]" ]
      "PublicModule.nestedArray", [ "int[,][]" ]
    ]
    run testMember
  }

  let loadStaticPropertyTest = parameterize {
    source [
      "TopLevelNamespace.StaticMemberClass.Property", [ "string" ]
      "TopLevelNamespace.StaticMemberClass.IndexedProperty", [ "string -> int" ]
    ]
    run testMember
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
    run testStaticMethod
  }

  let loadInstanceMemberTest = parameterize {
    source [
      "TopLevelNamespace.InstanceMemberClass.InstanceMethod1", [ "InstanceMemberClass => unit -> int" ]
      "TopLevelNamespace.InstanceMemberClass.InstanceMethod2", [ "InstanceMemberClass => int -> string" ]
      "TopLevelNamespace.InstanceMemberClass.InstanceMethod3", [ "InstanceMemberClass => string -> float -> float" ]
      "TopLevelNamespace.InstanceMemberClass.OverloadMethod", [ "InstanceMemberClass => int -> int"; "InstanceMemberClass => string -> int" ]
      "TopLevelNamespace.InstanceMemberClass.Property", [ "InstanceMemberClass => string" ]
      "TopLevelNamespace.InstanceMemberClass.IndexedProperty", [ "InstanceMemberClass => string -> int" ]
    ]
    run testMember
  }

  let loadOtherTypeTest = parameterize {
    source [
      "OtherTypes.Record.InstanceMethod1", [ "Record => unit -> int" ]
      "OtherTypes.Record.InstanceMethod2", [ "Record => int -> string" ]
      "OtherTypes.Record.OverloadMethod", [ "Record => unit -> string"; "Record => int -> float" ]
      "OtherTypes.Union.InstanceMethod", [ "Union => unit -> int" ]
      "OtherTypes.Struct.InstanceMethod", [ "Struct => unit -> int" ]
    ]
    run testMember
  }

  let loadOtherTypeStaticTest = parameterize {
    source [
      "OtherTypes.Record.StaticMethod1", [ "unit -> string" ]
      "OtherTypes.Record.StaticMethod2", [ "int * string -> float" ]
    ]
    run testStaticMethod
  }

  let nonloadedApiTest = parameterize {
    source [
      "PublicModule.internalFunction"
      "PublicModule.privateFunction"
      "InternalModule.publicFunction"
      "PrivateModule.publicFunction"
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
      "CSharpLoadTestAssembly.StaticMemberClass.ArrayMethod", [ "unit -> int[]" ]
      "CSharpLoadTestAssembly.StaticMemberClass.Array2dMethod", [ "unit -> int[,]" ]
      "CSharpLoadTestAssembly.StaticMemberClass.NestedArrayMethod", [ "unit -> int[][,]" ] // defined as int[,][] in C#
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

  let loadStaticMemberTest = parameterize {
    source [
      "CSharpLoadTestAssembly.StaticMemberClass.StaticProperty", [ "int" ]
    ]
    run (fun (name, signatures) -> test {
      let! apis = csharpAssemblyApi
      let actuals = Seq.filter (fun x -> x.Name = name) apis |> Seq.map (fun x -> x.Signature) |> Seq.toList |> List.sort
      let expecteds = signatures |> List.map (QueryParser.parseFSharpSignature >> TestHelpers.updateSource Source.Target) |> List.sort
      do! actuals |> assertEquals expecteds
    })
  }



  let nonloadedApiTest = parameterize {
    source [
      "CSharpLoadTestAssembly.StaticMemberClass.StaticField"
    ]
    run (fun name -> test {
      let! apis = csharpAssemblyApi
      let actual = Seq.tryFind (fun x -> x.Name = name) apis
      do! actual |> assertEquals None
    })
  }
