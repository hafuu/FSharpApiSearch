module ApiLoaderTest

open Persimmon
open Persimmon.Syntax.UseTestNameByReflection

open FSharpApiSearch
open TestHelpers.DSL
open System.Reflection
open System.IO

let fsharpAssemblyName = @"LoadTestAssembly"
let fsharpAssemblyPath =
  Path.Combine(
    Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)
    , fsharpAssemblyName + ".dll")

let csharpAssemblyName = @"CSharpLoadTestAssembly";
let csharpAssemblyPath =
  Path.Combine(
    Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)
    , csharpAssemblyName + ".dll")

let loadAssemblies = test {
  return ApiLoader.loadAssembly (Path.GetFullPath(fsharpAssemblyPath) :: Path.GetFullPath(csharpAssemblyPath) :: FSharpApiSearchClient.DefaultReferences)
}

module FSharpTest =
  let fsharpAssemblyApi = test {
    let! assemblies = loadAssemblies
    let assemblyApi = assemblies |> List.find (fun x -> x.FileName = Some fsharpAssemblyPath ) |> ApiLoader.load
    return assemblyApi
  }

  let testMember (name, signatures) = test {
    let! api = fsharpAssemblyApi
    let actuals = Seq.filter (fun x -> x.Name = name) api.Api |> Seq.map (fun x -> x.Signature) |> Seq.toList |> List.sort
    let expecteds =
      signatures
      |> List.map (TestHelpers.replaceFSharpTypes
        >> QueryParser.parseFSharpSignature
        >> TestHelpers.toFullName
        >> TestHelpers.replaceAbbreviation
        >> TestHelpers.updateSource Source.Target) |> List.sort
    do! actuals |> assertEquals expecteds
  }

  let testStaticMethod (name, signatures) = test {
    let! api = fsharpAssemblyApi
    let actuals = Seq.filter (fun x -> x.Name = name) api.Api |> Seq.map (fun x -> x.Signature) |> Seq.toList |> List.sort
    let expecteds =
      signatures
      |> List.map (TestHelpers.replaceFSharpTypes
        >> QueryParser.parseFSharpSignature
        >> TestHelpers.toStaticMethod
        >> TestHelpers.toFullName
        >> TestHelpers.replaceAbbreviation
        >> TestHelpers.updateSource Source.Target) |> List.sort
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
      "TopLevelNamespace.StaticMemberClass", [ "unit -> TopLevelNamespace.StaticMemberClass"; "int -> TopLevelNamespace.StaticMemberClass" ]
      "TopLevelNamespace.StaticMemberClass.OverloadMethod", [ "int -> int"; "string * int -> string" ]
      "TopLevelNamespace.StaticMemberClass.InferredFloatType", [ "float -> float" ]
    ]
    run testStaticMethod
  }

  let loadInstanceMemberTest = parameterize {
    source [
      "TopLevelNamespace.InstanceMemberClass.InstanceMethod1", [ "TopLevelNamespace.InstanceMemberClass => unit -> int" ]
      "TopLevelNamespace.InstanceMemberClass.InstanceMethod2", [ "TopLevelNamespace.InstanceMemberClass => int -> string" ]
      "TopLevelNamespace.InstanceMemberClass.InstanceMethod3", [ "TopLevelNamespace.InstanceMemberClass => string -> float -> float" ]
      "TopLevelNamespace.InstanceMemberClass.OverloadMethod", [ "TopLevelNamespace.InstanceMemberClass => int -> int"; "TopLevelNamespace.InstanceMemberClass => string -> int" ]
      "TopLevelNamespace.InstanceMemberClass.Property", [ "TopLevelNamespace.InstanceMemberClass => string" ]
      "TopLevelNamespace.InstanceMemberClass.IndexedProperty", [ "TopLevelNamespace.InstanceMemberClass => string -> int" ]
      "TopLevelNamespace.GenericClass.Method", [ "TopLevelNamespace.GenericClass<'a> => 'a -> int" ]
      "TopLevelNamespace.Interface.Method", [ "TopLevelNamespace.Interface => int -> string -> int" ]
      "TopLevelNamespace.Interface.Property", [ "TopLevelNamespace.Interface => string" ]
    ]
    run testMember
  }

  let interfaceInheritanceTest = parameterize {
    source [
      "InterfaceInheritance.ChildInterface.ChildMethod", [ "InterfaceInheritance.ChildInterface => unit -> float" ]
      "InterfaceInheritance.ChildInterface.ParentMethod", [ "InterfaceInheritance.ChildInterface => unit -> string" ]
      "InterfaceInheritance.ChildInterface.GrandParentMethod", [ "InterfaceInheritance.ChildInterface => unit -> int" ]
      "InterfaceInheritance.GenericChildInterface.ParentMethod", [ "InterfaceInheritance.GenericChildInterface<'a> => 'a -> 'b" ]
      "InterfaceInheritance.GenericChildInterface.GrandParentMethod", [ "InterfaceInheritance.GenericChildInterface<'a> => 'a -> 'u" ]
      "InterfaceInheritance.ConflictParameterInterface.ParentMethod", [ "InterfaceInheritance.ConflictParameterInterface<'b> => 'b -> 'b1" ]
      "InterfaceInheritance.IntChildInterface.ParentMethod", [ "InterfaceInheritance.IntChildInterface => int -> 'b" ]
    ]
    run testMember
  }

  let loadOtherTypeTest = parameterize {
    source [
      "OtherTypes.Record.InstanceMethod1", [ "OtherTypes.Record => unit -> int" ]
      "OtherTypes.Record.InstanceMethod2", [ "OtherTypes.Record => int -> string" ]
      "OtherTypes.Record.OverloadMethod", [ "OtherTypes.Record => unit -> string"; "OtherTypes.Record => int -> float" ]
      "OtherTypes.Record.FieldA", [ "OtherTypes.Record => int" ]
      "OtherTypes.Union.InstanceMethod", [ "OtherTypes.Union => unit -> int" ]
      "OtherTypes.Struct.InstanceMethod", [ "OtherTypes.Struct => unit -> int" ]
      "OtherTypes.Struct.A", [ "OtherTypes.Struct => int" ]
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
      let! api = fsharpAssemblyApi
      let actual = Seq.tryFind (fun x -> x.Name = name) api.Api
      do! actual |> assertEquals None
    })
  }

  let typeAbbreviationTest = parameterize {
    source[
      { Abbreviation = generic (fullIdentity "TypeAbbreviations.GenericTypeAbbreviation") [ variable "b" ]; Original = generic (fullIdentity "TypeAbbreviations.Original") [ variable "b" ] }
      { Abbreviation = fullIdentity "TypeAbbreviations.SpecializedTypeAbbreviation"; Original = generic (fullIdentity "TypeAbbreviations.Original") [ fullIdentity "TypeAbbreviations.A" ] }
      { Abbreviation = fullIdentity "TypeAbbreviations.NestedTypeAbbreviation"; Original = generic (fullIdentity "TypeAbbreviations.Original") [ fullIdentity "TypeAbbreviations.A" ] }
      { Abbreviation = generic (fullIdentity "TypeAbbreviations.NestedModule.TypeAbbreviationInModule") [ variable "a" ]; Original = generic (fullIdentity "TypeAbbreviations.Original") [ variable "a" ] }
    ]

    run (fun (ta) -> test {
      let! api = fsharpAssemblyApi
      let ta = { Abbreviation = TestHelpers.updateSource Source.Target ta.Abbreviation; Original = TestHelpers.updateSource Source.Target ta.Original }
      let actual = api.TypeAbbreviations |> List.contains ta
      do! actual |> assertEquals true
    })
  }

  let functionTypeAbbreviationTest = parameterize {
    source[
      identity "FunctionAbbreviation"
    ]

    run (fun (key) -> test {
      let! api = fsharpAssemblyApi
      let key = key |> TestHelpers.updateSource Source.Target
      let actual = api.TypeAbbreviations |> List.exists (fun x -> x.Abbreviation = key)
      do! actual |> assertEquals false
    })
  }

module CSharpTest =
  let csharpAssemblyApi = test {
    let! assemblies = loadAssemblies
    let assemblyApi = assemblies |> List.find (fun x -> x.FileName = Some csharpAssemblyPath ) |> ApiLoader.load
    return assemblyApi.Api
  }

  let loadStaticMethodTest = parameterize {
    source [
      "CSharpLoadTestAssembly.StaticMemberClass.StaticMethod1", [ "unit -> int" ]
      "CSharpLoadTestAssembly.StaticMemberClass.StaticMethod2", [ "int * int * string -> float" ]
      "CSharpLoadTestAssembly.StaticMemberClass.StaticMethod3", [ "unit -> unit" ]
      "CSharpLoadTestAssembly.StaticMemberClass.ArrayMethod", [ "unit -> int[]" ]
      "CSharpLoadTestAssembly.StaticMemberClass.Array2dMethod", [ "unit -> int[,]" ]
      "CSharpLoadTestAssembly.StaticMemberClass.NestedArrayMethod", [ "unit -> int[][,]" ] // defined as int[,][] in C#
      "CSharpLoadTestAssembly.StaticMemberClass", [ "unit -> CSharpLoadTestAssembly.StaticMemberClass"; "string * string -> CSharpLoadTestAssembly.StaticMemberClass" ]
      "CSharpLoadTestAssembly.OuterClass.InnerClass.StaticMethod", [ "unit -> int" ]
      "CSharpLoadTestAssembly.OuterClass.InnerClass", [ "unit -> CSharpLoadTestAssembly.OuterClass.InnerClass" ]
    ]
    run (fun (name, signatures) -> test {
      let! apis = csharpAssemblyApi
      let actuals = Seq.filter (fun x -> x.Name = name) apis |> Seq.map (fun x -> x.Signature) |> Seq.toList |> List.sort
      let expecteds =
        signatures
        |> List.map (TestHelpers.replaceFSharpTypes
          >> QueryParser.parseFSharpSignature
          >> TestHelpers.toStaticMethod
          >> TestHelpers.toFullName
          >> TestHelpers.replaceAbbreviation
          >> TestHelpers.updateSource Source.Target) |> List.sort
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
      let expecteds =
        signatures
        |> List.map (TestHelpers.replaceFSharpTypes
          >> QueryParser.parseFSharpSignature
          >> TestHelpers.toFullName
          >> TestHelpers.replaceAbbreviation
          >> TestHelpers.updateSource Source.Target) |> List.sort
      do! actuals |> assertEquals expecteds
    })
  }

  let loadInstanceMemterTest = parameterize {
    source [
      "CSharpLoadTestAssembly.Interface.Method", [ "CSharpLoadTestAssembly.Interface => int -> string -> int" ]
      "CSharpLoadTestAssembly.Interface.Property", [ "CSharpLoadTestAssembly.Interface => int" ]
      "CSharpLoadTestAssembly.GenericInterface.Method", [ "CSharpLoadTestAssembly.GenericInterface<'T> => 'T -> int" ]
      "CSharpLoadTestAssembly.GenericInterface.Property", [ "CSharpLoadTestAssembly.GenericInterface<'T> => 'T -> 'T" ]
    ]
    run (fun (name, signatures) -> test {
      let! apis = csharpAssemblyApi
      let actuals = Seq.filter (fun x -> x.Name = name) apis |> Seq.map (fun x -> x.Signature) |> Seq.toList |> List.sort
      let expecteds =
        signatures
        |> List.map (TestHelpers.replaceFSharpTypes
          >> QueryParser.parseFSharpSignature
          >> TestHelpers.toFullName
          >> TestHelpers.replaceAbbreviation
          >> TestHelpers.updateSource Source.Target) |> List.sort
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
