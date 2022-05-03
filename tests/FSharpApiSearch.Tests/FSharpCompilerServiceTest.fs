[<Persimmon.Category("fcs")>]
module FSharpCompilerServiceTest

open Persimmon
open Persimmon.Syntax.UseTestNameByReflection
open Persimmon.MuscleAssert
open TestAssemblies
open FSharp.Compiler.Symbols

module InferredFloat =
  let ``is float`` (t: FSharpType) = test {
    let entity = t.TypeDefinition
    do! sprintf "%s.%s" entity.AccessPath entity.CompiledName |> assertEquals "Microsoft.FSharp.Core.float"
    do! entity.TryFullName |> assertEquals None
    do! t.GenericArguments.Count |> assertEquals 0
  }

  let targetClass = test {
    let! assemblies = assemblies
    let assembly = assemblies |> Array.find (fun x -> x.FileName = Some fsharpAssemblyPath )
    return assembly.Contents.Entities |> Seq.find (fun x -> x.DisplayName = "StaticMemberClass") 
  }

  let ``float is inferred to float`` = test {
    let! class' = targetClass
    let method' = class'.MembersFunctionsAndValues |> Seq.find (fun x -> x.DisplayName = "InferredFloat")
    do! ``is float`` method'.ReturnParameter.Type
    do! ``is float`` method'.CurriedParameterGroups.[0].[0].Type
  } 

  let ``annotated float is float`` = test {
    let! class' = targetClass
    let method' = class'.MembersFunctionsAndValues |> Seq.find (fun x -> x.DisplayName = "AnnotatedFloat")
    do! ``is float`` method'.ReturnParameter.Type
    do! ``is float`` method'.CurriedParameterGroups.[0].[0].Type
  }

let ``auto generic parameter`` = test {
  let! assemblies = assemblies
  let assembly = assemblies |> Array.find (fun x -> x.FileName = Some fsharpAssemblyPath)
  let module' = assembly.Contents.Entities |> Seq.find (fun x -> x.DisplayName = "PublicModule")
  let autoGenericFun = module'.MembersFunctionsAndValues |> Seq.find (fun x -> x.DisplayName = "autoGenericFunction")
  let parameter = autoGenericFun.CurriedParameterGroups.[0].[0]
  do! parameter.Type.ToString() |> assertEquals "type 'a"
  do! autoGenericFun.GenericParameters.[0].Name.StartsWith("?") |> assertEquals true
}

let ``Enum.value__ test`` = test {
  let! assemblies = assemblies
  let assembly = assemblies |> Array.find (fun x -> x.FileName = Some fsharpAssemblyPath )
  let enum = assembly.Contents.Entities |> Seq.find (fun x -> x.DisplayName = "Enum")
  let value__ = enum.FSharpFields |> Seq.find (fun x -> x.DisplayName = "value__")
  do! value__.DisplayName |> assertEquals "value__"
  do! value__.IsCompilerGenerated |> assertEquals true
}

let ``ValueTuple test (F#)`` = test {
  let! assemblies = assemblies
  let assembly = assemblies |> Array.find (fun x -> x.FileName = Some fsharpAssemblyPath )
  let module' = assembly.Contents.Entities |> Seq.find (fun x -> x.DisplayName = "FSharp41")
  let value = module'.MembersFunctionsAndValues |> Seq.find (fun x -> x.DisplayName = "structTuple")
  let actual = value.ReturnParameter.Type
  do! actual.IsTupleType |> assertEquals true
  do! actual.IsStructTupleType |> assertEquals true
}

let ``ValueTuple test (C#)`` = test {
  let! assemblies = assemblies
  let assembly = assemblies |> Array.find (fun x -> x.FileName = Some csharpAssemblyPath )
  let class' = assembly.Contents.Entities |> Seq.find (fun x -> x.DisplayName = "Tuples")
  let method' = class'.MembersFunctionsAndValues |> Seq.find (fun x -> x.DisplayName = "G")
  let actual = method'.ReturnParameter.Type
  if actual.HasTypeDefinition then
    do! actual.TypeDefinition.FullName |> assertEquals "System.ValueTuple`2"
    do! actual.IsTupleType |> assertEquals false
    do! actual.IsStructTupleType |> assertEquals false
  else
    do! actual.IsTupleType |> assertEquals true
    do! actual.IsStructTupleType |> assertEquals true
}

let ``accesspath test`` = test {
  let! assemblies = assemblies
  let assembly = assemblies |> Array.find (fun x -> x.FileName = Some fsharpAssemblyPath )
  let module' = assembly.Contents.Entities |> Seq.find (fun x -> x.DisplayName = "M2")

  let c = module'.DeclaringEntity |> Option.get
  do! c.DisplayName |> assertEquals "C"

  let b = c.DeclaringEntity |> Option.get
  do! b.DisplayName |> assertEquals "B"

  do! b.DeclaringEntity |> assertEquals None
}