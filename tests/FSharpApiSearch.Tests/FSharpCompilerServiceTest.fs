module FSharpCompilerServiceTest

open Persimmon
open Persimmon.Syntax.UseTestNameByReflection
open Persimmon.MuscleAssert
open TestAssemblies
open Microsoft.FSharp.Compiler.SourceCodeServices

module InferredFloat =
  let ``is float<1>`` (t: FSharpType) = test {
    do! t.TypeDefinition.FullName |> assertEquals "Microsoft.FSharp.Core.float`1"
    do! t.GenericArguments.[0].TypeDefinition.FullName |> assertEquals "Microsoft.FSharp.Core.CompilerServices.MeasureOne"
  }

  let targetClass = test {
    let! assemblies = assemblies
    let assembly = assemblies |> Array.find (fun x -> x.FileName = Some fsharpAssemblyPath )
    return assembly.Contents.Entities |> Seq.find (fun x -> x.DisplayName = "StaticMemberClass") 
  }

  let ``float is inferred to float<1>.`` = test {
    let! class' = targetClass
    let method' = class'.MembersFunctionsAndValues |> Seq.find (fun x -> x.DisplayName = "InferredFloat")
    do! ``is float<1>`` method'.ReturnParameter.Type
    do! ``is float<1>`` method'.CurriedParameterGroups.[0].[0].Type
  }

  let ``is float`` (t: FSharpType) = test {
    do! t.TypeDefinition.FullName |> assertEquals "Microsoft.FSharp.Core.float`1"
    do! t.GenericArguments.Count |> assertEquals 0
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