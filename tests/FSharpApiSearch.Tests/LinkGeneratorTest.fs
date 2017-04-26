﻿module LinkGeneratorTest

open Persimmon
open Persimmon.Syntax.UseTestNameByReflection
open Persimmon.MuscleAssert
open FSharpApiSearch
open TestAssemblies

let fsharpTest = parameterize {
  source [
    "Microsoft.FSharp.Core.Operators.( >= )", Some "operators.%5b-%5d%3d-%5d%5b%27t%5d-function-%5bfsharp%5d"
    "Microsoft.FSharp.Core.Operators.( |Failure|_| )", Some "operators.failure-active-pattern-%5bfsharp%5d"
    "Microsoft.FSharp.Core.Operators.incr", Some "operators.incr-function-%5bfsharp%5d"
    "Microsoft.FSharp.Control.Async.Start", Some "async.start-method-%5bfsharp%5d"
    "Microsoft.FSharp.Control.Async.CancellationToken", Some "async.cancellationtoken-property-%5bfsharp%5d"
    "Microsoft.FSharp.Collections.Map<'Key, 'Value>.new", Some "collections.map%5b%27key%2c%27value%5d-constructor-%5bfsharp%5d"
    "Microsoft.FSharp.Core.array", Some "core.array%5b%27t%5d-type-abbreviation-%5bfsharp%5d"
    "Microsoft.FSharp.Core.unit", Some "core.unit-type-abbreviation-%5bfsharp%5d"

    "Microsoft.FSharp.Core.[]", None
    "Microsoft.FSharp.Core.FSharpTypeFunc", Some "core.fsharptypefunc-class-%5bfsharp%5d"

    "Microsoft.FSharp.Control.AsyncBuilder", Some "control.asyncbuilder-class-%5bfsharp%5d"
  ]
  run (fun (name, expected) -> test {
    let! apiDict = fscoreApi
    let api = apiDict.Api |> Array.find (fun a -> a.Name.Print() = name)
    let actual = LinkGenerator.FSharp.generate api
    do! actual |> assertEquals expected
  })
}

let msdnTest = parameterize {
  source [
    "System.Random", "type Random", None
    "System.Progress", "type Progress<'T>", None
    "System.Random.Next", "unit -> int", Some "system.random.next?#System_Random_Next"
    "System.Random.Next", "maxValue:int -> int", Some "system.random.next?#System_Random_Next_System_Int32_"
    "System.Random.Next", "minValue:int * maxValue:int -> int", Some "system.random.next?#System_Random_Next_System_Int32_System_Int32_"
    "System.Random.NextBytes", "buffer:byte[] -> unit", Some "system.random.nextbytes?#System_Random_NextBytes_System_Byte___"
    "System.Lazy<'T>.Value", "'T", Some "system.lazy-1.value?#System_Lazy_1_Value"
    "System.Collections.Generic.Dictionary<'TKey, 'TValue>.ValueCollection<'TKey, 'TValue>.Enumerator<'TKey, 'TValue>.Current", "'TValue", 
        Some "system.collections.generic.dictionary-2.valuecollection.enumerator.current?#System_Collections_Generic_Dictionary_2_ValueCollection_Enumerator_Current"
    "System.String.Split", "separator:char[] -> string[]", Some "system.string.split?#System_String_Split_System_Char___"
  ]
  run (fun (name, signature, expected) -> test {
    let! apiDict = mscorlibApi
    let api = apiDict.Api |> Array.find (fun a -> a.Name.Print() = name && a.Signature.Print() = signature)
    let actual = LinkGenerator.Msdn.generate api
    do! actual |> assertEquals expected
  })
}
