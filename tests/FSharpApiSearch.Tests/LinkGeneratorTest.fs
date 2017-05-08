module LinkGeneratorTest

open Persimmon
open Persimmon.Syntax.UseTestNameByReflection
open Persimmon.MuscleAssert
open FSharpApiSearch
open FSharpApiSearch.Printer
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
    "System.Random", None
    "System.Random.Next", Some "system.random.next.aspx"
    "System.Random.NextBytes", Some "system.random.nextbytes.aspx"
    "System.Lazy<'T>.Value", None
  ]
  run (fun (name, expected) -> test {
    let! apiDict = mscorlibApi
    let api = apiDict.Api |> Array.find (fun a -> a.Name.Print() = name)
    let actual = LinkGenerator.Msdn.generate api
    do! actual |> assertEquals expected
  })
}

let dotNetApiBrowserTest = parameterize {
  source [
    "System.Random", "type Random", Some "system.random?view=netframework-4.7"
    "System.Progress", "type Progress<'T>", Some "system.progress-1?view=netframework-4.7"
    "System.Random.Next", "unit -> int", Some "system.random.next?view=netframework-4.7#System_Random_Next"
    "System.Random.Next", "maxValue:int -> int", Some "system.random.next?view=netframework-4.7#System_Random_Next_System_Int32_"
    "System.Random.Next", "minValue:int * maxValue:int -> int", Some "system.random.next?view=netframework-4.7#System_Random_Next_System_Int32_System_Int32_"
    "System.Random.NextBytes", "buffer:byte[] -> unit", Some "system.random.nextbytes?view=netframework-4.7#System_Random_NextBytes_System_Byte___"
    "System.Lazy<'T>.Value", "'T", Some "system.lazy-1.value?view=netframework-4.7#System_Lazy_1_Value"
    "System.Collections.Generic.Dictionary<'TKey, 'TValue>.ValueCollection<'TKey, 'TValue>.Enumerator<'TKey, 'TValue>.Current", "'TValue", 
        Some "system.collections.generic.dictionary-2.valuecollection.enumerator.current?view=netframework-4.7#System_Collections_Generic_Dictionary_2_ValueCollection_Enumerator_Current"
    "System.String.Split", "separator:char[] -> string[]", Some "system.string.split?view=netframework-4.7#System_String_Split_System_Char___"
    "System.Collections.Generic.List<'T>.Reverse", "index:int * count:int -> unit" , Some "system.collections.generic.list-1.reverse?view=netframework-4.7#System_Collections_Generic_List_1_Reverse_System_Int32_System_Int32_"
    "System.Collections.Generic.Dictionary<'TKey, 'TValue>.TryGetValue" , "key:'TKey * value:byref<'TValue> -> bool", Some "system.collections.generic.dictionary-2.trygetvalue?view=netframework-4.7#System_Collections_Generic_Dictionary_2_TryGetValue__0__1__"
    "System.Random.new" , "unit -> Random", Some "system.random.-ctor?view=netframework-4.7#System_Random__ctor"
    "System.Random.new" , "Seed:int -> Random", Some "system.random.-ctor?view=netframework-4.7#System_Random__ctor_System_Int32_"
    "System.Progress<'T>.new" , "unit -> Progress<'T>" , Some "system.progress-1.-ctor?view=netframework-4.7#System_Progress_1__ctor"
    "System.String.Join", "separator:string * values:IEnumerable<'T> -> string", Some "system.string.join--1?view=netframework-4.7#System_String_Join__1_System_String_System_Collections_Generic_IEnumerable___0__"
    "System.String.Join", "separator:string * values:IEnumerable<string> -> string", Some "system.string.join?view=netframework-4.7#System_String_Join_System_String_System_Collections_Generic_IEnumerable_System_String__"
  ]
  run (fun (name, signature, expected) -> test {
    let! apiDict = mscorlibApi
    let api = apiDict.Api |> Array.find (fun a -> a.Name.Print() = name && a.Signature.Print() = signature)
    let actual = LinkGenerator.DotNetApiBrowser.generate "netframework-4.7" api
    do! actual |> assertEquals expected
  })
}

let dotNetApiBrowserExtensionTest = parameterize {
  source [
    "System.Linq.Queryable.SelectMany", "source:IQueryable<'TSource> * selector:Expression<Func<'TSource, IEnumerable<'TResult>>> -> IQueryable<'TResult>", 
        Some "system.linq.queryable.selectmany--2?view=netframework-4.7#System_Linq_Queryable_SelectMany__2_System_Linq_IQueryable___0__System_Linq_Expressions_Expression_System_Func___0_System_Collections_Generic_IEnumerable___1____"
  ]
  run (fun (name, signature, expected) -> test {
    let! apiDict = systemCoreApi
    let api = apiDict.Api |> Array.find (fun a -> a.Name.Print() = name && a.Signature.Print() = signature)
    let actual = LinkGenerator.DotNetApiBrowser.generate "netframework-4.7" api
    do! actual |> assertEquals expected
  })
}

let dotNetApiBrowserViewTest = parameterize {
  source [
    "System.String", "type String", "netcore-1.0", Some "system.string?view=netcore-1.0"
    "System.String", "type String", "netcore-1.1", Some "system.string?view=netcore-1.1"
    "System.String", "type String", "netframework-4.5", Some "system.string?view=netframework-4.5"
    "System.String", "type String", "netframework-4.5.1", Some "system.string?view=netframework-4.5.1"
    "System.String", "type String", "netframework-4.5.2", Some "system.string?view=netframework-4.5.2"
    "System.String", "type String", "netframework-4.6", Some "system.string?view=netframework-4.6"
    "System.String", "type String", "netframework-4.6.1", Some "system.string?view=netframework-4.6.1"
    "System.String", "type String", "netframework-4.6.2", Some "system.string?view=netframework-4.6.2"
    "System.String", "type String", "netframework-4.7", Some "system.string?view=netframework-4.7"
    "System.String", "type String", "netstandard-1.0", Some "system.string?view=netstandard-1.0"
    "System.String", "type String", "netstandard-1.1", Some "system.string?view=netstandard-1.1"
    "System.String", "type String", "netstandard-1.2", Some "system.string?view=netstandard-1.2"
    "System.String", "type String", "netstandard-1.3", Some "system.string?view=netstandard-1.3"
    "System.String", "type String", "netstandard-1.4", Some "system.string?view=netstandard-1.4"
    "System.String", "type String", "netstandard-1.5", Some "system.string?view=netstandard-1.5"
    "System.String", "type String", "netstandard-1.6", Some "system.string?view=netstandard-1.6"
    "System.String", "type String", "xamarinios-10.8", Some "system.string?view=xamarinios-10.8"
    "System.String", "type String", "xamarinandroid-7.1", Some "system.string?view=xamarinandroid-7.1"
    "System.String", "type String", "xamarinmac-3.0", Some "system.string?view=xamarinmac-3.0"
    ]
  run (fun (name, signature, view, expected) -> test {
    let! apiDict = mscorlibApi
    let api = apiDict.Api |> Array.find (fun a -> a.Name.Print() = name && a.Signature.Print() = signature)
    let actual = LinkGenerator.DotNetApiBrowser.generate view api
    do! actual |> assertEquals expected
  })
}
