module LinkGeneratorTest

open Persimmon
open Persimmon.Syntax.UseTestNameByReflection
open Persimmon.MuscleAssert
open FSharpApiSearch
open FSharpApiSearch.Printer
open TestAssemblies

let fsharpTest = parameterize {
  source [
    "Microsoft.FSharp.Core.Operators.( >= )<'T>", Some "operators.%5b-%5d%3d-%5d%5b%27t%5d-function-%5bfsharp%5d"
    "Microsoft.FSharp.Core.Operators.( |Failure|_| )", Some "operators.failure-active-pattern-%5bfsharp%5d"
    "Microsoft.FSharp.Core.Operators.incr", Some "operators.incr-function-%5bfsharp%5d"
    "Microsoft.FSharp.Control.Async.Start", Some "async.start-method-%5bfsharp%5d"
    "Microsoft.FSharp.Control.Async.CancellationToken", Some "async.cancellationtoken-property-%5bfsharp%5d"
    "Microsoft.FSharp.Collections.Map<'Key, 'Value>.new", Some "collections.map%5b%27key%2c%27value%5d-constructor-%5bfsharp%5d"
    "Microsoft.FSharp.Core.array<'T>", Some "core.array%5b%27t%5d-type-abbreviation-%5bfsharp%5d"
    "Microsoft.FSharp.Core.unit", Some "core.unit-type-abbreviation-%5bfsharp%5d"

    "Microsoft.FSharp.Core.[]<'T>", None
    "Microsoft.FSharp.Core.FSharpTypeFunc", Some "core.fsharptypefunc-class-%5bfsharp%5d"

    "Microsoft.FSharp.Control.AsyncBuilder", Some "control.asyncbuilder-class-%5bfsharp%5d"

    "Microsoft.FSharp.Core.Operators.( = )<'T>", Some "operators.%5b%3d%5d%27t-function-%5bfsharp%5d"
    "Microsoft.FSharp.Core.ExtraTopLevelOperators.array2D<'a, 'T>", Some "extratopleveloperators.array2d%5b%27t%5d-function-%5bfsharp%5d"
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

// テストケース表
// https://docs.google.com/spreadsheets/d/1pirq_tH9h-XdmgRUujGi-HXXBVtCYumDsqu-HP_xf_w/edit?usp=sharing
let dotNetApiBrowserTest = parameterize {
  source [
    // 1
    mscorlibApi, "System.Random", "type Random", Some "system.random?view=netframework-4.7"
    // 2
    mscorlibApi, "System.Progress<'T>", "type Progress<'T>", Some "system.progress-1?view=netframework-4.7"
    // 3
    mscorlibApi, "System.Random.Next", "unit -> int", Some "system.random.next?view=netframework-4.7#System_Random_Next"
    // 4
    mscorlibApi, "System.Random.Next", "maxValue:int -> int", Some "system.random.next?view=netframework-4.7#System_Random_Next_System_Int32_"
    // 5
    mscorlibApi, "System.Random.Next", "minValue:int * maxValue:int -> int", Some "system.random.next?view=netframework-4.7#System_Random_Next_System_Int32_System_Int32_"
    // 6
    mscorlibApi, "System.Random.NextBytes", "buffer:byte[] -> unit", Some "system.random.nextbytes?view=netframework-4.7#System_Random_NextBytes_System_Byte___"
    // 7
    mscorlibApi, "System.Lazy<'T>.Value", "'T", Some "system.lazy-1.value?view=netframework-4.7#System_Lazy_1_Value"
    // 8
    mscorlibApi, "System.Collections.Generic.Dictionary<'TKey, 'TValue>.ValueCollection<'TKey, 'TValue>.Enumerator<'TKey, 'TValue>.Current", "'TValue", 
        Some "system.collections.generic.dictionary-2.valuecollection.enumerator.current?view=netframework-4.7#System_Collections_Generic_Dictionary_2_ValueCollection_Enumerator_Current"
    // 9
    mscorlibApi, "System.Collections.Generic.List<'T>.Reverse", "index:int * count:int -> unit" , Some "system.collections.generic.list-1.reverse?view=netframework-4.7#System_Collections_Generic_List_1_Reverse_System_Int32_System_Int32_"
    // 10
    mscorlibApi, "System.Collections.Generic.Dictionary<'TKey, 'TValue>.TryGetValue" , "key:'TKey * value:byref<'TValue> -> bool", Some "system.collections.generic.dictionary-2.trygetvalue?view=netframework-4.7#System_Collections_Generic_Dictionary_2_TryGetValue__0__1__"
    // 11
    mscorlibApi, "System.Random.new" , "unit -> Random", Some "system.random.-ctor?view=netframework-4.7#System_Random__ctor"
    // 12
    mscorlibApi, "System.Random.new" , "Seed:int -> Random", Some "system.random.-ctor?view=netframework-4.7#System_Random__ctor_System_Int32_"
    // 13
    mscorlibApi, "System.Progress<'T>.new" , "unit -> Progress<'T>" , Some "system.progress-1.-ctor?view=netframework-4.7#System_Progress_1__ctor"
    // 14
    mscorlibApi, "System.String.Join<'T>", "separator:string * values:IEnumerable<'T> -> string", Some "system.string.join?view=netframework-4.7#System_String_Join__1_System_String_System_Collections_Generic_IEnumerable___0__"
    // 15
    mscorlibApi, "System.String.Join", "separator:string * values:IEnumerable<string> -> string", Some "system.string.join?view=netframework-4.7#System_String_Join_System_String_System_Collections_Generic_IEnumerable_System_String__"
    // 16
    mscorlibApi, "System.String.Split", "[<ParamArray>]separator:char[] -> string[]", Some "system.string.split?view=netframework-4.7#System_String_Split_System_Char___"
    // 17
    mscorlibApi, "System.Progress<'T>.new" , "unit -> Progress<'T>" , Some "system.progress-1.-ctor?view=netframework-4.7#System_Progress_1__ctor"
    // 18
    mscorlibApi, "System.Collections.Generic.Dictionary<'TKey, 'TValue>.KeyCollection<'TKey, 'TValue>.new", "dictionary:Dictionary<'TKey, 'TValue> -> KeyCollection<'TKey, 'TValue>", Some "system.collections.generic.dictionary-2.keycollection.-ctor?view=netframework-4.7#System_Collections_Generic_Dictionary_2_KeyCollection__ctor_System_Collections_Generic_Dictionary__0__1__"
    // 19
    mscorlibApi, "System.AggregateException.new", "innerExceptions:IEnumerable<exn> -> AggregateException", Some "system.aggregateexception.-ctor?view=netframework-4.7#System_AggregateException__ctor_System_Collections_Generic_IEnumerable_System_Exception__"
    // 20
    mscorlibApi, "System.Security.Claims.ClaimsIdentity.new", "identity:IIdentity * claims:IEnumerable<Claim> -> ClaimsIdentity", Some "system.security.claims.claimsidentity.-ctor?view=netframework-4.7#System_Security_Claims_ClaimsIdentity__ctor_System_Security_Principal_IIdentity_System_Collections_Generic_IEnumerable_System_Security_Claims_Claim__"
    // 21
    systemCoreApi, "System.Linq.ParallelEnumerable.Min<'TSource>", "source:ParallelQuery<'TSource> -> 'TSource", Some "system.linq.parallelenumerable.min?view=netframework-4.7#System_Linq_ParallelEnumerable_Min__1_System_Linq_ParallelQuery___0__"
    // 22
    systemCoreApi, "System.Linq.ParallelEnumerable.Max", "source:ParallelQuery<int> -> int", Some "system.linq.parallelenumerable.max?view=netframework-4.7#System_Linq_ParallelEnumerable_Max_System_Linq_ParallelQuery_System_Int32__"
    // 23
    mscorlibApi, "System.Array.ConvertAll<'TInput, 'TOutput>", "array:'TInput[] * converter:Converter<'TInput, 'TOutput> -> 'TOutput[]", Some "system.array.convertall?view=netframework-4.7#System_Array_ConvertAll__2___0___System_Converter___0___1__"
    // 24
    systemCoreApi, "System.Collections.Generic.HashSet<'T>.IsSubsetOf", "other:IEnumerable<'T> -> bool", Some "system.collections.generic.hashset-1.issubsetof?view=netframework-4.7#System_Collections_Generic_HashSet_1_IsSubsetOf_System_Collections_Generic_IEnumerable__0__"
    // 25
    systemCoreApi, "System.Linq.Expressions.Expression<'TDelegate>.Update", "body:Expression * parameters:IEnumerable<ParameterExpression> -> Expression<'TDelegate>", Some "system.linq.expressions.expression-1.update?view=netframework-4.7#System_Linq_Expressions_Expression_1_Update_System_Linq_Expressions_Expression_System_Collections_Generic_IEnumerable_System_Linq_Expressions_ParameterExpression__"
    // 26
    mscorlibApi, "System.Diagnostics.Tracing.EventSource.SendCommand", "eventSource:EventSource * command:EventCommand * commandArguments:IDictionary<string, string> -> unit", Some "system.diagnostics.tracing.eventsource.sendcommand?view=netframework-4.7#System_Diagnostics_Tracing_EventSource_SendCommand_System_Diagnostics_Tracing_EventSource_System_Diagnostics_Tracing_EventCommand_System_Collections_Generic_IDictionary_System_String_System_String__"
    // 27
    mscorlibApi, "System.Collections.Generic.List<'T>.ConvertAll<'TOutput>", "converter:Converter<'T, 'TOutput> -> List<'TOutput>", Some "system.collections.generic.list-1.convertall?view=netframework-4.7#System_Collections_Generic_List_1_ConvertAll__1_System_Converter__0___0__"
    // 28
    systemCoreApi, "System.Linq.Queryable.Aggregate<'TSource, 'TAccumulate>" ,"source:IQueryable<'TSource> * seed:'TAccumulate * func:Expression<Func<'TAccumulate, 'TSource, 'TAccumulate>> -> 'TAccumulate", Some "system.linq.queryable.aggregate?view=netframework-4.7#System_Linq_Queryable_Aggregate__2_System_Linq_IQueryable___0____1_System_Linq_Expressions_Expression_System_Func___1___0___1___"
    // 29
    systemCoreApi, "System.IO.Pipes.PipeAccessRule", "type PipeAccessRule", Some "system.io.pipes.pipeaccessrule?view=netframework-4.7"
    // 30
    systemCoreApi, "System.Collections.Generic.HashSet<'T>", "type HashSet<'T>", Some "system.collections.generic.hashset-1?view=netframework-4.7"
    // 31
    systemCoreApi, "System.IO.Pipes.PipeStream.ReadByte", "unit -> int", Some "system.io.pipes.pipestream.readbyte?view=netframework-4.7#System_IO_Pipes_PipeStream_ReadByte"
    // 32
    systemCoreApi, "System.IO.Pipes.PipeStream.SetLength", "value:int64 -> unit", Some "system.io.pipes.pipestream.setlength?view=netframework-4.7#System_IO_Pipes_PipeStream_SetLength_System_Int64_"
    // 33
    systemCoreApi, "System.IO.Pipes.PipeStream.Seek", "offset:int64 * origin:SeekOrigin -> int64", Some "system.io.pipes.pipestream.seek?view=netframework-4.7#System_IO_Pipes_PipeStream_Seek_System_Int64_System_IO_SeekOrigin_"
    // 34
    systemCoreApi, "System.IO.Pipes.PipeStream.Read", "buffer:byte[] * offset:int * count:int -> int", Some "system.io.pipes.pipestream.read?view=netframework-4.7#System_IO_Pipes_PipeStream_Read_System_Byte___System_Int32_System_Int32_"
    // 35
    systemCoreApi, "System.Runtime.CompilerServices.CallSite<'T>.Update", "'T", Some "system.runtime.compilerservices.callsite-1.update?view=netframework-4.7#System_Runtime_CompilerServices_CallSite_1_Update"
    // 36
    systemCoreApi, "System.Collections.Generic.HashSet<'T>.Enumerator<'T>.Current", "'T", Some "system.collections.generic.hashset-1.enumerator.current?view=netframework-4.7#System_Collections_Generic_HashSet_1_Enumerator_Current"
    // 37
    systemCoreApi, "System.Collections.Generic.HashSet<'T>.GetObjectData", "info:SerializationInfo * context:StreamingContext -> unit",
        Some "system.collections.generic.hashset-1.getobjectdata?view=netframework-4.7#System_Collections_Generic_HashSet_1_GetObjectData_System_Runtime_Serialization_SerializationInfo_System_Runtime_Serialization_StreamingContext_"
    // 38
    systemCoreApi, "System.Linq.Enumerable.Join<'TOuter, 'TInner, 'TKey, 'TResult>", "outer:IEnumerable<'TOuter> * inner:IEnumerable<'TInner> * outerKeySelector:Func<'TOuter, 'TKey> * innerKeySelector:Func<'TInner, 'TKey> * resultSelector:Func<'TOuter, 'TInner, 'TResult> * comparer:IEqualityComparer<'TKey> -> IEnumerable<'TResult>",
        Some "system.linq.enumerable.join?view=netframework-4.7#System_Linq_Enumerable_Join__4_System_Collections_Generic_IEnumerable___0__System_Collections_Generic_IEnumerable___1__System_Func___0___2__System_Func___1___2__System_Func___0___1___3__System_Collections_Generic_IEqualityComparer___2__"
    // 39
    systemCoreApi, "System.IO.Pipes.PipeSecurity.new", "unit -> PipeSecurity", Some "system.io.pipes.pipesecurity.-ctor?view=netframework-4.7#System_IO_Pipes_PipeSecurity__ctor"
    // 40
    systemCoreApi, "System.IO.Pipes.PipeAuditRule.new", "identity:string * rights:PipeAccessRights * flags:AuditFlags -> PipeAuditRule",
        Some "system.io.pipes.pipeauditrule.-ctor?view=netframework-4.7#System_IO_Pipes_PipeAuditRule__ctor_System_String_System_IO_Pipes_PipeAccessRights_System_Security_AccessControl_AuditFlags_"
    // 41
    systemCoreApi, "System.Linq.EnumerableQuery<'T>.new", "enumerable:IEnumerable<'T> -> EnumerableQuery<'T>",
        Some "system.linq.enumerablequery-1.-ctor?view=netframework-4.7#System_Linq_EnumerableQuery_1__ctor_System_Collections_Generic_IEnumerable__0__"
    // 42
    systemCoreApi, "System.Runtime.CompilerServices.CallSiteOps.MoveRule<'T>", "cache:RuleCache<'T> * rule:'T * i:int -> unit",
        Some "system.runtime.compilerservices.callsiteops.moverule?view=netframework-4.7#System_Runtime_CompilerServices_CallSiteOps_MoveRule__1_System_Runtime_CompilerServices_RuleCache___0____0_System_Int32_"
    // 43
    systemCoreApi, "System.Linq.Expressions.Expression.ListInit", "newExpression:NewExpression * initializers:IEnumerable<Expression> -> ListInitExpression",
        Some "system.linq.expressions.expression.listinit?view=netframework-4.7#System_Linq_Expressions_Expression_ListInit_System_Linq_Expressions_NewExpression_System_Collections_Generic_IEnumerable_System_Linq_Expressions_Expression__"
     // 44
    systemCoreApi, "System.Linq.Queryable.SelectMany<'TSource, 'TResult>", "source:IQueryable<'TSource> * selector:Expression<Func<'TSource, IEnumerable<'TResult>>> -> IQueryable<'TResult>", 
        Some "system.linq.queryable.selectmany?view=netframework-4.7#System_Linq_Queryable_SelectMany__2_System_Linq_IQueryable___0__System_Linq_Expressions_Expression_System_Func___0_System_Collections_Generic_IEnumerable___1____"
    //special
    systemCoreApi, "System.Linq.Enumerable.SingleOrDefault<'TSource>", "source:IEnumerable<'TSource> -> 'TSource", Some "system.linq.enumerable.singleordefault?view=netframework-4.7#System_Linq_Enumerable_SingleOrDefault__1_System_Collections_Generic_IEnumerable___0__"
    mscorlibApi, "System.String.new", "value:nativeptr<char> -> String", Some "system.string.-ctor?view=netframework-4.7#System_String__ctor_System_Char__"
    ]
  run (fun (dict, name, signature, expected) -> test {
    let! apiDict = dict
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

let fparsecLinkTest = parameterize {
  source [
    "FParsec.Primitives", "module Primitives", Some "primitives.html"
    "FParsec.Primitives.Parser<'Result, 'UserState>", "type Parser<'Result, 'UserState> = FParsec.CharStream<'UserState> -> FParsec.Reply<'Result>", Some "primitives.html#members.Parser"
    "FParsec.Primitives.Ok", "ReplyStatus", Some "primitives.html#members.Ok"
    "FParsec.Primitives.Inline", "type Inline", Some "primitives.html#members.Inline"
    "FParsec.Primitives.Inline.Many<'T, 'State, 'Result, 'U>", "stateFromFirstElement:('T -> 'State) * foldState:('State -> 'T -> 'State) * resultFromState:('State -> 'Result) * elementParser:Parser<'T, 'U> * ?firstElementParser:Parser<'T, 'U> * ?resultForEmptySequence:(unit -> 'Result) -> Parser<'Result, 'U>", Some "primitives.html#members.Inline..Many"
    "FParsec.Error.ParserError.new", "Position * userState:obj * ErrorMessageList -> ParserError", None
    "FParsec.Error.ParserError.Position", "Position", Some "error.html#members.Position"

    "FParsec.Primitives.ParserCombinator", "type ParserCombinator, { if/then; let!; return; return!; try/finally; try/with }", Some "primitives.html#members.ParserCombinator"
    "FParsec.Primitives.preturn<'a, 'u>", "'a -> Parser<'a, 'u>", Some "primitives.html#members.preturn"
    "FParsec.Primitives.( .>>. )<'a, 'u, 'b>", "Parser<'a, 'u> -> Parser<'b, 'u> -> Parser<'a * 'b, 'u>", Some "primitives.html#members...:62::62:.."
    
    "FParsec.Internals.referenceEquals<'a>", "x:'a -> y:'a -> bool", None
    "FParsec.CharParsers.NumberLiteralResultFlags.IsNaN", "NumberLiteralResultFlags", None
    "FParsec.Emit", "module Emit", None
  ]
  run (fun (name, signature, expected) -> test {
    let! apiDict = fparsecApi
    let api = apiDict.Api |> Array.find (fun a -> a.Name.Print() = name && a.Signature.Print() = signature)
    let actual = LinkGenerator.FParsec.generate api
    do! actual |> assertEquals expected 
  })
}