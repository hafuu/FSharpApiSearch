module MatcherTest

open Persimmon
open Persimmon.Syntax.UseTestNameByReflection
open Persimmon.MuscleAssert
open FSharpApiSearch
open TestHelper
open TestHelper.DSL

let typeA = createType "Test.A" []

let typeB = createType "Test.B" []

let typeC arg = createType "Test.C<'a>" [ arg ]

let typeD1 arg  = createType "Test.D<'a>" [ arg ]

let typeD2 arg1 arg2  = createType "Test.D<'a, 'b>" [ arg1; arg2 ]

let variableA = variable "'A"
let variableB = variable "'B"

let unit = typeAbbreviation SpecialTypes.LowType.Unit (createType "Microsoft.FSharp.Core.unit" [])
let Int32 = createType "System.Int32" []
let int = typeAbbreviation Int32 (createType "Microsoft.FSharp.Core.int" [])
let list t = typeAbbreviation (createType "Microsoft.FSharp.Collections.List<'t>" [ t ]) (createType "Microsoft.FSharp.Collections.list<'t>" [ t ])

let func2 t tresult = delegate' (createType "System.Func<'T, 'TResult>" [ t; tresult ]) [ t; tresult ]
let func3 t1 t2 tresult = delegate' (createType "System.Func<'T1, 'T2, 'TResult>" [ t1; t2; tresult]) [ t1; t2; tresult ]
let action0 = delegate' (createType "System.Action" []) [ unit; unit ]

let csharpList t = createType "System.Collections.Generic.List<'t>" [ t ]

let curriedMethod = method' "method" [ [ ptype typeA] ; [ ptype typeB ] ] typeA
let nonCurriedMethod = method' "method" [ [ ptype typeA; ptype typeB ] ] typeA
let tupleMethod = method' "method" [ [ ptype (tuple [ typeA; typeB ]) ] ] typeA
let unitArgmentMethod = method' "method" [ [ ptype unit ] ] typeA

let typeA_constructor = method' "A" [ [ ptype typeB ] ] typeA
let typeA_tuple_constructor = method' "A" [ [ ptype (tuple [ typeB; typeB ]) ] ] typeA
let typeA_2arg_constructor = method' "A" [ [ ptype typeB; ptype typeB ] ] typeA

let property = member' "property" (MemberKind.Property PropertyKind.GetSet) [] typeA
let indexedProperty = member' "property" (MemberKind.Property PropertyKind.GetSet) [ [ ptype typeB ] ] typeA

type Expected =
  | Always of bool
  | WhenEnabled of bool

let expectedValue optValue expected =
  match expected, optValue with
  | Always b, _ -> b
  | WhenEnabled b, Enabled -> b
  | WhenEnabled b, Disabled -> not b

let respectNameDifferenceInequalitiesTest =
  test {
    let query = QueryParser.parse "?a -> ?b -> 'a -> 'b"
    let opt = SearchOptions.defaultOptions
    let eqs = MatcherTypes.Equations.empty |> MatcherInitializer.initialEquations opt query
    do! eqs.Inequalities |> assertEquals [ (queryVariable "'a", queryVariable "'b"); (Wildcard (Some "a"), Wildcard (Some "b")) ]
  }

let matchTest trace abbTable (options, query, name, target, expected) = test {
  use listener = if trace then new System.Diagnostics.TextWriterTraceListener(System.Console.Out) else null
  do if trace then System.Diagnostics.Debug.Listeners.Add(listener) |> ignore
  try
    let targetApi: Api = { Name = name; Signature = target; TypeConstraints = []; Document = None }
    let dict: ApiDictionary = { AssemblyName = ""; Api = [| targetApi |]; TypeDefinitions = [||]; TypeAbbreviations = Array.append TestHelper.fsharpAbbreviationTable abbTable }
    let actual = Matcher.search [| dict |] options [ dict ] query |> Seq.length = 1
    do! actual |> assertEquals expected
  finally
    do if trace then System.Diagnostics.Debug.Listeners.Remove(listener)
}

let nameMatchTest =
  let createMap m = moduleFunction' [ [ ptype (arrow [ variableA; variableB ]) ]; [ ptype (m variableA) ]; [ ptype (m variableB) ] ]
  let listMap = createMap list
  let cMap = createMap typeC
  
  parameterize {
    source [
      "map : _", Name.displayNameOfString "Microsoft.FSharp.Collections.List.map", listMap, true
      "bind : _", Name.displayNameOfString "Microsoft.FSharp.Collections.List.map", listMap, false
      "map : ('a -> 'b) -> 'a list -> 'b list", Name.displayNameOfString "Test.C.map", cMap, false
      "(+) : _", Name.displayNameOfOperatorString "Test.(+)", listMap, true
      "(+) : _", Name.displayNameOfString "Test.op_Addition", listMap, true
      "(-) : _", Name.displayNameOfOperatorString "Test.(+)", listMap, false
      "A.B : _", Name.displayNameOfString "A.B", listMap, true
      "A.B : _", Name.displayNameOfString "X.A.B", listMap, true
      "A.B : _", Name.displayNameOfString "X.Y.B", listMap, false
      "A.B : _", Name.displayNameOfString "B", listMap, false
      "* : _", Name.displayNameOfString "A", listMap, true
      "* : _", Name.displayNameOfString "B", listMap, true
    ]
    run (fun (query, targetName, targetSig, expected) -> matchTest false [||] (SearchOptions.defaultOptions, query, targetName, targetSig, expected))
  }

let ignoreCaseMatchTest =
  let dummyName = Name.displayNameOfString "dummy"

  let cases = [
    "b : _", Name.displayNameOfString "A.B", moduleValue typeA, WhenEnabled true
    "a.B : _", Name.displayNameOfString "A.B", moduleValue typeA, WhenEnabled true

    "a", dummyName, moduleValue typeA, WhenEnabled true
    "test.A", dummyName, moduleValue typeA, WhenEnabled true

    "int", dummyName, moduleValue int, Always true
    "Int", dummyName, moduleValue int, Always false
    "int32", dummyName, moduleValue Int32, WhenEnabled true
    "int32", dummyName, moduleValue int, WhenEnabled true

    // "List<'t>" and "list<'t>" are same as ignore case. 
    "List<'a>", dummyName, moduleValue (csharpList variableA), Always true
    "List<'a>", dummyName, moduleValue (list variableA), Always true

    "list<'a>", dummyName, moduleValue (csharpList variableA), Always false
    "list<'a>", dummyName, moduleValue (list variableA), Always true

    "LIST<'a>", dummyName, moduleValue (csharpList variableA), WhenEnabled true
    "LIST<'a>", dummyName, moduleValue (list variableA), WhenEnabled true
  ]

  parameterize {
    source [
      for opt in [ Enabled; Disabled ] do
        for (query, targetName, targetSig, expected) in cases do
          let options = { SearchOptions.defaultOptions with IgnoreCase = opt }
          let expected = expectedValue opt expected
          yield (options, query, targetName, targetSig, expected)
    ]
    run (fun (options, query, targetName, targetSig, expected) -> matchTest false [||] (options, query, targetName, targetSig, expected))
  }

let matchTypeDefTest =
  parameterize {
    source [
      "Record : _", true
      "UnknownType : _", false
      "GenericRecord<'t>", true
      "GenericRecord<'t, 'u>", false
      "UnknownType", false

      "'a", false
      "?", false
    ]
    run (fun (query, expected) -> test {
      let! apiDict = TestAssemblies.fsharpAssemblyApi
      let! dictionaries = TestAssemblies.apiDictionary
      let opt = SearchOptions.defaultOptions |> SearchOptions.GreedyMatching.Set Enabled
      let actual = Matcher.search dictionaries opt [| apiDict |] query |> Seq.filter (fun result -> match result.Api.Kind with ApiKind.TypeDefinition -> true | _ -> false)
      do! Seq.length actual >= 1 |> assertEquals expected
    })
  }

let matchTypeAbbreviationTest =
  let cases = [
    "GenericTypeAbbreviation : _", Always true
    "Original : _", Always false
    "UnknownType : _", Always false

    "Original<'t>", Always true
    "GenericTypeAbbreviation<'t>", Always true
    "SpecializedTypeAbbreviation", Always true
    "specializedtypeabbreviation", Always true
    "NestedTypeAbbreviation", Always true

    "UnknownType", WhenEnabled true

    "'a", Always false
    "?", Always false
  ]
  parameterize {
    source (seq {
      for greedyOpt in [ Enabled; Disabled ] do
        for (query, expected) in cases do
          let options = { SearchOptions.defaultOptions with GreedyMatching = greedyOpt; }
          yield (options, query, expectedValue greedyOpt expected)
    })
    run (fun (opt, query, expected) -> test {
      let! apiDict = TestAssemblies.fsharpAssemblyApi
      let! dictionaries = TestAssemblies.apiDictionary
      let actual = Matcher.search dictionaries opt [| apiDict |] query |> Seq.filter (fun result -> match result.Api.Kind with ApiKind.TypeAbbreviation -> true | _ -> false)
      do! Seq.length actual >= 1 |> assertEquals expected
    })
  }

let assemblyNameTest =
  test {
    let api: Api = { Name = Name.displayNameOfString "test"; Signature = moduleValue int; TypeConstraints = []; Document = None }
    let dummyDict = { AssemblyName = "dummyAssembly"; Api = [| api |]; TypeDefinitions = [||]; TypeAbbreviations = [||] }
    let actual = Matcher.search Array.empty SearchOptions.defaultOptions [ dummyDict ] "?" |> Seq.toList
    do! actual |> assertEquals [ { AssemblyName = "dummyAssembly"; Api = api; Distance = 0 } ]
  }

let greedyMatchingTest trace abbTable greedyOpt cases = parameterize {
  source (seq {
    for respectNameDiffOpt in [ Enabled; Disabled ] do
      for (query, target, expected) in cases do
        let options = { SearchOptions.defaultOptions with GreedyMatching = greedyOpt; RespectNameDifference = respectNameDiffOpt }
        yield (options, query, Name.displayNameOfString "test", target, expectedValue respectNameDiffOpt expected)
  })
  run (matchTest trace abbTable)
}

let functionParamStyleTest trace cases = parameterize {
  source (seq {
      for opt in [ Enabled; Disabled ] do
      for (query, target, expected) in cases do
        let options = { SearchOptions.defaultOptions with IgnoreParameterStyle = opt }
        yield (options, query, Name.displayNameOfString "test", target, expectedValue opt expected)
  })
  run (matchTest trace [||])
}

module RespectNameDifferenceTest_WithNonGreedy =
  let matchTest cases = greedyMatchingTest false [||] Disabled cases
  let trace cases = greedyMatchingTest true [||] Disabled cases

  let identityTest =
    matchTest [
      "A", moduleValue typeA, Always true
      "Test.A", moduleValue typeA, Always true
      "AnotherPath.A", moduleValue typeA, Always false
      "A", moduleValue typeB, Always false
      "Test.A", moduleValue typeB, Always false
    ]

  let variableTest =
    matchTest [
      "A", moduleValue variableA, Always false
      "'A", moduleValue typeA, Always false
      "'A", moduleValue variableA, Always true
      "'A", moduleValue variableB, Always true
      "'A", moduleValue (tuple [ typeA; typeB ]), Always false
      "A * B", moduleValue variableA, Always false
      "'A", moduleFunction' [ [ ptype typeA ]; [ ptype typeA ] ], Always false
      "'A -> 'B", moduleFunction' [ [ ptype variableA ]; [ ptype variableA ] ], WhenEnabled false
    ]

  let tupleTest =
    matchTest [
      "A * A", moduleValue (tuple [ typeA; typeA ]), Always true
      "A * A", moduleValue (tuple [ typeA; typeA; typeA ]), Always false
      "A * A", moduleValue (tuple [ typeA; typeB ]), Always false
    ]

  let arrowTest =
    matchTest [
      "A -> A", moduleFunction' [ [ ptype typeA ]; [ ptype typeA ] ], Always true
      "A -> B", moduleFunction' [ [ ptype typeA ]; [ ptype typeA ] ], Always false
      "A -> A -> A", moduleFunction' [ [ ptype typeA ]; [ ptype typeA ] ], Always false
      "(A -> A) -> A", moduleFunction' [ [ ptype (arrow [ typeA; typeA ]) ]; [ ptype typeA ] ], Always true
      "A -> A -> A", moduleFunction' [ [ ptype (arrow [ typeA; typeA ]) ]; [ ptype typeA ] ], Always false

      "A -> A -> A", moduleFunction' [ [ ptype typeA ]; [ ptype typeA ]; [ ptype typeA ] ], Always true
      "A * A -> A", moduleFunction' [ [ ptype (tuple [ typeA; typeA ]) ]; [ ptype typeA ] ], Always true
    ]

  let genericTest =
    matchTest [
      "C<A>", moduleValue (typeC typeA), Always true
      "C<B>", moduleValue (typeC typeA), Always false
      "C", moduleValue (typeC typeA), Always false
      "C<A>", moduleValue (typeD1 typeA), Always false
      "D<A>", moduleValue (typeD1 typeA), Always true
      "D<A>", moduleValue (typeD2 typeA typeA), Always false

      "C<'a>", moduleValue (typeC variableA), Always true
      "C<'a>", moduleValue (typeC variableB), Always true
      "C<'a>", moduleValue (typeC typeA), Always false
    ]

  let arrayTest =
    matchTest [
      "A[]", moduleValue (array typeA), Always true
      "A[,][]", moduleValue (array (array2D typeA)), Always true
      "A[]", moduleValue (array2D typeA), Always false
      "C<A>[]", moduleValue (array (typeD1 typeA)), Always false
    ]

  let wildcardTest =
    matchTest [
      "?", moduleValue typeA, Always true
      "?", moduleFunction' [ [ ptype typeA ]; [ ptype typeA ] ], Always false
      "?", moduleValue variableA, Always true
      "? -> ?", moduleFunction' [ [ ptype typeA ]; [ ptype typeB ] ], Always true
      "? -> ?", moduleValue typeA, Always false
      "? -> ?", moduleFunction' [ [ ptype (arrow [ typeA; typeB ]) ]; [ ptype typeB ] ], Always true

      "?<A>", moduleValue typeA, Always false
      "?<A>", moduleValue (typeC typeA), Always true
      "?<?>", moduleValue (typeC typeA), Always true
    ]

  let wildcardGroupTest =
    matchTest [
      "?a", moduleValue typeA, Always true
      "?a -> ?a", moduleFunction' [ [ ptype typeA ]; [ ptype typeA ] ], Always true
      "?a -> ?a", moduleFunction' [ [ ptype typeA ]; [ ptype typeB ] ], Always false
      "?a -> ?a", moduleFunction' [ [ ptype typeA ]; [ ptype variableA ] ], Always false
      "?a -> ?b", moduleFunction' [ [ ptype typeA ]; [ ptype typeB ] ], Always true
      "?a -> ?b", moduleFunction' [ [ ptype typeA ]; [ ptype typeA ] ], WhenEnabled false
    ]

  let typeAbbreviationTest =
    matchTest [
      "int", moduleValue int, Always true
      "int", moduleValue Int32, Always true
      "Int32", moduleValue int, Always true
      "Int32", moduleValue Int32, Always true

      "list<'a>", moduleValue (list (variable "'t")), Always true
      "list<int>", moduleValue (list int), Always true
      "list<int>", moduleValue (list (variable "'t")), Always false

      "float", moduleValue int, Always false
      "float", moduleValue Int32, Always false
    ]

  let functionTypeAbbreviationTest =
    let charStream = createType "CharStream<'TResult>" [ variable "'TResult" ]
    let reply = createType "Reply<'TUserState>" [ variable "'TUserState" ]
    let parser = typeAbbreviation (arrow [ charStream; reply ]) (createType "Parser<'TResult, 'TUserState>" [ variable "'TResult"; variable "'TUserState" ])

    let table = [|
      typeAbbreviationDef "Parser<'TResult, 'TUserState>" (arrow [ charStream; reply ])
    |]
    let matchTest = greedyMatchingTest false table Disabled
    matchTest [
      "Parser<'a, 'b>", moduleValue parser, Always true
      "Parser<'a, 'b>", moduleFunction' [ [ ptype charStream ]; [ ptype reply ] ], Always true
      "CharStream<'a> -> Reply<'b>", moduleValue parser, Always true
      "CharStream<'a> -> Reply<'b>", moduleFunction' [ [ ptype charStream ]; [ ptype reply ] ], Always true
      "Parser<'a, 'b> -> Parser<'a, 'b>", moduleFunction' [ [ ptype parser ]; [ ptype parser ] ], Always true
      "(CharStream<'a> -> Reply<'b>) -> Parser<'a, 'b>", moduleFunction' [ [ ptype parser ]; [ ptype parser ] ], Always true
    ]

  let privateTypeAbbreviationTest =
    let original = createType "Test.Original" []
    let privateTypeAbbreviation = createType "Test.PrivateTypeAbbreviation" []
    let table = [|
      (typeAbbreviationDef "Test.PrivateTypeAbbreviation" (createType "Test.Original" [])).AsPrivate
    |]
    let matchTest = greedyMatchingTest false table Disabled
    matchTest [
      "Test.PrivateTypeAbbreviation", moduleValue original, Always false
      "Test.PrivateTypeAbbreviation", moduleValue privateTypeAbbreviation, Always true
    ]

  let nestedClassTest =
    let genericOuter = createType "Test.GenericOuter<'T>" [ variable "'T" ]
    let genericInner = createType "Test.GenericOuter<'T>.GenericInner<'T, 'U>" [ variable "'T"; variable "'U" ]
    matchTest [
      "GenericInner<'T, 'U>", moduleValue genericInner, Always true
      "GenericOuter.GenericInner<'T, 'U>", moduleValue genericInner, Always true
    ]

module RespectNameDifferenceTest_WithGreedy =
  let matchTest cases = greedyMatchingTest false [||] Enabled cases
  let trace cases = greedyMatchingTest true [||] Enabled cases

  let variableTest =
    matchTest [
      "'a", moduleValue variableA, Always true
      "'a", moduleValue typeA, Always true
      "A", moduleValue variableA, Always true

      "'a<'b>", moduleValue (typeC typeA), Always true
    ]

  // bug #12 recursive and circular generic type
  let recursiveAndCircularTest =
    matchTest [
      "'a -> 'a", moduleFunction' [ [ ptype (typeC variableA) ]; [ ptype variableA ] ], Always false // nativeptr<'T> -> 'T
      "('a -> 'b) -> C<'a> -> C<'b>", moduleFunction' [ [ ptype (arrow [ variableA; typeC variableB ]) ]; [ ptype (typeC variableA) ]; [ ptype (typeC variableB) ] ], Always false // ('a -> 'b option) -> 'a option -> 'b option
      "('a -> 'b) -> 'a 'm -> 'b 'm", moduleFunction' [ [ ptype (arrow [ variableA; typeC variableB ]) ]; [ ptype (typeD1 variableA) ]; [ ptype variableB ] ], Always false // ('T -> option<'U>) -> seq<'T> -> 'U
    ]

  let arrowTest =
    matchTest [
      "'a -> 'b", moduleFunction' [ [ ptype variableA ]; [ ptype variableB ] ], Always true
      "A -> B", moduleFunction' [ [ ptype typeA ]; [ ptype typeB ] ], Always true

      "'a -> 'b", moduleFunction' [ [ ptype typeA ]; [ ptype typeB ] ], Always true
      "'a -> 'b", moduleFunction' [ [ ptype typeA ]; [ ptype typeA ] ], WhenEnabled false

      "A -> B", moduleFunction' [ [ ptype variableA ]; [ ptype variableB ] ], Always true
      "A -> A", moduleFunction' [ [ ptype variableA ]; [ ptype variableB ] ], Always true
    ]

  let typeAbbreviationTest =
    matchTest [
      "int", moduleValue int, Always true
      "'a", moduleValue int, Always true
      "list<'a>", moduleValue (list variableA), Always true
      "list<'a>", moduleValue (list typeA), Always true
      "list<A>", moduleValue (list typeB), Always false

      // bug #68
      "(int -> string) -> ?<int> -> ?<string>", moduleFunction' [ [ ptype (arrow [ variableA; variableB]) ]; [ ptype (list variableA) ]; [ ptype (list variableB) ] ], Always true
      "(Int -> String) -> ?<Int> -> ?<String>", moduleFunction' [ [ ptype (arrow [ variableA; variableB]) ]; [ ptype (list variableA) ]; [ ptype (list variableB) ] ], Always true
    ]

  let distanceTest = parameterize {
    source [
      "A", moduleValue typeA, 0
      "'a", moduleValue typeA, 1
      "A -> B", moduleFunction' [ [ ptype variableA ]; [ ptype variableB ] ], 2
      "? -> 'a", moduleFunction' [ [ ptype typeA ]; [ ptype variableA ] ], 0
      "? -> 'a", moduleFunction' [ [ ptype typeA ]; [ ptype (typeC typeA) ] ], 1
      "? -> 'a", moduleFunction' [ [ ptype typeA ]; [ ptype (arrow [ typeA; typeB]) ] ], 2
      "? -> 'a", moduleFunction' [ [ ptype typeA ]; [ ptype (arrow [ typeA; typeA; typeA ]) ] ], 3
      "? -> 'a", moduleFunction' [ [ ptype typeA ]; [ ptype (arrow [ (arrow [ typeA; typeA ]); typeA; typeA ]) ] ], 4
      "('a -> 'b) -> C<'a> -> C<'b>", moduleFunction' [ [ ptype (arrow [ variableA; variableB ]) ]; [ ptype (typeC variableA) ]; [ ptype (typeC variableB) ] ], 0
      "('a -> 'b) -> C<'a> -> C<'b>", moduleFunction' [ [ ptype (arrow [ variableA; variableB ]) ]; [ ptype (typeC variableA) ]; [ ptype variableA ] ], 1

      "? -> ?", moduleFunction' [ [ ptype variableA ]; [ ptype variableB ] ], 0
      "? -> A", moduleFunction' [ [ ptype variableA ]; [ ptype variableB ] ], 1

      // bug #16
      "('a * A) -> A", moduleFunction' [ [ ptype (tuple [ variableA; variableB ]) ]; [ ptype variableB ] ], 1
      "('a * A) -> A", moduleFunction' [ [ ptype (tuple [ variableA; variableB ]) ]; [ ptype variableA ] ], 2
    ]

    run (fun (query, target, expected) -> test {
      let targetApi: Api = { Name = Name.displayNameOfString "test"; Signature = target; TypeConstraints = []; Document = None }
      let dict: ApiDictionary = { AssemblyName = ""; Api = [| targetApi |]; TypeDefinitions = Array.empty; TypeAbbreviations = TestHelper.fsharpAbbreviationTable }
      let options = { GreedyMatching = Enabled; RespectNameDifference = Enabled; IgnoreParameterStyle = Enabled; IgnoreCase = Enabled; Parallel = Disabled }
      let actual = Matcher.search [| dict |] options [ dict ] query |> Seq.head
      do! actual.Distance |> assertEquals expected
    })
  }

module IgnoreParameterStyleTest =
  let matchTest cases = functionParamStyleTest false cases
  let trace cases = functionParamStyleTest true cases

  let arrowTest =
    matchTest [
      "A -> A -> A", moduleFunction' [ [ ptype typeA ]; [ ptype typeA ]; [ ptype typeA ] ], Always true
      "A -> A -> A", moduleFunction' [ [ ptype (tuple [ typeA; typeA ]) ]; [ ptype typeA ] ], WhenEnabled true
      "A -> A -> A", moduleFunction' [ [ ptype typeA; ptype typeA ]; [ ptype typeA ] ], WhenEnabled true
      "A * A -> A", moduleFunction' [ [ ptype (tuple [ typeA; typeA ]) ]; [ ptype typeA ] ], Always true
      "A * A -> A", moduleFunction' [ [ ptype typeA; ptype typeA ]; [ ptype typeA ] ], Always true
      "A * A -> A", moduleFunction' [ [ ptype typeA ]; [ ptype typeA ]; [ ptype typeA ] ], WhenEnabled true

      "? -> A", moduleFunction' [ [ ptype typeA ]; [ ptype typeA ]; [ ptype typeA ] ], Always false
      "? -> A", moduleFunction' [ [ ptype (tuple [ typeA; typeA ]) ]; [ ptype typeA ] ], Always true
      "? -> A", moduleFunction' [ [ ptype typeA; ptype typeA ]; [ ptype typeA ] ], Always false

      "A -> ? -> A", moduleFunction' [ [ ptype typeA ]; [ ptype typeA ]; [ ptype typeA ] ], Always true
      "A -> ? -> A", moduleFunction' [ [ ptype typeA ]; [ ptype (tuple [ typeB; typeB ]) ]; [ ptype typeA ] ], Always true
      "A -> ? -> A", moduleFunction' [ [ ptype typeA ]; [ ptype typeA; ptype typeB ]; [ ptype typeA ] ], Always true

      "A -> B * A -> A", moduleFunction' [ [ ptype typeA ]; [ ptype typeB; ptype typeA ]; [ ptype typeA ] ], Always true
      "A -> B * A -> A", moduleFunction' [ [ ptype typeA ]; [ ptype (tuple [ typeB; typeA ]) ]; [ ptype typeA ] ], Always true
      "A -> (B -> A) -> A", moduleFunction' [ [ ptype typeA ]; [ ptype typeB; ptype typeA ]; [ ptype typeA ] ], Always false
    ]

  let staticMemberTest =
    matchTest [
      "A -> B -> A", staticMember typeA curriedMethod, Always true
      "A * B -> A", staticMember typeA curriedMethod, WhenEnabled true

      "A -> B -> A", staticMember typeA nonCurriedMethod, WhenEnabled true
      "A * B -> A", staticMember typeA nonCurriedMethod, Always true

      "A -> B -> A", staticMember typeA tupleMethod, WhenEnabled true
      "A * B -> A", staticMember typeA tupleMethod, Always true

      "?", staticMember typeA tupleMethod, Always false
      "? -> A", staticMember typeA tupleMethod, Always true
      "? -> A", staticMember typeA nonCurriedMethod, Always false // bug #78

      "A", staticMember typeA property, Always true
      "B", staticMember typeA property, Always false
      "?", staticMember typeA property, Always true

      "B -> A", staticMember typeA indexedProperty, Always true
      "B -> B", staticMember typeA indexedProperty, Always false
      "A", staticMember typeA indexedProperty, Always false
      "?", staticMember typeA indexedProperty, Always false
    ]

  let constructorRule =
    matchTest [
      "B -> A", constructor' typeA typeA_constructor, Always true
      "B -> B -> A", constructor' typeA typeA_constructor, Always false

      "B * B -> A", constructor' typeA typeA_2arg_constructor, Always true
      "B -> B -> A", constructor' typeA typeA_2arg_constructor, WhenEnabled true

      "B * B -> A", constructor' typeA typeA_tuple_constructor, Always true
      "B -> B -> A", constructor' typeA typeA_tuple_constructor, WhenEnabled true
    ]

  let instanceMemberTest =
    matchTest [
      "A => A -> B -> A", instanceMember typeA curriedMethod, Always true
      "A => A * B -> A", instanceMember typeA curriedMethod, WhenEnabled true
      "A => A -> A -> A", instanceMember typeA curriedMethod, Always false
      "B => A -> B -> A", instanceMember typeA curriedMethod, Always false

      "A => A -> B -> A", instanceMember typeA nonCurriedMethod, WhenEnabled true
      "A => A * B -> A", instanceMember typeA nonCurriedMethod, Always true
      "A => A * A -> A", instanceMember typeA nonCurriedMethod, Always false

      "? => A -> B -> A", instanceMember typeA curriedMethod, Always true
      "? => ?", instanceMember typeA curriedMethod, Always false
      "?", instanceMember typeA curriedMethod, Always false

      "A => A * B -> A", instanceMember typeA tupleMethod, Always true
      "A => A -> B -> A", instanceMember typeA tupleMethod, WhenEnabled true

      "A => A", instanceMember typeA property, Always true
      "A => B", instanceMember typeA property, Always false
      "A => ?", instanceMember typeA property, Always true
      "?", instanceMember typeA property, Always false

      "A => B -> A", instanceMember typeA indexedProperty, Always true
      "A => B -> B", instanceMember typeA indexedProperty, Always false
      "A => A", instanceMember typeA indexedProperty, Always false
      "A => ?", instanceMember typeA indexedProperty, Always false

      "C<'a> => A -> B -> A", instanceMember (typeC variableA) curriedMethod, Always true
      "C<A> => A -> B -> A", instanceMember (typeC variableA) curriedMethod, Always false
    ]

  let firstArgAsReceiverTest =
    matchTest [
      "A -> A -> B -> A", instanceMember typeA curriedMethod, Always true
      "B -> A -> B -> A", instanceMember typeA curriedMethod, Always false
      "A -> A * B -> A", instanceMember typeA nonCurriedMethod, Always true
      "A -> A * B -> A", instanceMember typeA tupleMethod, Always true

      "A -> A -> B -> A", instanceMember typeA nonCurriedMethod, WhenEnabled true
      "A -> A -> B -> A", instanceMember typeA tupleMethod, WhenEnabled true
      "A -> A * B -> A", instanceMember typeA curriedMethod, WhenEnabled true
      
      "A -> A", instanceMember typeA property, Always true
      "A -> B -> A", instanceMember typeA indexedProperty, Always true
    ]

  let instanceMemberSpetialRuleTest =
    matchTest [
      "A => A", instanceMember typeA unitArgmentMethod, Always true
      "A => B -> C<A>", moduleFunction' [ [ ptype typeB ]; [ ptype typeA ]; [ ptype (typeC typeA) ] ], Always true
      "A => B -> C<A>", moduleFunction' [ [ ptype typeB; ptype typeA ]; [ ptype (typeC typeA) ] ], Always false
      "A => B -> C<A>", moduleFunction' [ [ ptype (tuple [ typeB; typeA ]) ]; [ ptype (typeC typeA) ] ], Always false

      "A -> A", instanceMember typeA unitArgmentMethod, Always false
      "A -> B -> C<A>", moduleFunction' [ [ ptype typeB ]; [ ptype typeA ]; [ ptype (typeC typeA) ] ], Always false
    ]

  let delegateTest =
    matchTest [
      "A -> A", moduleValue (func2 typeA typeA), Always true
      "A -> B", moduleValue (func2 typeA typeA), Always false

      "A -> A -> B", moduleValue (func3 typeA typeA typeB), Always true
      "A * A -> B", moduleValue (func3 typeA typeA typeB), WhenEnabled true
      "A -> A -> B", moduleValue (func2 typeA typeB), Always false

      "Func<A, A>", moduleValue (func2 typeA typeA), Always true
      "Func<A, B>", moduleValue (func2 typeA typeA), Always false
      "Func<A, B>", moduleValue (func3 typeA typeA typeB), Always false
      "Function<A, A>", moduleValue (func2 typeA typeA), Always false
      
      "Action", moduleValue (action0), Always true
      "unit -> unit", moduleValue (action0), Always true

      "list<A> -> Func<A, B> -> list<B>", moduleFunction' [ [ ptype (list typeA) ]; [ ptype (func2 typeA typeB) ]; [ ptype (list typeB) ]], Always true
      "list<A> -> (A -> B) -> list<B>", moduleFunction' [ [ ptype (list typeA) ]; [ ptype (func2 typeA typeB) ]; [ ptype (list typeB) ] ], Always true
      "list<A> -> (A -> A) -> list<B>", moduleFunction' [ [ ptype (list typeA) ]; [ ptype (func2 typeA typeB) ]; [ ptype (list typeB) ] ], Always false

      "list<A> -> (A -> int -> B) -> list<B>", moduleFunction' [ [ ptype (list typeA) ]; [ ptype (func3 typeA int typeB) ]; [ ptype (list typeB) ] ], Always true
      "list<A> -> (A * int -> B) -> list<B>", moduleFunction' [ [ ptype (list typeA) ]; [ ptype (func3 typeA int typeB) ] ; [ ptype (list typeB) ] ], WhenEnabled true
    ]

  let optionalParameterTest =
    matchTest [
      "A -> A * A -> A", instanceMember typeA (method' "test" [ [ ptype typeA; popt >> ptype typeA ] ] typeA), Always true
      "A => A * A -> A", instanceMember typeA (method' "test" [ [ ptype typeA; popt >> ptype typeA ] ] typeA), Always true
      "A -> A -> A", instanceMember typeA (method' "test" [ [ ptype typeA; popt >> ptype typeA ] ] typeA), Always true
      "A => A -> A", instanceMember typeA (method' "test" [ [ ptype typeA; popt >> ptype typeA ] ] typeA), Always true

      "A -> A -> A -> A", instanceMember typeA (method' "test" [ [ ptype typeA; ptype typeA; popt >> ptype typeA ] ] typeA), WhenEnabled true
      "A => A -> A -> A", instanceMember typeA (method' "test" [ [ ptype typeA; ptype typeA; popt >> ptype typeA ] ] typeA), WhenEnabled true

      "A * A -> A", staticMember typeA (method' "test" [ [ ptype typeA; popt >> ptype typeA ] ] typeA), Always true
      "A -> A -> A", staticMember typeA (method' "test" [ [ ptype typeA; ptype typeA; popt >> ptype typeA ] ] typeA), WhenEnabled true
      "A -> A", staticMember typeA (method' "test" [ [ ptype typeA; popt >> ptype typeA ] ] typeA), Always true
    ]

  let unionCaseTest =
    matchTest [
      "A", unionCase typeA "test" [], Always true
      "int -> A", unionCase typeA "test" [ (None, int) ], Always true
      "string -> A", unionCase typeA "test" [ (None, int) ], Always false

      "int * B -> A", unionCase typeA "test" [ (None, int); (None, typeB) ], Always true
      "int * int -> A", unionCase typeA "test" [ (None, int); (None, typeB) ], Always false

      "int -> B -> A", unionCase typeA "test" [ (None, int); (None, typeB) ], WhenEnabled true
    ]

  let distanceTest = parameterize {
    source [
      "A -> B -> A", staticMember typeA curriedMethod, 0
      "A * B -> A", staticMember typeA curriedMethod, 1
      "A -> B -> A", staticMember typeA nonCurriedMethod, 1
      "A -> B -> A", staticMember typeA tupleMethod, 1

      "A => A", instanceMember typeA property, 0
      "A", staticMember typeA property, 0

      "A => A", instanceMember typeA unitArgmentMethod, 1
      "A => B -> C<A>", moduleFunction' [ [ ptype typeB ]; [ ptype typeA ]; [ ptype (typeC typeA) ] ], 1

      "A -> A", instanceMember typeA property, 1

      "Func<A, A, B>", moduleValue (func3 typeA typeA typeB), 0
      "A -> A -> B", moduleValue (func3 typeA typeA typeB), 1
      "A * A -> B", moduleValue (func3 typeA typeA typeB), 2

      "list<A> -> Func<A, int, B> -> list<B>", moduleFunction' [ [ ptype (list typeA) ]; [ ptype (func3 typeA int typeB) ]; [ ptype (list typeB) ] ], 0
      "list<A> -> (A -> int -> B) -> list<B>", moduleFunction' [ [ ptype (list typeA) ]; [ ptype (func3 typeA int typeB) ]; [ ptype (list typeB) ] ], 1
      "list<A> -> (A * int -> B) -> list<B>", moduleFunction' [ [ ptype (list typeA) ]; [ ptype (func3 typeA int typeB) ]; [ ptype (list typeB) ] ], 2
    ]

    run (fun (query, target, expected) -> test {
      let targetApi: Api = { Name = Name.displayNameOfString "test"; Signature = target; TypeConstraints = []; Document = None }
      let dict: ApiDictionary = { AssemblyName = ""; Api = [| targetApi |]; TypeDefinitions = Array.empty; TypeAbbreviations = TestHelper.fsharpAbbreviationTable }
      let options = { GreedyMatching = Disabled; RespectNameDifference = Enabled; IgnoreParameterStyle = Enabled; IgnoreCase = Enabled; Parallel = Disabled }
      let actual = Matcher.search [| dict |] options [ dict ] query |> Seq.head
      do! actual.Distance |> assertEquals expected
    })
  }

module TypeConstraintTest =
  type TypeAbbreviationDefinition with
    member this.LowType = TypeAbbreviation this.TypeAbbreviation

  let instantiate (def: FullTypeDefinition) args =
    match def.LowType with
    | Generic (id, _) -> Generic (id, args)
    | x -> x

  let subtypeCon v (parent: FullTypeDefinition) =
    { Variables = [ tv v ]; Constraint = SubtypeConstraints parent.LowType }
  let subtypeCon_abbreviation v (parent: TypeAbbreviationDefinition) =
    { Variables = [ tv v ]; Constraint = SubtypeConstraints (TypeAbbreviation parent.TypeAbbreviation) }
  let genericSubtypeCon v (parent: FullTypeDefinition) args =
    { Variables = [ tv v ]; Constraint = SubtypeConstraints (instantiate parent args) }
  let subtypeCon_variable v parent =
    { Variables = [ tv v ]; Constraint = SubtypeConstraints (variable parent) }
  let nullnessCon v =
    { Variables = [ tv v ]; Constraint = NullnessConstraints }
  let memberCon vs modifier member' =
    { Variables = List.map tv vs; Constraint = MemberConstraints (modifier, member') }
  let valueTypeCon v =
    { Variables = [ tv v ]; Constraint = ValueTypeConstraints }
  let refTypeCon v =
    { Variables = [ tv v ]; Constraint = ReferenceTypeConstraints }
  let defaultConstructorCon v =
    { Variables = [ tv v ]; Constraint = DefaultConstructorConstraints }
  let equalityCon v =
    { Variables = [ tv v ]; Constraint = EqualityConstraints }
  let comparisonCon v =
    { Variables = [ tv v ]; Constraint = ComparisonConstraints }

  let empty: FullTypeDefinition = {
    Name = []
    FullName = ""
    AssemblyName = "test"
    Accessibility = Public
    Kind = TypeDefinitionKind.Type
    BaseType = None
    AllInterfaces = []
    GenericParameters = []
    TypeConstraints = []
    InstanceMembers = []
    StaticMembers = []
    
    ImplicitInstanceMembers = []
    ImplicitStaticMembers = []

    SupportNull = NotSatisfy
    ReferenceType = Satisfy
    ValueType = NotSatisfy
    DefaultConstructor = NotSatisfy
    Equality = Satisfy
    Comparison = NotSatisfy
  }

  let Object = {
    empty with
      AssemblyName = "mscorlib"
      Name = DisplayName.ofString "System.Object"
      FullName = "System.Object"
      SupportNull = Satisfy
  }

  let Tuple2 = {
    empty with
      Name = DisplayName.ofString "System.Tuple<'T1, 'T2>"
      FullName = "System.Tuple`2"
      AssemblyName = "mscorlib"
      BaseType = Some (instantiate Object [])
      GenericParameters = [ tv "'T1"; tv "'T2" ]
      InstanceMembers =
        [
          member' "Item1" (MemberKind.Property PropertyKind.Get) [] (variable "'T1")
          member' "Item2" (MemberKind.Property PropertyKind.Get) [] (variable "'T2")
        ]
      Equality = Dependence [ tv "'T1"; tv "'T2" ]
      Comparison = Dependence [ tv "'T1"; tv "'T2" ]
  }

  let Tuple3 = {
    empty with
      Name = DisplayName.ofString "System.Tuple<'T1, 'T2, 'T3>"
      FullName = "System.Tuple`3"
      AssemblyName = "mscorlib"
      BaseType = Some (instantiate Object [])
      GenericParameters = [ tv "'T1"; tv "'T2"; tv "'T3" ]
      InstanceMembers =
        [
          member' "Item1" (MemberKind.Property PropertyKind.Get) [] (variable "'T1")
          member' "Item2" (MemberKind.Property PropertyKind.Get) [] (variable "'T2")
          member' "Item3" (MemberKind.Property PropertyKind.Get) [] (variable "'T3")
        ]
      Equality = Dependence [ tv "'T1"; tv "'T2"; tv "'T3" ]
      Comparison = Dependence [ tv "'T1"; tv "'T2"; tv "'T3" ]
  }

  let Array' = {
    empty with
      Name = DisplayName.ofString "System.Array"
      AssemblyName = "mscorlib"
      BaseType = Some (instantiate Object [])
  }

  let mscorlibDict = {
    AssemblyName = "mscorlib"
    Api = [||]
    TypeDefinitions =
      [|
        Object;
        Array';
        Tuple2; Tuple3;
      |]
    TypeAbbreviations = [||]
  }

  let Array1D = {
    empty with
      Name = DisplayName.ofString "Microsoft.FSharp.Core.[]<'a>"
      FullName = "Microsoft.FSharp.Core.[]`1"
      AssemblyName = "FSharp.Core"
      BaseType = Some (instantiate Array' [])
      GenericParameters = [ tv "'a" ]
      Equality = Dependence [ tv "'a" ]
      Comparison = Dependence [ tv "'a" ]
  }

  let fscoreLib = {
    AssemblyName = "FSharp.Core"
    Api = [||]
    TypeDefinitions =
      [|
        Array1D
      |]
    TypeAbbreviations = [||]
  }

  let Parent = {
    empty with
      Name = DisplayName.ofString "Test.Parent"
      FullName = "Test.Parent"
      BaseType = Some (instantiate Object [])
  }

  let Child = {
    empty with
      Name = DisplayName.ofString "Test.Child"
      FullName = "Test.Child"
      BaseType = Some (instantiate Parent [])
  }

  let GenericParent = {
    empty with
      Name = DisplayName.ofString "Test.GenericParent<'a, 'b>"
      FullName = "Test.GenericParent`2"
      GenericParameters = [ tv "'a"; tv "'b" ]
      BaseType = Some (instantiate Object [])
  }

  let GenericChild = {
    empty with
      Name = DisplayName.ofString "Test.GenericChild<'a, 'b>"
      FullName = "Test.GenericChild`2"
      GenericParameters = [ tv "'a"; tv "'b" ]
      BaseType = Some (instantiate GenericParent [ variable "'a"; variable "'b" ])
  }

  let AnotherGeneric = {
    empty with
      Name = DisplayName.ofString "Test.AnotherGeneric<'a, 'b>"
      FullName = "Test.AnotherGeneric`2"
      GenericParameters = [ tv "'a"; tv "'b" ]
      BaseType = Some (instantiate Object [])
  }

  let IA = {
    empty with
      Name = DisplayName.ofString "Test.IA"
      FullName = "Test.IA"
      GenericParameters = []
  }

  let IB = {
    empty with
      Name = DisplayName.ofString "Test.IB"
      FullName = "Test.IB"
      GenericParameters = []
  }

  let ImplA = {
    empty with
      Name = DisplayName.ofString "Test.ImplA"
      FullName = "Test.ImplA"
      GenericParameters = []
      BaseType = Some (instantiate Object [])
      AllInterfaces = [ instantiate IA [] ]
  }

  let ImplAB = {
    empty with
      Name = DisplayName.ofString "Test.ImplAB"
      FullName = "Test.ImplAB"
      GenericParameters = []
      BaseType = Some (instantiate Object [])
      AllInterfaces = [ instantiate IA []; instantiate IB [] ]
  }

  let Foo_X = {
    empty with
      Name = DisplayName.ofString "Foo.X"
      FullName = "Foo.X"
      GenericParameters = []
      BaseType = Some (instantiate Object [])
      AllInterfaces = [ instantiate IA [] ]
  }

  let Bar_X = {
    empty with
      Name = DisplayName.ofString "Bar.X"
      FullName = "Bar.X"
      GenericParameters = []
      BaseType = Some (instantiate Object [])
      AllInterfaces = [ instantiate IB [] ]
  }

  let GenericInterface = {
    empty with
      Name = DisplayName.ofString "Test.GenericInterface"
      FullName = "Test.GenericInterface`1"
      GenericParameters = [ tv "'a" ]
  }

  let GenericInterfaceImplement = {
    empty with
      Name = DisplayName.ofString "Test.GenericInterfaceImplement"
      FullName = "Test.GenericInterfaceImplement"
      BaseType = Some (instantiate Object [])
      AllInterfaces = [ instantiate GenericInterface [ ImplA.LowType ] ]
  }

  let OriginalTypeAbbreviatedInterface = {
    empty with
      Name = DisplayName.ofString "Test.OriginalTypeAbbreviatedInterface"
      FullName = "Test.OriginalTypeAbbreviatedInterface"
  }

  let TypeAbbreviationInterface = {
    Name = DisplayName.ofString "Test.TypeAbbreviationInterface"
    FullName = "Test.TypeAbbreviationInterface"
    AssemblyName = "test"
    Accessibility = Public
    GenericParameters = []
    Abbreviated = OriginalTypeAbbreviatedInterface.LowType
    Original = OriginalTypeAbbreviatedInterface.LowType
  }

  let AbbreviationImplement = {
    empty with
      Name = DisplayName.ofString "Test.AbbreviationImplement"
      FullName = "Test.AbbreviationImplement"
      BaseType = Some (instantiate Object [])
      AllInterfaces = [ TypeAbbreviationInterface.LowType ]
  }

  let NullableType = {
    empty with
      Name = DisplayName.ofString "Test.NullableType"
      FullName = "Test.NullableType"
      BaseType = Some (instantiate Object [])
      SupportNull = Satisfy
  }

  let NonNullableType = {
    empty with
      Name = DisplayName.ofString "Test.NonNullableType"
      FullName = "Test.NonNullableType"
      BaseType = Some (instantiate Object [])
      SupportNull = NotSatisfy
  }

  let MemberTestType = {
    empty with
      Name = DisplayName.ofString "Test.MemberTestType"
      FullName = "Test.MemberTestType"
      BaseType = Some (instantiate Object [])
      StaticMembers =
        [
          method' "StaticMethod" [ [ ptype int ] ] int
          property' "Property" PropertyKind.GetSet [] int
          method' "Overload" [ [ ptype unit ] ] typeA
          method' "Overload" [ [ ptype int ] ] typeA
        ]
      InstanceMembers =
        [
          method' "InstanceMethod" [ [ ptype unit ] ] unit
        ]
  }

  let GenericMemberTestType = {
    empty with
      Name = DisplayName.ofString "Test.GenericMemberTestType<'T>"
      FullName = "Test.GenericMemberTestType`1"
      BaseType = Some (instantiate Object [])
      GenericParameters = [ tv "'T" ]
      StaticMembers =
        [
          { method' "Method1" [ [ ptype (variable "'a") ] ] int with GenericParameters = [ tv "'a" ] }
          method' "Method2" [ [ ptype (variable "'T") ] ] int
        ]
  }

  let ImplicitMemberType = {
    empty with
      Name = DisplayName.ofString "Test.ImplicitMemberType"
      FullName = "Test.ImplicitMemberType"
      BaseType = Some (instantiate Object [])
      ImplicitInstanceMembers =
        [
          method' "InstanceMethod" [ [ ptype unit ] ] unit
        ]
      ImplicitStaticMembers =
        [
          method' "StaticMethod" [ [ ptype unit ] ] unit
        ]
  }

  let ValueType = {
    empty with
      Name = DisplayName.ofString "Test.ValueType"
      FullName = "Test.ValueType"
      BaseType = Some (instantiate Object [])
      ValueType = Satisfy
      ReferenceType = NotSatisfy
  }

  let ReferenceType = {
    empty with
      Name = DisplayName.ofString "Test.ReferenceType"
      FullName = "Test.ReferenceType"
      BaseType = Some (instantiate Object [])
      ValueType = NotSatisfy
      ReferenceType = Satisfy
  }

  let WithDefaultConstructor = {
    empty with
      Name = DisplayName.ofString "Test.WithDefaultConstructor"
      FullName = "Test.WithDefaultConstructor"
      BaseType = Some (instantiate Object [])
      DefaultConstructor = Satisfy
  }

  let WithoutDefaultConstructor = {
    empty with
      Name = DisplayName.ofString "Test.WithoutDefaultConstructor"
      FullName = "Test.WithoutDefaultConstructor"
      BaseType = Some (instantiate Object [])
      DefaultConstructor = NotSatisfy
  }

  let EqualityType = {
    empty with
      Name = DisplayName.ofString "Test.EqualityType"
      FullName = "Test.EqualityType"
      BaseType = Some (instantiate Object [])
      Equality = Satisfy
  }

  let NoEqualityType = {
    empty with
      Name = DisplayName.ofString "Test.NoEqualityType"
      FullName = "Test.NoEqualityType"
      BaseType = Some (instantiate Object [])
      Equality = NotSatisfy
  }

  let DependenceEqualityType1 = {
    empty with
      Name = DisplayName.ofString "Test.DependenceEqualityType1<'a>"
      FullName = "Test.DependenceEqualityType1`1"
      BaseType = Some (instantiate Object [])
      GenericParameters = [ tv "'a" ]
      Equality = Dependence [ tv "'a" ]
  }

  let DependenceEqualityType2 = {
    empty with
      Name = DisplayName.ofString "Test.DependenceEqualityType2<'a, 'b>"
      FullName = "Test.DependenceEqualityType2`2"
      BaseType = Some (instantiate Object [])
      GenericParameters = [ tv "'a"; tv "'b" ]
      Equality = Dependence [ tv "'a"; tv "'b" ]
  }

  let DependenceEqualityType3 = {
    empty with
      Name = DisplayName.ofString "Test.DependenceEqualityType3<'a, 'b>"
      FullName = "Test.DependenceEqualityType3`2"
      BaseType = Some (instantiate Object [])
      GenericParameters = [ tv "'a"; tv "'b" ]
      Equality = Dependence [ tv "'b" ]
  }

  let ComparisonType = {
    empty with
      Name = DisplayName.ofString "Test.ComparisonType"
      FullName = "Test.ComparisonType"
      BaseType = Some (instantiate Object [])
      Comparison = Satisfy
  }

  let NoComparisonType = {
    empty with
      Name = DisplayName.ofString "Test.NoComparisonType"
      FullName = "Test.NoComparisonType"
      BaseType = Some (instantiate Object [])
      Comparison = NotSatisfy
  }

  let DependenceComparisonType = {
    empty with
      Name = DisplayName.ofString "Test.DependenceComparisonType<'a>"
      FullName = "Test.DependenceComparisonType`1"
      BaseType = Some (instantiate Object [])
      GenericParameters = [ tv "'a" ]
      Comparison = Dependence [ tv "'a" ]
  }

  let dictionary = {
    AssemblyName = "test"
    Api = [||]
    TypeDefinitions =
      [|
        Parent; Child; GenericParent; GenericChild; AnotherGeneric; IA; IB; ImplA; ImplAB; Foo_X; Bar_X;
        GenericInterface; GenericInterfaceImplement; OriginalTypeAbbreviatedInterface; AbbreviationImplement;
        NullableType; NonNullableType;
        MemberTestType; GenericMemberTestType; ImplicitMemberType;
        ValueType; ReferenceType;
        WithDefaultConstructor; WithoutDefaultConstructor;
        EqualityType; NoEqualityType; DependenceEqualityType1; DependenceEqualityType2; DependenceEqualityType3;
        ComparisonType; NoComparisonType; DependenceComparisonType;
      |]
    TypeAbbreviations =
      [| TypeAbbreviationInterface |]
  }

  let testConstraint trace (query, target, constraints, expected) = test {
    use listener = new System.Diagnostics.TextWriterTraceListener(System.Console.Out)
    do if trace then System.Diagnostics.Debug.Listeners.Add(listener) |> ignore
    let targetApi: Api = { Name = Name.displayNameOfString "test"; Signature = target; TypeConstraints = constraints; Document = None }

    let dictionaries = [|
      mscorlibDict
      fscoreLib
      dictionary
    |]

    let options = { GreedyMatching = Enabled; RespectNameDifference = Enabled; IgnoreParameterStyle = Enabled; IgnoreCase = Enabled; Parallel = Disabled }
    let dummyDict = { AssemblyName = "dummy"; Api = [| targetApi |]; TypeDefinitions = [||]; TypeAbbreviations = [||] }
    let actual = Matcher.search dictionaries options [ dummyDict ] query |> Seq.length = 1
    do if trace then System.Diagnostics.Debug.Listeners.Remove(listener)
    do! actual |> assertEquals expected
  }

  let subtypeConstraintsTest = parameterize {
    source [
      ("Child",
        moduleValue (variable "'a"),
        [ subtypeCon "'a" Parent ],
        true)
      ("Parent",
        moduleValue (variable "'a"),
        [ subtypeCon "'a" Parent ],
        true)
      ("Object",
        moduleValue (variable "'a"),
        [ subtypeCon "'a" Parent ],
        false)
      ("GenericChild<'a, 'b>",
        moduleValue (variable "'t"), // #GenericParent<'a, 'b>
        [ genericSubtypeCon "'t" GenericParent [ variable "'a"; variable "'b" ] ],
        true)
      ("GenericChild<'a, 'b>",
        moduleValue (variable "'t"), // #AnotherGeneric<'a, 'b>
        [ genericSubtypeCon "'t" AnotherGeneric [ variable "'a"; variable "'b" ] ],
        false)
      ("GenericChild<A, B>",
        moduleValue (variable "'t"), // #GenericParent<B, B>
        [ genericSubtypeCon "'t" GenericParent [ typeB; typeB ] ],
        false)
      ("GenericInterfaceImplement", // #GenericInterface<ImplA>
        moduleValue (variable "'a"),
        [ genericSubtypeCon "'a" GenericInterface [ ImplA.LowType ] ],
        true)
      ("GenericInterfaceImplement", // #GenericInterface<#IA>
        moduleValue (variable "'a"),
        [ genericSubtypeCon "'a" GenericInterface [ variable "'b" ]; subtypeCon "'b" IA ],
        true)
      ("GenericInterfaceImplement", // #GenericInterface<'b>
        moduleValue (variable "'a"),
        [ genericSubtypeCon "'a" GenericInterface [ variable "'b" ] ],
        true)
      ("GenericInterfaceImplement", // #GenericInterface<Child>
        moduleValue (variable "'a"),
        [ genericSubtypeCon "'a" GenericInterface [ Child.LowType] ],
        false)
      // Foo.X implements only IA, and Bar.X implements only IB. These have same name 'X'.
      ("X -> X",
        moduleFunction' [ [ ptype (variable "'t") ]; [ ptype (variable "'u") ] ], // #IA -> #IA
        [ subtypeCon "'t" IA; subtypeCon "'u" IA ],
        true)
      ("X -> X",
        moduleFunction' [ [ ptype (variable "'t") ]; [ ptype (variable "'u") ] ], // #IA -> #IB
        [ subtypeCon "'t" IA; subtypeCon "'u" IB ],
        false)
      ("X -> X",
        moduleFunction' [ [ ptype (variable "'t") ]; [ ptype (variable "'u") ] ], // #IB -> #IA
        [ subtypeCon "'t" IB; subtypeCon "'u" IA ],
        false)
      // Type abbreviation base type
      ("AbbreviationImplement",
        moduleValue (variable "'a"),
        [ subtypeCon "'a" OriginalTypeAbbreviatedInterface ],
        true)
      ("AbbreviationImplement",
        moduleValue (variable "'a"),
        [ subtypeCon_abbreviation "'a" TypeAbbreviationInterface ],
        true)
      ("TypeAbbreviationInterface",
        moduleValue (variable "'a"),
        [ subtypeCon "'a" OriginalTypeAbbreviatedInterface ],
        true)
      ("TypeAbbreviationInterface",
        moduleValue (variable "'a"),
        [ subtypeCon_abbreviation "'a" TypeAbbreviationInterface ],
        true)
      // indirect
      ("'t -> 't",
        moduleFunction' [ [ ptype (variable "'a") ]; [ ptype (Identity (FullIdentity Object.FullIdentity)) ] ],
        [ subtypeCon "'a" Parent ],
        false)
      ("'t -> 't",
        moduleFunction' [ [ ptype (variable "'a") ]; [ ptype (Identity (FullIdentity Parent.FullIdentity)) ] ],
        [ subtypeCon "'a" Parent ],
        true)
      ("'t -> 't",
        moduleFunction' [ [ ptype (variable "'a") ]; [ ptype (Identity (FullIdentity Child.FullIdentity)) ] ],
        [ subtypeCon "'a" Parent ],
        true)
      // variable
      ("Foo.X -> Bar.X",
        moduleFunction' [ [ ptype (variable "'a") ]; [ ptype (variable "'b") ] ],
        [ subtypeCon_variable "'a" "'b" ],
        false)
      ("Foo.X -> System.Object",
        moduleFunction' [ [ ptype (variable "'a") ]; [ ptype (variable "'b") ] ],
        [ subtypeCon_variable "'a" "'b" ],
        false)
      ("Foo.X -> Foo.X",
        moduleFunction' [ [ ptype (variable "'a") ]; [ ptype (variable "'b") ] ],
        [ subtypeCon_variable "'a" "'b" ],
        true)
    ]
    run (testConstraint false)
  }

  let nullnessConstraintTest = parameterize {
    source [
      ("NullableType",
        moduleValue (variable "'a"),
        [ nullnessCon "'a" ],
        true)
      ("NonNullableType",
        moduleValue (variable "'a"),
        [ nullnessCon "'a" ],
        false)
      ("'a * 'b",
        moduleValue (variable "'a"),
        [ nullnessCon "'a" ],
        false)
    ]
    run (testConstraint false)
  }

  let memberConstraintTest = parameterize {
    source [
      ("MemberTestType",
        moduleValue (variable "^a"),
        [ memberCon [ "^a" ] MemberModifier.Static (method' "StaticMethod" [ [ ptype int ] ] int) ],
        true)
      ("MemberTestType",
        moduleValue (variable "^a"),
        [ memberCon [ "^a" ] MemberModifier.Static (method' "Nonexistence" [ [ ptype int ] ] int) ],
        false)
      ("MemberTestType",
        moduleValue (variable "^a"),
        [ memberCon [ "^a" ] MemberModifier.Static (method' "StaticMethod" [ [ ptype int ] ] unit) ], // signature not matched.
        false)
      ("MemberTestType",
        moduleValue (variable "^a"),
        [ memberCon [ "^a" ] MemberModifier.Static (method' "get_Property" [ [ ptype unit ] ] int) ],
        true)
      ("MemberTestType",
        moduleValue (variable "^a"),
        [ memberCon [ "'a" ] MemberModifier.Static (method' "set_Property" [ [ ptype int ] ] unit) ],
        true)
      ("MemberTestType",
        moduleValue (variable "^a"),
        [ memberCon [ "^a" ] MemberModifier.Static (method' "Overload" [ [ ptype unit ] ] typeA) ],
        true)
      ("MemberTestType",
        moduleValue (variable "^a"),
        [ memberCon [ "^a" ] MemberModifier.Static (method' "Overload" [ [ ptype int ] ] typeA) ],
        true)
      ("MemberTestType",
        moduleValue (variable "^a"),
        [ memberCon [ "^a" ] MemberModifier.Static (method' "Overload" [ [ ptype typeB ] ] typeA) ],
        false)
      ("MemberTestType",
        moduleValue (variable "^a"),
        [ memberCon [ "^a" ] MemberModifier.Instance (method' "InstanceMethod" [ [ ptype unit ] ] unit) ],
        true)
      ("MemberTestType",
        moduleValue (variable "^a"),
        [ memberCon [ "^a" ] MemberModifier.Instance (method' "StaticMethod" [ [ ptype int ] ] int) ],
        false)

      // Generic
      ("GenericMemberTestType<'X>",
        moduleValue (variable "^a"),
        [ memberCon [ "^a" ] MemberModifier.Static (method' "Method1" [ [ ptype (variable "'b") ] ] int) ],
        true)
      ("GenericMemberTestType<'X>",
        moduleValue (variable "^a"),
        [ memberCon [ "^a" ] MemberModifier.Static (method' "Method2" [ [ ptype (variable "'b") ] ] int) ],
        true)
      ("GenericMemberTestType<'X>",
        moduleValue (variable "^a"),
        [ memberCon [ "^a" ] MemberModifier.Static (method' "Method2" [ [ ptype int ] ] int) ],
        true)
      ("GenericMemberTestType<'X> -> 'X",
        moduleFunction' [ [ ptype (variable "^a") ]; [ ptype typeA ] ],
        [ memberCon [ "^a" ] MemberModifier.Static (method' "Method2" [ [ ptype int ] ] int) ],
        false)
      ("GenericMemberTestType<A>",
        moduleValue (variable "^a"),
        [ memberCon [ "^a" ] MemberModifier.Static (method' "Method2" [ [ ptype (variable "'b") ] ] int) ],
        true)
      ("GenericMemberTestType<A>",
        moduleValue (variable "^a"),
        [ memberCon [ "^a" ] MemberModifier.Static (method' "Method2" [ [ ptype typeA ] ] int) ],
        true)
      ("GenericMemberTestType<A>",
        moduleValue (variable "^a"),
        [ memberCon [ "^a" ] MemberModifier.Static (method' "Method2" [ [ ptype unit ] ] int) ],
        false)

      // or
      ("MemberTestType -> Object",
        moduleFunction' [ [ ptype (variable "^a") ]; [ ptype (variable "^b") ] ],
        [ memberCon [ "^a"; "^b" ] MemberModifier.Instance (method' "InstanceMethod" [ [ ptype unit ] ] unit) ],
        true)
      ("Object -> MemberTestType",
        moduleFunction' [ [ ptype (variable "^a") ]; [ ptype (variable "^b") ] ],
        [ memberCon [ "^a"; "^b" ] MemberModifier.Instance (method' "InstanceMethod" [ [ ptype unit ] ] unit) ],
        true)
      ("MemberTestType -> MemberTestType",
        moduleFunction' [ [ ptype (variable "^a") ]; [ ptype (variable "^b") ] ],
        [ memberCon [ "^a"; "^b" ] MemberModifier.Instance (method' "InstanceMethod" [ [ ptype unit ] ] unit) ],
        true)
      ("Object -> Object",
        moduleFunction' [ [ ptype (variable "^a") ]; [ ptype (variable "^b") ] ],
        [ memberCon [ "^a"; "^b" ] MemberModifier.Instance (method' "InstanceMethod" [ [ ptype unit ] ] unit) ],
        false)

      // implicit
      ("ImplicitMemberType",
        moduleValue (variable "^a"),
        [ memberCon [ "^a" ] MemberModifier.Instance (method' "InstanceMethod" [ [ ptype unit ] ] unit) ],
        true)
      ("ImplicitMemberType",
        moduleValue (variable "^a"),
        [ memberCon [ "^a" ] MemberModifier.Static (method' "StaticMethod" [ [ ptype unit ] ] unit) ],
        true)
      ("ImplicitMemberType",
        moduleValue (variable "^a"),
        [ memberCon [ "^a" ] MemberModifier.Static (method' "MissingMethod" [ [ ptype unit ] ] unit) ],
        false)
    ]

    run (testConstraint false)
  }

  let valueAndReferenceTypeConstraintTest = parameterize {
    source [
      ("ValueType",
        moduleValue (variable "'a"),
        [ valueTypeCon "'a" ],
        true)
      ("ValueType",
        moduleValue (variable "'a"),
        [ refTypeCon "'a" ],
        false)
      ("ReferenceType",
        moduleValue (variable "'a"),
        [ valueTypeCon "'a" ],
        false)
      ("ReferenceType",
        moduleValue (variable "'a"),
        [ refTypeCon "'a" ],
        true)
    ]
    run (testConstraint false)
  }

  let defaultConstructorConstraintTest = parameterize {
    source [
      ("WithDefaultConstructor",
        moduleValue (variable "'a"),
        [ defaultConstructorCon "'a" ],
        true)
      ("WithoutDefaultConstructor",
        moduleValue (variable "'a"),
        [ defaultConstructorCon "'a" ],
        false)
    ]
    run (testConstraint false)
  }

  let equalityConstraintTest = parameterize {
    source [
      ("EqualityType",
        moduleValue (variable "'a"),
        [ equalityCon "'a" ],
        true)
      ("NoEqualityType",
        moduleValue (variable "'a"),
        [ equalityCon "'a" ],
        false)
      ("(EqualityType -> EqualityType) -> 'x",
        moduleFunction' [ [ ptype (variable "'a") ]; [ ptype (variable "'b") ] ],
        [ equalityCon "'a" ],
        false)
      ("DependenceEqualityType1<EqualityType>",
        moduleValue (variable "'a"),
        [ equalityCon "'a" ],
        true)
      ("DependenceEqualityType1<NoEqualityType>",
        moduleValue (variable "'a"),
        [ equalityCon "'a" ],
        false)
      ("DependenceEqualityType1<EqualityType -> EqualityType>",
        moduleValue (variable "'a"),
        [ equalityCon "'a" ],
        false)
      ("DependenceEqualityType2<EqualityType, EqualityType>",
        moduleValue (variable "'a"),
        [ equalityCon "'a" ],
        true)
      ("DependenceEqualityType2<NoEqualityType, EqualityType>",
        moduleValue (variable "'a"),
        [ equalityCon "'a" ],
        false)
      ("DependenceEqualityType3<EqualityType, EqualityType>",
        moduleValue (variable "'a"),
        [ equalityCon "'a" ],
        true)
      ("DependenceEqualityType3<NoEqualityType, EqualityType>",
        moduleValue (variable "'a"),
        [ equalityCon "'a" ],
        true)
      ("DependenceEqualityType3<EqualityType, NoEqualityType>",
        moduleValue (variable "'a"),
        [ equalityCon "'a" ],
        false)
      ("EqualityType * EqualityType",
        moduleValue (variable "'a"),
        [ equalityCon "'a" ],
        true)
      ("'a * 'b",
        moduleValue (variable "'a"),
        [ equalityCon "'a" ],
        true)
      ("EqualityType * NoEqualityType",
        moduleValue (variable "'a"),
        [ equalityCon "'a" ],
        false)
      ("EqualityType[]",
        moduleValue (variable "'a"),
        [ equalityCon "'a" ],
        true)
      ("NoEqualityType[]",
        moduleValue (variable "'a"),
        [ equalityCon "'a" ],
        false)
      // bug #59
      ("?[]",
        moduleValue (variable "'a"),
        [ equalityCon "'a" ],
        true)
    ]
    run (testConstraint false)
  }

  let comparisonConstraintTest = parameterize {
    source [
      ("ComparisonType",
        moduleValue (variable "'a"),
        [ comparisonCon "'a" ],
        true)
      ("NoComparisonType",
        moduleValue (variable "'a"),
        [ comparisonCon "'a" ],
        false)
      ("(ComparisonType -> ComparisonType) -> 'x",
        moduleFunction' [ [ ptype (variable "'a") ]; [ ptype (variable "'b") ] ],
        [ comparisonCon "'a" ],
        false)
      ("DependenceComparisonType<ComparisonType>",
        moduleValue (variable "'a"),
        [ comparisonCon "'a" ],
        true)
      ("DependenceComparisonType<NoComparisonType>",
        moduleValue (variable "'a"),
        [ comparisonCon "'a" ],
        false)
      ("ComparisonType * ComparisonType",
        moduleValue (variable "'a"),
        [ comparisonCon "'a" ],
        true)
      ("'a * 'b",
        moduleValue (variable "'a"),
        [ comparisonCon "'a" ],
        true)
      ("ComparisonType * NoComparisonType",
        moduleValue (variable "'a"),
        [ comparisonCon "'a" ],
        false)
      ("ComparisonType[]",
        moduleValue (variable "'a"),
        [ comparisonCon "'a" ],
        true)
      ("NoComparisonType[]",
        moduleValue (variable "'a"),
        [ comparisonCon "'a" ],
        false)
    ]
    run (testConstraint false)
  }

module ActivePatternTest =
  let testActivePattern trace (query, target, expected) = test {
    use listener = new System.Diagnostics.TextWriterTraceListener(System.Console.Out)
    do if trace then System.Diagnostics.Debug.Listeners.Add(listener) |> ignore
    try
      let targetApi: Api = { Name = Name.displayNameOfString "test"; Signature = target; TypeConstraints = []; Document = None }
      let dict: ApiDictionary = { AssemblyName = ""; Api = [| targetApi |]; TypeDefinitions = Array.empty; TypeAbbreviations = TestHelper.fsharpAbbreviationTable }
      let options = SearchOptions.defaultOptions
      let actual = Matcher.search [| dict |] options [ dict ] query |> Seq.length = 1
      do! actual |> assertEquals expected
    finally
      do if trace then System.Diagnostics.Debug.Listeners.Remove(listener)
  }

  let activePatternTest = parameterize {
    source [
      "(||) : ... -> A -> ?", activePattern [ [ ptype typeA ]; [ ptype typeB ] ], true
      "(||) : ... -> A -> ?", activePattern [ [ ptype typeB ]; [ ptype typeA ]; [ ptype typeB ] ], true
      "(||) : ... -> A -> ?", activePattern [ [ ptype typeB ]; [ ptype typeA ] ], false

      "(||) : A -> ?", activePattern [ [ ptype typeA ]; [ ptype typeB ] ], true
      "(||) : A -> ?", activePattern [ [ ptype typeB ]; [ ptype typeA ]; [ ptype typeB ] ], false

      "(||) : ? -> A -> ?", activePattern [ [ ptype typeA ]; [ ptype typeA ]; [ ptype typeB ] ], true
      "(||) : ? -> A -> ?", activePattern [ [ ptype typeA ]; [ ptype typeB ] ], false

      "(|_|) : ... -> A -> ?", partialActivePattern [ [ ptype typeA ]; [ ptype typeB ] ], true
      "(|_|) : ... -> A -> ?", activePattern [ [ ptype typeA ]; [ ptype typeB ] ], false
    ]
    run (testActivePattern false)
  }

  let arrowAndActivePatternTest = parameterize {
    source [
      "A -> B", activePattern [ [ ptype typeA ]; [ ptype typeB ] ], true
    ]
    run (testActivePattern false)
  }

module TypeExtensionTest =
  let matchTest cases = functionParamStyleTest false cases
  let trace cases = functionParamStyleTest true cases
  let testModule = DisplayName.ofString "test"

  let instanceMemberTest =
    matchTest [
      "A => B", typeExtension typeA testModule MemberModifier.Instance (property' "test" PropertyKind.Get [] typeB), Always true
      "B => B", typeExtension typeA testModule MemberModifier.Instance (property' "test" PropertyKind.Get [] typeB), Always false

      "A => A -> B", typeExtension typeA testModule MemberModifier.Instance (property' "test" PropertyKind.Get [ [ ptype typeA ] ] typeB), Always true
      "A => A -> A", typeExtension typeA testModule MemberModifier.Instance (property' "test" PropertyKind.Get [ [ ptype typeA ] ] typeB), Always false

      "A => A * B -> A", typeExtension typeA testModule MemberModifier.Instance (method' "test" [ [ ptype typeA; ptype typeB ] ] typeA), Always true
      "A => A -> B -> A", typeExtension typeA testModule MemberModifier.Instance (method' "test" [ [ ptype typeA; ptype typeB ] ] typeA), WhenEnabled true

      "A => B", typeExtension typeA testModule MemberModifier.Instance (method' "test" [ [ ptype unit ] ] typeB), Always true

      "A => B", typeExtension typeA testModule MemberModifier.Static (property' "test" PropertyKind.Set [] typeB), Always false
    ]

  let staticMemberTest =
    matchTest [
      "A -> B", typeExtension typeA testModule MemberModifier.Static (method' "test" [ [ ptype typeA ] ] typeB), Always true
      "A -> A", typeExtension typeA testModule MemberModifier.Static (method' "test" [ [ ptype typeA ] ] typeB), Always false

      "A", typeExtension typeA testModule MemberModifier.Static (property' "test" PropertyKind.Get [] typeA), Always true
      "A", typeExtension typeA testModule MemberModifier.Static (property' "test" PropertyKind.Get [] typeB), Always false

      "A -> B", typeExtension typeA testModule MemberModifier.Static (property' "test" PropertyKind.Get [ [ ptype typeA ] ] typeB), Always true
      "A -> B", typeExtension typeA testModule MemberModifier.Static (property' "test" PropertyKind.Get [ [ ptype typeA ] ] typeA), Always false


      "A * A -> B", typeExtension typeA testModule MemberModifier.Static (method' "test" [ [ ptype typeA; ptype typeA ] ] typeB), Always true
      "A -> A -> B", typeExtension typeA testModule MemberModifier.Static (method' "test" [ [ ptype typeA; ptype typeA ] ] typeB), WhenEnabled true

      "A -> B", typeExtension typeA testModule MemberModifier.Instance (method' "test" [ [ ptype typeA ] ] typeB), Always false
    ]

  let extensionMemberTest =
    matchTest [
      "A => B", extensionMember (method' "test" [ [ ptype typeA ] ] typeB), Always true
      "A => B", extensionMember (method' "test" [ [ ptype typeA ] ] typeA), Always false
      "A => B", extensionMember (method' "test" [ [ ptype typeA ] ] typeA), Always false

      "A => A -> B", extensionMember (method' "test" [ [ ptype typeA; ptype typeA ] ] typeB), Always true
      "A => A -> A -> B", extensionMember (method' "test" [ [ ptype typeA; ptype typeA; ptype typeA ] ] typeB), WhenEnabled true

      "A => B", extensionMember (method' "test" [ [ ptype typeA; ptype unit ] ] typeB), Always true

      // extension member as static method
      "A -> B", extensionMember (method' "test" [ [ ptype typeA ] ] typeB), Always true
      "A -> B", extensionMember (method' "test" [ [ ptype typeA ] ] typeA), Always false
      "A -> B -> A", extensionMember (method' "test" [ [ ptype typeA; ptype typeB ] ] typeA), WhenEnabled true
    ]
