﻿module PrinterTest

open Persimmon
open Persimmon.Syntax.UseTestNameByReflection
open Persimmon.MuscleAssert
open FSharpApiSearch
open FSharpApiSearch.Printer

open TestHelper.DSL
open TestHelper.Types

let typeA = createType "a" []
let typeB = createType "b" []
let typeC = createType "c" []

let genericA = createType "A" [ variable "'a" ]
let genericB = createType "B" [ variable "'a"; variable "'b" ]

let variableA = variable "'a"
let variableB = variable "'b"

let memberMethod = method' "test" [ [ ptype variableA; ptype typeB ] ] typeC
let memberOptArgMethod = method' "test" [ [ popt >> pname "x" >> ptype variableA; ptype typeB ] ] typeC
let memberCurriedMethod = method' "test" [ [ ptype variableA ]; [ ptype typeB ] ] typeC
let memberProperty = member' "test" (MemberKind.Property PropertyKind.Get) [] typeA

let printLowTypeTest = parameterize {
  source [
    createType "A" [], "A"
    createType "A.B" [], "B"
    createType "A.B<'C>" [ variable "'C" ], "B<'C>"
  ]
  run (fun (input: LowType, expected) -> test {
    do! input.Print() |> assertEquals expected
  })
}

let printName_long_test = parameterize {
  source[
    Name.ofString "A.B", "A.B"
    Name.ofString "A.B<'C>", "A.B"
    Name.ofString "A<'C>.B<'D>", "A<'C>.B"
  ]
  run (fun (input: Name, expected) -> test {
    do! input.Print() |> assertEquals expected
  })
}

let printApiSignatureTest =
  let Int32 = createType "System.Int32" []
  parameterize {
    source [
      moduleValue typeA, "a"
      moduleFunction' [ [ ptype typeA ]; [ ptype typeB ] ], "a -> b"
      moduleFunction' [ [ ptype (arrow [ typeA; typeB ]) ]; [ ptype typeC ] ], "(a -> b) -> c"

      moduleFunction' [ [ pname "x" >> ptype typeA; pname "y" >> ptype typeB ]; [ ptype typeB ] ], "x:a * y:b -> b"
      moduleFunction' [ [ pname "x" >> ptype typeA ]; [ pname "y" >> ptype typeB ]; [ ptype typeB ] ], "x:a -> y:b -> b"

      moduleValue variableA, "'a"
      moduleValue (variable "^a"), "^a"
      moduleValue (array typeA), "a[]"
      moduleValue (array (array2D typeA)), "a[,][]"
      moduleValue (createType "A" [ typeC]), "A<c>"
      moduleValue (genericB), "B<'a, 'b>"
      moduleValue (tuple [ typeA; variableB; typeC ]), "a * 'b * c"
      moduleValue (tuple [ typeA; (tuple [ typeB; typeC ]) ]), "a * (b * c)"
      moduleValue (structTuple [ typeA; typeA; typeC ]), "struct (a * a * c)"
      moduleValue (structTuple [ typeA; structTuple [ typeB; typeB ]; typeC ]), "struct (a * struct (b * b) * c)"
      moduleValue (structTuple [ typeA; tuple [ typeB; typeB ]; typeC ]), "struct (a * (b * b) * c)"
      moduleValue (structTuple [ arrow [ typeA; typeB ]; typeC ]), "struct ((a -> b) * c)"
      instanceMember typeA memberMethod, "'a * b -> c"
      instanceMember typeA memberOptArgMethod, "?x:'a * b -> c"
      instanceMember typeA memberCurriedMethod, "'a -> b -> c"
      instanceMember typeA memberProperty, "a"
      staticMember typeA memberMethod, "'a * b -> c"
      staticMember typeA memberProperty, "a"

      moduleValue (array (tuple [ typeA; typeB ])), "(a * b)[]"
      moduleValue (array (structTuple [ typeA; typeB ])), "struct (a * b)[]"
      moduleValue (array (arrow [ typeA; typeB ])), "(a -> b)[]"

      unionCase typeA "Case" [], "a"
      unionCase typeA "Case" [ (None, typeB) ], "b -> a"
      unionCase typeA "Case" [ (Some "value1", typeB); (Some "value2", typeA) ], "value1:b * value2:a -> a"
      unionCase typeA "Case" [ (None, tuple [ typeA; typeB ]) ], "(a * b) -> a"
      unionCase typeA "Case" [ (Some "value1", tuple [ typeA; typeB ]) ], "value1:(a * b) -> a"
      unionCase typeA "Case" [ (None, arrow [ typeA; typeB ]) ], "(a -> b) -> a"
      unionCase typeA "Case" [ (Some "value1", arrow [ typeA; typeB ]) ], "value1:(a -> b) -> a"

      typeAbbreviationApi (typeAbbreviationDef "FSharp.Collections.list<'a>" (generic (identity "System.Collections.Generic.List") [ variable "'a" ])), "type list<'a> = System.Collections.Generic.List<'a>"
      typeAbbreviationApi (typeAbbreviationDef "Test.A" (generic (identity "System.Collections.Generic.List") [ Int32 ])), "type A = System.Collections.Generic.List<System.Int32>"
      typeAbbreviationApi (typeAbbreviationDef "Test.B<'a>" (variable "'a")), "type B<'a> = 'a"
      typeAbbreviationApi (typeAbbreviationDef "Test.C<'a>" (arrow [ variable "'a"; variable "'a" ])), "type C<'a> = 'a -> 'a"
      typeAbbreviationApi (typeAbbreviationDef "Test.D<'a>" (array (variable "'a"))), "type D<'a> = 'a[]"
    ]
    run (fun (input, expected) -> test {
      let actual = ApiSignature.print input
      do! actual |> assertEquals expected
    })
  }

let debugPrintTest = parameterize {
  source [
    moduleValue typeA, "a"
    moduleValue variableA, "'t_a"
    instanceMember typeA memberMethod, "a => 't_a * b -> c"
    instanceMember typeA memberProperty, "a => a"
    instanceMember genericA memberMethod, "A<'t_a> => 't_a * b -> c"
    staticMember typeA memberMethod, "'t_a * b -> c"
    staticMember typeA memberProperty, "a"
  ]
  run (fun (input, expected) -> test {
    let actual = ApiSignature.debug input
    do! actual |> assertEquals expected
  })
}

let printCSharpLowTypeTest =
  parameterize {
    source[
      (array (identity "A")), "A[]"
      (array2D (identity "A")), "A[,]"
      (array (generic (identity "A") [ identity "B" ])), "A<B>[]"
      (array (array2D (identity "A"))), "A[][,]"
    ]
    run (fun (input, expected) -> test {
      let sb = System.Text.StringBuilder()
      do CSharpImpl.printLowType input sb |> ignore
      let actual = sb.ToString()
      do! actual |> assertEquals expected
    })
  }

let printCSharpSignatureTest =
  let n = Name.ofString
  let t = createType "T" []
  parameterize {
    source [
      api (n "M<'T>.value") (moduleValue (variable "'T")), "static T M<T>.value { get; }"
      api (n "M.value") (moduleValue (fsharpList int)), "static FSharpList<int> M.value { get; }"
        
      api (n "M.tuple") (moduleValue (tuple [ int; string ])), "static Tuple<int, string> M.tuple { get; }"
      api (n "M.tuple") (moduleValue (structTuple [ int; string ])), "static (int, string) M.tuple { get; }"

      api (n "M.fn") (moduleFunction' [ [ ptype unit ]; [ ptype unit ] ]), "static void M.fn()"
      api (n "M.fn<'T>") (moduleFunction' [ [ ptype (variable "'T") ]; [ ptype int ] ]), "static int M.fn<T>(T)"

      api (n "T.method<'T>") (instanceMember t (method' "method" [ [ ptype t ] ] int)), "int T.method<T>(T)"
      api (n "T.method") (instanceMember t (method' "method" [ [ ptype int >> pname "x"; ptype string >> pname "y" ] ] int)), "int T.method(int x, string y)"
      api (n "T.method") (instanceMember t (method' "method" [ [ ptype unit ] ] unit)), "void T.method()"
      api (n "T.method") (instanceMember t (method' "method" [ [ ptype unit; ptype int ] ] unit)), "void T.method(Unit, int)"

      api (n "T.method") (staticMember t (method' "method" [ [ ptype unit ] ] unit)), "static void T.method()"

      api (n "T.prop") (instanceMember t (property' "prop" PropertyKind.Get [] int)), "int T.prop { get; }"
      api (n "T.prop") (instanceMember t (property' "prop" PropertyKind.GetSet [ [ ptype string ] ] int)), "int T.prop[string] { get; set; }"
      api (n "T.prop") (instanceMember t (property' "prop" PropertyKind.GetSet [ [ ptype string >> pname "x" ] ] int)), "int T.prop[string x] { get; set; }"
      api (n "T.prop") (staticMember t (property' "prop" PropertyKind.Get [] int)), "static int T.prop { get; }"

      api (n "T.new") (constructor' t (method' "new" [ [ ptype unit ] ] t)), "T.T()"
      api (n "T.new") (constructor' t (method' "new" [ [ ptype int >> pname "x"; ptype string >> pname "y" ] ] t)), "T.T(int x, string y)"

      api (n "T.ext") (extensionMember (method' "ext" [ [ ptype int >> pname "x"; ptype int >> pname "y" ] ] unit)), "void T.ext(this int x, int y)"
    ]

    run (fun (input: Api, expected) -> test {
      do! CSharp.printSignatureAndName input |> assertEquals expected
    })
  }