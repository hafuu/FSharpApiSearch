module PrinterTest

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
let memberParamArrayMethod = method' "test" [ [ pparams >> pname "xs" >> ptype (array int) ] ] typeC
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
    Name.ofString "A.B<'C>", "A.B<'C>"
    Name.ofString "A<'C>.B<'D>", "A<'C>.B<'D>"
  ]
  run (fun (input: Name, expected) -> test {
    do! input.Print() |> assertEquals expected
  })
}

let printAccessPathTest = parameterize {
  source [
    "A.B.C.D", None, "A.B.C"
    "A.B.C.D", Some 1, "C"
    "A.B.C.D", Some 2, "B.C"
    "A.B.C.D", Some 10, "A.B.C"

    "A.B.C<'A>.D", None, "A.B.C<'A>"
  ]
  run (fun (input, depth, expected) -> test {
    let name = Name.ofString input
    let sb = System.Text.StringBuilder()
    do FSharpImpl.printAccessPath depth name sb |> ignore
    let actual = sb.ToString()
    do! actual |> assertEquals expected
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

      moduleFunction' [ [ ptype (tuple [ typeA; typeB ]) ]; [ ptype typeB ] ], "a * b -> b"
      moduleFunction' [ [ pname "x" >> ptype (tuple [ typeA; typeB ]) ]; [ ptype typeB ] ], "x:(a * b) -> b"

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
      instanceMember typeA memberParamArrayMethod, "[<ParamArray>]xs:int[] -> c"
      instanceMember typeA memberCurriedMethod, "'a -> b -> c"
      instanceMember typeA memberProperty, "a"
      instanceMember typeA (method' "test" [ [ ptype unit ] ] (arrow [ typeA; typeA ])), "unit -> (a -> a)"
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

      typeAbbreviationApi (typeAbbreviationDef "Test.A1<'a>" (fsharpOption (variable "'a" ))), "type A1<'a> = Microsoft.FSharp.Core.Option<'a>"
      typeAbbreviationApi (typeAbbreviationDef "Test.A2" (fsharpOption Int32)), "type A2 = Microsoft.FSharp.Core.Option<System.Int32>"
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

let printCSharpAccessPathTest = parameterize {
  source [
    "A.B.C.D", None, "A.B.C"
    "A.B.C.D", Some 1, "C"
    "A.B.C.D", Some 2, "B.C"
    "A.B.C.D", Some 10, "A.B.C"

    "A.B.C<'A>.D", None, "A.B.C<A>"
  ]
  run (fun (input, depth, expected) -> test {
    let name = Name.ofString input
    let sb = System.Text.StringBuilder()
    do CSharpImpl.printAccessPath depth name sb |> ignore
    let actual = sb.ToString()
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

let printCSharpConstraints =
  parameterize {
    source [
      [ constraint' [ "'T" ] (Constraint.SubtypeConstraints (identity "IA")) ], "where T : IA"
      [ constraint' [ "'T" ] (Constraint.SubtypeConstraints (identity "IA")); constraint' [ "'T" ] (Constraint.SubtypeConstraints (identity "IB")) ], "where T : IA, IB"
      [ constraint' [ "'T" ] (Constraint.SubtypeConstraints (identity "IA")); constraint' [ "'U" ] (Constraint.SubtypeConstraints (identity "IA")) ], "where T : IA where U : IA"

    ]

    run (fun (constraints, expected) -> test {
      let api = { Name = Name.ofString "test"; Signature = moduleValue (identity "a"); TypeConstraints = constraints; Document = None }
      let actual = CSharp.tryPrintTypeConstraints api
      do! actual |> assertEquals (Some expected)
    })
  }

let printCSharpTest =
  let n = Name.ofString
  let t = createType "T" []
  parameterize {
    source [
      api (n "M<'T>.value") (moduleValue (variable "'T")), " : T"
      api (n "M.value") (moduleValue (fsharpList int)), " : FSharpList<int>"
        
      api (n "M.tuple") (moduleValue (tuple [ int; string ])), " : Tuple<int, string>"
      api (n "M.tuple") (moduleValue (structTuple [ int; string ])), " : (int, string)"

      api (n "M.fn") (moduleFunction' [ [ ptype unit ]; [ ptype unit ] ]), "() : void"
      api (n "M.fn<'T>") (moduleFunction' [ [ ptype (variable "'T") ]; [ ptype int ] ]), "(T) : int"

      api (n "T.method<'T>") (instanceMember t (method' "method" [ [ ptype t ] ] int)), "(T) : int"
      api (n "T.method") (instanceMember t (method' "method" [ [ ptype int >> pname "x"; ptype string >> pname "y" ] ] int)), "(int x, string y) : int"
      api (n "T.method") (instanceMember t (method' "method" [ [ ptype unit ] ] unit)), "() : void"
      api (n "T.method") (instanceMember t (method' "method" [ [ ptype unit; ptype int ] ] unit)), "(Unit, int) : void"

      api (n "T.method") (instanceMember t (method' "method" [ [ ptype int >> pname "x"; popt >> ptype string >> pname "y" ] ] unit)), "(int x, [string y]) : void"
      api (n "T.method") (instanceMember t (method' "method" [ [ pparams >> ptype (array int) >> pname "xs" ] ] unit)), "(params int[] xs) : void"

      api (n "T.method") (staticMember t (method' "method" [ [ ptype unit ] ] unit)), "() : void"

      api (n "T.prop") (instanceMember t (property' "prop" PropertyKind.Get [] int)), " : int"
      api (n "T.prop") (instanceMember t (property' "prop" PropertyKind.GetSet [ [ ptype string ] ] int)), "[string] : int"
      api (n "T.prop") (instanceMember t (property' "prop" PropertyKind.GetSet [ [ ptype string >> pname "x" ] ] int)), "[string x] : int"
      api (n "T.prop") (staticMember t (property' "prop" PropertyKind.Get [] int)), " : int"

      api (n "T.new") (constructor' t (method' "new" [ [ ptype unit ] ] t)), "() : void"
      api (n "T.new") (constructor' t (method' "new" [ [ ptype int >> pname "x"; ptype string >> pname "y" ] ] t)), "(int x, string y) : void"

      api (n "T.ext") (extensionMember (method' "ext" [ [ ptype int >> pname "x"; ptype int >> pname "y" ] ] unit)), "(this int x, int y) : void"

      api (n "T.byref") (instanceMember t (method' "method" [ [ ptype (byref int) >> pname "x"; ptype (out string) >> pname "y" ] ] (byref int))), "(ref int x, out string y) : ref int"
    ]

    run (fun (input: Api, expected) -> test {
      do! CSharp.printSignature input |> assertEquals expected
    })
  }