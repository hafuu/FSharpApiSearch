module TypesTest

open Persimmon
open Persimmon.Syntax.UseTestNameByReflection
open Persimmon.MuscleAssert
open FSharpApiSearch

open TestHelper.DSL

let parseDisplayNameTest = parameterize {
  source [
    "A.B", [ { FSharpName = "B"; InternalFSharpName = "B"; GenericParametersForDisplay = [] }; { FSharpName = "A"; InternalFSharpName = "A"; GenericParametersForDisplay = [] } ]
    "A.B<'T>", [ { FSharpName = "B"; InternalFSharpName = "B"; GenericParametersForDisplay = [ "T" ] }; { FSharpName = "A"; InternalFSharpName = "A"; GenericParametersForDisplay = [] } ]
  ]
  run (fun (x, expected) -> test {
    do! DisplayName.ofString x |> assertEquals expected
  })
}

let parseOperatorDisplayNameTest = parameterize {
  source [
    "A.( + )", [ { FSharpName = "( + )"; InternalFSharpName = "op_Addition"; GenericParametersForDisplay = [] }; { FSharpName = "A"; InternalFSharpName = "A"; GenericParametersForDisplay = [] } ]
  ]
  run (fun (x, expected) -> test {
    do! DisplayName.ofOperatorString x |> assertEquals expected
  })
}

module PrintTest =
  let typeA = createType "a" []
  let typeB = createType "b" []
  let typeC = createType "c" []

  let genericA = createType "A" [ variable "a" ]
  let genericB = createType "B" [ variable "a"; variable "b" ]

  let variableA = variable "a"
  let variableB = variable "b"

  let memberMethod = method' "test" [ [ ptype variableA; ptype typeB ] ] typeC
  let memberCurriedMethod = method' "test" [ [ ptype variableA ]; [ ptype typeB ] ] typeC
  let memberProperty = member' "test" (MemberKind.Property PropertyKind.Get) [] typeA

  let printLowTypeTest = parameterize {
    source [
      createType "A" [], "A"
      createType "A.B" [], "B"
      createType "A.B<'C>" [ variable "C" ], "B<'C>"
    ]
    run (fun (input: LowType, expected) -> test {
      do! input.Print() |> assertEquals expected
    })
  }

  let printName_long_test = parameterize {
    source[
      Name.displayNameOfString "A.B", "A.B"
      Name.displayNameOfString "A.B<'C>", "A.B<'C>"
      Name.displayNameOfString "A<'C>.B<'D>", "A<'C>.B<'D>"
    ]
    run (fun (input: Name, expected) -> test {
      do! input.Print() |> assertEquals expected
    })
  }

  let printApiSignatureTest = parameterize {
    source [
      moduleValue typeA, "a"
      moduleFunction' [ [ ptype typeA ]; [ ptype typeB ] ], "a -> b"
      moduleFunction' [ [ ptype (arrow [ typeA; typeB ]) ]; [ ptype typeC ] ], "(a -> b) -> c"

      moduleFunction' [ [ pname "x" >> ptype typeA; pname "y" >> ptype typeB ]; [ ptype typeB ] ], "x:a * y:b -> b"
      moduleFunction' [ [ pname "x" >> ptype typeA ]; [ pname "y" >> ptype typeB ]; [ ptype typeB ] ], "x:a -> y:b -> b"

      moduleValue variableA, "'a"
      moduleValue (array typeA), "a[]"
      moduleValue (array (array2D typeA)), "a[,][]"
      moduleValue (createType "A" [ typeC]), "A<c>"
      moduleValue (genericB), "B<'a, 'b>"
      moduleValue (tuple [ typeA; variableB; typeC ]), "a * 'b * c"
      moduleValue (tuple [ typeA; (tuple [ typeB; typeC ]) ]), "a * (b * c)"
      instanceMember typeA memberMethod, "'a * b -> c"
      instanceMember typeA memberCurriedMethod, "'a -> b -> c"
      instanceMember typeA memberProperty, "a"
      staticMember typeA memberMethod, "'a * b -> c"
      staticMember typeA memberProperty, "a"

      moduleValue (array (tuple [ typeA; typeB ])), "(a * b)[]"
      moduleValue (array (arrow [ typeA; typeB ])), "(a -> b)[]"
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

module QueryTest = // TODO: Matcherのテストに移動
  let replaceAbbreviationTest =  parameterize {
    source [
      "a", (identity "a")
      "string", (typeAbbreviation (identity "String") (identity "string"))
      "Option<string>", (generic (identity "Option") [ typeAbbreviation (identity "String") (identity "string") ])
      "'a list", (typeAbbreviation (generic (identity "List") [ queryVariable "a" ]) (generic (identity "list") [ queryVariable "a" ]))
      "'b list", (typeAbbreviation (generic (identity "List") [ queryVariable "b" ]) (generic (identity "list") [ queryVariable "b" ]))
      "int list", (typeAbbreviation (generic (identity "List") [ identity "int" ]) (generic (identity "list") [ identity "int" ]))
      "string list", (typeAbbreviation (generic (identity "List") [ (typeAbbreviation (identity "String") (identity "string")) ]) (generic (identity "list") [ identity "string" ]))
      "map<'a, 'b, 'c>", (generic (identity "map") [ queryVariable "a"; queryVariable "b"; queryVariable "c" ])
      "list", (identity "list")
      "intList", (typeAbbreviation (generic (identity "List") [ identity "int" ]) (identity "intList"))
      "intKeyMap<'value>", (typeAbbreviation (generic (identity "Map") [ identity "int"; queryVariable "value" ]) (generic (identity "intKeyMap") [ queryVariable "value" ]))
      "reverseArg<'front, 'end>", (typeAbbreviation (generic (identity "ReverseArg") [ queryVariable "end"; queryVariable "front" ]) (generic (identity "reverseArg") [ queryVariable "front"; queryVariable "end" ]))
      "samemap<int>", (typeAbbreviation (generic (identity "Map") [ identity "int"; identity "int" ]) (generic (identity "samemap") [ identity "int" ]))
      "override", (typeAbbreviation (identity "B.Override") (identity "override"))
      "B.override", (typeAbbreviation (identity "B.Override") (identity "B.override"))
      "A.override", (typeAbbreviation (identity "A.Hidden") (identity "A.override"))
      "override<'a>", (typeAbbreviation (generic (identity "B.Override") [ queryVariable "a" ]) (generic (identity "override") [ queryVariable "a" ]))
      "B.override<'a>", (typeAbbreviation (generic (identity "B.Override") [ queryVariable "a" ]) (generic (identity "B.override") [ queryVariable "a" ]))
      "A.override<'a>", (typeAbbreviation (generic (identity "A.Hidden") [ queryVariable "a" ]) (generic (identity "A.override") [ queryVariable "a" ]))
    ]
    run (fun (query, expected) -> test {
      let abbreviations: TypeAbbreviationDefinition[] = [|
        typeAbbreviationDef "string" (identity "String")
        typeAbbreviationDef "list<'a>" (generic (identity "List") [ variable "a" ])
        typeAbbreviationDef "intList" (generic (identity "List") [ identity "int" ])
        typeAbbreviationDef "reverseArg<'b, 'a>" (generic (identity "ReverseArg") [ variable "a"; variable "b" ])
        typeAbbreviationDef "map<'a, 'b>" (generic (identity "Map") [ variable "a"; variable "b" ])
        typeAbbreviationDef "intKeyMap<'v>" (generic (identity "Map") [ identity "int"; variable "v" ])
        typeAbbreviationDef "samemap<'a>" (generic (identity "Map") [ variable "a"; variable "a" ])
        typeAbbreviationDef "A.override" (identity "A.Hidden")
        typeAbbreviationDef "B.override" (identity "B.Override")
        typeAbbreviationDef "A.override<'a>" (generic (identity "A.Hidden") [ variable "a" ])
        typeAbbreviationDef "B.override<'a>" (generic (identity "B.Override") [ variable "a" ])
      |]
      let dictionaries = Seq.singleton { AssemblyName = "test"; Api = Array.empty; TypeDefinitions = Array.empty; TypeAbbreviations = abbreviations }
      let actual = MatcherInitializer.initializeQuery dictionaries (QueryParser.parse query)
      let expected: Query = { OriginalString = query; Method = QueryMethod.BySignature (SignatureQuery.Signature expected) }
      do! actual |> assertEquals expected
    })
  }