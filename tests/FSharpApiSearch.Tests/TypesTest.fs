module TypesTest

open Persimmon
open Persimmon.Syntax.UseTestNameByReflection
open FSharpApiSearch

open TestHelper.DSL

module PrintTest =
  let typeA = createType "a" []
  let typeB = createType "b" []
  let typeC = createType "c" []

  let genericA = createType "A" [ variable "a" ]
  let genericB = createType "B" [ variable "a"; variable "b" ]

  let variableA = variable "a"
  let variableB = variable "b"

  let memberMethod = method' "test" [ variableA; typeB ] typeC
  let memberCurriedMethod = curriedMethod "test" [ variableA; typeB ] typeC
  let memberProperty = member' "test" (MemberKind.Property PropertyKind.Get) [] typeA

  let printApiSignatureTest = parameterize {
    source [
      moduleValue typeA, "a"
      moduleFunction [ typeA; typeB ], "a -> b"
      moduleFunction [ arrow [ typeA; typeB ]; typeC], "(a -> b) -> c"
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
      let abbreviations = [|
          { Abbreviation = identity "string"; Original = identity "String" }
          { Abbreviation = generic (identity "list") [ variable "a" ]; Original = generic (identity "List") [ variable "a" ] }
          { Abbreviation = identity "intList"; Original = generic (identity "List") [ identity "int" ] }
          { Abbreviation = generic (identity "reverseArg") [ variable "b"; variable "a" ]; Original = generic (identity "ReverseArg") [ variable "a"; variable "b" ] }
          { Abbreviation = generic (identity "map") [ variable "a"; variable "b" ]; Original = generic (identity "Map") [ variable "a"; variable "b" ] }
          { Abbreviation = generic (identity "intKeyMap") [ variable "v" ]; Original = generic (identity "Map") [ identity "int"; variable "v" ] }
          { Abbreviation = generic (identity "samemap") [ variable "a" ]; Original = generic (identity "Map") [ variable "a"; variable "a" ] }
          { Abbreviation = identity "A.override"; Original = identity "A.Hidden" }
          { Abbreviation = identity "B.override"; Original = identity "B.Override" }
          { Abbreviation = generic (identity "A.override") [ variable "a" ]; Original = generic (identity "A.Hidden") [ variable "a" ] }
          { Abbreviation = generic (identity "B.override") [ variable "a" ]; Original = generic (identity "B.Override") [ variable "a" ] }
        |]
      let dictionaries = Seq.singleton { AssemblyName = "test"; Api = Array.empty; TypeDefinitions = Array.empty; TypeAbbreviations = abbreviations }
      let actual = Matcher.Initializer.initializeQuery dictionaries (QueryParser.parse query)
      let expected: Query = { OriginalString = query; Method = QueryMethod.BySignature (SignatureQuery.Signature expected) }
      do! actual |> assertEquals expected
    })
  }