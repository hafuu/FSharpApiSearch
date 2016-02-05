module TypesTest

open Persimmon
open Persimmon.Syntax.UseTestNameByReflection
open FSharpApiSearch
open TestHelpers.DSL

module SignatureTest =
  let displayTest = parameterize {
    source [
      identity "a", "a"
      variable "a", "'a"
      generic (identity "a") [ identity "b"; identity "c" ], "a<b, c>"
      arrow [ identity "a"; identity "b"; identity "c" ], "a -> b -> c"
      arrow [ identity "a"; arrow [ identity "a"; identity "b" ]; identity "b" ], "a -> (a -> b) -> b"
      tuple [ identity "a"; identity "b" ], "a * b"
      tuple [ identity "a"; tuple [ identity "a"; identity "b" ] ], "a * (a * b)"
      staticMethod [ identity "a" ] (identity "b"), "a -> b"
      staticMethod [ identity "a"; identity "b" ] (identity "b"), "a * b -> b"
      instanceMember (identity "a") [] (identity "b"), "b"
      instanceMember (identity "a") [ identity "a" ] (identity "b"), "a -> b"
      instanceMember (identity "a") [ identity "a"; identity "b" ] (identity "b"), "a * b -> b"
      array (identity "a"), "a[]"
      array2d (identity "a"), "a[,]"
      array (array2d (identity "a")), "a[,][]"
    ]
    run (fun (input, expected) -> test {
      let actual = Signature.display input
      do! actual |> assertEquals expected
    })
  }

  let debugDisplayTest = parameterize {
    source [
      identity "a", "a"
      variable "a", "'qa"
      generic (identity "a") [ identity "b"; identity "c" ], "a<b, c>"
      arrow [ identity "a"; identity "b"; identity "c" ], "a -> b -> c"
      arrow [ identity "a"; arrow [ identity "a"; identity "b" ]; identity "b" ], "a -> (a -> b) -> b"
      tuple [ identity "a"; identity "b" ], "a * b"
      tuple [ identity "a"; tuple [ identity "a"; identity "b" ] ], "a * (a * b)"
      staticMethod [ identity "a" ] (identity "b"), "a -> b"
      staticMethod [ identity "a"; identity "b" ] (identity "b"), "a * b -> b"
      instanceMember (identity "a") [] (identity "b"), "a => b"
      instanceMember (identity "a") [ identity "a" ] (identity "b"), "a => a -> b"
      instanceMember (identity "a") [ identity "a"; identity "b" ] (identity "b"), "a => a * b -> b"
      array (identity "a"), "a[]"
      array2d (identity "a"), "a[,]"
      array (array2d (identity "a")), "a[,][]"
    ]
    run (fun (input, expected) -> test {
      let actual = Signature.debugDisplay input
      do! actual |> assertEquals expected
    })
  }

  let replaceAbbreviationTest =
    let p = QueryParser.parseFSharpSignature
    parameterize {
      source [
        "a", (p "a")
        "string", (abbreviation (p "String") (p "string"))
        "Option<string>", (generic (identity "Option") [ abbreviation (identity "String") (identity "string") ])
        "'a list", (abbreviation (p "List<'a>") (p "list<'a>"))
        "map<'a, 'b, 'c>", (p "map<'a, 'b, 'c>")
        "'b list", (abbreviation (p "List<'b>") (p "list<'b>"))
        "int list", (abbreviation (p "List<int>") (p "list<int>"))
        "list", (identity "list")
        "intList", (abbreviation (p "List<int>") (p "intList"))
        "intKeyMap<value>", (abbreviation (p "Map<int, value>") (p "intKeyMap<value>"))
        "reverseArg<front, end>", (abbreviation (p "ReverseArg<end, front>") (p "reverseArg<front, end>"))
        "samemap<int>", (abbreviation (p "Map<int, int>") (p "samemap<int>"))
        "override", (abbreviation (p "Override") (p "override"))
        "override<'a>", (abbreviation (p "Override<'a>") (p "override<'a>"))
      ]
      run (fun (query, expected) -> test {
        let table = [
          { Abbreviation = p "string"; Original = p "String" }
          { Abbreviation = p "'a list"; Original = p "List<'a>" }
          { Abbreviation = p "intList"; Original = p "List<int>" }
          { Abbreviation = p "reverseArg<'b, 'a>" ; Original = p "ReverseArg<'a, 'b>" }
          { Abbreviation = p "map<'a, 'b>"; Original = p "Map<'a, 'b>" }
          { Abbreviation = p "intKeyMap<'v>"; Original = p "Map<int, 'v>" }
          { Abbreviation = p "samemap<'a>"; Original = p "Map<'a, 'a>" }
          { Abbreviation = p "override"; Original = p "Hidden" }
          { Abbreviation = p "override"; Original = p "Override" }
          { Abbreviation = p "override<'a>"; Original = p "Hidden<'a>" }
          { Abbreviation = p "override<'a>"; Original = p "Override<'a>" }
        ]
        let actual = Signature.replaceAbbreviation table (QueryParser.parseFSharpSignature query)
        do! actual |> assertEquals expected
      })
    }