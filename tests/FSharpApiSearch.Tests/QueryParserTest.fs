module QueryParserTest

open Persimmon
open Persimmon.Syntax.UseTestNameByReflection
open Persimmon.MuscleAssert
open FSharpApiSearch
open TestHelper.DSL

module BySignature =
  let runParseTest (input, expectedSignature) = test {
    let actual = QueryParser.parse input
    let expected: Query = { OriginalString = input; Method = QueryMethod.BySignature expectedSignature }
    do! actual |> assertEquals expected
  }

  let runSignatureTest (input, expected) = runParseTest (input, SignatureQuery.Signature expected)

  let parseTest = parameterize {
    source [
      "a", (identity "a")
      "a_'2", (identity "a_'2")
      "a.b", (identity "a.b")
      "a.b.c", (identity "a.b.c")
      "'a", (queryVariable "'a")
    ]
    run runSignatureTest
  }

  let wildcardTest = parameterize {
    source [
      "?", (wildcard)
      "?a", (wildcardGroup "a")
      "? -> ?", (arrow [ wildcard; wildcard ])
      "a<?, ?b>", (generic (identity "a") [ wildcard; wildcardGroup "b" ])
    ]
    run runSignatureTest
  }

  let arrowParseTest = parameterize {
    source [
      "a -> a", (arrow [ identity "a"; identity "a" ])
      "('a -> 'b) -> 'c", (arrow [ (arrow [ queryVariable "'a"; queryVariable "'b" ]); queryVariable "'c" ])
      "('a -> 'b)", (arrow [ queryVariable "'a"; queryVariable "'b" ])
    ]
    run runSignatureTest
  }

  let dotNetGenericParseTest = parameterize {
    source [
      "a<b, c>", (generic (identity "a") [ identity "b"; identity "c" ])
      "'a -> b<c, d> -> d", (arrow [ queryVariable "'a"; generic (identity "b") [ identity "c"; identity "d" ]; identity "d" ])
      "?<b, c>", (generic wildcard [ identity "b"; identity "c" ])
      "?a<b, c>", (generic (wildcardGroup "a") [ identity "b"; identity "c" ])
    ]
    run runSignatureTest
  }

  let mlGenericParseTest = parameterize {
    source [
      "a b", (generic (identity "b") [ identity "a" ])
      "a b -> c", (arrow [ generic (identity "b") [ identity "a" ]; identity "c" ])
      "(a, b) c", (generic (identity "c") [ identity "a"; identity "b" ])
      "(a, b) c -> d", (arrow [ generic (identity "c") [ identity "a"; identity "b" ]; identity "d" ])
      "(a, b -> b) c", (generic (identity "c") [ identity "a"; arrow [ identity "b"; identity "b" ] ])
      "a<b> c", (generic (identity "c") [ generic (identity "a") [ identity "b" ] ])
      "a ?", (generic (wildcard) [ identity "a" ])
      "b ?a", (generic (wildcardGroup "a") [ identity "b" ])
      "a b c", (generic (identity "c") [ generic (identity "b") [ identity "a" ] ])
      "b<a> c d", (generic (identity "d") [ generic (identity "c") [ generic (identity "b") [ identity "a" ] ] ])
    ]
    run runSignatureTest
  }

  let tupleParseTest = parameterize {
    source [
      "a * b", (tuple [ identity "a"; identity "b" ])
      "a * b -> b", (arrow [ tuple [ identity "a"; identity "b" ]; identity "b" ])
      "a<b * c, d>", (generic (identity "a") [ tuple [ identity "b"; identity "c" ]; identity "d" ])
      "a * b c", (tuple [ identity "a"; generic (identity "c") [ identity "b" ] ])
      "a -> (a * b)", (arrow [ identity "a"; tuple [ identity "a"; identity "b" ] ])
      "(a * b) -> a", (arrow [ tuple [ identity "a"; identity "b" ]; identity "a" ])
    ]
    run runSignatureTest
  }

  let arrayParseTest = parameterize {
    source [
      "a[]", (queryArray (identity "a"))
      "a[,]", (queryArray2D (identity "a"))
      "a[][]", (queryArray (queryArray (identity "a")))
      "a[][,]", (queryArray2D (queryArray (identity "a")))
      "a<b>[]", (queryArray (generic (identity "a") [ identity "b" ]))
      "a<b[]>", (generic (identity "a") [ queryArray (identity "b") ])
      "(a -> b)[]", (queryArray (arrow [ identity "a"; identity "b" ]))
      "a[] b", (generic (identity "b") [ queryArray (identity "a") ])
    ]
    run runSignatureTest
  }

  let instanceMemberParseTest = parameterize {
    source [
      "a => b", (SignatureQuery.InstanceMember (identity "a", [], identity "b"))
      "a => b -> c", (SignatureQuery.InstanceMember (identity "a", [ identity "b" ], identity "c"))
      "a => b -> c -> d", (SignatureQuery.InstanceMember (identity "a", [ identity "b"; identity "c" ], identity "d"))
    ]
    run runParseTest
  }

module ByName =
  let parseTest =
    let alpha = queryVariable "'a"
    let beta = queryVariable "'b"
    let option_alpha = generic (identity "option") [ alpha ]
    let option_beta = generic (identity "option") [ beta ]

    let Compare = NameMatchMethod.StringCompare
    let Regex = NameMatchMethod.Regex
    parameterize {
      source [
        "map : _", [ "map", Compare ], SignatureQuery.Wildcard
        "a_'3 : _", [ "a_'3", Compare ], SignatureQuery.Wildcard
        "bind : ('a -> 'b option) -> 'a option -> 'b option", [ "bind", Compare ], (SignatureQuery.Signature (arrow [ (arrow [ alpha; option_beta ]); option_alpha; option_beta ]))
        "ToString : obj => unit -> string", [ "ToString", Compare ], (SignatureQuery.InstanceMember (identity "obj", [ identity "unit" ], identity "string"))
        "(+) : _", [ "op_Addition", Compare ], SignatureQuery.Wildcard
        "( + ) : _", [ "op_Addition", Compare ], SignatureQuery.Wildcard
        "A.B : _", [ ("B", Compare); ("A", Compare) ], SignatureQuery.Wildcard
        "* : _", [ "*", Regex ], SignatureQuery.Wildcard
        "( * ) : _", [ "op_Multiply", Compare ], SignatureQuery.Wildcard

        ".ctor : _", [ ".ctor", Compare ], SignatureQuery.Wildcard
        "A..ctor : _", [ (".ctor", Compare); ("A", Compare) ], SignatureQuery.Wildcard

        "ma* : _", [ "ma*", Regex ], SignatureQuery.Wildcard
        "m*p : _", [ "m*p", Regex ], SignatureQuery.Wildcard
        "*ap : _", [ "*ap", Regex ], SignatureQuery.Wildcard
        "A.ma* : _", [ ("ma*", Regex); ("A", Compare) ], SignatureQuery.Wildcard
        "A.*ap : _", [ ("*ap", Regex); ("A", Compare) ], SignatureQuery.Wildcard
      ]
      run (fun (input, expectedName, expectedSig) -> test {
        let actual = QueryParser.parse input
        let expected: Query = { OriginalString = input; Method = QueryMethod.ByName (expectedName, expectedSig) }
        do! actual |> assertEquals expected
      })
    }

module ByActivePattern =
  let parseTest =
    parameterize {
      source [
        "(||) : ... -> 'a -> ?", QueryMethod.ByActivePattern { Kind = ActivePatternKind.ActivePattern; Signature = ActivePatternSignature.AnyParameter (queryVariable "'a", wildcard) }
        "(||) : 'a -> ?", QueryMethod.ByActivePattern { Kind = ActivePatternKind.ActivePattern; Signature = ActivePatternSignature.Specified (arrow [ queryVariable "'a"; wildcard ]) }
        "(||) : 'a -> 'b -> ?", QueryMethod.ByActivePattern { Kind = ActivePatternKind.ActivePattern; Signature = ActivePatternSignature.Specified (arrow [ queryVariable "'a"; queryVariable "'b"; wildcard ]) }

        "(|_|) : ... -> 'a -> ?", QueryMethod.ByActivePattern { Kind = ActivePatternKind.PartialActivePattern; Signature = ActivePatternSignature.AnyParameter (queryVariable "'a", wildcard) }
      ]
      run (fun (input, expected) -> test {
        let actual = QueryParser.parse input
        let expected: Query = { OriginalString = input; Method = expected }
        do! actual |> assertEquals expected
      })
    }

module ByComputationExpression =
  let parseTest =
    parameterize {
      source [
        "{} : ?", QueryMethod.ByComputationExpression { Syntaxes = []; Type = wildcard }
        "{ } : ?", QueryMethod.ByComputationExpression { Syntaxes = []; Type = wildcard }
        "{ _ } : ?", QueryMethod.ByComputationExpression { Syntaxes = []; Type = wildcard }
        "{ use } : ?", QueryMethod.ByComputationExpression { Syntaxes = [ "use" ]; Type = wildcard }
        "{ let!; return } : 'a", QueryMethod.ByComputationExpression { Syntaxes = [ "let!"; "return" ]; Type = queryVariable "'a" }
      ]
      run (fun (input, expected) -> test {
        let actual = QueryParser.parse input
        let expected: Query = { OriginalString = input; Method = expected }
        do! actual |> assertEquals expected
      })
    }

  let syntaxErrorTest =
    parameterize {
      source [
        "{ _; use } : ?"
      ]
      run (fun (input) -> test {
        let! e = trap { it (QueryParser.parse input) }
        do! e |> assertNotEquals null
      })
    }