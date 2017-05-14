module QueryParserTest

open Persimmon
open Persimmon.Syntax.UseTestNameByReflection
open Persimmon.MuscleAssert
open FSharpApiSearch
open TestHelper.DSL
open TestHelper.Types

module FSharp =
  module BySignature =
    let runParseTest (input, expectedSignature) = test {
      let actual = QueryParser.FSharp.parse input
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

        "struct (a * b)", (structTuple [ identity "a"; identity "b" ])
        "struct (a * b) c", (generic (identity "c") [ structTuple [ identity "a"; identity "b" ] ])
        "struct (a * b * c)", (structTuple [ identity "a"; identity "b"; identity "c" ])
        "struct ((a * b) * c)", (structTuple [ tuple [ identity "a"; identity "b" ]; identity "c" ])
        "struct (struct (a * b) * struct (c * d))", (structTuple [ structTuple [ identity "a"; identity "b" ]; structTuple [ identity "c"; identity "d" ] ]) 
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

    let flexibleTest = parameterize {
      source [
        "#A", (flexible (identity "A"))
        "#A<B>", (flexible (generic (identity "A") [ identity "B" ]))
        "A<#B>", (generic (identity "A") [ flexible (identity "B") ])
      ]
      run runSignatureTest
    }

  module ByName =
    let parseTest =
      let alpha = queryVariable "'a"
      let beta = queryVariable "'b"
      let option_alpha = generic (identity "option") [ alpha ]
      let option_beta = generic (identity "option") [ beta ]

      let Compare = NameMatchMethod.StringCompare
      let Regex = NameMatchMethod.Regex
      let Any = NameMatchMethod.Any

      let byName expected method = { Expected = expected; GenericParameters = []; MatchMethod = method }

      parameterize {
        source [
          "map : _", [ byName "map" Compare  ], SignatureQuery.Wildcard
          "a_'3 : _", [ byName "a_'3" Compare ], SignatureQuery.Wildcard
          "bind : ('a -> 'b option) -> 'a option -> 'b option", [ byName "bind" Compare ], (SignatureQuery.Signature (arrow [ (arrow [ alpha; option_beta ]); option_alpha; option_beta ]))
          "(+) : _", [ byName "op_Addition" Compare ], SignatureQuery.Wildcard
          "( + ) : _", [ byName "op_Addition" Compare ], SignatureQuery.Wildcard
          "A.B : _", [ byName "B" Compare; byName "A" Compare ], SignatureQuery.Wildcard
          "* : _", [ byName "*" Any ], SignatureQuery.Wildcard
          "( * ) : _", [ byName "op_Multiply" Compare ], SignatureQuery.Wildcard

          ".ctor : _", [ byName ".ctor" Compare ], SignatureQuery.Wildcard
          "A..ctor : _", [ byName ".ctor" Compare; byName "A" Compare ], SignatureQuery.Wildcard

          "ma* : _", [ byName "^ma.*$" Regex ], SignatureQuery.Wildcard
          "m*p : _", [ byName "^m.*p$" Regex ], SignatureQuery.Wildcard
          "*ap : _", [ byName "^.*ap$" Regex ], SignatureQuery.Wildcard
          "A.ma* : _", [ byName "^ma.*$" Regex; byName "A" Compare ], SignatureQuery.Wildcard
          "A.*ap : _", [ byName "^.*ap$" Regex; byName "A" Compare ], SignatureQuery.Wildcard
        ]
        run (fun (input, expectedName, expectedSig) -> test {
          let actual = QueryParser.FSharp.parse input
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

          "(||) : ... -> 'a * 'b -> ?", QueryMethod.ByActivePattern { Kind = ActivePatternKind.ActivePattern; Signature = ActivePatternSignature.AnyParameter (tuple [ queryVariable "'a"; queryVariable "'b" ], wildcard) }
          "(||) : ... -> ('a -> 'b) -> ?", QueryMethod.ByActivePattern { Kind = ActivePatternKind.ActivePattern; Signature = ActivePatternSignature.AnyParameter (arrow [ queryVariable "'a"; queryVariable "'b" ], wildcard) }
        ]
        run (fun (input, expected) -> test {
          let actual = QueryParser.FSharp.parse input
          let expected: Query = { OriginalString = input; Method = expected }
          do! actual |> assertEquals expected
        })
      }

    let parseErrorTest =
      parameterize {
        source [
          "(||) : ... -> 'a -> 'b -> ?"
        ]
        run (fun input -> test {
          let! ex = trap { it (QueryParser.FSharp.parse input) }
          do! ex |> assertNotEquals null
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
          "{ try/finally } : ?", QueryMethod.ByComputationExpression { Syntaxes = [ "try/finally" ]; Type = wildcard }
          "{ let!; return } : 'a", QueryMethod.ByComputationExpression { Syntaxes = [ "let!"; "return" ]; Type = queryVariable "'a" }
        ]
        run (fun (input, expected) -> test {
          let actual = QueryParser.FSharp.parse input
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
          let! e = trap { it (QueryParser.FSharp.parse input) }
          do! e |> assertNotEquals null
        })
      }

module CSharp =
  let runParseTest (input, expected) = test {
    let actual = QueryParser.CSharp.parse input
    let expected: Query = { OriginalString = input; Method = expected }
    do! actual |> assertEquals expected
  }
  module BySignature =
    let runSignatureTest (input, expected) = runParseTest (input, QueryMethod.BySignature (SignatureQuery.Signature expected))

    let parseTest = parameterize {
      source [
        "a", (identity "a")
        "a_'2", (identity "a_'2")
        "a.b", (identity "a.b")
        "a.b.c", (identity "a.b.c")
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

    let dotNetGenericTest = parameterize {
      source [
        "a<b, c>", (generic (identity "a") [ identity "b"; identity "c" ])
        "a<b<c>, d>", (generic (identity "a") [ generic (identity "b") [ identity "c" ]; identity "d" ])
        "a -> b<c, d> -> d", (arrow [ identity "a"; generic (identity "b") [ identity "c"; identity "d" ]; identity "d" ])
        "?<b, c>", (generic wildcard [ identity "b"; identity "c" ])
        "?a<b, c>", (generic (wildcardGroup "a") [ identity "b"; identity "c" ])
      ]
      run runSignatureTest
    }

    let arrowTest = parameterize {
      source [
        "a -> a", (arrow [ identity "a"; identity "a" ])
        "(a -> b) -> c", (arrow [ (arrow [ identity "a"; identity "b" ]); identity "c" ])
        "(a -> b)", (arrow [ identity "a"; identity "b" ])
        "a, b -> c", (arrow [ tuple [ identity "a"; identity "b" ]; identity "c" ])
        "(a, (b, c)) -> d", (arrow [ tuple [ identity "a"; structTuple [ identity "b"; identity "c" ] ]; identity "d" ])
        "((a, b)) -> c", (arrow [ structTuple [ identity "a"; identity "b" ]; identity "c" ])
      ]
      run runSignatureTest
    }

    let tupleTest = parameterize {
      source [
        "(a, b)", (structTuple [ identity "a"; identity "b" ])
        "(a, (b, c), d)", (structTuple [ identity "a"; structTuple [ identity "b"; identity "c" ]; identity "d" ])
        "a<(b, c), d>", (generic (identity "a") [ structTuple [ identity "b"; identity "c" ]; identity "d" ])
        
      ]
      run runSignatureTest
    }

    let arrayTest = parameterize {
      source [
        "a[]", (queryArray (identity "a" ))
        "a[][,]", (queryArray (queryArray2D (identity "a")))
        "a<b>[]", (queryArray (generic (identity "a") [ identity "b" ]))
      ]
      run runSignatureTest
    }

    let variableTest = parameterize {
      source [
        "<a> : a -> b", (arrow [ queryVariable "'a"; identity "b" ])
        "<a, b> : a -> b", (arrow [ queryVariable "'a"; queryVariable "'b" ])
      ]

      run runSignatureTest
    }

    let unitTest = parameterize {
      source [
        "() -> void", (arrow [ Unit; Unit ])
      ]

      run runSignatureTest
    }

    let byrefTest = parameterize {
      source [
        "ref A -> B", (arrow [ byref (identity "A"); identity "B" ])
        "out A[] -> B", (arrow [ out (queryArray (identity "A")); identity "B" ])
        "string, out int -> bool", (arrow [ tuple [ identity "string"; out (identity "int") ]; identity "bool" ])
      ]

      run runSignatureTest
    }

    let flexibleTest = parameterize {
      source [
        "#A", (flexible (identity "A"))
        "#A<B>", (flexible (generic (identity "A") [ identity "B" ]))
        "A<#B>", (generic (identity "A") [ flexible (identity "B") ])
      ]
      run runSignatureTest
    }

  module ByName =
    let runByNameTest (input, expectedName, expectedSignature) = runParseTest (input, QueryMethod.ByName (expectedName, expectedSignature))

    let Compare = NameMatchMethod.StringCompare
    let Regex = NameMatchMethod.Regex
    let Any = NameMatchMethod.Any

    let byName expected method = { Expected = expected; GenericParameters = []; MatchMethod = method }
    let byName2 expected generics method = { Expected = expected; GenericParameters = generics; MatchMethod = method }

    let byNameTest = parameterize {
      source [
        "a.b : _", [ byName "b" Compare; byName "a" Compare ], SignatureQuery.Wildcard
        "* : _", [ byName "*" Any ], SignatureQuery.Wildcard
        "a.* : b", [ byName "*" Any; byName "a" Compare ], SignatureQuery.Signature (identity "b")
        "a* : _", [ byName "^a.*$" Regex ], SignatureQuery.Wildcard
      ]

      run runByNameTest
    }

    let byNameAndVariableTest = parameterize {
      source [
        "test<a, b, c> : b", [ byName2 "test" [ "a"; "b"; "c" ] Compare ], SignatureQuery.Signature (queryVariable "'b")
        "*<a, b, c> : b", [ byName2 "*" [ "a"; "b"; "c" ] Any ], SignatureQuery.Signature (queryVariable "'b")
        "class.method<b> : a, c -> b", [ byName2 "method" [ "b" ] Compare; byName2 "class" [] Compare ],
          SignatureQuery.Signature (arrow [ tuple [ identity "a"; identity "c" ]; queryVariable "'b" ])
        "class<a>.method<b> : a, c -> b", [ byName2 "method" [ "b" ] Compare; byName2 "class" [ "a" ] Compare ],
          SignatureQuery.Signature (arrow [ tuple [ queryVariable "'a"; identity "c" ]; queryVariable "'b" ])
      ]

      run runByNameTest
    }