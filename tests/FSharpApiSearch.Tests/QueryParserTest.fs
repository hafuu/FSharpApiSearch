[<Persimmon.Category("parser")>]
module QueryParserTest

open Persimmon
open Persimmon.Syntax.UseTestNameByReflection
open Persimmon.MuscleAssert
open FSharpApiSearch
open TestHelper.DSL
open TestHelper.Types

let buildSpaceTestSource xs =
  let buildQuery (xs: string seq) =
    [
      for i = 0 to Seq.length xs do
        let elems = ResizeArray(xs)
        elems.Insert(i, " ")
        yield System.String.Concat(elems)
    ]
  [
    for (elems, parameters) in xs do
      for input in buildQuery elems do
        yield (input, parameters)
  ]

module FSharp =
  module BySignature =
    let runSignatureTest (input, expected) = test {
      let actual = QueryParser.FSharp.parse input
      let expected: Query = { OriginalString = input; Method = QueryMethod.BySignature (SignatureQuery.Signature expected) }
      do! actual |> assertEquals expected
    }

    let parseTest = parameterize {
      source [
        "a", (userInput "a")
        "a_'2", (userInput "a_'2")
        "a.b", (userInput "a.b")
        "a.b.c", (userInput "a.b.c")
        "'a", (queryVariable "'a")
        "_a", (userInput "_a")
      ]
      run runSignatureTest
    }

    let wildcardTest = parameterize {
      source [
        "?", (wildcard)
        "?a", (wildcardGroup "a")
        "? -> ?", (arrow [ wildcard; wildcard ])
        "a<?, ?b>", (generic (userInput "a") [ wildcard; wildcardGroup "b" ])
        "_", (wildcard)
      ]
      run runSignatureTest
    }

    let arrowParseTest = parameterize {
      source [
        "a -> a", (arrow [ userInput "a"; userInput "a" ])
        "('a -> 'b) -> 'c", (arrow [ (arrow [ queryVariable "'a"; queryVariable "'b" ]); queryVariable "'c" ])
        "('a -> 'b)", (arrow [ queryVariable "'a"; queryVariable "'b" ])
      ]
      run runSignatureTest
    }

    let dotNetGenericParseTest = parameterize {
      source [
        "a<b, c>", (generic (userInput "a") [ userInput "b"; userInput "c" ])
        "'a -> b<c, d> -> d", (arrow [ queryVariable "'a"; generic (userInput "b") [ userInput "c"; userInput "d" ]; userInput "d" ])
        "?<b, c>", (generic wildcard [ userInput "b"; userInput "c" ])
        "?a<b, c>", (generic (wildcardGroup "a") [ userInput "b"; userInput "c" ])
      ]
      run runSignatureTest
    }

    let mlGenericParseTest = parameterize {
      source [
        "a b", (generic (userInput "b") [ userInput "a" ])
        "a b -> c", (arrow [ generic (userInput "b") [ userInput "a" ]; userInput "c" ])
        "(a, b) c", (generic (userInput "c") [ userInput "a"; userInput "b" ])
        "(a, b) c -> d", (arrow [ generic (userInput "c") [ userInput "a"; userInput "b" ]; userInput "d" ])
        "(a, b -> b) c", (generic (userInput "c") [ userInput "a"; arrow [ userInput "b"; userInput "b" ] ])
        "a<b> c", (generic (userInput "c") [ generic (userInput "a") [ userInput "b" ] ])
        "a ?", (generic (wildcard) [ userInput "a" ])
        "b ?a", (generic (wildcardGroup "a") [ userInput "b" ])
        "a b c", (generic (userInput "c") [ generic (userInput "b") [ userInput "a" ] ])
        "b<a> c d", (generic (userInput "d") [ generic (userInput "c") [ generic (userInput "b") [ userInput "a" ] ] ])
      ]
      run runSignatureTest
    }

    let tupleParseTest = parameterize {
      source [
        "a * b", (tuple [ userInput "a"; userInput "b" ])
        "a * b -> b", (arrow [ tuple [ userInput "a"; userInput "b" ]; userInput "b" ])
        "a<b * c, d>", (generic (userInput "a") [ tuple [ userInput "b"; userInput "c" ]; userInput "d" ])
        "a * b c", (tuple [ userInput "a"; generic (userInput "c") [ userInput "b" ] ])
        "a -> (a * b)", (arrow [ userInput "a"; tuple [ userInput "a"; userInput "b" ] ])
        "(a * b) -> a", (arrow [ tuple [ userInput "a"; userInput "b" ]; userInput "a" ])

        "struct (a * b)", (structTuple [ userInput "a"; userInput "b" ])
        "struct (a * b) c", (generic (userInput "c") [ structTuple [ userInput "a"; userInput "b" ] ])
        "struct (a * b * c)", (structTuple [ userInput "a"; userInput "b"; userInput "c" ])
        "struct ((a * b) * c)", (structTuple [ tuple [ userInput "a"; userInput "b" ]; userInput "c" ])
        "struct (struct (a * b) * struct (c * d))", (structTuple [ structTuple [ userInput "a"; userInput "b" ]; structTuple [ userInput "c"; userInput "d" ] ]) 
      ]
      run runSignatureTest
    }

    let arrayParseTest = parameterize {
      source [
        "a[]", (queryArray (userInput "a"))
        "a[,]", (queryArray2D (userInput "a"))
        "a[][]", (queryArray (queryArray (userInput "a")))
        "a[][,]", (queryArray2D (queryArray (userInput "a")))
        "a<b>[]", (queryArray (generic (userInput "a") [ userInput "b" ]))
        "a<b[]>", (generic (userInput "a") [ queryArray (userInput "b") ])
        "(a -> b)[]", (queryArray (arrow [ userInput "a"; userInput "b" ]))
        "a[] b", (generic (userInput "b") [ queryArray (userInput "a") ])
      ]
      run runSignatureTest
    }

    let subtypeTest = parameterize {
      source [
        "#A", (subtype (userInput "A"))
        "#A<B>", (subtype (generic (userInput "A") [ userInput "B" ]))
        "A<#B>", (generic (userInput "A") [ subtype (userInput "B") ])
      ]
      run runSignatureTest
    }

  module ByName =
    let runByNameTest (input, expectedName, expectedSig) = test {
      let actual = QueryParser.FSharp.parse input
      let expected: Query = { OriginalString = input; Method = QueryMethod.ByName (expectedName, expectedSig) }
      do! actual |> assertEquals expected
    }

    let parseTest =
      let alpha = queryVariable "'a"
      let beta = queryVariable "'b"
      let option_alpha = generic (userInput "option") [ alpha ]
      let option_beta = generic (userInput "option") [ beta ]

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

          "ma* : _", [ byName "ma" StartsWith ], SignatureQuery.Wildcard
          "m*p : _", [ byName "^m.*p$" Regex ], SignatureQuery.Wildcard
          "*a* : _", [ byName "a" Contains ], SignatureQuery.Wildcard
          "*ap : _", [ byName "ap" EndsWith ], SignatureQuery.Wildcard
          "A.ma* : _", [ byName "ma" StartsWith; byName "A" Compare ], SignatureQuery.Wildcard
          "A.*ap : _", [ byName "ap" EndsWith; byName "A" Compare ], SignatureQuery.Wildcard

          "A<'t> : _", [ byGenericName "A" [ "t" ] Compare ], SignatureQuery.Wildcard
          "A<'t>.B<'u> : _", [ byGenericName "B" [ "u" ] Compare; byGenericName "A" [ "t" ] Compare ], SignatureQuery.Wildcard
        ]
        run runByNameTest
      }

  module ByActivePattern =
    let runByActivePatternTest (input, expected) = test {
      let actual = QueryParser.FSharp.parse input
      let expected: Query = { OriginalString = input; Method = expected }
      do! actual |> assertEquals expected
    }

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
        run runByActivePatternTest
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
    let runByComputationExpressionTest (input, expected) = test {
      let actual = QueryParser.FSharp.parse input
      let expected: Query = { OriginalString = input; Method = expected }
      do! actual |> assertEquals expected
    }

    let parseTest =
      parameterize {
        source [
          "{} : ?", QueryMethod.ByComputationExpression { Syntaxes = []; Type = wildcard }
          "{ } : ?", QueryMethod.ByComputationExpression { Syntaxes = []; Type = wildcard }
          "{ _ } : ?", QueryMethod.ByComputationExpression { Syntaxes = []; Type = wildcard }
          "{ use } : ?", QueryMethod.ByComputationExpression { Syntaxes = [ syn "use" ]; Type = wildcard }
          "{ try/finally } : ?", QueryMethod.ByComputationExpression { Syntaxes = [ syn "try/finally" ]; Type = wildcard }
          "{ let!; return } : 'a", QueryMethod.ByComputationExpression { Syntaxes = [ syn "let!"; syn "return" ]; Type = queryVariable "'a" }
        ]
        run runByComputationExpressionTest
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

  module Spaces =
    let signatureTest =
      let cases  = [
        [ "a"; "."; "b"  ], userInput "a.b"
        [ "a"; "[]"; "[,]" ], (queryArray2D (queryArray (userInput "a")))
        [ "a"; "*"; "b"; "."; "c"; "*"; "d"; ], tuple [ userInput "a"; userInput "b.c"; userInput "d" ]
        [ "struct"; "("; "a"; "*"; "b" ; ")" ], structTuple [ userInput "a"; userInput "b" ]
        [ "A"; "."; "a"; "<"; "B"; "."; "b"; ","; "C"; "."; "c"; ">" ], (generic (userInput "A.a") [ userInput "B.b"; userInput "C.c" ])
        [ "a "; "b "; "c" ], (generic (userInput "c") [ generic (userInput "b") [ userInput "a" ] ])
        [ "("; "a"; ","; "b"; ")"; "c" ], (generic (userInput "c") [ userInput "a"; userInput "b" ])
        [ "a"; "->"; "b"; "->"; "c" ], (arrow [ userInput "a"; userInput "b"; userInput "c" ])
      ]
      parameterize {
        source (buildSpaceTestSource cases)
        run BySignature.runSignatureTest
      }

    let byNameTest =
      let cases = [
        [ "name"; ":"; "a" ], ([ byName "name" Compare  ], SignatureQuery.Signature (userInput "a"))
        [ "map"; ":"; "_" ], ([ byName "map" Compare  ], SignatureQuery.Wildcard)
        [ "("; "+"; ")"; ": _" ], ([ byName "op_Addition" Compare ], SignatureQuery.Wildcard)
        [ "A"; "."; ".ctor";  ": _" ], ([ byName ".ctor" Compare; byName "A" Compare ], SignatureQuery.Wildcard)
      ]
      parameterize {
        source (buildSpaceTestSource cases)
        run (fun (input, (expectedName, expectedSig)) -> ByName.runByNameTest (input, expectedName, expectedSig))
      }

    let byActivePatternTest =
      let cases = [
        [ "(||)";  ":";  "...";  "->"; "'a";  "->"; "?" ], QueryMethod.ByActivePattern { Kind = ActivePatternKind.ActivePattern; Signature = ActivePatternSignature.AnyParameter (queryVariable "'a", wildcard) }
        [ "(||)";  ":"; "'a"; "->"; "?" ], QueryMethod.ByActivePattern { Kind = ActivePatternKind.ActivePattern; Signature = ActivePatternSignature.Specified (arrow [ queryVariable "'a"; wildcard ]) }
      ]
      parameterize {
        source (buildSpaceTestSource cases)
        run ByActivePattern.runByActivePatternTest
      }

    let byComputationExpressionTest =
      let cases = [
        [ "{"; "}"; ":"; "?" ], QueryMethod.ByComputationExpression { Syntaxes = []; Type = wildcard }
        [ "{"; "_"; "}"; ": ?" ], QueryMethod.ByComputationExpression { Syntaxes = []; Type = wildcard }
        [ "{"; "let!"; ";"; "return"; "}"; ":"; "'a" ], QueryMethod.ByComputationExpression { Syntaxes = [ syn "let!"; syn "return" ]; Type = queryVariable "'a" }
      ]
      parameterize {
        source (buildSpaceTestSource cases)
        run ByComputationExpression.runByComputationExpressionTest
      }

module CSharp =
  let runParseTest (input, expected) = test {
    let actual = QueryParser.CSharp.parse input
    let expected: Query = { OriginalString = input; Method = expected }
    do! actual |> assertEquals expected
  }
  module BySignature =
    let runSignatureTest (input, expected) =
      runParseTest (input, QueryMethod.BySignature (SignatureQuery.Signature expected))

    let parseTest = parameterize {
      source [
        "A", (userInput "A")
        "A_'2", (userInput "A_'2")
        "A.B", (userInput "A.B")
        "A.B.C", (userInput "A.B.C")

        "aBc", (userInput "aBc")
        "aBc<D>", (generic (userInput "aBc") [ userInput "D" ])
      ]

      run runSignatureTest
    }

    let wildcardTest = parameterize {
      source [
        "?", (wildcard)
        "?a", (wildcardGroup "a")
        "? -> ?", (arrow [ wildcard; wildcard ])
        "A<?, ?b>", (generic (userInput "A") [ wildcard; wildcardGroup "b" ])
      ]
      run runSignatureTest
    }

    let dotNetGenericTest = parameterize {
      source [
        "A<B, C>", (generic (userInput "A") [ userInput "B"; userInput "C" ])
        "A<B<C>, D>", (generic (userInput "A") [ generic (userInput "B") [ userInput "C" ]; userInput "D" ])
        "A -> B<C, D> -> D", (arrow [ userInput "A"; generic (userInput "B") [ userInput "C"; userInput "D" ]; userInput "D" ])
        "?<B, C>", (generic wildcard [ userInput "B"; userInput "C" ])
        "?a<B, C>", (generic (wildcardGroup "a") [ userInput "B"; userInput "C" ])
      ]
      run runSignatureTest
    }

    let arrowTest = parameterize {
      source [
        "A -> A", (arrow [ userInput "A"; userInput "A" ])
        "(A -> B) -> C", (arrow [ (arrow [ userInput "A"; userInput "B" ]); userInput "C" ])
        "(A -> B)", (arrow [ userInput "A"; userInput "B" ])
        "A, B -> C", (arrow [ tuple [ userInput "A"; userInput "B" ]; userInput "C" ])
        "(A, (B, C)) -> D", (arrow [ tuple [ userInput "A"; structTuple [ userInput "B"; userInput "C" ] ]; userInput "D" ])
        "((A, B)) -> C", (arrow [ structTuple [ userInput "A"; userInput "B" ]; userInput "C" ])
      ]
      run runSignatureTest
    }

    let tupleTest = parameterize {
      source [
        "(A, B)", (structTuple [ userInput "A"; userInput "B" ])
        "(A, (B, C), D)", (structTuple [ userInput "A"; structTuple [ userInput "B"; userInput "C" ]; userInput "D" ])
        "A<(B, C), D>", (generic (userInput "A") [ structTuple [ userInput "B"; userInput "C" ]; userInput "D" ])
        
      ]
      run runSignatureTest
    }

    let arrayTest = parameterize {
      source [
        "A[]", (queryArray (userInput "A" ))
        "A[][,]", (queryArray (queryArray2D (userInput "A")))
        "A<B>[]", (queryArray (generic (userInput "A") [ userInput "B" ]))
      ]
      run runSignatureTest
    }

    let variableTest = parameterize {
      source [
        "<A> : A -> B", (arrow [ queryVariable "'A"; userInput "B" ])
        "<A, B> : A -> B", (arrow [ queryVariable "'A"; queryVariable "'B" ])
        "<T> : #A<T>", (subtype (generic (userInput "A") [ queryVariable "'T" ]))
        "<T> : ref T", (byref (queryVariable "'T"))

        "v", (queryVariable "'v")
        "variable", (queryVariable "'variable")
        "A<var1, var2>", (generic (userInput "A") [ queryVariable "'var1"; queryVariable "'var2" ])
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
        "ref A -> B", (arrow [ byref (userInput "A"); userInput "B" ])
        "out A[] -> B", (arrow [ out (queryArray (userInput "A")); userInput "B" ])
        "string, out int -> bool", (arrow [ tuple [ userInput "string"; out (userInput "int") ]; userInput "bool" ])
      ]

      run runSignatureTest
    }

    let subtypeTest = parameterize {
      source [
        "#A", (subtype (userInput "A"))
        "#A<B>", (subtype (generic (userInput "A") [ userInput "B" ]))
        "A<#B>", (generic (userInput "A") [ subtype (userInput "B") ])
      ]
      run runSignatureTest
    }

  module ByName =
    let runByNameTest (input, expectedName, expectedSignature) = runParseTest (input, QueryMethod.ByName (expectedName, expectedSignature))

    let Compare = NameMatchMethod.StringCompare
    let Regex = NameMatchMethod.Regex
    let StartsWith = NameMatchMethod.StartsWith
    let EndsWith = NameMatchMethod.EndsWith
    let Contains = NameMatchMethod.Contains
    let Any = NameMatchMethod.Any

    let byName expected method = { Expected = expected; GenericParameters = []; MatchMethod = method }
    let byName2 expected generics method = { Expected = expected; GenericParameters = generics; MatchMethod = method }

    let byNameTest = parameterize {
      source [
        "a.b : _", [ byName "b" Compare; byName "a" Compare ], SignatureQuery.Wildcard
        "* : _", [ byName "*" Any ], SignatureQuery.Wildcard
        "a.* : B", [ byName "*" Any; byName "a" Compare ], SignatureQuery.Signature (userInput "B")
        "a* : _", [ byName "a" StartsWith ], SignatureQuery.Wildcard
      ]

      run runByNameTest
    }

    let byNameAndVariableTest = parameterize {
      source [
        "test<A, B, C> : B", [ byName2 "test" [ "A"; "B"; "C" ] Compare ], SignatureQuery.Signature (queryVariable "'B")
        "*<A, B, C> : B", [ byName2 "*" [ "A"; "B"; "C" ] Any ], SignatureQuery.Signature (queryVariable "'B")
        "class.method<B> : A, C -> B", [ byName2 "method" [ "B" ] Compare; byName2 "class" [] Compare ],
          SignatureQuery.Signature (arrow [ tuple [ userInput "A"; userInput "C" ]; queryVariable "'B" ])
        "class<A>.method<B> : A, C -> B", [ byName2 "method" [ "B" ] Compare; byName2 "class" [ "A" ] Compare ],
          SignatureQuery.Signature (arrow [ tuple [ queryVariable "'A"; userInput "C" ]; queryVariable "'B" ])
      ]

      run runByNameTest
    }