module TypesTest

open Persimmon
open Persimmon.Syntax.UseTestNameByReflection
open Persimmon.MuscleAssert
open FSharpApiSearch

open TestHelper.DSL
open TestHelper.Types

let parseDisplayNameTest = parameterize {
  source [
    "A.B", [ { Name = SymbolName "B"; GenericParameters = [] }; { Name = SymbolName "A"; GenericParameters = [] } ]
    "A.B<'T>", [ { Name = SymbolName "B"; GenericParameters = [ tv "'T" ] }; { Name = SymbolName "A"; GenericParameters = [] } ]
  ]
  run (fun (x, expected) -> test {
    do! Name.ofString x |> assertEquals expected
  })
}

let parseOperatorDisplayNameTest = parameterize {
  source [
    "A.( + )", [ { Name = OperatorName ("( + )", "op_Addition"); GenericParameters = [] }; { Name = SymbolName "A"; GenericParameters = [] } ]
  ]
  run (fun (x, expected) -> test {
    do! Name.ofOperatorString x |> assertEquals expected
  })
}