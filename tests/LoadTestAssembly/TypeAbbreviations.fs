module TypeAbbreviations

type Original<'a> = Original of 'a

type A = A

type GenericTypeAbbreviation<'b> = Original<'b>
type SpecializedTypeAbbreviation = Original<A>

type NestedTypeAbbreviation = SpecializedTypeAbbreviation

type FunctionAbbreviation = int -> int

let functionWithFunctionAbbreviation (x: FunctionAbbreviation): FunctionAbbreviation = x

module NestedModule =
  type TypeAbbreviationInModule<'a> = Original<'a>