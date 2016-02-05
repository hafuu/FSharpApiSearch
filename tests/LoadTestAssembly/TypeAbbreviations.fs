module TypeAbbreviations

type Original<'a> = Original of 'a

type A = A

type GenericTypeAbbreviation<'b> = Original<'b>
type SpecializedTypeAbbreviation = Original<A>

type NestedTypeAbbreviation = SpecializedTypeAbbreviation

type FunctionAbbreviation = int -> int

module NestedModule =
  type TypeAbbreviationInModule<'a> = Original<'a>