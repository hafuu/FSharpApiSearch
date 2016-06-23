module Operators

let (+) x y = x + y

type A() =
  static member (-) (x: A, y: A) = x