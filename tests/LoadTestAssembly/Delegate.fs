module Delegate

open System

type TestDelegate = delegate of int * int -> bool
type GenericDelegate<'a, 'b> = delegate of 'a * 'b -> bool

let f1 = TestDelegate(fun _ _ -> true)
let f2 (x: 'a) (f: TestDelegate) = 0

let f3 (f: GenericDelegate<'a, 'b>) = 0
let f4 (f: GenericDelegate<int, string>) = 0

let f5 (f: Func<int, 'a>) = true