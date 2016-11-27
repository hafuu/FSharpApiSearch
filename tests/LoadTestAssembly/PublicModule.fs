module PublicModule

let nonGenericFunction (x: int) (y: int) = 3

let genericFunction (x: 'a) (y : 'b) = y

let multiParamFunction (x: 'a, y: 'b, z: 'c) = x

let autoGenericFunction x = x
let autoGenericFunction2 (x: 'a) y z = x
let autoGenericFunction3 x (y: 'b) z = x
let autoGenericFunction4 x y (z: 'c) = x
let autoGenericFunction5 (x: 'a0) y z = x
let autoGenericFunction6 (x: 'a) (y: 'a0) z = x

let flexible (x: #seq<_>) = ()

let unitParamFunction () = 0

let listmap (f: 'a -> 'b) (xs: 'a list) = List.map f xs

let partialGenericMap (x: Map<int, 'a>) = x.Item(3)

let internal internalFunction x = x

let private privateFunction x y = y

let value = 3

let floatReturnType (x: int): float = 0.0

let array: int[] = Array.zeroCreate<int> 0
let array2d: int[,] = Array2D.zeroCreate<int> 0 0
let nestedArray: int[,][] = Array.zeroCreate<int[,]> 0

let (|ActivePattern|) x = if x = 1 then "one" else string x
let (|PartialActivePattern|_|) (y: 'a) (x: 'a) = if x = y then Some x else None

module NestedModule =
  let publicFunction (x: int) = 3