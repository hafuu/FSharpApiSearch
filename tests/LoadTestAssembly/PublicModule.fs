module PublicModule

let nonGenericFunction (x: int) (y: int) = 3

let genericFunction (x: 'a) (y : 'b) = y

let tupleFunction (x: 'a, y: 'b, z: 'c) = x

let listmap (f: 'a -> 'b) (xs: 'a list) = List.map f xs

let partialGenericMap (x: Map<int, 'a>) = x.Item(3)

let internal internalFunction x = x

let private privateFunction x y = y

let value = 3

module NestedModule =
  let publicFunction (x: int) = 3