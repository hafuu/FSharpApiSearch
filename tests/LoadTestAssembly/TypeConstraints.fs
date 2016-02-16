module TypeConstraints

let subtypeConFunction<'Tseq when 'Tseq :> seq<int>> (x: 'Tseq) = ()

type SubTypeClass<'a when 'a :> seq<int>>() =
  static member Method<'b when 'b :> seq<string>>(x: 'a, y: 'b) = ()

type SubTypeRecord<'a when 'a :> seq<int>> = {
  Field: 'a
}

let nullnessFunction<'a when 'a : null> (x: 'a) = ()

let inline memberConstraint_instanceMethod1<'a when 'a : (member Method : int -> int -> int)> (x: 'a) = ()
let inline memberConstraint_instanceMethod2<'a when 'a : (member Method : int * int -> int)> (x: 'a) = ()
let inline memberConstraint_tupleMethod<'a when 'a : (member Method : (int * int) -> int)> (x: 'a) = ()
let inline memberConstraint_staticMember<'a when 'a : (static member Method : int -> int)> (x: 'a) = ()
let inline memberConstraint_or<'a, 'b when ('a or 'b) : (static member Method : int -> int)> (x: 'a) (y: 'b) = ()
let inline memberConstraint_noArgumentMember<'a when 'a : (member Method: int)> (x: 'a) = ()
let inline memberConstraint_unitMethod<'a when 'a : (member Method: unit -> int)> (x: 'a) = ()
let inline memberConstraint_unitIntMethod<'a when 'a : (member Method: unit -> int -> int)> (x: 'a) = ()
let inline memberConstraint_getterMethod<'a when 'a : (member get_Property : unit -> int)> (x: 'a) = ()
let inline memberConstraint_setterMethod<'a when 'a : (member set_Property : int -> unit)> (x: 'a) = ()
let inline memberConstraint_getProperty<'a when 'a : (member Property : int with get)> (x: 'a) = ()
let inline memberConstraint_setProperty<'a when 'a : (member Property : int with set)> (x: 'a) = ()
let inline memberConstraint_indexedGetProperty<'a when 'a : (member Property : int -> int with get)> (x: 'a) = ()
let inline memberConstraint_indexedSetProperty<'a when 'a : (member Property : int -> int with set)> (x: 'a) = ()

let inline memberConstraint_staticNoArgumentMember<'a when 'a : (static member Method: int)> (x: 'a) = ()
let inline memberConstraint_staticUnitMethod<'a when 'a : (static member Method: unit -> int)> (x: 'a) = ()
let inline memberConstraint_staticGetterMethod<'a when 'a : (static member get_Property: unit -> int)> (x: 'a) = ()
let inline memberConstraint_staticSetterMethod<'a when 'a : (static member set_Property: int -> unit)> (x: 'a) = ()
let inline memberConstraint_staticGetProperty<'a when 'a : (static member Property: int with get)> (x: 'a) = ()
let inline memberConstraint_staticSetProperty<'a when 'a : (static member Property: int with set)> (x: 'a) = ()

let inline memberConstraint_generic<'a, 'b when 'a : (member Method: 'b -> unit)> (x: 'a) = ()

let inline memberConstraint_operator<'a, 'b, 'c when ('a or 'b) : (static member (+): 'a -> 'b -> 'c)> (x: 'a) (y: 'b) = ()

let valueTypeConstraint<'a when 'a : struct> (x: 'a) = ()
let referenceTypeConstraint<'a when 'a : not struct> (x: 'a) = ()

let defaultConstructorConstraint<'a when 'a : (new : unit -> 'a)> (x: 'a) = ()

let equalityConstraint<'a when 'a : equality> (x: 'a) = ()
let comparisonConstraint<'a when 'a : comparison> (x: 'a) = ()