module internal FSharpApiSearch.TypeNameEquality

open System
open System.Collections.Generic
open EngineTypes

type FailureReason =
  | DifferentGenericParameter of leftLength:int * rightLength:int
  | DifferentName
  | DifferentAssemblyName

type Result = Result<Distance, FailureReason>

type TestBuilder() =
  member inline __.Bind(x, f) =
    match x with
    | Ok d1 ->
      match f() with
      | Ok d2 -> Ok (d1 + d2)
      | failure -> failure
    | failure -> failure

  member inline __.Zero() = Ok 0

let private test = TestBuilder()

let testee (x: NameItem) =
  match x.Name with
  | SymbolName n -> n
  | OperatorName (_, n) -> n
  | WithCompiledName (n, _) -> n

let private forall2 (f: 'a -> 'a -> Result) (xs: 'a list) (ys: 'a list) : Result =
  let distance = ref 0
  let rec loop xs ys =
    match xs, ys with
    | x :: xs, y :: ys ->
      match f x y with
      | Ok d ->
        distance := !distance + d
        loop xs ys
      | failure -> failure
    | _ -> Ok !distance
  loop xs ys

let testGenericParameterCount (x: int) (y: int) =
  if x = y then
    Ok 0
  else
    Error (DifferentGenericParameter (x, y))

let testStringExact (cmp: StringComparison) (x: string) (y: string) : Result =
  if x.Equals(y, cmp) then
    Ok 0
  else
    Error DifferentName

let testName cmp (xs: Name) (ys: Name) =
  let f x y = test {
    do! testStringExact cmp (testee x) (testee y)
    do! testGenericParameterCount x.GenericParameters.Length y.GenericParameters.Length
  }
  forall2 f xs ys

let testAssemblyName (x: ConcreteType) (y: ConcreteType) =
  if x.AssemblyName = y.AssemblyName then
    Ok 0
  else
    Error DifferentAssemblyName

let testConcreteType (x: ConcreteType) (y: ConcreteType) = test {
  do! testAssemblyName x y
  do! testName StringComparison.InvariantCulture x.Name y.Name
}

let concreteTypeComparer =
  { new IEqualityComparer<ConcreteType> with
      member this.Equals(x, y) = testConcreteType x y = Ok 0
      member this.GetHashCode(x) =
        let mutable value = x.AssemblyName.GetHashCode()
        for item in x.Name do
          value <- value ^^^ item.Name.GetHashCode()
          value <- value ^^^ item.GenericParameters.Length.GetHashCode()
        value
  }

let testStringSubstring (cmp: StringComparison) (userInput: string) (actual: string) =
  let index = actual.IndexOf(userInput, cmp)
  if index < 0 then
    Error DifferentName
  else
    Ok (actual.Length - userInput.Length)

let testUserInputAndConcreteType (cmp: StringComparison) (testStr: StringComparison -> string -> string -> Result) (userInput: UserInputType) (actual: ConcreteType) =
  let testNameItem (p: NameItem) (f: NameItem) = test {
    match p.GenericParameters, f.GenericParameters with
    | [], _ ->
      do! testStr cmp (testee p) (testee f)
    | _ ->
      do! testStr cmp (testee p) (testee f)
      do! testGenericParameterCount p.GenericParameters.Length f.GenericParameters.Length
  }
  test {
    do! testGenericParameterCount userInput.GenericParameterCount actual.GenericParameterCount
    
    do! forall2 testNameItem userInput.Name actual.Name
  }

let private sameName' cmp testStr x y =
  match x, y with
  | ConcreteType left, ConcreteType right -> testConcreteType left right
  | ConcreteType concrete, UserInputType userInput
  | UserInputType userInput, ConcreteType concrete -> testUserInputAndConcreteType cmp testStr userInput concrete
  | UserInputType left, UserInputType right ->
    test {
      do! testGenericParameterCount left.GenericParameterCount right.GenericParameterCount
      do! testName cmp left.Name right.Name
    }

type Equality = Identifier -> Identifier -> Result

let sameName x y = sameName' StringComparison.InvariantCulture testStringExact x y
let sameNameIgnoreCase x y = sameName' StringComparison.InvariantCultureIgnoreCase testStringExact x y

let equalityFromOptions opt : Equality =
  let comparison =
    match opt.IgnoreCase with
    | Enabled -> StringComparison.InvariantCultureIgnoreCase
    | Disabled -> StringComparison.InvariantCulture
  let testStr =
    match opt.Substring with
    | Enabled -> testStringSubstring
    | Disabled -> testStringExact
  sameName' comparison testStr