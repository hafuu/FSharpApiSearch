module internal FSharpApiSearch.TypeNameEquality

open System
open System.Collections.Generic

[<RequireQualifiedAccess>]
type Result =
  | Matched
  | DifferentGenericParameter of int * int
  | DifferentName
  | DifferentAssemblyName

type TestBuilder() =
  member inline __.Bind(x, f) =
    match x with
    | Result.Matched -> f()
    | failure -> failure

  member inline __.Zero() = Result.Matched

let private test = TestBuilder()

let testee (x: NameItem) =
  match x.Name with
  | SymbolName n -> n
  | OperatorName (_, n) -> n
  | WithCompiledName (n, _) -> n

let private forall2 (f: 'a * 'a -> Result) (xs: 'a list) (ys: 'a list) : Result =
  let rec loop xs ys =
    match xs, ys with
    | x :: xs, y :: ys ->
      match f (x, y) with
      | Result.Matched -> loop xs ys
      | failure -> failure
    | _ -> Result.Matched
  loop xs ys

let testGenericParameterCount (x: int) (y: int) =
  if x = y then
    Result.Matched
  else
    Result.DifferentGenericParameter (x, y)

let testString cmp x y =
  if String.equalsWithComparer cmp x y then
    Result.Matched
  else
    Result.DifferentName

let testName cmp (xs: Name) (ys: Name) =
  let f (x, y) = test {
    do! testString cmp (testee x) (testee y)
    do! testGenericParameterCount x.GenericParameters.Length y.GenericParameters.Length
  }
  forall2 f xs ys

let testAssemblyName (x: ActualType) (y: ActualType) =
  if x.AssemblyName = y.AssemblyName then
    Result.Matched
  else
    Result.DifferentAssemblyName

let testActualType (x: ActualType) (y: ActualType) = test {
  do! testAssemblyName x y
  do! testName StringComparer.InvariantCulture x.Name y.Name
}

let actualTypeComparer =
  { new IEqualityComparer<ActualType> with
      member this.Equals(x, y) = testActualType x y = Result.Matched
      member this.GetHashCode(x) =
        let mutable value = x.AssemblyName.GetHashCode()
        for item in x.Name do
          value <- value ^^^ item.Name.GetHashCode()
          value <- value ^^^ item.GenericParameters.Length.GetHashCode()
        value
  }

let testUserInputAndActualType cmp (userInput: UserInputType) (actual: ActualType) =
  let testNameItem (p: NameItem, f: NameItem) = test {
    match p.GenericParameters, f.GenericParameters with
    | [], _ ->
      do! testString cmp (testee p) (testee f)
    | _ ->
      do! testString cmp (testee p) (testee f)
      do! testGenericParameterCount p.GenericParameters.Length f.GenericParameters.Length
  }
  test {
    do! testGenericParameterCount userInput.GenericParameterCount actual.GenericParameterCount
    do! forall2 testNameItem userInput.Name actual.Name
  }

let private sameName' cmp x y =
  match x, y with
  | ActualType left, ActualType right -> testActualType left right
  | ActualType actual, UserInputType userInput
  | UserInputType userInput, ActualType actual -> testUserInputAndActualType cmp userInput actual
  | UserInputType left, UserInputType right ->
    test {
      do! testGenericParameterCount left.GenericParameterCount right.GenericParameterCount
      do! testName cmp left.Name right.Name
    }

type Equality = TypeInfo -> TypeInfo -> Result

let sameName x y = sameName' StringComparer.InvariantCulture x y
let sameNameIgnoreCase x y = sameName' StringComparer.InvariantCultureIgnoreCase x y

let equalityFromOptions opt : Equality =
  match opt.IgnoreCase with
  | Enabled -> sameNameIgnoreCase
  | Disabled -> sameName