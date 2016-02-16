module FullTypeDefinition

(* Subtype Constraints *)
type PlainClass() = class end

type PlainInterface = interface end

type InterfaceImplClass() =
  interface PlainInterface

type InterfaceInherit =
  inherit PlainInterface

(* Nullness Constraints  *)

[<AllowNullLiteral>]
type SupportNullClass() = class end

type SupportNullSubClass() = inherit SupportNullClass()

[<AllowNullLiteral>]
type SupportNullInterface = interface end

type NonSupportNullSubInterface = inherit SupportNullInterface

[<AllowNullLiteral>]
type SupportNullSubInterface = inherit SupportNullInterface

(*
let f<'a when 'a : null> (x: 'a) = ()
do f (SupportNullClass()) // OK
do f (SupportNullSubClass()) // NG
do f ({ new SupportNullInterface }) // OK
do f ({ new NonSupportNullSubInterface }) // OK
do f ({ new SupportNullSubInterface }) // OK
*)

type MemberClass() =
  static member StaticMethod () = 0
  static member (+) (x: MemberClass, y: int) = x
  member this.InstanceMethod (_: int) = 0
  member this.Property = 0

type WithoutDefaultConstructor(x: int) = class end

module EqualityConstraints =
  type EqualityType() = class end

  [<NoEquality; NoComparison>]
  type NoEqualityType() = class end

  type InferredEqualityRecord = {
    X: int
    Y: string
  }

  type InferredNoEqualityRecord = {
    X: int
    Y: int -> int
  }

  type InferredEqualityUnion =
    | A of int
    | B of string

  type InferredNoEqualityUnion =
    | A of int
    | B of NoEqualityType

  [<CustomEquality; NoComparison>]
  type CustomEqualityRecord = {
    X: int -> int
  }
  with
    override this.Equals(_) = true
    override this.GetHashCode() = 0

  type GenericClass<'a, 'b>() = class end
  type EqualityConditionalClass<[<EqualityConditionalOn>]'a, 'b>() = class end

  [<CustomEquality; NoComparison>]
  type CustomEqualityAndConditionalRecord<[<EqualityConditionalOn>]'a, 'b> = {
    X: 'a
    Y: 'b
  }
  with
    override this.Equals(_) = true
    override this.GetHashCode() = 0

  type EqualityGenericRecord<'a,'b> = {
    X: 'a
    Y: 'b
    Z: int
  }

  type NoEqualityGenericRecord<'a, 'b> = {
    X: 'a
    Y: 'b
    Z: int -> int
  }

  type EqualityWithGenericType = {
    X: list<int>
  }

  type NoEqualityWithGenericType = {
    X: list<int -> int>
  }

  type RecursiveType<'a> =
    | A of 'a * RecursiveType<'a>
    | B

module ComparisonConstraints =
  open System
  open System.Collections

  type ComparisonType() =
    override this.Equals(_) = true
    override this.GetHashCode() = 0
    interface IComparable with
      member this.CompareTo(_) = 0

  type NotComparisonType() =
    interface IComparable<NotComparisonType> with
      member this.CompareTo(_) = 0

  type StructualComparisonType() =
    interface IStructuralComparable with
      member this.CompareTo(_, _) = 0

  type InferredComparisonRecord = {
    X: int
    Y: string
  }

  type InferredNoComparisonRecord = {
    X: int
    Y: int -> int
  }

  [<NoEquality; NoComparison>]
  type NoComparisonRecord = {
    X: int
    Y: string
  }

  type InferredComparisonUnion =
    | A of int
    | B of string

  type InferredNoComparisonUnion =
    | A of int
    | B of NotComparisonType

  [<CustomEquality; CustomComparison>]
  type CustomComparisonRecord = {
    X: int -> int
  }
  with
    override this.Equals(_) = true
    override this.GetHashCode() = 0
    interface IComparable with
      member this.CompareTo(_) = 0

  type GenericNoComparisonClass<'a, 'b>() = class end

  type GenericComparisonClass<'a, 'b>() =
    override this.Equals(_) = true
    override this.GetHashCode() = 0
    interface IComparable with
      member this.CompareTo(_) = 0
      
  type ComparisonConditionalClass<[<ComparisonConditionalOn>]'a, 'b>() =
    override this.Equals(_) = true
    override this.GetHashCode() = 0
    interface IComparable with
      member this.CompareTo(_) = 0

  [<CustomEquality; CustomComparison>]
  type CustomComparisonAndConditionalRecord<[<ComparisonConditionalOn>]'a, 'b> = {
    X: 'a
    Y: 'b
  }
  with
    override this.Equals(_) = true
    override this.GetHashCode() = 0
    interface IComparable with
      member this.CompareTo(_) = 0

  type ComparisonGenericRecord<'a, 'b> = {
    X: 'a
    Y: 'b
    Z: int
  }

  type NoComparisonGenericRecord<'a, 'b> = {
    X: 'a
    Y: 'b
    Z: int -> int
  }

  type ComparisonWithGenericType = {
    X: list<int>
  }

  type NoComparisonWithGenericType = {
    X: list<int -> int>
  }

  type RecursiveType<'a> =
    | A of 'a * RecursiveType<'a>
    | B

  let test<'a when 'a : comparison>(_: 'a) = ()