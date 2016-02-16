namespace TopLevelNamespace

type StaticMemberClass() =
  new (x: int) = StaticMemberClass()

  static member NoArgumentMethod () = 3
  static member OneArgumentMethod (x: int) = 3
  static member NonCurriedMethod (x: int, y: string) = x
  static member CurriedMethod (x: int) (y: string) = x
  static member TupleMethod (x: int * string) = fst x

  static member InferredFloat (x) = 0.0 + x
  static member AnnotatedFloat (x: float): float = 0.0

  static member OverloadMethod (x: int) = 0
  static member OverloadMethod (x: string, y: int) = ""

  static member Getter with get() = ""
  static member Setter with set(_: int) = ()
  static member GetterSetter with get() = 0.0 and set(_: float) = ()

  static member IndexedGetter with get(_: int) = ""
  static member IndexedSetter with set(_: int) (_: string) = ()
  static member IndexedGetterSetter with get(_: string) = 0 and set(_: string) (_: int) = ()

  static member private PrivateMethod() = 0
  static member internal InternalMethod() = 0

type InstanceMemberClass() =
  member this.NoArgumentMethod () = 3
  member this.OneArgumentMethod (x: int) = 3
  member this.NonCurriedMethod (x: int, y: string) = x
  member this.CurriedMethod (x: int) (y: string) = x
  member this.TupleMethod (x: int * string) = fst x

  member this.OverloadMethod (x: int) = 0
  member this.OverloadMethod (x: string, y: int) = ""

  member this.Getter with get() = ""
  member this.Setter with set(_: int) = ()
  member this.GetterSetter with get() = 0.0 and set(_: float) = ()

  member this.IndexedGetter with get(_: int) = ""
  member this.IndexedSetter with set(_: int) (_: string) = ()
  member this.IndexedGetterSetter with get(_: string) = 0 and set(_: string) (_: int) = ()

type private PrivateClass() =
  static member PublicMethod() = 0

type internal InternalClass() =
  static member PublicMethod() = 0

type GenericClass<'a>() =
  member this.Method(x: 'a) = 3

type Interface = interface
  abstract member Method: int * string -> int
  abstract member Property: string with get, set
end