namespace TopLevelNamespace

type StaticMemberClass() =
  new (x: int) = StaticMemberClass()

  static member StaticMethod1 () = 3
  static member StaticMethod2 (x: int, y: string) = x
  static member FloatReturnType (x: single, y: float): float = 0.0
  static member SingleReturnType (x: int) = single 0.0
  static member InferredFloatType (x) = 0.0 + x

  static member OverloadMethod (x: int) = 0
  static member OverloadMethod (x: string, y: int) = ""

  static member val Property = "" with get, set
  static member IndexedProperty with get(name: string) = 0 and set (name: string) (value: int) = ()

type InstanceMemberClass() =
  member this.InstanceMethod1() = 0
  member this.InstanceMethod2(x: int) = ""
  member this.InstanceMethod3(x: string, y: float): float = 0.0

  member this.OverloadMethod (x: int) = 0
  member this.OverloadMethod (x: string) = 0

  member val Property = "" with get, set
  member this.IndexedProperty with get(name: string) = 0 and set (name: string) (value: int) = ()

type GenericClass<'a>() =
  member this.Method(x: 'a) = 3