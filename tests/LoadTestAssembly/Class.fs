namespace TopLevelNamespace

type StaticMemberClass() =
  new (x: int) = StaticMemberClass()

  static member StaticMethod1 () = 3
  static member StaticMethod2 (x: int, y: string) = x
  static member FloatReturnType (x: single, y: float): float = 0.0
  static member SingleReturnType (x: int) = single 0.0
  static member InferencedFloatType (x) = 0.0 + x

  static member OverloadMethod (x: int) = 0
  static member OverloadMethod (x: string, y: int) = ""

  static member val Property = 0 with get, set

type InstanceMemberClass() =
  member this.InstanceMethod1() = 3
  member this.InstanceMethod2(x: int) = ""
  member this.InstanceMethod3(x: string, y: float): float = 0.0

  member this.OverloadMethod (x: int) = 0
  member this.OverloadMethod (x: string) = 0