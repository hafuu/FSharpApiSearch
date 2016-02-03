namespace OtherTypes

type Record = {
  FieldA: int
  FieldB: string
}
with
  member this.InstanceMethod1() = 0
  member this.InstanceMethod2(x: int) = ""
  member this.InstanceProperty with get() = 0 and set(_: int) = ()
  member this.OverloadMethod() = ""
  member this.OverloadMethod(x: int) = 0.0
  static member StaticMethod1() = ""
  static member StaticMethod2(x: int, y: string) = 0.0
  static member StaticProperty with get() = "" and set(_: string) = ()

type Union = A | B of int * string
with
  member this.InstanceMethod() = 0

type Enum = A = 0 | B = 1

type Struct = struct
  val A: int
  val B: string

  member this.InstanceMethod() = 0
end