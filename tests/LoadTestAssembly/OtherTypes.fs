namespace OtherTypes

type Record = {
  FieldA: int
  FieldB: string
}
with
  member this.InstanceMethod() = 0
  member this.InstanceProperty with get() = 0 and set(_: int) = ()
  static member StaticMethod() = ""

type GenericRecord<'a> = {
  Field: 'a
}

type Union = A | B of int * string
with
  member this.InstanceMethod() = 0

type Enum = A = 0 | B = 1

type Struct = struct
  val A: int
  val B: string

  member this.InstanceMethod() = 0
end