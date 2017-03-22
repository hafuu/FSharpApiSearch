module FSharp41

let tuple = (1, "a")
let structTuple = struct (1, "a")

[<Struct>]
type StructRecord = {
  FieldA: int
  FieldB: string
}

[<Struct>]
type StructUnion = A of a:string | B of b:int * c:string