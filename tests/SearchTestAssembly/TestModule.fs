module TestModule

let f (x: int) (y: int) = 0
let g (x: string) (y: string) = ""
let h (x: int) (y: int) = ""

type TestClass() =
  static member f (x: int, y: int) = 0
  static member g (x: string, y: string) = ""
  static member h (x: int, y: int) = ""