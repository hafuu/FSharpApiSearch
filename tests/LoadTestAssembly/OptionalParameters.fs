module OptionalParameters

type X() =
  member this.F(x: int, ?y: string) = x
  member this.G(x: string option, ?y: int option) = x