module OptionalParameters

open System.Runtime.InteropServices

type X() =
  member this.F(x: int, ?y: string) = x
  member this.G(x: string option, ?y: int option) = x
  member this.H([<Optional; DefaultParameterValue(null:string)>]x: string) = x