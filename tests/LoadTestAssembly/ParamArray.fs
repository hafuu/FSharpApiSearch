module ParamArray

type X() =
  member this.F([<System.ParamArray>]xs: int[]) = ()