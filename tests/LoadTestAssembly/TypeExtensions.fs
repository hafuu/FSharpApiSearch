module TypeExtensions

type System.Int32 with
  member this.Method(x: int) = ()
  member this.CurriedMethod (x: int) (y: string) = x
  member this.NoncurriedMethod (x: int, y: string) = y
  
  member this.GetterProperty with get() = 3
  member this.SetterProperty with set(value: string) = ()
  member this.GetterSetterProperty with get() = "" and set (value: string) = ()

  member this.GetterIndexedProperty with get(_: int) = ""
  member this.SetterIndexedProperty with set (_: int) (_: string) = ()
  member this.GetterSetterIndexedProperty with get (_: string) = 0 and set (_: string) (_: int) = ()

type List<'a> with
  static member Method(_: 'a) = ()
  static member CurriedMethod (x: int) (y: 'b) = y
  static member NoncurriedMethod (x: int, y: 'b) = x

  static member GetterProperty with get() = 3
  static member SetterProperty with set(value: string) = ()
  static member GetterSetterProperty with get() = "" and set (value: string) = ()

  static member GetterIndexedProperty with get(_: int) = ""
  static member SetterIndexedProperty with set (_: int) (_: string) = ()
  static member GetterSetterIndexedProperty with get (_: string) = 0 and set (_: string) (_: int) = ()

open System.Runtime.CompilerServices

[<Extension>]
type TestExtensions private () =
  [<Extension>]
  static member ExtensionMethod(x: int) = x
  [<Extension>]
  static member ExtensionMethod2(x: int, y: int, z: string) = ()