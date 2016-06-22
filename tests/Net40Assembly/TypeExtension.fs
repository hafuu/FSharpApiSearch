namespace Net40Assembly

open System.Runtime.CompilerServices

[<Extension>]
type TestExtensions =
  [<Extension>]
  static member ExtensionMethod(x: int) = x
