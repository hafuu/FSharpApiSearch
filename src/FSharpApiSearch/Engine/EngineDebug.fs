namespace FSharpApiSearch

open System.Diagnostics

module internal EngineDebug =
  [<Literal>]
  let Configuration = "EngineDebug"

type internal EngineDebug private () =
  [<Conditional(EngineDebug.Configuration)>]
  static member Indent() = Debug.Indent()

  [<Conditional(EngineDebug.Configuration)>]
  static member Unindent() = Debug.Unindent()

  [<Conditional(EngineDebug.Configuration)>]
  static member WriteLine(message: string) = Debug.WriteLine(message)