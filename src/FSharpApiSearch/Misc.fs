namespace FSharpApiSearch

[<AutoOpen>]
module internal OptionModule =
  type OptionBuilder() =
    member inline this.Bind(x, f) = Option.bind f x
    member inline this.Return(x) = Some x
    member inline this.ReturnFrom(x) = x

  let option = OptionBuilder()