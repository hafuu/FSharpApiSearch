namespace FSharpApiSearch

[<AutoOpen>]
module internal OptionModule =
  type OptionBuilder() =
    member inline this.Bind(x, f) = Option.bind f x
    member inline this.Return(x) = Some x
    member inline this.ReturnFrom(x) = x

  let option = OptionBuilder()

module internal String =
  open System
  let equalsWithComparer (cmp: StringComparer) x y = cmp.Compare(x, y) = 0
  let equals (x: string) (y: string) = equalsWithComparer StringComparer.InvariantCulture x y
  let equalsIgnoreCase x y = equalsWithComparer StringComparer.InvariantCultureIgnoreCase x y
  