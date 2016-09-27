namespace FSharpApiSearch

type Lens<'a, 'b> = {
  Get: 'a -> 'b
  Set: 'b -> 'a -> 'a
}

[<AutoOpen>]
module internal OptionModule =
  type OptionBuilder() =
    member inline this.Bind(x, f) = Option.bind f x
    member inline this.Return(x) = Some x
    member inline this.ReturnFrom(x) = x

  let option = OptionBuilder()

module internal Seq =
  let foldOptionMapping f xs =
    xs
    |> Seq.fold (fun acc x ->
      option {
        let! acc = acc
        let! result = f x
        return result :: acc
      }
    ) (Some [])
    |> Option.map Seq.rev

module internal String =
  open System
  let equalsWithComparer (cmp: StringComparer) x y = cmp.Compare(x, y) = 0
  let equals (x: string) (y: string) = equalsWithComparer StringComparer.InvariantCulture x y
  let equalsIgnoreCase x y = equalsWithComparer StringComparer.InvariantCultureIgnoreCase x y
  