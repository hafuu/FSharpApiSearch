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
  
[<AutoOpen>]
module internal Extensions =
  open System.Text
  type StringBuilder with
    member this.Append(print: StringBuilder -> StringBuilder) = print this
    member this.AppendJoin(sep: string, xs: 'a seq, print: 'a -> StringBuilder -> StringBuilder) : StringBuilder =
      if Seq.isEmpty xs then
        this
      else
        let first = ref true

        for x in xs do
          if !first then
            this.Append(print x) |> ignore
            first := false
          else
            this.Append(sep).Append(print x) |> ignore
        this

    member this.AppendJoin(sep: string, xs: string seq) = this.AppendJoin(sep, xs, (fun x sb -> sb.Append(x)))

module internal IDictionary =
  open System.Collections.Generic
  let empty<'k, 'v when 'k : equality> = dict (Seq.empty<'k * 'v>)

module internal Map =
  let ofList2 (keys: 'k list) (values: 'v list) =
    let rec loop keys values map =
      match keys, values with
      | key :: keys, value :: values -> Map.add key value map |> loop keys values
      | [], [] -> map
      | _ -> failwith "invalid length"
    loop keys values Map.empty