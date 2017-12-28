module FSharpApiSearch.HtmlPrintHelper

open System.Text
open System.Collections.Generic
open System

type ColorId = int

type StructuredResult = {
  Text: (string * ColorId option)[]
  AllText: string
}

type private Context = {
  BeginIndex: int
  EndIndex: int
  ColorId: ColorId option
}

module private Extensions =
  type System.Collections.Generic.List<'t> with
    member inline this.Last = this.[this.Count - 1]
    member inline this.IsEmpty = this.Count = 0

  type System.Collections.Generic.Stack<'t> with
    member inline this.TryPeek =
      if this.Count = 0 then
        None
      else
        Some (this.Peek())
    member inline this.IsEmpty = this.Count = 0

open Extensions

type StructuredWriter() =
  let sb = StringBuilder()
  let results = ResizeArray<Context>()
  let stack = Stack<ColorId>()

  let add beginIndex endIndex color =
    if beginIndex > endIndex then
      ()
    else
      results.Add({ BeginIndex = beginIndex; EndIndex = endIndex; ColorId = color })

  member private this.Push() =
    let beginIndex =
      if results.IsEmpty then
        0
      else
        results.Last.EndIndex + 1
    let endIndex = sb.Length - 1
    let color = stack.TryPeek

    add beginIndex endIndex color

  member this.BeginColor(color: ColorId) =
    this.Push()
    stack.Push(color)
    
  member this.EndColor(color: ColorId) =
    this.Push()
    let popped = stack.Pop()
    assert (color = popped)
    ()

  member this.Result : StructuredResult =
    assert (stack.IsEmpty)
    this.Push()
    {
      Text = results.ToArray() |> Array.map (fun c -> sb.ToString(c.BeginIndex, c.EndIndex - c.BeginIndex + 1), c.ColorId)
      AllText = sb.ToString()
    }

  member this.Write(str: string) = sb.Append(str) |> ignore

  interface Writer with
    member this.Write(str: string) = this.Write(str)
    member this.WriteLine(str: string) = sb.AppendLine(str) |> ignore
    member this.WriteLine() = sb.AppendLine() |> ignore

type Handler() =
  interface SignaturePrinterHandler<StructuredWriter> with
    member this.BeginPrintType(writer, _, queryId) = queryId |> Option.iter (fun q -> writer.BeginColor(q.Id))
    member this.EndPrintType(writer, _, queryId) = queryId |> Option.iter (fun q -> writer.EndColor(q.Id))

  interface QueryPrinterHandler<StructuredWriter> with
    member this.BeginPrintType(writer, queryId) = writer.BeginColor(queryId.Id)
    member this.EndPrintType(writer, queryId) = writer.EndColor(queryId.Id)

let signature (result: Result) (print: SignaturePrinter<_> -> SignaturePrinter<_>) =
  let writer = StructuredWriter()
  let printer = SignaturePrinter<_>(writer, Handler(), result)
  print printer |> ignore
  writer.Result

let query (print: QueryPrinter<_> -> QueryPrinter<_>) =
  let writer = StructuredWriter()
  let printer = QueryPrinter<_>(writer, Handler())
  print printer |> ignore
  writer.Result