namespace FSharpApiSearch

open System
open System.IO

type Writer =
  abstract Write : string -> unit
  abstract WriteLine : string -> unit
  abstract WriteLine : unit -> unit

module Writer =
  let wrap (w: #System.IO.TextWriter) =
    { new Writer with
        member this.Write(str: string) = w.Write(str)
        member this.WriteLine(str: string) = w.WriteLine(str)
        member this.WriteLine() = w.WriteLine()
    }

type SignaturePrinterHandler<'Writer> =
  abstract BeginPrintType : 'Writer * LowType * QueryId option -> unit
  abstract EndPrintType : 'Writer * LowType * QueryId option -> unit  

module SignaturePrinterHandler =
  let empty<'w> =
    { new SignaturePrinterHandler<'w> with
        member this.BeginPrintType(_, _, _) = ()
        member this.EndPrintType(_, _, _) = ()
    }

type SignaturePrinter<'Writer when 'Writer :> Writer> internal (writer: 'Writer, handler: SignaturePrinterHandler<'Writer>, positions: Map<SignatureId, QueryId>) =
  new (writer, handler, result: Result) = new SignaturePrinter<_>(writer, handler, result.MatchPositions)
  new (writer, handler) = new SignaturePrinter<_>(writer, handler, Map.empty)

  member this.Write(str: string) : unit = writer.Write(str)
  member this.Write(print: SignaturePrinter<_> -> SignaturePrinter<_>) : unit = print this |> ignore
  member this.WriteLine() : unit = writer.WriteLine()
  member this.WriteLine(str: string) : unit = writer.WriteLine(str)
  member this.WriteLine(print: SignaturePrinter<_> -> SignaturePrinter<_>) : unit = print this |> ignore; writer.WriteLine()

  member this.Append(str: string) : SignaturePrinter<_> = writer.Write(str); this
  member this.Append(print: SignaturePrinter<_> -> SignaturePrinter<_>) : SignaturePrinter<_> = print this
  member this.AppendJoin(sep: string, xs: seq<_>, print: _ -> SignaturePrinter<_> -> SignaturePrinter<_>) : SignaturePrinter<_> =
    let mutable first = true
    for x in xs do
      if not first then
        this.Append(sep) |> ignore
      else
        first <- false
      this.Append(print x) |> ignore
    this
  member this.AppendJoin(sep: string, xs: seq<_>) : SignaturePrinter<_> = this.AppendJoin(sep, xs, (fun x p -> p.Append(x)))
  member this.BeginPrintType(t: LowType) : SignaturePrinter<_> =
    match t.Position with
    | AtQuery (Some pos, _) -> handler.BeginPrintType(writer, t, Some pos)
    | AtSignature pos -> handler.BeginPrintType(writer, t, positions.TryFind(pos))
    | AtQuery (None, _) | Unknown -> handler.BeginPrintType(writer, t, None)
    this
  member this.EndPrintType(t: LowType) : SignaturePrinter<_> =
    match t.Position with
    | AtQuery (Some pos, _) -> handler.EndPrintType(writer, t, Some pos)
    | AtSignature pos -> handler.EndPrintType(writer, t, positions.TryFind(pos))
    | AtQuery (None, _) | Unknown -> handler.EndPrintType(writer, t, None)
    this

type QueryPrinterHandler<'Writer when 'Writer :> Writer> =
  abstract BeginPrintType : 'Writer * QueryId -> unit
  abstract EndPrintType : 'Writer * QueryId -> unit

module QueryPrinterHandler =
  let empty<'Writer when 'Writer :> Writer> =
    { new QueryPrinterHandler<'Writer> with
        member this.BeginPrintType(_, _) = ()
        member this.EndPrintType(_, _) = ()
    }

type QueryPrinter<'Writer when 'Writer :> Writer>(writer: 'Writer, handler: QueryPrinterHandler<'Writer>) =
  new (writer) = QueryPrinter<_>(writer, QueryPrinterHandler.empty)

  member this.Write(str: string) = writer.Write(str)
  member this.WriteLine(str: string) = writer.WriteLine(str)
  member this.WriteLine() = writer.WriteLine()

  member this.BeginPrintType(id: QueryId) = handler.BeginPrintType(writer, id)
  member this.EndPrintType(id: QueryId) = handler.EndPrintType(writer, id)