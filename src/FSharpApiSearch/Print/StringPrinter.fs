module FSharpApiSearch.StringPrinter

open System.IO
open System.Text
open System.Runtime.CompilerServices

let internal printStringBuilder (sb: StringBuilder) (print: SignaturePrinter<_> -> SignaturePrinter<_>) =
  use writer = new StringWriter(sb)
  let p = SignaturePrinter(Writer.wrap writer, SignaturePrinterHandler.empty)
  print p |> ignore
  sb

let internal printString (print: SignaturePrinter<_> -> SignaturePrinter<_>) =
  let sb = StringBuilder()
  printStringBuilder sb print |> ignore
  sb.ToString()

module FSharp =
  let printFullName (api: Api) = Printer.FSharp.printFullName api |> printString
  let printApiName (api: Api) = Printer.FSharp.printApiName api |>printString
  let printAccessPath (depth: int option) (api: Api) = Printer.FSharp.printAccessPath depth api |> printString
  let printSignature (api:Api) = Printer.FSharp.printSignature api |> printString
  let printKind (api: Api) = Printer.FSharp.printKind api |> printString
  let printTypeConstraints (api: Api) = Printer.FSharp.printTypeConstraints api |> printString
  let tryPrintTypeConstraints (api: Api) =
    if Printer.FSharp.hasTypeConstraints api then
      Some (printTypeConstraints api)
    else
      None

module CSharp =
  let printFullName (api: Api) = Printer.CSharp.printFullName api |> printString
  let printApiName (api: Api) = Printer.CSharp.printApiName api |> printString
  let printAccessPath (depth: int option) (api: Api) = Printer.CSharp.printAccessPath depth api |> printString
  let printSignature (api:Api) = Printer.CSharp.printSignature api |> printString
  let printKind (api: Api) = Printer.CSharp.printKind api |> printString
  let printTypeConstraints (api: Api) = Printer.CSharp.printTypeConstraints api |> printString
  let tryPrintTypeConstraints (api: Api) =
    if Printer.CSharp.hasTypeConstraints api then
      Some (printTypeConstraints api)
    else
      None

type TypeVariable with
  member this.Print(sb: StringBuilder) = printStringBuilder sb (FSharpFormat.printTypeVariable false VariableSource.Target this)
  member this.Print() = printString (FSharpFormat.printTypeVariable false VariableSource.Target this)

type NameItem with
  member this.Print() = printString (FSharpFormat.printNameItem this)

type ApiName with
  member this.Print() = printString (FSharpFormat.printName_full (ApiName.toName this))

type LowType with
  member this.Print() = printString (FSharpFormat.printLowType_short false this)
  member internal this.Debug() = printString (FSharpFormat.printLowType_short true this)

type ApiSignature with
  member this.Print() = printString (FSharpFormat.printApiSignature false this)
  member internal this.Debug() = printString (FSharpFormat.printApiSignature true this)

type TypeConstraint with
  member this.Print() = printString (FSharpFormat.printConstraint false this)
  member internal this.Debug() = printString (FSharpFormat.printConstraint true this)
  
type FullTypeDefinition with
  member this.Print() = printString (FSharpFormat.printFullTypeDefinition false this)
  member internal this.Debug() = printString (FSharpFormat.printFullTypeDefinition true this)
  
module internal LowType =
  let debug (x: LowType) = x.Debug()

module internal ApiSignature =
  let debug (x: ApiSignature) = x.Debug()
  let print (x: ApiSignature) = x.Print()
  
module internal TypeConstraint =
  let debug (x: TypeConstraint) = x.Debug()
  
module internal FullTypeDefinition =
  let debug (x: FullTypeDefinition) = x.Debug()

[<Extension>]
type internal Extensions() =
  [<Extension>]
  static member Print(name: Name) = printString (FSharpFormat.printName_full name)