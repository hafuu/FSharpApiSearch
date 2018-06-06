namespace FSharpApiSearch.Desktop

open FSharpApiSearch
open System.Threading
open System.Windows.Media
open WpfHelpers

[<CLIMutable>]
type SignatureItem = {
  Text: string
  Color: Color option
}

[<CLIMutable>]
type SearchResult = {
  AccessPath: string
  DeclarationType: string
  Name: string
  Signature: SignatureItem[]
  Constraints: string option
  Kind: string
  Assembly: string
  Document: string option
  Link: string option
}

type FSharpApiSearchSession(database: Lazy<Database>) =
  inherit NotificationObject()

  let client = lazy (FSharpApiSearchClient(FSharpApiSearchClient.DefaultTargets, database.Value))
  
  let mutable _query = ""
  let mutable _results : SearchResult[] option = None
  let mutable _errorMessage : string option = None

  let colorTable = [|
    Colors.LightGreen
    Colors.Red
    Colors.Orange
    Colors.Cyan
  |]

  member this.Query with get() = _query and set(value) = _query <- value; this.RisePropertyChanged()
  member this.Results with get() = _results and set(value) = _results <- value; this.RisePropertyChanged()
  member this.ErrorMessage with get() = _errorMessage and set(value) = _errorMessage <- value; this.RisePropertyChanged()

  member this.InitializeInBackground() =
    async {
      try
        do client.Force() |> ignore
      with _ -> ()
    }
    |> Async.Start

  member this.Clear() =
    this.Results <- None
    this.ErrorMessage <- None

  member this.Search() : unit =
    this.Clear()

    try
      let opt = SearchOptions.defaultOptions
      let client = client.Value
      let _, results = client.Search(this.Query, opt)

      let results =
        client.Sort(results)
        |> Seq.map (fun result ->
          let api = result.Api
          {
            AccessPath = StringPrinter.FSharp.printAccessPath None api
            DeclarationType = StringPrinter.FSharp.printAccessPath (Some 1) api
            Name = StringPrinter.FSharp.printApiName api
            Signature =
              (HtmlPrintHelper.signature result (Printer.FSharp.printSignature api)).Text
              |> Array.map (fun (text, colorId) ->
                {
                  Text = text
                  Color = colorId |> Option.map (fun id -> colorTable.[id % colorTable.Length])
                }
              )
            Constraints = StringPrinter.FSharp.tryPrintTypeConstraints api
            Kind = StringPrinter.FSharp.printKind api
            Assembly = result.AssemblyName
            Document = api.Document
            Link = None
          })
        |> Seq.toArray

      this.Results <- Some results
    with
      ex -> this.ErrorMessage <- Some ex.Message