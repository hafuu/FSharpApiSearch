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
          {
            AccessPath = StringPrinter.FSharp.printAccessPath None result.Api
            DeclarationType = StringPrinter.FSharp.printAccessPath (Some 1) result.Api
            Name = StringPrinter.FSharp.printApiName result.Api
            Signature =
              (HtmlPrintHelper.signature result (Printer.FSharp.printSignature result.Api)).Text
              |> Array.map (fun (text, colorId) ->
                {
                  Text = text
                  Color = colorId |> Option.map (fun id -> colorTable.[id % colorTable.Length])
                }
              )
          })
        |> Seq.toArray

      this.Results <- Some results
    with
      ex -> this.ErrorMessage <- Some ex.Message