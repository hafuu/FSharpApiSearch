namespace FSharpApiSearch.Desktop

open FSharpApiSearch

type SearchResult = {
  AccessPath: string
  Name: string
  Signature: string
}

type FSharpApiSearchSession(database: Lazy<Database>) =
  
  let client = lazy (FSharpApiSearchClient(FSharpApiSearchClient.DefaultTargets, database.Value))
  
  member this.InitializeBackground() =
    async { try do client.Force() |> ignore with _ -> () } |> Async.Start

  member this.SearchAsync(query: string) = async {
    let opt = SearchOptions.defaultOptions
    let client = client.Value
    let _, results = client.Search(query, opt)
    return
      client.Sort(results)
      |> Seq.map (fun x ->
        {
          AccessPath = StringPrinter.FSharp.printAccessPath None x.Api
          Name = StringPrinter.FSharp.printApiName x.Api
          Signature = StringPrinter.FSharp.printSignature x.Api
        })
      |> Seq.toArray
  }