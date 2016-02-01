namespace FSharpApiSearch

open Microsoft.FSharp.Compiler.SourceCodeServices

type SearchResult = {
  Distance: int
  Api: Api
}

type OptionStatus = Enabled | Disabled

type SearchOptions = {
  SimilaritySearching: OptionStatus
  StrictQueryVariable: OptionStatus
}

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module SearchOptions =
  let defaultOptions = { SimilaritySearching = Disabled; StrictQueryVariable = Enabled }

  let context query options =
    let initialEquations =
      let eqs = Matcher.Equations.initialize query
      match options.StrictQueryVariable with
      | Enabled -> Matcher.Equations.strictVariables query eqs
      | Disabled -> eqs
    Matcher.Context.initialize initialEquations

  let rule options =
    match options.SimilaritySearching with
    | Enabled -> Matcher.similaritySearchingRule
    | Disabled -> Matcher.defaultRule

type FSharpApiSearchClient (apis: Api seq) =
  new (assemblies: FSharpAssembly seq) = FSharpApiSearchClient(Seq.collect ApiLoader.collectApi assemblies)
  new (references: string seq) = FSharpApiSearchClient(ApiLoader.loadAssembly references)
  new () = FSharpApiSearchClient(Seq.empty<string>)

  member this.Search(query: Query, ?options) =
    let options = defaultArg options SearchOptions.defaultOptions
    let initialContext = SearchOptions.context query options
    let rule = SearchOptions.rule options
    apis
    |> Seq.map (fun api -> (api, Matcher.matches query api rule initialContext))
    |> Seq.filter (snd >> ((<>)Matcher.Failure))
    |> Seq.map (fun (api, result) -> { Api = api; Distance = Matcher.Result.distance result })
    |> Seq.sort
    |> Seq.cache

  member this.Search(query: string, ?options) =
    let query = QueryParser.parse query
    match options with
    | Some options -> this.Search(query, options)
    | None -> this.Search(query)