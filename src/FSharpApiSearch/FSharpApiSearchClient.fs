namespace FSharpApiSearch

open Microsoft.FSharp.Compiler.SourceCodeServices

type SearchResult = {
  Distance: int
  Api: Api
}

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

type FSharpApiSearchClient (targets: string seq, dictionaries: ApiDictionary seq) =
  let targetAssemblies = dictionaries |> Seq.filter (fun x -> targets |> Seq.exists ((=)x.AssemblyName)) |> Seq.toList
  let apis = targetAssemblies |> Seq.collect (fun x -> x.Api) |> Seq.cache
  do Async.Start <| async { do apis |> Seq.iter (fun _ -> ()) }
  let abbreviationTable = dictionaries |> Seq.collect (fun x -> x.TypeAbbreviations) |> Seq.toList

  static member DefaultReferences = [
    "mscorlib" 
    "System"
    "System.Core"
    "System.Xml"
    "System.Configuration"
    "FSharp.Core"
  ]
  static member DefaultTargets = [
    "mscorlib" 
    "System"
    "System.Core"
    "FSharp.Core"
  ]

  new (targets, assemblies: FSharpAssembly seq) = FSharpApiSearchClient(targets, Seq.map ApiLoader.load assemblies)
  new (targets: string seq, references: string seq) = FSharpApiSearchClient(targets, ApiLoader.loadAssembly references)
  new () = FSharpApiSearchClient(FSharpApiSearchClient.DefaultTargets, FSharpApiSearchClient.DefaultReferences)

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

  member this.ParseQuery(query: string) =
    QueryParser.parse query |> Query.replaceAbbreviation abbreviationTable

  member this.Search(query: string, ?options) =
    let query = this.ParseQuery(query)
    match options with
    | Some options -> this.Search(query, options)
    | None -> this.Search(query)

  member this.TargetAssemblies with get() = targetAssemblies |> List.map (fun x -> x.AssemblyName)