namespace FSharpApiSearch

open Microsoft.FSharp.Compiler.SourceCodeServices
open FSharpApiSearch.Types

type FSharpApiSearchClient (apis: Api seq) =
  new (assemblies: FSharpAssembly seq) = FSharpApiSearchClient(Seq.collect ApiLoader.collectApi assemblies)
  new (references: string seq) = FSharpApiSearchClient(ApiLoader.loadAssembly references)
  new () = FSharpApiSearchClient(Seq.empty<string>)

  member this.Search(query: Query) =
    let initialEquations = Matcher.Equations.initialize query |> Matcher.Equations.strictVariables query
    let initialContext = Matcher.Context.initialize initialEquations
    apis
    |> Seq.map (fun api -> (api, Matcher.matches query api Matcher.defaultRule initialContext))
    |> Seq.filter (snd >> ((<>)Matcher.Failure))
    |> Seq.map (fun (api, result) -> { Api = api; Distance = Matcher.Result.distance result })
    |> Seq.sort
    |> Seq.cache

  member this.Search(query: string) = this.Search(QueryParser.parse query)