namespace FSharpApiSearch

open Microsoft.FSharp.Compiler.SourceCodeServices
open FSharpApiSearch.Types

type FSharpApiSearchClient (apis: Api seq) =
  new (assemblies: FSharpAssembly seq) = FSharpApiSearchClient(Seq.collect ApiLoader.collectApi assemblies)
  new (references: string seq) = FSharpApiSearchClient(ApiLoader.loadAssembly references)
  new () = FSharpApiSearchClient(Seq.empty<string>)

  member this.Search(query: Query) =
    let initialEquations = Matcher.Equations.strict query
    apis
    |> Seq.filter (fun api -> Matcher.matches query api.Signature initialEquations)
    |> Seq.cache

  member this.Search(query: string) = this.Search(QueryParser.parse query)