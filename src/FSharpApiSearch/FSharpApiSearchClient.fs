namespace FSharpApiSearch

open Microsoft.FSharp.Compiler.SourceCodeServices
open FSharpApiSearch.AssemblyLoader
open FSharpApiSearch.StringPrinter
open FSharp.Collections.ParallelSeq

type TargetSummary = {
  AssemblyName: string
  PublicApiNumber: int
}

type FSharpApiSearchClient(targets: string seq, database: Database) =
  let targetDictionaries = database |> Seq.filter (fun x -> targets |> Seq.exists ((=)x.AssemblyName)) |> Seq.toArray

  static member DefaultTargets = [
    "mscorlib" 
    "System"
    "System.Core"
    "FSharp.Core"
  ]

  member this.Search(query: string, options: SearchOptions) : Query * seq<Result> = Engine.search database options targetDictionaries query

  member this.Sort(results: seq<Result>) =
    let sortKey (result: Result) =
      let kind =
        match result.Api.Kind with
        | ApiKind.ModuleDefinition -> 0
        | ApiKind.TypeDefinition -> 0
        | ApiKind.TypeAbbreviation -> 1
        | ApiKind.ComputationExpressionBuilder -> 1
        | _ -> 2
      let distance = result.Distance
      let name = FSharp.printApiName result.Api
      let type' = FSharp.printAccessPath None result.Api
      (kind, distance, name, type')
    match results with
    | :? pseq<Result> as xs -> PSeq.sortBy sortKey xs :> seq<Result>
    | xs -> Seq.sortBy sortKey xs
    

  member this.TargetAssemblies: string list = targetDictionaries |> Array.map (fun x -> x.AssemblyName) |> Array.toList

  member this.Targets: TargetSummary list = targetDictionaries |> Array.map (fun x -> { AssemblyName = x.AssemblyName; PublicApiNumber = x.PublicApiNumber }) |> Array.toList