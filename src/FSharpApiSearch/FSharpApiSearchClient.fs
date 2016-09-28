namespace FSharpApiSearch

open Microsoft.FSharp.Compiler.SourceCodeServices
open FSharpApiSearch.AssemblyLoader
open FSharp.Collections.ParallelSeq

type FSharpApiSearchClient(targets: string seq, dictionaries: ApiDictionary seq) =
  let dictionaries = dictionaries |> Seq.toArray
  let targetDictionaries = dictionaries |> Seq.filter (fun x -> targets |> Seq.exists ((=)x.AssemblyName)) |> Seq.toArray

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

  member this.Search(query: string, options: SearchOptions) = Matcher.search dictionaries options targetDictionaries query

  member this.Sort(results: seq<Result>) =
    let sortKey (result: Result) =
      let kind =
        match result.Api.Kind with
        | ApiKind.TypeDefinition -> 0
        | ApiKind.TypeAbbreviation -> 1
        | _ -> 2
      let distance = result.Distance
      let name = result.Api.Name.Print()
      (kind, distance, name)
    match results with
    | :? pseq<Result> as xs -> PSeq.sortBy sortKey xs :> seq<Result>
    | xs -> Seq.sortBy sortKey xs
    

  member this.TargetAssemblies: string list = targetDictionaries |> Array.map (fun x -> x.AssemblyName) |> Array.toList