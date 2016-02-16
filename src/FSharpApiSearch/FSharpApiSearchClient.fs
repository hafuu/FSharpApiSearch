namespace FSharpApiSearch

open Microsoft.FSharp.Compiler.SourceCodeServices

type FSharpApiSearchClient(targets: string seq, dictionaries: ApiDictionary seq) =
  let dictionaries = dictionaries |> Seq.toArray
  let targetDictionaries = dictionaries |> Seq.filter (fun x -> targets |> Seq.exists ((=)x.AssemblyName)) |> Seq.toArray
  let apis = targetDictionaries |> Seq.collect (fun x -> x.Api) |> Seq.toArray

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
  new (targets: string seq, references: string seq) = FSharpApiSearchClient(targets, FSharpApiSearch.AssemblyLoader.load references)
  new () = FSharpApiSearchClient(FSharpApiSearchClient.DefaultTargets, FSharpApiSearchClient.DefaultReferences)

  member this.Search(query: string, options: SearchOptions) = Matcher.search dictionaries options apis query

  member this.TargetAssemblies: string list = targetDictionaries |> Array.map (fun x -> x.AssemblyName) |> Array.toList