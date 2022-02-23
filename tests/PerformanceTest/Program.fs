open FSharpApiSearch
open BenchmarkDotNet
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running

[<MemoryDiagnoser>]
[<ShortRunJob>]
type MyBenchmarks() =
  let mutable client : FSharpApiSearchClient = Unchecked.defaultof<_>

  let parallelOpt = SearchOptions.defaultOptions |> SearchOptions.Parallel.Set Enabled
  let nonParallelOpt = SearchOptions.defaultOptions |> SearchOptions.Parallel.Set Disabled

  [<GlobalSetup>]
  member this.GlobalSetup() =
    let targets = FSharpApiSearchClient.DefaultTargets
    let database = Database.loadFromFile Database.databaseName
    client <- FSharpApiSearchClient(targets, database)

  [<Params(
    "ToString",
    "seq<'t>",
    "seq<'a> -> 'a",
    "?a -> ?a",
    "?a -> ?b -> ?a",
    "('a -> 'b) -> ?<'a> -> ?<'b>",
    "int -> #seq<'a>",
    "#seq<#obj> -> int",
    "#seq<'a> -> int",
    "#seq<?a> -> ?a",
    "List*.* : _",
    "*List.* : _",
    "*List*.* : _",
    "*L*t*.* : _"
  )>]
  member val Query = "" with get, set

  [<Benchmark>]
  member this.Parallel() = client.Search(this.Query, parallelOpt) |> snd |> ResizeArray

  [<Benchmark>]
  member this.NonParallel() = client.Search(this.Query, nonParallelOpt) |> snd |> ResizeArray

[<EntryPoint>]
let main argv =
  let summary = BenchmarkRunner.Run<MyBenchmarks>()
  0
