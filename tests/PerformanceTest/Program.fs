open FSharpApiSearch
open System.Diagnostics
open System.Threading.Tasks
open System.Threading

type TestResult = {
  Query: string
  Count: int
  AverageTimeNonParallel: int64
  AverageTimeParallel: int64
}

let queries = [
  "seq<'a> -> 'a"
  "?a -> ?a"
  "?a -> ?b -> ?a"
  "('a -> 'b) -> ?<'a> -> ?<'b>"
  "int -> #seq<'a>"
  " #seq<#obj> -> int"
  " #seq<'a> -> int"
  " #seq<?a> -> ?a"
  "List*.* : _"
  "*List.* : _"
  "*List*.* : _"
  "*L*t*.* : _"
]

[<EntryPoint>]
let main argv =
  let client =
    let targets = FSharpApiSearchClient.DefaultTargets
    let database = ApiLoader.loadFromFile ApiLoader.databaseName
    FSharpApiSearchClient(targets, database)

  let run query option =
    let sw = Stopwatch.StartNew()
    let results =
      client.Search(query, option)
      |> ResizeArray
    sw.Stop()
    (float results.Count, float sw.ElapsedMilliseconds)

  let skip = 3
  let n = skip + 50

  let nonParallelOpt = SearchOptions.defaultOptions |> SearchOptions.Parallel.Set Disabled
  let parallelOpt = SearchOptions.defaultOptions |> SearchOptions.Parallel.Set Enabled

  printfn "query\tcount\tnon parallel\tparallel"

  for x = 1 to 10 do
    run "int -> string" parallelOpt |> ignore

  queries
  |> List.map (fun query ->
    let nonParallelResults = Array.init n (fun _ -> run query nonParallelOpt) |> Array.skip skip
    let parallelResults = Array.init n (fun _ -> run query parallelOpt) |> Array.skip skip
    {
      Query = query
      Count = (Array.averageBy fst nonParallelResults + Array.averageBy fst parallelResults) / 2.0 |> int
      AverageTimeNonParallel = Array.averageBy snd nonParallelResults |> int64
      AverageTimeParallel = Array.averageBy snd parallelResults |> int64
    }
  )
  |> List.iter (fun result ->
    printfn "%s\t%d\t%d\t%d" result.Query result.Count result.AverageTimeNonParallel result.AverageTimeParallel
  )

  0
