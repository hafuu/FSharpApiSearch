#r @"src\FSharpApiSearch\bin\Release\FSharpApiSearch.dll"
#r @"src\FSharpApiSearch\bin\Release\FSharp.Compiler.Service.dll"
#r @"src\FSharpApiSearch\bin\Release\System.Collections.Immutable.dll"
#r @"src\FSharpApiSearch\bin\Release\System.Reflection.Metadata.dll"
#r @"src\FSharpApiSearch\bin\Release\FSharp.Collections.ParallelSeq.dll"
#r @"src\FSharpApiSearch\bin\Release\FParsec.dll"
#r @"src\FSharpApiSearch\bin\Release\FParsecCS.dll"


let database =  fsi.CommandLineArgs.[1]

open System.Diagnostics
open FSharpApiSearch
open FSharpApiSearch.Printer
open System

let temp = ApiLoader.loadFromFile database
            |> Array.filter (fun x -> x.AssemblyName = "FSharp.Core")
            |> Array.collect (fun x -> x.Api)
let sw = Stopwatch()
sw.Start()
for i = 0 to 2000 do
  temp |> Array.choose (fun api -> LinkGenerator.fsharp "" api) |> ignore

sw.Stop()
let ts = sw.ElapsedMilliseconds
printfn "%s" (ts.ToString())
