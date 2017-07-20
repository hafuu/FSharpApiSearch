#r @"src\FSharpApiSearch\bin\Release\FSharpApiSearch.dll"
#r @"src\FSharpApiSearch\bin\Release\FSharp.Compiler.Service.dll"
#r @"src\FSharpApiSearch\bin\Release\System.Collections.Immutable.dll"
#r @"src\FSharpApiSearch\bin\Release\System.Reflection.Metadata.dll"
#r @"src\FSharpApiSearch\bin\Release\FSharp.Collections.ParallelSeq.dll"
#r @"src\FSharpApiSearch\bin\Release\FParsec.dll"
#r @"src\FSharpApiSearch\bin\Release\FParsecCS.dll"

let database = fsi.CommandLineArgs.[1]

open FSharpApiSearch
open FSharpApiSearch.Printer

ApiLoader.loadFromFile database
|> Array.filter (fun x -> x.AssemblyName = "FParsec")
|> Array.collect (fun x -> x.Api)
|> Array.choose (fun api ->
  match LinkGenerator.fparsec "http://www.quanttec.com/fparsec/reference/" api with
  | Some link -> printfn "%s: %s" (api.Name.Print()) link
  | None -> printfn "%s: None" (api.Name.Print())
)