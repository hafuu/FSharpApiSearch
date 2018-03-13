#r @"src\FSharpApiSearch\bin\Release\FSharpApiSearch.dll"
#r @"src\FSharpApiSearch\bin\Release\FSharp.Compiler.Service.dll"
#r @"src\FSharpApiSearch\bin\Release\System.Collections.Immutable.dll"
#r @"src\FSharpApiSearch\bin\Release\System.Reflection.Metadata.dll"
#r @"src\FSharpApiSearch\bin\Release\FSharp.Collections.ParallelSeq.dll"
#r @"src\FSharpApiSearch\bin\Release\FParsec.dll"
#r @"src\FSharpApiSearch\bin\Release\FParsecCS.dll"
#r @"src\FSharpApiSearch\bin\Release\FsPickler.dll"
#r @"System.Net.dll"
#r @"System.Xml"
#r @"System.Xml.Linq"

open FSharpApiSearch
open System.Web
open System.IO

let docDir = fsi.CommandLineArgs.[2]
let fsharpDictionary = ApiLoader.loadFromFile (fsi.CommandLineArgs.[1]) |> Array.find (fun x -> x.AssemblyName = "FSharp.Core")

fsharpDictionary.Api
|> Seq.iter (fun api ->
  let apiName: string = api.Name.Print()
  let link = LinkGenerator.fsharp "" api |> Option.map HttpUtility.UrlDecode
  match link with
  | Some link ->
    let exists =
      try
        File.Exists(Path.Combine(docDir, link + ".md"))
      with
        _ -> false
    printfn "%s, %s, %b" apiName link exists
  | None -> printfn "%s, None" apiName
)