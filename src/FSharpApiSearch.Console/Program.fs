open FSharpApiSearch
open System.Diagnostics
open System
open System.Reflection
open Microsoft.FSharp.Compiler.SourceCodeServices

type Args = {
  Query: string option
  SearchOptions: SearchOptions
  Help: bool
}

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Args =
  let empty = { Query = None; SearchOptions = SearchOptions.defaultOptions; Help = false }

  let (|Status|_|) (name: string) (str: string) =
    if str.StartsWith(name) then
      match str.Substring(name.Length) with
      | "+" -> Some true
      | "-" -> Some false
      | _ -> None
    else
      None

  let boolToOptionStatus = function true -> Enabled | false -> Disabled

  let rec parse opt = function
    | Status "--strict" v :: rest -> parse { opt with SearchOptions = { opt.SearchOptions with StrictQueryVariable = boolToOptionStatus v } } rest
    | Status "--similarity" v :: rest -> parse { opt with SearchOptions = { opt.SearchOptions with SimilaritySearching = boolToOptionStatus v } } rest
    | "--help" :: rest -> parse { opt with Help = true } rest
    | query :: rest ->
      let q =
        match opt.Query with
        | None -> Some query
        | Some _ as q -> q
      parse { opt with Query = q } rest
    | [] -> opt

let searchAndShowResult (client: FSharpApiSearchClient) (query: string) opt =
  let results = client.Search(query, opt)
  results
  |> Seq.filter (fun x -> x.Distance < 3)
  |> Seq.iter (fun x ->
    Console.Write(sprintf "%s: %s" x.Api.Name (Signature.display x.Api.Signature))
    Console.ForegroundColor <- ConsoleColor.DarkGray
    Console.WriteLine(sprintf ", distance: %d" x.Distance)
    Console.ResetColor()
  )
  Console.WriteLine()

module Interactive =
  type Lens<'a, 'b> = {
    Get: 'a -> 'b
    Set: 'b -> 'a -> 'a
  }

  let tryParseOptionStatus (x: string) =
    match x.ToLower() with
    | "enable" -> Some Enabled
    | "disable" -> Some Disabled
    | _ -> None

  let (|OptionSetting|_|) (name: string) (lens: Lens<_, _>) opt (str: string) =
    match str.Split([| ' ' |], 2) with
    | [| key; value |] when key = name ->
      match tryParseOptionStatus value with
      | Some value -> Some (lens.Set value opt)
      | None -> printfn "invalid value"; Some opt
    | [| key |] when key = name ->
      printfn "%A" (lens.Get opt)
      Some opt
    | _ -> None

  let StrictQueryVariable = { Get = (fun x -> x.StrictQueryVariable ); Set = (fun value x -> { x with StrictQueryVariable = value }) }
  let SimilaritySearching = { Get = (fun x -> x.SimilaritySearching ); Set = (fun value x -> { x with SimilaritySearching = value }) }

  let rec loop (client: FSharpApiSearchClient) opt =
    printf "> "
    match Console.ReadLine().TrimEnd(';') with
    | "#q" -> opt
    | OptionSetting "#strict" StrictQueryVariable opt newOpt -> loop client newOpt
    | OptionSetting "#similarity" SimilaritySearching opt newOpt -> loop client newOpt
    | query ->
      try searchAndShowResult client query opt with ex -> printfn "%A" ex
      loop client opt

let helpMessage = """usage: FSharpApiSearch.exe <query> <options>

query: Optional. If you omit will start in interactive mode.
options:
  --strict[+|-]
      Enables or disables to strictly match the variables in the query.
  --similarity[+|-]
      Enables or disables the similarity searching.
  --help
      Print this message."""

let ignoreFSharpCompilerServiceError() =
  typeof<FSharpChecker>.Assembly.GetType("Microsoft.FSharp.Compiler.AbstractIL.Diagnostics")
  |> Option.ofObj
  |> Option.bind (fun diagMod -> diagMod.GetMember("diagnosticsLog", BindingFlags.NonPublic ||| BindingFlags.Static) |> Array.tryHead)
  |> Option.bind (tryUnbox<PropertyInfo>)
  |> Option.bind (fun x -> x.GetValue(null) |> Option.ofObj)
  |> Option.bind (tryUnbox<ref<Option<System.IO.TextWriter>>>)
  |> Option.iter (fun x -> x := None)

[<EntryPoint>]
let main argv =
  ignoreFSharpCompilerServiceError()
  let args = Args.parse Args.empty (List.ofArray argv)
  match args with
  | { Help = true } ->
    printfn "%s" helpMessage
  | { Query = Some query } ->
    let client = FSharpApiSearchClient()
    searchAndShowResult client query args.SearchOptions
  | { Query = None } ->
    printfn "Initializing."
    let client = FSharpApiSearchClient()
    printfn "Input query or #q to quit."
    Interactive.loop client args.SearchOptions |> ignore
  0