open FSharpApiSearch
open System.Diagnostics
open System
open Microsoft.FSharp.Compiler.SourceCodeServices

type Args = {
  Query: string option
  Targets: string list
  References: string list
  SearchOptions: SearchOptions
  Help: bool
}

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Args =
  let empty = { Query = None; Targets = []; References = []; SearchOptions = SearchOptions.defaultOptions; Help = false }

  let (|Status|_|) (name: string) (str: string) =
    if str.StartsWith(name) then
      match str.Substring(name.Length) with
      | "+" -> Some true
      | "-" -> Some false
      | _ -> None
    else
      None

  let (|KeyValue|_|) key (str: string) =
    match str.Split([| ':' |], 2) |> Array.toList with
    | [ k; v ] when key = k -> Some v
    | _ -> None

  let boolToOptionStatus = function true -> Enabled | false -> Disabled

  let rec parse arg = function
    | Status "--strict" v :: rest -> parse { arg with SearchOptions = { arg.SearchOptions with StrictQueryVariable = boolToOptionStatus v } } rest
    | Status "--similarity" v :: rest -> parse { arg with SearchOptions = { arg.SearchOptions with SimilaritySearching = boolToOptionStatus v } } rest
    | (KeyValue "--target" t | KeyValue "-t" t) :: rest -> parse { arg with Targets = t :: arg.Targets } rest
    | (KeyValue "--reference" r | KeyValue "-r" r) :: rest -> parse { arg with References = r :: arg.References } rest
    | ("--help" | "-h") :: rest -> parse { arg with Help = true } rest
    | query :: rest ->
      let q =
        match arg.Query with
        | None -> Some query
        | Some _ as q -> q
      parse { arg with Query = q } rest
    | [] -> arg

  let targetAndReference arg =
    if List.isEmpty arg.Targets then
      FSharpApiSearchClient.DefaultTargets, FSharpApiSearchClient.DefaultReferences
    else
      let targets =
        List.rev arg.Targets
        |> List.map (fun x -> let name = System.IO.Path.GetFileName(x) in if name.EndsWith(".dll") then name.Substring(0, name.Length - 4) else name)
      let references = List.concat [ (List.rev arg.Targets); (List.rev arg.References); FSharpApiSearchClient.DefaultReferences ] |> List.distinct
      targets, references

let propertyKindText = function
  | PropertyKind.GetSet -> "get set"
  | PropertyKind.Set -> "set"
  | PropertyKind.Get -> "get"

let apiKindText = function
  | ApiKind.Constructor -> "constructor"
  | ApiKind.ModuleValue -> "module value"
  | ApiKind.StaticMethod -> "static method"
  | ApiKind.StaticProperty prop -> sprintf "static property with %s" (propertyKindText prop)
  | ApiKind.InstanceMethod -> "instance method"
  | ApiKind.InstanceProperty prop -> sprintf "instance property with %s" (propertyKindText prop)
  | ApiKind.Field -> "field"

let searchAndShowResult (client: FSharpApiSearchClient) (query: string) opt =
  let results = client.Search(query, opt)
  results
  |> Seq.filter (fun x -> x.Distance < 3)
  |> Seq.iter (fun x ->
    Console.Write(sprintf "%s: %s" x.Api.Name (Signature.display x.Api.Signature))
    Console.ForegroundColor <- ConsoleColor.DarkGray
    Console.WriteLine(sprintf ", %s, distance: %d" (apiKindText x.Api.Kind) x.Distance)
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

let helpMessage = """usage: FSharpApiSearch.Console.exe <query> <options>

query: Optional. If omitted, it will start in interactive mode.
options:
  --strict[+|-]
      Enables or disables to strictly match the variables in the query.
  --similarity[+|-]
      Enables or disables the similarity searching.
  --target:<assembly>, -t:<assembly>
      Specifies the assembly name or the assembly path of the searching target.
      If omitted, it will target 'FSharp.Core', 'mscorlib', 'System' and 'System.Core'.
  --reference:<assembly>, -r:<assembly>
      Specifies the assembly name or the assembly path that the targets are dependent.
      By default, 'mscorlib', 'System', 'System.Core', 'System.Xml', 'System.Configuration' and 'FSharp.Core' are specified.
  --help, -h
      Print this message."""

[<EntryPoint>]
let main argv =
  let args = Args.parse Args.empty (List.ofArray argv)
  let targets, references = Args.targetAndReference args
  match args with
  | { Help = true } ->
    printfn "%s" helpMessage
  | { Query = Some query } ->
    let client = FSharpApiSearchClient(targets, references)
    searchAndShowResult client query args.SearchOptions
  | { Query = None } ->
    printfn "Initializing."
    let client = FSharpApiSearchClient(targets, references)
    printfn "Targets the following assemblies."
    client.TargetAssemblies |> List.iter (printfn "  %s")
    printfn "Input query or #q to quit."
    Interactive.loop client args.SearchOptions |> ignore
  0