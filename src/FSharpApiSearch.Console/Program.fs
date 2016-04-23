module FSharpApiSearch.Console.Program

open FSharpApiSearch
open System.Diagnostics
open System
open Microsoft.FSharp.Compiler.SourceCodeServices

let searchAndShowResult (client: FSharpApiSearchClient) (query: string) opt =
  let results = client.Search(query, opt)
  results
  |> Seq.filter (fun x -> x.Distance < 3)
  |> Seq.iter (fun x ->
    Console.Write(sprintf "%s: %s" (ReverseName.toString x.Api.Name) (x.Api.Signature.Print()))
    Console.ForegroundColor <- ConsoleColor.DarkGray
    Console.WriteLine(sprintf ", %s, distance: %d" (x.Api.Kind.Print()) x.Distance)
    if x.Api.TypeConstraints.IsEmpty = false then
      Console.WriteLine(sprintf "  %s" (x.Api.PrintTypeConstraints()))
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

  let (|OptionSetting|_|) (name: string) (lens: Lens<_, _>) target (str: string) =
    match str.Split([| ' ' |], 2) with
    | [| key; value |] when key = name ->
      match tryParseOptionStatus value with
      | Some value -> Some (lens.Set value target)
      | None -> printfn "invalid value"; Some target
    | [| key |] when key = name ->
      printfn "%A" (lens.Get target)
      Some target
    | _ -> None

  let StrictQueryVariable = { Get = (fun x -> x.StrictQueryVariable ); Set = (fun value x -> { x with StrictQueryVariable = value }) }
  let SimilaritySearching = { Get = (fun x -> x.SimilaritySearching ); Set = (fun value x -> { x with SimilaritySearching = value }) }
  let IgnoreArgumentStyle = { Get = (fun x -> x.IgnoreArgumentStyle); Set = (fun value x -> { x with IgnoreArgumentStyle = value }) }
  let StackTrace = { Get = (fun x -> x.StackTrace); Set = (fun value x -> { x with StackTrace = value }) }

  let rec loop (client: FSharpApiSearchClient) arg =
    printf "> "
    match Console.ReadLine().TrimEnd(';') with
    | "#q" -> arg
    | OptionSetting "#strict" StrictQueryVariable arg.SearchOptions newOpt -> loop client { arg with SearchOptions = newOpt }
    | OptionSetting "#similarity" SimilaritySearching arg.SearchOptions newOpt -> loop client { arg with SearchOptions = newOpt }
    | OptionSetting "#ignore-argstyle" IgnoreArgumentStyle arg.SearchOptions newOpt -> loop client { arg with SearchOptions = newOpt }
    | OptionSetting "#stacktrace" StackTrace arg newArg -> loop client newArg
    | query ->
      try
        searchAndShowResult client query arg.SearchOptions
      with ex ->
        match arg.StackTrace with
        | Enabled -> printfn "%A" ex
        | Disabled -> printfn "%s" ex.Message
      loop client arg

let helpMessage = """usage: FSharpApiSearch.Console.exe <query> <options>

query: Optional. If omitted, it will start in interactive mode.
options:
  --strict[+|-]
      Enables or disables to strictly deal with variables of different name in the query.
      The default is enabled.
  --similarity[+|-]
      Enables or disables the similarity searching.
      The default is disabled.
  --ignore-argstyle[+|-]
      Enables of disables to ignore the difference of the argument style.
      The argument style refers to curried argument, multi argument and tuple argument.
      The default is enabled.
  --target:<assembly>, -t:<assembly>
      Specifies the assembly name or the assembly path of the searching target.
      If omitted, it will target 'FSharp.Core', 'mscorlib', 'System' and 'System.Core'.
  --stacktrace[+|-]
      Enables or disables stacktrace output if an exception occurs.
      The default is disabled.
  --help, -h
      Print this message."""

[<EntryPoint>]
let main argv =
  let args = Args.parse Args.empty (List.ofArray argv)
  let targets = Args.targetsOrDefault args
  match args with
  | { Help = true } ->
    printfn "%s" helpMessage
  | { Query = Some query } ->
    let client = FSharpApiSearchClient(targets, ApiLoader.databaseName)
    searchAndShowResult client query args.SearchOptions
  | { Query = None } ->
    let client = FSharpApiSearchClient(targets, ApiLoader.databaseName)
    printfn "Targets the following assemblies."
    client.TargetAssemblies |> List.iter (printfn "  %s")
    printfn "Input query or #q to quit."
    Interactive.loop client args |> ignore
  0