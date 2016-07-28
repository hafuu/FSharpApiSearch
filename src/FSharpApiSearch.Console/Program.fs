module FSharpApiSearch.Console.Program

open FSharpApiSearch
open System.Diagnostics
open System
open System.IO
open Microsoft.FSharp.Compiler.SourceCodeServices

let searchAndShowResult (client: FSharpApiSearchClient) (query: string) args =
  let opt = args.SearchOptions
  let results = client.Search(query, opt)
  results
  |> Seq.sortBy (fun x -> (x.Distance, x.Api.Name.Print()))
  |> Seq.iter (fun x ->
    Console.Write(sprintf "%s: %s" (x.Api.Name.Print()) (x.Api.PrintSignature()))
    Console.ForegroundColor <- ConsoleColor.DarkGray
    Console.WriteLine(sprintf ", %s, %s" (x.Api.PrintKind()) x.AssemblyName)
    if x.Api.TypeConstraints.IsEmpty = false then
      Console.WriteLine(sprintf "  %s" (x.Api.PrintTypeConstraints()))
    match args.ShowXmlDocument, x.Api.Document with
    | Enabled, Some doc -> Console.WriteLine(doc)
    | _ -> ()
    Console.ResetColor()
  )
  Console.WriteLine()

let showException (arg: Args) (ex: exn) =
  match arg.StackTrace with
  | Enabled -> printfn "%A" ex
  | Disabled -> printfn "%s" ex.Message

let createClient (targets: string list) (databasePath: string) =
  if File.Exists(databasePath) = false then
    failwith @"The database is not found. Create the database by executing ""FSharpApiSearch.Database.exe""."
  else
    try
      FSharpApiSearchClient(targets, ApiLoader.loadFromFile databasePath)
    with
      ex ->
        let loadFailure = Exception(@"It failed to load the database. Create the database by executing ""FSharpApiSearch.Database.exe"".", ex)
        raise loadFailure

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

  let RespectNameDifference = { Get = (fun x -> x.RespectNameDifference ); Set = (fun value x -> { x with RespectNameDifference = value }) }
  let GreedyMatching = { Get = (fun x -> x.GreedyMatching ); Set = (fun value x -> { x with GreedyMatching = value }) }
  let IgnoreArgumentStyle = { Get = (fun x -> x.IgnoreArgumentStyle); Set = (fun value x -> { x with IgnoreArgumentStyle = value }) }
  let ShowXmlDocument = { Get = (fun x -> x.ShowXmlDocument); Set = (fun value x -> { x with ShowXmlDocument = value }) }
  let StackTrace = { Get = (fun x -> x.StackTrace); Set = (fun value x -> { x with StackTrace = value }) }

  let helpMessage = """
FSharpApiSearch.Console interactive mode directive:
  #respect-name-difference [enable|disable]
      Enables or disables to respect the variable name difference in the query.
  #greedy-matching [enable|disable]
      Enables or disables the greedy matching.
  #ignore-argstyle [enable|disable]
      Enables of disables to ignore the difference of the argument style.
      The argument style refers to curried argument, multi argument and tuple argument.
  #xmldoc [enable|disable]
      Enables or disables to show xml document of API.
  #stacktrace [enable|disable]
      Enables or disables stacktrace output if an exception occurs.
  #help
      Print this message.
  #q
      Quit interactive mode.

  If ommit the option value, print the current value of the option."""

  let rec loop (client: FSharpApiSearchClient) arg =
    printf "> "
    match Console.ReadLine().TrimEnd(';') with
    | "#q" -> arg
    | OptionSetting "#respect-name-difference" RespectNameDifference arg.SearchOptions newOpt -> loop client { arg with SearchOptions = newOpt }
    | OptionSetting "#greedy-matching" GreedyMatching arg.SearchOptions newOpt -> loop client { arg with SearchOptions = newOpt }
    | OptionSetting "#ignore-argstyle" IgnoreArgumentStyle arg.SearchOptions newOpt -> loop client { arg with SearchOptions = newOpt }
    | OptionSetting "#xmldoc" ShowXmlDocument arg newArg -> loop client newArg
    | OptionSetting "#stacktrace" StackTrace arg newArg -> loop client newArg
    | "#help" ->
      Console.WriteLine(helpMessage)
      Console.WriteLine()
      loop client arg
    | invalidOption when invalidOption.StartsWith("#") ->
      printfn "invalid option"
      loop client arg
    | query ->
      try
        searchAndShowResult client query arg
      with ex ->
        showException arg ex
      loop client arg

let helpMessage = """usage: FSharpApiSearch.Console.exe <query> <options>

query: Optional. If omitted, it will start in interactive mode.
options:
  --respect-name-difference[+|-]
      Enables or disables to respect the variable name difference in the query.
      The default is enabled.
  --greedy-matching[+|-]
      Enables or disables the greedy matching.
      The default is disabled.
  --ignore-argstyle[+|-]
      Enables of disables to ignore the difference of the argument style.
      The argument style refers to curried argument, multi argument and tuple argument.
      The default is enabled.
  --target:<assembly>, -t:<assembly>
      Specifies the assembly name of the searching target.
      If omitted, it will target 'FSharp.Core', 'mscorlib', 'System' and 'System.Core'.
  --xmldoc[+|-]
      Enables or disables to show xml document of API.
      Default is disabled.
  --stacktrace[+|-]
      Enables or disables stacktrace output if an exception occurs.
      The default is disabled.
  --help, -h
      Print this message.
      
To print the help of interactive mode, enter '#help' in interactive mode."""

[<EntryPoint>]
let main argv =
  let args = Args.parse Args.empty (List.ofArray argv)
  let targets = Args.targetsOrDefault args
  match args with
  | { Help = true } ->
    printfn "%s" helpMessage
  | { Query = Some query } ->
    try
      let client = createClient targets ApiLoader.databaseName
      searchAndShowResult client query args
    with
      ex -> showException args ex
  | { Query = None } ->
    try
      let client = createClient targets ApiLoader.databaseName
      printfn "Targets the following assemblies."
      client.TargetAssemblies |> List.iter (printfn "  %s")
      printfn "Input query, #help to print help or #q to quit."
      Interactive.loop client args |> ignore
    with
      ex -> showException args ex
  0