module FSharpApiSearch.Console.Program

open FSharpApiSearch
open System.Diagnostics
open System
open System.IO
open Microsoft.FSharp.Compiler.SourceCodeServices

let searchAndShowResult (client: Lazy<FSharpApiSearchClient>) (query: string) args =
  let opt = args.SearchOptions
  let client = client.Value
  let results =
    client.Search(query, opt)
    |> client.Sort
  results
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

exception InitializeFailed of Msg:string * Cause:exn option

let showException (arg: Args) (ex: exn) =
  match ex with
  | :? InitializeFailed as ex ->
    printfn "%s" ex.Msg
    match arg.StackTrace, ex.Cause with
    | Enabled, Some cause -> printfn "%A" cause
    | _ -> ()
  | ex ->
    match arg.StackTrace with
    | Enabled -> printfn "%A" ex
    | Disabled -> printfn "%s" ex.Message

let createClient (targets: string list) (databasePath: string) =
  lazy (
    if File.Exists(databasePath) = false then
      raise (InitializeFailed (@"The database is not found. Create the database by executing ""FSharpApiSearch.Database.exe"".", None))
    else
      try
        FSharpApiSearchClient(targets, ApiLoader.loadFromFile databasePath)
      with
        ex ->
          raise (InitializeFailed (@"It failed to load the database. Create the database by executing ""FSharpApiSearch.Database.exe"".", Some ex))
  )

module Interactive =
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

  let ShowXmlDocument = { Get = (fun x -> x.ShowXmlDocument); Set = (fun value x -> { x with ShowXmlDocument = value }) }
  let StackTrace = { Get = (fun x -> x.StackTrace); Set = (fun value x -> { x with StackTrace = value }) }

  let helpMessage = """
FSharpApiSearch.Console interactive mode directive:
  #targets
      Print target assemblies.
  #respect-name-difference [enable|disable]
      Enables or disables to respect the variable name difference in the query.
  #greedy-matching [enable|disable]
      Enables or disables the greedy matching.
  #ignore-param-style [enable|disable]
      Enables of disables to ignore the difference of the parameter style.
      The parameter style refers to curried parameter, multi parameter and tuple parameter.
  #ignore-case
      Enables or disables to use ignore case matching.
  #xmldoc [enable|disable]
      Enables or disables to show xml document of API.
  #stacktrace [enable|disable]
      Enables or disables stacktrace output if an exception occurs.
  #help
      Print this message.
  #q
      Quit interactive mode.

  If ommit the option value, print the current value of the option."""

  let rec loop (client: Lazy<FSharpApiSearchClient>) arg =
    printf "> "
    match Console.ReadLine().TrimEnd(';') with
    | "#q" -> arg
    | "#targets" ->
      client.Value.TargetAssemblies |> List.iter (printfn "  %s")
      Console.WriteLine()
      loop client arg
    | OptionSetting "#respect-name-difference" SearchOptions.RespectNameDifference arg.SearchOptions newOpt -> loop client { arg with SearchOptions = newOpt }
    | OptionSetting "#greedy-matching" SearchOptions.GreedyMatching arg.SearchOptions newOpt -> loop client { arg with SearchOptions = newOpt }
    | OptionSetting "#ignore-param-style" SearchOptions.IgnoreParameterStyle arg.SearchOptions newOpt -> loop client { arg with SearchOptions = newOpt }
    | OptionSetting "#ignore-case" SearchOptions.IgnoreCase arg.SearchOptions newOpt -> loop client { arg with SearchOptions = newOpt }
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
      with
        | :? InitializeFailed -> reraise()
        | ex -> showException arg ex
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
  --ignore-param-style[+|-]
      Enables or disables to ignore the difference of the parameter style.
      The parameter style refers to curried parameter, multi parameter and tuple parameter.
      The default is enabled.
  --ignore-case[+|-]
      Enables or disables to use ignore case matching.
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

let initBackground (args: Args) (client: Lazy<FSharpApiSearchClient>) =
  async { try do client.Force() |> ignore with _ -> () }
  |> Async.Start

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
      initBackground args client
      printfn "Input query, #help to print help or #q to quit."
      Interactive.loop client args |> ignore
    with
      ex -> showException args ex
  0