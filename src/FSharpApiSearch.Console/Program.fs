module FSharpApiSearch.Console.Program

open FSharpApiSearch
open FSharpApiSearch.Printer
open System.Diagnostics
open System
open System.IO
open Microsoft.FSharp.Compiler.SourceCodeServices

let fsharpPrinter args (result: Result) =
  Console.ForegroundColor <- ConsoleColor.DarkGray
  Console.Write(FSharp.printAccessPath None result.Api)
  Console.Write(".")
  Console.ResetColor()
  Console.Write(FSharp.printApiName result.Api)
  Console.Write(" : ")
  Console.Write(FSharp.printSignature result.Api)
  Console.ForegroundColor <- ConsoleColor.DarkGray
  Console.Write(", {0}", FSharp.printKind result.Api)
  Console.Write(", {0}", result.AssemblyName)
  if args.ShowDistance = Enabled then Console.Write(", {0}", result.Distance)
  Console.WriteLine()
  match FSharp.tryPrintTypeConstraints result.Api with
  | Some constraints -> Console.WriteLine(sprintf "  %s" constraints)
  | None -> ()
  Console.ResetColor()
  
let csharpPrinter args (result: Result) =
  Console.ForegroundColor <- ConsoleColor.DarkGray
  Console.Write(CSharp.printAccessPath None result.Api)
  Console.Write(".")
  Console.ResetColor()
  Console.Write(CSharp.printApiName result.Api)
  Console.Write(CSharp.printSignature result.Api)
  Console.ForegroundColor <- ConsoleColor.DarkGray
  Console.Write(", {0}", CSharp.printKind result.Api)
  Console.Write(", {0}", result.AssemblyName)
  if args.ShowDistance = Enabled then Console.Write(", {0}", result.Distance)
  Console.WriteLine()
  match CSharp.tryPrintTypeConstraints result.Api with
  | Some constraints -> Console.WriteLine(sprintf "  %s" constraints)
  | None -> ()
  Console.ResetColor()

let getPrinter (args: Args) =
  match SearchOptions.Language.Get args.SearchOptions with
  | FSharp -> fsharpPrinter args
  | CSharp -> csharpPrinter args

let searchAndShowResult (client: Lazy<FSharpApiSearchClient>) (query: string) args =
  let opt = args.SearchOptions
  let client = client.Value
  let printResult = getPrinter args
  let printXmlDoc result =
    match args.ShowXmlDocument, result.Api.Document with
    | Enabled, Some doc ->
      Console.ForegroundColor <- ConsoleColor.DarkGray
      Console.WriteLine(doc)
      Console.ResetColor()
    | _ -> ()
  let results =
    client.Search(query, opt)
    |> client.Sort
  results
  |> Seq.iter (fun result ->
    printResult result
    printXmlDoc result
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
        FSharpApiSearchClient(targets, Database.loadFromFile databasePath)
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

  let tryParseInt (x: string) =
    match Int32.TryParse(x) with
    | true, n -> Some n
    | false, _ -> None

  let setting tryParse (name: string) (lens: Lens<_, _>) target (str: string) =
    match str.Split([| ' ' |], 2, StringSplitOptions.RemoveEmptyEntries) with
    | [| key; value |] when key = name ->
      match tryParse value with
      | Some value -> Some (lens.Set value target)
      | None -> printfn "invalid value"; Some target
    | [| key |] when key = name ->
      printfn "%A" (lens.Get target)
      Some target
    | _ -> None

  let (|OptionSetting|_|) (name: string) (lens: Lens<_, _>) target (str: string) =
    setting tryParseOptionStatus name lens target str

  let (|NumberSetting|_|) (name: string) (lens: Lens<_, _>) target (str: string) =
    setting tryParseInt name lens target str

  let (|LanguageSetting|_|) (name: string) (lens: Lens<_, _>) target (str: string) =
    setting Language.tryParse name lens target str

  let ShowXmlDocument = { Get = (fun x -> x.ShowXmlDocument); Set = (fun value x -> { x with ShowXmlDocument = value }) }
  let ShowDistance = { Get = (fun x -> x.ShowDistance); Set = (fun value x -> { x with ShowDistance = value }) }
  let StackTrace = { Get = (fun x -> x.StackTrace); Set = (fun value x -> { x with StackTrace = value }) }

  let helpMessage = """
FSharpApiSearch.Console interactive mode directive:
  #targets
      Prints target assemblies.
  #respect-name-difference [enable|disable]
      Enables or disables to respect the variable name difference in the query.
  #greedy-matching [enable|disable]
      Enables or disables the greedy matching.
  #ignore-param-style [enable|disable]
      Enables of disables to ignore the difference of the parameter style.
      The parameter style refers to curried parameter, multi parameter and tuple parameter.
  #ignore-case [enable|disable]
      Enables or disables to use ignore case matching.
  #swap-order [enable|disable]
      Enables of disables to swap parameters and tuple elements.
  #complement [enable|disable]
      Enables of disables to complement parameters and tuple elements.
  #single-letter-as-variable [enable|disable]
      Enables or disables to convert a single letter to a type variable name.
  #xmldoc [enable|disable]
      Enables or disables to show xml document of API.
  #language [F#|fsharp|C#|csharp]
      Specifies the language.
  #stacktrace [enable|disable]
      Enables or disables stacktrace output if an exception occurs.
  #clear
      Clears the console buffer.
  #help
      Prints this message.
  #q
      Quits interactive mode.

  If ommit the option value, print the current value of the option."""

  let rec loop (client: Lazy<FSharpApiSearchClient>) arg =
    printf "> "
    match Console.ReadLine().TrimEnd(';') with
    | "#q" -> arg
    | "#targets" ->
      client.Value.Targets |> List.iter (fun t -> printfn "  %s : %d" t.AssemblyName t.PublicApiNumber)
      Console.WriteLine()
      loop client arg
    | OptionSetting "#respect-name-difference" SearchOptions.RespectNameDifference arg.SearchOptions newOpt -> loop client { arg with SearchOptions = newOpt }
    | OptionSetting "#greedy-matching" SearchOptions.GreedyMatching arg.SearchOptions newOpt -> loop client { arg with SearchOptions = newOpt }
    | OptionSetting "#ignore-param-style" SearchOptions.IgnoreParameterStyle arg.SearchOptions newOpt -> loop client { arg with SearchOptions = newOpt }
    | OptionSetting "#ignore-case" SearchOptions.IgnoreCase arg.SearchOptions newOpt -> loop client { arg with SearchOptions = newOpt }
    | OptionSetting "#swap-order" SearchOptions.SwapOrder arg.SearchOptions newOpt -> loop client { arg with SearchOptions = newOpt }
    | OptionSetting "#complement" SearchOptions.Complement arg.SearchOptions newOpt -> loop client { arg with SearchOptions = newOpt }
    | OptionSetting "#single-letter-as-variable" SearchOptions.SingleLetterAsVariable arg.SearchOptions newOpt -> loop client { arg with SearchOptions = newOpt }
    | LanguageSetting "#language" SearchOptions.Language arg.SearchOptions newOpt -> loop client { arg with SearchOptions = newOpt }
    | OptionSetting "#xmldoc" ShowXmlDocument arg newArg -> loop client newArg
    | OptionSetting "#distance" ShowDistance arg newArg -> loop client newArg
    | OptionSetting "#stacktrace" StackTrace arg newArg -> loop client newArg
    | "#clear" ->
      Console.Clear()
      loop client arg
    | "#help" ->
      Console.WriteLine(helpMessage)
      Console.WriteLine()
      loop client arg
    | invalidOption when invalidOption.StartsWith("#") ->
      printfn "Invalid option. If you want to search with the subtype, insert a space at the beginning of the query."
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
  --swap-order[+|-]
      Enables or disables to swap parameters and tuple elements.
      The default is enabled.
  --complement[+|-]
      Enables or disables to complement parameters and tuple elements.
      The default is enabled.
  --single-letter-as-variable[+|-]
      Enables or disables to convert a single letter to a type variable name.
      The default is enabled.
  --language:[F#|fsharp|C#|csharp]
      Specifies the language.
      The default is F#.
  --target:<assembly>, -t:<assembly>
      Specifies the assembly name of the searching target.
      If omitted, it will target 'FSharp.Core', 'mscorlib', 'System' and 'System.Core'.
  --xmldoc[+|-]
      Enables or disables to show xml document of API.
      The default is disabled.
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
      let client = createClient targets Database.databaseName
      searchAndShowResult client query args
    with
      ex -> showException args ex
  | { Query = None } ->
    try
      let client = createClient targets Database.databaseName
      initBackground args client
      printfn "Input query, #help to print help or #q to quit."
      Interactive.loop client args |> ignore
    with
      ex -> showException args ex
  0