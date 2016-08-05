namespace FSharpApiSearch.Console

open FSharpApiSearch
open System.Diagnostics
open System
open Microsoft.FSharp.Compiler.SourceCodeServices

type Args = {
  Query: string option
  Targets: string list
  SearchOptions: SearchOptions
  ShowXmlDocument: OptionStatus
  StackTrace: OptionStatus
  Help: bool
}

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Args =
  open FSharpApiSearch.CommandLine

  let empty =
    let defaultOpt = { SearchOptions.defaultOptions with Parallel = Enabled }
    { Query = None; Targets = []; SearchOptions = defaultOpt; ShowXmlDocument = Disabled; StackTrace = Disabled; Help = false }

  let boolToOptionStatus = function true -> Enabled | false -> Disabled

  let rec parse arg = function
    | Status "--respect-name-difference" v :: rest -> parse { arg with SearchOptions = { arg.SearchOptions with RespectNameDifference = boolToOptionStatus v } } rest
    | Status "--greedy-matching" v :: rest -> parse { arg with SearchOptions = { arg.SearchOptions with GreedyMatching = boolToOptionStatus v } } rest
    | Status "--ignore-param-style" v :: rest -> parse { arg with SearchOptions = { arg.SearchOptions with IgnoreParameterStyle = boolToOptionStatus v } } rest
    | (KeyValue "--target" t | KeyValue "-t" t) :: rest -> parse { arg with Targets = t :: arg.Targets } rest
    | Status "--xmldoc" v :: rest -> parse { arg with ShowXmlDocument = boolToOptionStatus v } rest
    | Status "--stacktrace" v :: rest -> parse { arg with StackTrace = boolToOptionStatus v } rest
    | ("--help" | "-h") :: rest -> parse { arg with Help = true } rest
    | query :: rest ->
      let q =
        match arg.Query with
        | None -> Some query
        | Some _ as q -> q
      parse { arg with Query = q } rest
    | [] -> arg

  let targetsOrDefault arg =
    if List.isEmpty arg.Targets then
      FSharpApiSearchClient.DefaultTargets
    else
      arg.Targets