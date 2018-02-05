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
  ShowDistance: OptionStatus
  StackTrace: OptionStatus
  Help: bool
}

module Args =
  open FSharpApiSearch.CommandLine

  let empty =
    let defaultOpt = SearchOptions.defaultOptions |> SearchOptions.Parallel.Set Enabled
    { Query = None; Targets = []; SearchOptions = defaultOpt; ShowXmlDocument = Disabled; ShowDistance = Disabled; StackTrace = Disabled; Help = false }

  let boolToOptionStatus = function true -> Enabled | false -> Disabled

  let rec parse arg = function
    | Status "--respect-name-difference" v :: rest -> parse { arg with SearchOptions = SearchOptions.RespectNameDifference.Set (boolToOptionStatus v) arg.SearchOptions } rest
    | Status "--greedy-matching" v :: rest -> parse { arg with SearchOptions = SearchOptions.GreedyMatching.Set (boolToOptionStatus v) arg.SearchOptions } rest
    | Status "--ignore-param-style" v :: rest -> parse { arg with SearchOptions = SearchOptions.IgnoreParameterStyle.Set (boolToOptionStatus v) arg.SearchOptions } rest
    | Status "--ignore-case" v :: rest -> parse { arg with SearchOptions = SearchOptions.IgnoreCase.Set (boolToOptionStatus v) arg.SearchOptions } rest
    | Status "--substring" v :: rest -> parse { arg with SearchOptions = SearchOptions.Substring.Set (boolToOptionStatus v) arg.SearchOptions } rest
    | Status "--swap-order" v :: rest -> parse { arg with SearchOptions = SearchOptions.SwapOrder.Set (boolToOptionStatus v) arg.SearchOptions } rest
    | Status "--complement" v :: rest -> parse { arg with SearchOptions = SearchOptions.Complement.Set (boolToOptionStatus v) arg.SearchOptions } rest
    | Status "--single-letter-as-variable" v :: rest -> parse { arg with SearchOptions = SearchOptions.SingleLetterAsVariable.Set (boolToOptionStatus v) arg.SearchOptions } rest
    | Language "--language" lang :: rest -> parse { arg with SearchOptions = SearchOptions.Language.Set lang arg.SearchOptions } rest
    | (KeyValue "--target" t | KeyValue "-t" t) :: rest -> parse { arg with Targets = t :: arg.Targets } rest
    | Status "--xmldoc" v :: rest -> parse { arg with ShowXmlDocument = boolToOptionStatus v } rest
    | Status "--distance" v :: rest -> parse { arg with ShowDistance = boolToOptionStatus v } rest
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