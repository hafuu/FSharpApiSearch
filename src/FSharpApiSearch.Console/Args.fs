namespace FSharpApiSearch.Console

open FSharpApiSearch
open System.Diagnostics
open System
open Microsoft.FSharp.Compiler.SourceCodeServices

type Args = {
  Query: string option
  Targets: string list
  References: string list
  SearchOptions: SearchOptions
  StackTrace: OptionStatus
  Help: bool
}

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Args =
  let empty = { Query = None; Targets = []; References = []; SearchOptions = SearchOptions.defaultOptions; StackTrace = Disabled; Help = false }

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
    | Status "--stacktrace" v :: rest -> parse { arg with StackTrace = boolToOptionStatus v } rest
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