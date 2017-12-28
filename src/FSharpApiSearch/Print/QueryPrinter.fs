module FSharpApiSearch.QueryPrinter

module internal Impl =
  let collectPosition (x: LowType) =
    let result = ResizeArray()
    let add x = result.Add(x)
    let rec f = function
      | Wildcard (_, p) -> add p
      | Variable (_, _, p) -> add p
      | Identifier (_, p) -> add p
      | Arrow ((ps, ret), p) -> List.iter f ps; f ret; add p
      | Tuple ({ Elements = xs }, p) -> List.iter f xs; add p
      | Generic (id, args, p) -> f id; List.iter f args; add p
      | ByRef (_, t, p) -> f t; add p
      | Subtype (t, p) -> f t; add p
      | Choice (o, xs, p) -> f o; List.iter f xs; add p
      | TypeAbbreviation _ | Delegate _ -> ()
      | LoadingType _ -> Name.loadingNameError()
    f x
    result.ToArray()

  let collectQueryId (query: Query) =
    let collect = function
      | SignatureQuery.Signature s -> collectPosition s
      | SignatureQuery.Wildcard _ -> [||]
    match query.Method with
    | QueryMethod.ByName (_, q) -> collect q
    | QueryMethod.BySignature q -> collect q
    | QueryMethod.ByNameOrSignature (_, q) -> collect q
    | QueryMethod.ByActivePattern _ -> [||]
    | QueryMethod.ByComputationExpression _ -> [||]

  let rangeToPrint (ps: Position[]) =
    ps
    |> Array.choose (function
      | AtQuery (Some id, range) -> Some (id, range)
      | AtQuery (None, _) -> None
      | AtSignature _ -> None
      | Unknown _ -> None
    )
    |> Array.groupBy (fun (id, _) -> id)
    |> Array.map (fun (id, values) ->
      let range = {
        Begin = values |> Array.map (fun (_, range) -> range.Begin) |> Array.min
        End = values |> Array.map (fun (_, range) -> range.End) |> Array.max
      }
      (id, range)
    )
    |> Array.sortBy (fun (id, _) -> id)

  let split (query: string) (ranges: (QueryId * QueryRange)[]) : (string * QueryId option)[] =
    let result = ResizeArray()
    let mutable index = 0

    for (queryId, range) in ranges do
      if index <> range.Begin then
        result.Add(query.[index..(range.Begin - 1)], None)
    
      result.Add(query.[range.Begin..(range.End - 1)], Some queryId)
      index <- range.End

    if index < query.Length then
      result.Add(query.[index..(query.Length - 1)], None)
  
    result.ToArray()

open Impl

let print (query: Query) (printer: QueryPrinter<_>) =
  collectQueryId query
  |> rangeToPrint
  |> split query.OriginalString
  |> Array.iter (fun (str, queryId) ->
    match queryId with
    | Some id ->
      printer.BeginPrintType(id)
      printer.Write(str)
      printer.EndPrintType(id)
    | None ->
      printer.Write(str)
  )
  printer