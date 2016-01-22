module FSharpApiSearch.QueryParser

open FSharpApiSearch.Types
open FParsec

let identity = regex @"\w+"

let inline trim p = spaces >>. p .>> spaces
let inline pcharAndTrim c = pchar c |> trim

let query, queryRef = createParserForwardedToRef()

let ptype = identity |>> (fun name -> Type name) |> trim
let variable = pchar ''' .>>. identity |>> (fun (prefix, name) -> Variable (Source.Query, string prefix + name)) |> trim  
let typeIdentity = attempt variable <|> ptype

let dotNetGeneric = typeIdentity .>>. between (pcharAndTrim '<') (pcharAndTrim '>') (sepBy1 query (pchar ',')) |>> (fun (id, parameter) -> Generic (id, parameter))

let term1 =
  choice [
    attempt dotNetGeneric
    attempt (between (pcharAndTrim '(') (pcharAndTrim ')') query)
    (typeIdentity |>> TypeIdentity)
  ]
  
let mlMultiGenericParameter = between (pcharAndTrim '(') (pcharAndTrim ')') (sepBy1 query (pchar ','))
let mlSingleGenericParameter = term1 |>> List.singleton
let mlGenericParameter = attempt mlMultiGenericParameter <|> mlSingleGenericParameter
let mlGeneric = mlGenericParameter .>>. typeIdentity |>> (fun (parameter, id) -> Generic (id, parameter))

let term2 = choice [ attempt mlGeneric; term1 ]

let maybeTuple t = sepBy1 t (pstring "*") |>> function [ x ] -> x | xs -> Tuple xs

let term3 = maybeTuple term2

let maybeArrow t = sepBy1 t (pstring "->") |>> function [ x ] -> x | xs -> Arrow xs

let term4 = maybeArrow term3

do queryRef := term4

let parse (queryStr: string) =
  match runParserOnString (query .>> eof) () "" queryStr with
  | Success (q, _, _) -> { OriginalString = queryStr; Query = q }
  | Failure (msg, _, _) -> failwithf "%s" msg