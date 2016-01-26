module FSharpApiSearch.QueryParser

open FSharpApiSearch.Types
open FParsec

let inline trim p = spaces >>. p .>> spaces
let inline pcharAndTrim c = pchar c |> trim

module SignatureParser =
  let name = regex @"\w+"

  let signature, signatureRef = createParserForwardedToRef()

  let identity = name |>> (fun name -> Identity name) |> trim
  let variable = pchar ''' >>. name |>> (fun name -> Variable (Source.Query, name)) |> trim  

  let idOrVariable = attempt identity <|> variable

  let dotNetGeneric = idOrVariable .>>. between (pcharAndTrim '<') (pcharAndTrim '>') (sepBy1 signature (pchar ',')) |>> (fun (id, parameter) -> Generic (id, parameter))

  let term1 =
    choice [
      attempt dotNetGeneric
      attempt (between (pcharAndTrim '(') (pcharAndTrim ')') signature)
      idOrVariable
    ]
  
  let mlMultiGenericParameter = between (pcharAndTrim '(') (pcharAndTrim ')') (sepBy1 signature (pchar ','))
  let mlSingleGenericParameter = term1 |>> List.singleton
  let mlGenericParameter = attempt mlMultiGenericParameter <|> mlSingleGenericParameter
  let mlGeneric = mlGenericParameter .>>. idOrVariable |>> (fun (parameter, id) -> Generic (id, parameter))

  let term2 = choice [ attempt mlGeneric; term1 ]

  let maybeTuple t = sepBy1 t (pstring "*") |>> function [ x ] -> x | xs -> Tuple xs

  let term3 = maybeTuple term2

  let maybeArrow t = sepBy1 t (pstring "->") |>> function [ x ] -> x | xs -> Arrow xs

  let term4 = maybeArrow term3

  do signatureRef := term4

let memberName = many1 (letter <|> anyOf "_'") |> trim |>> (fun xs -> System.String(Array.ofList xs))
let wildcard = pstring "_" |> trim >>% Wildcard

let wildCardOrSignature = attempt wildcard <|> (SignatureParser.signature |>> SignatureQuery)
let nameQuery = memberName .>> pstring ":" .>>. wildCardOrSignature |>> (fun (name, sigPart) -> ByName (name, sigPart))

let signatureQuery = SignatureParser.signature |>> BySignature
let query = attempt nameQuery <|> signatureQuery

let parseSignature (sigStr: string) =
  match runParserOnString (SignatureParser.signature .>> eof) () "" sigStr with
  | Success (s, _, _) -> s
  | Failure (msg, _, _) -> failwithf "%s" msg

let parse (queryStr: string) =
  match runParserOnString (query .>> eof) () "" queryStr with
  | Success (query, _, _) -> { OriginalString = queryStr; Method = query }
  | Failure (msg, _, _) -> failwithf "%s" msg