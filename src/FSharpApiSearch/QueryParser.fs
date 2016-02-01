module FSharpApiSearch.QueryParser

open FParsec

let inline trim p = spaces >>. p .>> spaces
let inline pcharAndTrim c = pchar c |> trim

module FSharpSignatureParser =
  let name = regex @"\w+" <?> "name"

  let fsharpSignature, fsharpSignatureRef = createParserForwardedToRef()

  let strong = pchar '!'
  let identity = opt strong .>>. name |>> (function (None, name) -> Identity name | (Some _, name) -> StrongIdentity name) |> trim <?> "identity"
  let variable = opt strong .>> pchar ''' .>>. name |>> (function (None, name) -> Variable (Source.Query, name) | (Some _, name) -> StrongVariable (Source.Query, name)) |> trim <?> "variable"
  let wildcard = pchar '?' >>. opt name |>> (function Some name -> WildcardGroup name | None -> Wildcard) |> trim <?> "wildcard"

  let genericId =
    choice [
      attempt identity
      attempt variable
      wildcard
    ]

  let dotNetGeneric = genericId .>>. between (pcharAndTrim '<') (pcharAndTrim '>') (sepBy1 fsharpSignature (pchar ',')) |>> (fun (id, parameter) -> Generic (id, parameter))

  let term1 =
    choice [
      attempt dotNetGeneric
      attempt (between (pcharAndTrim '(') (pcharAndTrim ')') fsharpSignature)
      attempt identity
      attempt variable
      wildcard
    ]
  
  let mlMultiGenericParameter = between (pcharAndTrim '(') (pcharAndTrim ')') (sepBy1 fsharpSignature (pchar ','))
  let mlSingleGenericParameter = term1 |>> List.singleton
  let mlGenericParameter = attempt mlMultiGenericParameter <|> mlSingleGenericParameter
  let mlGeneric = mlGenericParameter .>>. genericId |>> (fun (parameter, id) -> Generic (id, parameter))

  let term2 = choice [ attempt mlGeneric; term1 ]

  let maybeTuple t = sepBy1 t (pstring "*") |>> function [ x ] -> x | xs -> Tuple xs

  let term3 = maybeTuple term2

  let maybeArrow t = sepBy1 t (pstring "->") |>> function [ x ] -> x | xs -> Arrow xs

  let term4 = maybeArrow term3

  do fsharpSignatureRef := term4

let memberName = many1 (letter <|> anyOf "_'") |> trim |>> (fun xs -> System.String(Array.ofList xs))
let wildcard = pstring "_" |> trim >>% AnySignature

let anyOrSignature = attempt wildcard <|> (FSharpSignatureParser.fsharpSignature |>> SignatureQuery)
let nameQuery = memberName .>> pstring ":" .>>. anyOrSignature |>> (fun (name, sigPart) -> ByName (name, sigPart))

let signatureQuery = FSharpSignatureParser.fsharpSignature |>> BySignature
let query = attempt nameQuery <|> signatureQuery

let parseFSharpSignature (sigStr: string) =
  match runParserOnString (FSharpSignatureParser.fsharpSignature .>> eof) () "" sigStr with
  | Success (s, _, _) -> s
  | Failure (msg, _, _) -> failwithf "%s" msg

let parse (queryStr: string) =
  match runParserOnString (query .>> eof) () "" queryStr with
  | Success (query, _, _) -> { OriginalString = queryStr; Method = query }
  | Failure (msg, _, _) -> failwithf "%s" msg