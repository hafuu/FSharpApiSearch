module FSharpApiSearch.QueryParser

open FParsec

let inline trim p = spaces >>. p .>> spaces
let inline pcharAndTrim c = pchar c |> trim

let source = Source.Query

module FSharpSignatureParser =
  let name = regex @"\w+" <?> "name"
  let partialName = sepBy1 name (pchar '.') |>> fun xs -> PartialName (List.rev xs)

  let fsharpSignature, fsharpSignatureRef = createParserForwardedToRef()

  let strong = pchar '!'
  let identity = opt strong .>>. partialName |>> (function (None, name) -> Identity name | (Some _, name) -> StrongIdentity name) |> trim <?> "identity"
  let variable = opt strong .>> pchar ''' .>>. name |>> (function (None, name) -> Variable (source, name) | (Some _, name) -> StrongVariable (source, name)) |> trim <?> "variable"
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
  
  let arraySymbol = regex arrayRegexPattern |> trim
  let maybeArray t =
    t
    .>>. many arraySymbol
    |>> (function
      | (t, []) -> t
      | (t, x :: xs) ->
        let x = Signature.partialName x
        let xs = xs |> List.map Signature.partialName
        List.fold (fun x array -> Generic (Identity array, [ x ])) (Generic (Identity x, [ t ])) xs)
  let term2 = maybeArray term1

  let mlMultiGenericParameter = between (pcharAndTrim '(') (pcharAndTrim ')') (sepBy1 fsharpSignature (pchar ','))
  let mlSingleGenericParameter = term2 |>> List.singleton
  let mlLeftGenericParameter = attempt mlMultiGenericParameter <|> mlSingleGenericParameter
  let foldGeneric parameter ids =
    let rec foldGeneric' acc = function
      | id :: rest -> foldGeneric' (Generic (id, [ acc ])) rest
      | [] -> acc
    foldGeneric' (Generic (List.head ids, parameter)) (List.tail ids)
  let mlGeneric = mlLeftGenericParameter .>>. many1 genericId |>> (fun (parameter, ids) -> foldGeneric parameter ids)

  let term3 = choice [ attempt mlGeneric; term2 ]

  let maybeTuple t = sepBy1 t (pstring "*") |>> function [ x ] -> x | xs -> Signature.tuple xs

  let term4 = maybeTuple term3

  let maybeArrow t = sepBy1 t (pstring "->") |>> function [ x ] -> x | xs -> Arrow xs

  let term5 = maybeArrow term4

  do fsharpSignatureRef := term5

  let instanceMember =
    fsharpSignature .>> pstring "=>" .>>. fsharpSignature
    |>> (fun (receiver, argsAndRet) ->
      let args, ret =
        match argsAndRet with
        | Arrow xs -> (List.take (xs.Length - 1) xs), (List.last xs)
        | ret -> [], ret
      InstanceMember { Source = source; Receiver = receiver; Arguments = args; ReturnType = ret }
    )

  let extendedFsharpSignature = choice [ attempt instanceMember <|> fsharpSignature ]

let memberName = many1 (letter <|> anyOf "_'") |> trim |>> (fun xs -> System.String(Array.ofList xs))
let wildcard = pstring "_" |> trim >>% AnySignature

let anyOrSignature = attempt wildcard <|> (FSharpSignatureParser.extendedFsharpSignature |>> SignatureQuery)
let nameQuery = memberName .>> pstring ":" .>>. anyOrSignature |>> (fun (name, sigPart) -> ByName (name, sigPart))

let signatureQuery = FSharpSignatureParser.extendedFsharpSignature |>> BySignature
let query = attempt nameQuery <|> signatureQuery

let parseFSharpSignature (sigStr: string) =
  match runParserOnString (FSharpSignatureParser.extendedFsharpSignature .>> eof) () "" sigStr with
  | Success (s, _, _) -> s
  | Failure (msg, _, _) -> failwithf "%s" msg

let parse (queryStr: string) =
  match runParserOnString (query .>> eof) () "" queryStr with
  | Success (query, _, _) -> { OriginalString = queryStr; Method = query }
  | Failure (msg, _, _) -> failwithf "%s" msg