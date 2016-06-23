module FSharpApiSearch.QueryParser

open FParsec

open FSharpApiSearch.SpecialTypes

let inline trim p = spaces >>. p .>> spaces
let inline pcharAndTrim c = pchar c |> trim

module FSharpSignatureParser =
  let name = regex @"\w+" <?> "name"
  let partialName = sepBy1 name (pchar '.') |>> (List.map (fun n -> { FSharpName = n; GenericParametersForDisplay = [] } ) >> List.rev)

  let fsharpSignature, fsharpSignatureRef = createParserForwardedToRef()

  let identity = partialName |>> (function name -> Identity (PartialIdentity { Name = name; GenericParameterCount = 0 })) |> trim <?> "identity"
  let variable = pchar ''' >>. name |>> (function name -> Variable (VariableSource.Query, name)) |> trim <?> "variable"
  let wildcard = pchar '?' >>. opt name |>> Wildcard |> trim <?> "wildcard"

  let genericId =
    choice [
      attempt identity
      attempt variable
      wildcard
    ]

  let createGeneric id parameters =
    let parameterCount = List.length parameters
    let id =
      match id with
      | Identity (PartialIdentity p) ->
        let newName =
          match p.Name with
          | [] -> []
          | n :: tail -> { n with GenericParametersForDisplay = List.init parameterCount (sprintf "T%d") } :: tail
        Identity (PartialIdentity { p with Name = newName; GenericParameterCount = parameterCount })
      | other -> other
    Generic (id, parameters)

  let dotNetGeneric = genericId .>>. between (pcharAndTrim '<') (pcharAndTrim '>') (sepBy1 fsharpSignature (pchar ',')) |>> (fun (id, parameter) -> createGeneric id parameter)

  let term1 =
    choice [
      attempt dotNetGeneric
      attempt (between (pcharAndTrim '(') (pcharAndTrim ')') fsharpSignature)
      attempt identity
      attempt variable
      wildcard
    ]
  
  let arraySymbol = regex arrayRegexPattern |> trim |>> (fun array -> Identity (PartialIdentity { Name = [ { FSharpName = array; GenericParametersForDisplay = [ "T" ] } ]; GenericParameterCount = 1 }))
  let maybeArray t =
    t
    .>>. many arraySymbol
    |>> (function
      | (t, []) -> t
      | (t, x :: xs) ->
        List.fold (fun x array -> createGeneric array [ x ]) (createGeneric x [ t ]) xs)
  let term2 = maybeArray term1

  let mlMultiGenericParameter = between (pcharAndTrim '(') (pcharAndTrim ')') (sepBy1 fsharpSignature (pchar ','))
  let mlSingleGenericParameter = term2 |>> List.singleton
  let mlLeftGenericParameter = attempt mlMultiGenericParameter <|> mlSingleGenericParameter
  let foldGeneric parameter ids =
    let rec foldGeneric' acc = function
      | id :: rest -> foldGeneric' (createGeneric id [ acc ]) rest
      | [] -> acc
    foldGeneric' (createGeneric (List.head ids) parameter) (List.tail ids)
  let mlGeneric = mlLeftGenericParameter .>>. many1 genericId |>> (fun (parameter, ids) -> foldGeneric parameter ids)

  let term3 = choice [ attempt mlGeneric; term2 ]

  let maybeTuple t = sepBy1 t (pstring "*") |>> function [ x ] -> x | xs -> Tuple xs

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
      SignatureQuery.InstanceMember (Receiver = receiver, Arguments = args, ReturnType = ret)
    )

  let extendedFsharpSignature = choice [ attempt instanceMember <|> (fsharpSignature |>> SignatureQuery.Signature) ]

  let singleTerm = term4

let activePatternKind =
  (skipString "(||)" >>% ActivePatternKind.ActivePattern)
  <|> (skipString "(|_|)" >>% ActivePatternKind.PartialActivePattern)
let allPatterns = trim (skipString "...") >>. skipString "->" >>. FSharpSignatureParser.singleTerm .>> skipString "->" .>>. FSharpSignatureParser.singleTerm |>> ActivePatternSignature.AnyParameter
let activePattern = FSharpSignatureParser.fsharpSignature |>> ActivePatternSignature.Specified
let activePatternQuery = trim activePatternKind .>> skipString ":" .>>. (attempt allPatterns <|> activePattern) |>> (fun (kind, sig') -> QueryMethod.ByActivePattern { Kind = kind; Signature = sig' })

let memberName = many1 (letter <|> anyOf "_'") |> trim |>> (fun xs -> System.String(Array.ofList xs))
let wildcard = pstring "_" |> trim >>% SignatureQuery.Wildcard

let anyOrSignature = attempt wildcard <|> (FSharpSignatureParser.extendedFsharpSignature)
let nameQuery = memberName .>> pstring ":" .>>. anyOrSignature |>> (fun (name, sigPart) -> QueryMethod.ByName (name, sigPart))

let signatureQuery = FSharpSignatureParser.extendedFsharpSignature |>> QueryMethod.BySignature

let query = choice [ attempt activePatternQuery; attempt nameQuery; signatureQuery ]

let parseFSharpSignature (sigStr: string) =
  match runParserOnString (FSharpSignatureParser.extendedFsharpSignature .>> eof) () "" sigStr with
  | Success (s, _, _) -> s
  | Failure (msg, _, _) -> failwithf "%s" msg

let parse (queryStr: string) =
  match runParserOnString (query .>> eof) () "" queryStr with
  | Success (queryMethod, _, _) -> { OriginalString = queryStr; Method = queryMethod }: Query
  | Failure (msg, _, _) -> failwithf "%s" msg