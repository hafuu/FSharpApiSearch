module internal FSharpApiSearch.QueryParser

open FParsec

open FSharpApiSearch.SpecialTypes

let inline trim p = spaces >>. p .>> spaces
let inline pcharAndTrim c = pchar c |> trim

module FSharpSignatureParser =
  let pidentifier =
    let head = letter <|> pchar '_'
    let tail = letter <|> digit <|> anyOf "_'"
    head .>>. manyChars tail |>> (fun (h, t) -> string h + t) <?> "identifier"
  
  let partialName = sepBy1 pidentifier (pchar '.') |>> (List.map (fun n -> { FSharpName = n; InternalFSharpName = n; GenericParametersForDisplay = [] } ) >> List.rev)

  let fsharpSignature, fsharpSignatureRef = createParserForwardedToRef()

  let identity = partialName |>> (function name -> Identity (PartialIdentity { Name = name; GenericParameterCount = 0 })) |> trim <?> "identity"
  let variable = pchar ''' >>. pidentifier |>> (function name -> Variable (VariableSource.Query, { Name = name; IsSolveAtCompileTime = false })) |> trim <?> "variable"
  let wildcard = pchar '?' >>. opt pidentifier |>> Wildcard |> trim <?> "wildcard"

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
          | n :: tail -> { n with GenericParametersForDisplay = List.init parameterCount (fun n -> { Name = sprintf "T%d" n; IsSolveAtCompileTime = false }) } :: tail
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
  
  let arraySymbol =
    let t = { Name = "T"; IsSolveAtCompileTime = false }
    regex arrayRegexPattern |> trim |>> (fun array -> Identity (PartialIdentity { Name = [ { FSharpName = array; InternalFSharpName = array; GenericParametersForDisplay = [ t ] } ]; GenericParameterCount = 1 }))
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
    |>> (fun (receiver, paramsAndRet) ->
      let parameters, ret =
        match paramsAndRet with
        | Arrow xs -> (List.take (xs.Length - 1) xs), (List.last xs)
        | ret -> [], ret
      SignatureQuery.InstanceMember (Receiver = receiver, Parameters = parameters, ReturnType = ret)
    )

  let extendedFsharpSignature = choice [ attempt instanceMember <|> (fsharpSignature |>> SignatureQuery.Signature) ]

  let singleTerm = term4

let activePatternKind =
  (skipString "(||)" >>% ActivePatternKind.ActivePattern)
  <|> (skipString "(|_|)" >>% ActivePatternKind.PartialActivePattern)
let allPatterns = trim (skipString "...") >>. skipString "->" >>. FSharpSignatureParser.singleTerm .>> skipString "->" .>>. FSharpSignatureParser.singleTerm |>> ActivePatternSignature.AnyParameter
let activePattern = FSharpSignatureParser.fsharpSignature |>> ActivePatternSignature.Specified
let activePatternQuery = trim activePatternKind .>> skipString ":" .>>. (attempt allPatterns <|> activePattern) |>> (fun (kind, sig') -> QueryMethod.ByActivePattern { Kind = kind; Signature = sig' })

let opName = trim (skipChar '(') >>. many1Chars (anyOf "!%&*+-./<=>?@^|~:[]")  .>> trim (skipChar ')') |>> Microsoft.FSharp.Compiler.PrettyNaming.CompileOpName
let signatureWildcard = pstring "_" |> trim >>% SignatureQuery.Wildcard

let memberNamePartial =
  let pident =
    let head = letter <|> anyOf "_*" 
    let tail = letter <|> digit <|> anyOf "_'*"
    head .>>. manyChars tail |>> (fun (h, t) -> string h + t)
  let ctor = pstring ".ctor"
  (ctor <|> pident) <?> "identifier"

let anyOrSignature = attempt signatureWildcard <|> (FSharpSignatureParser.extendedFsharpSignature)
let nameQuery =
  let name = trim (memberNamePartial <|> opName)
  sepBy1 name (pchar '.') .>> pstring ":" .>>. anyOrSignature |>> (fun (name, sigPart) ->
    let expecteds = List.rev name |> List.map (fun n -> NameMatchMethod.ofString n)
    QueryMethod.ByName (expecteds, sigPart))

let signatureQuery = FSharpSignatureParser.extendedFsharpSignature |>> QueryMethod.BySignature

let computationExpressionSyntax = manyChars (letter <|> pchar '/') .>>. opt (pstring "!") |>> (fun (syntax, bang) -> match bang with Some bang -> syntax + bang | None -> syntax)  
let computationExpressionQuery =
  let underScore = skipString "_" >>% []
  let syntaxes = sepBy1 (trim computationExpressionSyntax) (pchar ';') |>> List.filter ((<>)"")
  let left = skipString "{" >>. trim (attempt underScore <|> syntaxes) .>> skipString "}"
  trim left .>> skipString ":" .>>. trim FSharpSignatureParser.fsharpSignature |>> (fun (syntax, t) -> QueryMethod.ByComputationExpression { Syntaxes = syntax; Type = t })

let query = choice [ attempt computationExpressionQuery;attempt activePatternQuery; attempt nameQuery; signatureQuery ]

let parseFSharpSignature (sigStr: string) =
  match runParserOnString (FSharpSignatureParser.extendedFsharpSignature .>> eof) () "" sigStr with
  | Success (s, _, _) -> s
  | Failure (msg, _, _) -> failwithf "%s" msg

let parse (queryStr: string) =
  match runParserOnString (query .>> eof) () "" queryStr with
  | Success (queryMethod, _, _) -> { OriginalString = queryStr; Method = queryMethod }: Query
  | Failure (msg, _, _) -> failwithf "%s" msg