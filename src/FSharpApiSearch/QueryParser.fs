module internal FSharpApiSearch.QueryParser

open FParsec

open FSharpApiSearch.SpecialTypes

let inline trim p = spaces >>. p .>> spaces
let inline pcharAndTrim c = pchar c |> trim
let inline pstringAndTrim s = pstring s |> trim

let inline sepBy2 p sep = p .>> sep .>>. sepBy1 p sep |>> (fun (x, xs) -> x :: xs)

let compose firstParser (ps: _ list) =
  let compose' prevParser p =
    let self, selfRef = createParserForwardedToRef()
    do selfRef := p self prevParser
    attempt self <|> prevParser
  List.fold compose' (compose' firstParser ps.Head) ps.Tail

let parray =
  between (pchar '[') (pchar ']') (manyChars (pchar ',')) |>> fun x -> "[" + x + "]"

module FSharp =
  let struct' = "struct"
  let keywords = [
    struct'
  ]
  let pidentifier =
    let head = letter <|> pchar '_'
    let tail = letter <|> digit <|> anyOf "_'"
    head .>>. manyChars tail |>> (fun (h, t) -> string h + t)
    >>= (fun x -> if keywords |> List.exists ((=)x) then fail (sprintf "%s is the keyword." x) else preturn x)
    <?> "identifier"
  
  let partialName = sepBy1 pidentifier (pchar '.') |>> (List.map (fun n -> { Name = SymbolName n; GenericParameters = [] } ) >> List.rev)

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
          | n :: tail -> { n with GenericParameters = List.init parameterCount (fun n -> { Name = sprintf "T%d" n; IsSolveAtCompileTime = false }) } :: tail
        Identity (PartialIdentity { p with Name = newName; GenericParameterCount = parameterCount })
      | other -> other
    Generic (id, parameters)

  let dotNetGeneric = genericId .>>. between (pcharAndTrim '<') (pcharAndTrim '>') (sepBy1 fsharpSignature (pchar ',')) |>> (fun (id, parameter) -> createGeneric id parameter)

  let flexible =
    let prefix = pcharAndTrim '#'
    let t = attempt dotNetGeneric <|> identity
    prefix >>. t |>> Flexible

  let ptype =
    choice [
      attempt (between (pcharAndTrim '(') (pcharAndTrim ')') fsharpSignature)
      attempt dotNetGeneric
      attempt identity
      attempt variable
      attempt flexible
      wildcard
    ]
  
  let array _ typeParser =
    let arraySymbol =
      let t = { Name = "T"; IsSolveAtCompileTime = false }
      parray |> trim |>> (fun array -> Identity (PartialIdentity { Name = [ { Name = SymbolName array; GenericParameters = [ t ] } ]; GenericParameterCount = 1 }))
    typeParser
    .>>. many1 arraySymbol
    |>> fun (t, xs) -> List.fold (fun x array -> createGeneric array [ x ]) t xs

  let structTuple self typeParser =
    let elem =
      choice [
        attempt typeParser
        self
      ]
    let tupleElements = sepBy1 elem (pstring "*") |>> fun xs -> Tuple { Elements = xs; IsStruct = true }
    pstringAndTrim struct' >>. between (pcharAndTrim '(') (pcharAndTrim ')') tupleElements

  let mlGeneric _ typeParser =
    let mlMultiGenericParameter = between (pcharAndTrim '(') (pcharAndTrim ')') (sepBy1 fsharpSignature (pchar ','))
    let mlSingleGenericParameter = typeParser |>> List.singleton
    let mlLeftGenericParameter = attempt mlMultiGenericParameter <|> mlSingleGenericParameter
    let foldGeneric parameter ids =
      let rec foldGeneric' acc = function
        | id :: rest -> foldGeneric' (createGeneric id [ acc ]) rest
        | [] -> acc
      foldGeneric' (createGeneric (List.head ids) parameter) (List.tail ids)
    
    mlLeftGenericParameter .>>. many1 genericId |>> (fun (parameter, ids) -> foldGeneric parameter ids)


  let tuple _ typeParser = sepBy2 typeParser (pstring "*") |>> fun xs -> Tuple { Elements = xs; IsStruct = false }

  let arrow _ typeParser = sepBy2 typeParser (pstring "->") |>> (Arrow.ofLowTypeList >> Arrow)

  do fsharpSignatureRef := compose ptype [ array; structTuple; mlGeneric; tuple; arrow ]

  let activePatternKind =
    (skipString "(||)" >>% ActivePatternKind.ActivePattern)
    <|> (skipString "(|_|)" >>% ActivePatternKind.PartialActivePattern)
  let allPatterns =
    trim (skipString "...") >>. skipString "->" >>. fsharpSignature
    >>= function
        | Arrow ([ x ], y) -> preturn (ActivePatternSignature.AnyParameter (x, y))
        | _ -> fail "parse error"
  let activePattern = fsharpSignature |>> ActivePatternSignature.Specified
  let activePatternQuery = trim activePatternKind .>> skipString ":" .>>. (attempt allPatterns <|> activePattern) |>> (fun (kind, sig') -> QueryMethod.ByActivePattern { Kind = kind; Signature = sig' })

  let opName =
    trim (skipChar '(') >>. many1Chars (anyOf "!%&*+-./<=>?@^|~:[]")  .>> trim (skipChar ')')
    |>> (fun name ->
      let compiledOpName = Microsoft.FSharp.Compiler.PrettyNaming.CompileOpName name
      { Expected = compiledOpName; GenericParameters = []; MatchMethod = NameMatchMethod.StringCompare })

  let memberNamePartial =
    let pident =
      let head = letter <|> anyOf "_*" 
      let tail = letter <|> digit <|> anyOf "_'*"
      head .>>. manyChars tail |>> (fun (h, t) -> string h + t)
    let ctor = pstring ".ctor"
    let genericPart =
      let variable = trim (skipChar ''' >>. pidentifier)
      let xs = sepBy1 variable (pchar ',')
      between (pchar '<') (pchar '>') xs

    (ctor <|> pident) .>>. opt genericPart
    |>> (fun (n, genericPart) ->
      let expected, matchMethod = NameMatchMethod.ofString n
      { Expected = expected; GenericParameters = Option.defaultValue [] genericPart; MatchMethod = matchMethod })
    <?> "identifier"

  let signatureWildcard = pstring "_" |> trim >>% SignatureQuery.Wildcard
  let anyOrSignature = attempt signatureWildcard <|> (fsharpSignature |>> SignatureQuery.Signature)
  
  let nameQuery =
    let name = trim (memberNamePartial <|> opName)
    sepBy1 name (pchar '.') .>> pstring ":" .>>. anyOrSignature
    |>> (fun (name, sigPart) -> QueryMethod.ByName (List.rev name, sigPart))

  let signatureQuery = fsharpSignature |>> (SignatureQuery.Signature >> QueryMethod.BySignature)

  let computationExpressionSyntax = manyChars (letter <|> pchar '/') .>>. opt (pstring "!") |>> (fun (syntax, bang) -> match bang with Some bang -> syntax + bang | None -> syntax)  
  let computationExpressionQuery =
    let underScore = skipString "_" >>% []
    let syntaxes = sepBy1 (trim computationExpressionSyntax) (pchar ';') |>> List.filter ((<>)"")
    let left = skipString "{" >>. trim (attempt underScore <|> syntaxes) .>> skipString "}"
    trim left .>> skipString ":" .>>. trim fsharpSignature |>> (fun (syntax, t) -> QueryMethod.ByComputationExpression { Syntaxes = syntax; Type = t })

  let query = choice [ attempt computationExpressionQuery;attempt activePatternQuery; attempt nameQuery; signatureQuery ]

  let parse (queryStr: string) =
    match runParserOnString (query .>> eof) () "" queryStr with
    | Success (queryMethod, _, _) -> { OriginalString = queryStr; Method = queryMethod }: Query
    | Failure (msg, _, _) -> failwithf "%s" msg

module CSharp =
  let ref = "ref"
  let out = "out"
  let keywords = [
    ref
    out
  ]

  let punit = trim (skipString "void" <|> skipString "()") |>> fun _ -> SpecialTypes.LowType.Unit

  let pidentifier =
    let head = letter <|> pchar '_'
    let tail = letter <|> digit <|> anyOf "_'"
    head .>>. manyChars tail |>> (fun (h, t) -> string h + t)
    >>= (fun x -> if keywords |> List.exists ((=)x) then fail (sprintf "%s is the keyword." x) else preturn x)
    <?> "identifier"
  
  let partialName = sepBy1 pidentifier (pchar '.') |>> (List.map (fun n -> { Name = SymbolName n; GenericParameters = [] } ) >> List.rev)
  let csharpSignature, csharpSignatureRef = createParserForwardedToRef()

  let identity = partialName |>> (function name -> Identity (PartialIdentity { Name = name; GenericParameterCount = 0 })) |> trim <?> "identity"
  let wildcard = pchar '?' >>. opt pidentifier |>> Wildcard |> trim <?> "wildcard"

  let genericId =
    choice [
      attempt identity
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
          | n :: tail -> { n with GenericParameters = List.init parameterCount (fun n -> { Name = sprintf "T%d" n; IsSolveAtCompileTime = false }) } :: tail
        Identity (PartialIdentity { p with Name = newName; GenericParameterCount = parameterCount })
      | other -> other
    Generic (id, parameters)

  let generic = genericId .>>. between (pcharAndTrim '<') (pcharAndTrim '>') (sepBy1 csharpSignature (pchar ',')) |>> (fun (id, parameter) -> createGeneric id parameter)

  let flexible =
    let prefix = pcharAndTrim '#'
    let t = attempt generic <|> identity
    prefix >>. t |>> Flexible

  let ptype =
    choice [
      attempt punit
      attempt (between (pcharAndTrim '(') (pcharAndTrim ')') csharpSignature)
      attempt generic
      attempt identity
      attempt flexible
      wildcard
    ]

  let array _ typeParser =
    let arraySymbol =
      let t = { Name = "T"; IsSolveAtCompileTime = false }
      parray |> trim |>> (fun array -> Identity (PartialIdentity { Name = [ { Name = SymbolName array; GenericParameters = [ t ] } ]; GenericParameterCount = 1 }))
    typeParser
    .>>. many1 arraySymbol
    |>> fun (t, xs) -> List.foldBack (fun array x -> createGeneric array [ x ]) xs t

  let structTuple self typeParser =
    let elems = attempt self <|> typeParser
    between (pcharAndTrim '(') (pcharAndTrim ')') (sepBy2 elems (pstring ",")) |>> fun xs -> Tuple { Elements = xs; IsStruct = true }

  let byref _ typeParser =
    trim ((pstring ref <|> pstring out) .>> spaces1 .>>. typeParser)
    |>> (fun (refType, t) -> let isOut = refType = out in ByRef (isOut, t))

  let arrow _ typeParser =
    let args =
      let elems = sepBy1 typeParser (pstring ",")
      let elemsWithParen = between (pcharAndTrim '(') (pcharAndTrim ')') elems
      attempt elemsWithParen <|> attempt elems <|> (typeParser |>> List.singleton)
      |>> function
        | [ x ] -> x
        | xs -> Tuple { Elements = xs; IsStruct = false }
    sepBy2 args (pstring "->") |>> (Arrow.ofLowTypeList >> Arrow)

  do csharpSignatureRef := compose ptype [ array; structTuple; byref; arrow ]

  let rec replaceWithVariable (variableNames: Set<string>) = function
    | Identity (PartialIdentity { Name = [ { Name = SymbolName name } ] }) when Set.contains name variableNames ->
        Variable (VariableSource.Query, { Name = name; IsSolveAtCompileTime = false })
    | Generic (id, args) -> Generic (replaceWithVariable variableNames id, List.map (replaceWithVariable variableNames) args)
    | Tuple tpl -> Tuple { tpl with Elements = List.map (replaceWithVariable variableNames) tpl.Elements }
    | Arrow (ps, ret) -> Arrow (List.map (replaceWithVariable variableNames) ps, replaceWithVariable variableNames ret)
    | ByRef (isOut, t) -> ByRef (isOut, replaceWithVariable variableNames t)
    | Flexible t -> Flexible (replaceWithVariable variableNames t)
    | (Wildcard _ | Variable _ | Identity _ | TypeAbbreviation _ | Delegate _ | Choice _) as t -> t

  let signatureWildcard = pstring "_" |> trim >>% SignatureQuery.Wildcard

  let genericPart =
    let variableNames = sepBy1 (trim pidentifier) (skipChar ',')
    between (pstringAndTrim "<") (pstringAndTrim ">") variableNames

  let memberNamePartial =
    let pident =
      let head = letter <|> anyOf "_*" 
      let tail = letter <|> digit <|> anyOf "_'*"
      head .>>. manyChars tail |>> (fun (h, t) -> string h + t)
    let ctor = pstring ".ctor"
    (ctor <|> pident) .>>. opt (attempt genericPart)
    |>> (fun (n, genericParameters) ->
      let expected, matchMethod = NameMatchMethod.ofString n
      let genericParameters = genericParameters |> Option.defaultValue []
      { Expected = expected; GenericParameters = genericParameters; MatchMethod = matchMethod })
    <?> "identifier"

  let anyOrSignature = attempt signatureWildcard <|> (csharpSignature |>> SignatureQuery.Signature)
  let nameQuery =
    sepBy1 (trim memberNamePartial) (pchar '.') .>> skipString ":" .>>. anyOrSignature
    |>> (fun (namePart, sigPart) ->
      let sigPart =
        match sigPart with
        | SignatureQuery.Signature sig' ->
          let variableNames = namePart |> List.collect (fun n -> n.GenericParameters) |> Set.ofList 
          SignatureQuery.Signature (replaceWithVariable variableNames sig')
        | SignatureQuery.Wildcard -> SignatureQuery.Wildcard
      QueryMethod.ByName (List.rev namePart, sigPart))

  let genericQuery =
    genericPart .>> skipString ":" .>>. csharpSignature
    |>> (fun (variableNames, csharpSig) ->
      let variableNames = Set.ofList variableNames
      let csharpSig = replaceWithVariable variableNames csharpSig
      QueryMethod.BySignature (SignatureQuery.Signature csharpSig))

  let signatureQuery = csharpSignature |>> (SignatureQuery.Signature >> QueryMethod.BySignature)

  let query = choice [ attempt genericQuery ; attempt nameQuery; signatureQuery ]

  let parse (queryStr: string) =
    match runParserOnString (query .>> eof) () "" queryStr with
    | Success (queryMethod, _, _) -> { OriginalString = queryStr; Method = queryMethod }: Query
    | Failure (msg, _, _) -> failwithf "%s" msg