module internal FSharpApiSearch.QueryParser

open FParsec

open FSharpApiSearch.SpecialTypes

let inline trim p = spaces >>. p .>> spaces
let inline pcharAndTrim c = pchar c |> trim
let inline pstringAndTrim s = pstring s |> trim

let inline sepBy2 p sep = p .>> sep .>>. sepBy1 p sep |>> (fun (x, xs) -> x :: xs)

let atQuery range = AtQuery (None, range)

let getIndex = getPosition |>> (fun p -> int p.Index)
let indexed p = tuple3 getIndex p getIndex |>> (fun (b, x, e) -> x, { Begin = b; End = e })

let maxEndRange (xs: LowType list) =
  xs
  |> List.choose (fun x ->
    match x.Position with
    | AtQuery (_, range) -> Some range.End
    | AtSignature _ | Unknown -> None
  )
  |> List.max

let minBeginRange (xs: LowType list) =
  xs
  |> List.choose (fun x ->
    match x.Position with
    | AtQuery (_, range) -> Some range.Begin
    | AtSignature _ | Unknown -> None
  )
  |> List.min

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
  
  let partialName =
    pidentifier .>>. many (attempt (spaces >>. skipChar '.' >>. spaces >>. pidentifier))
    |>> (fun (head, tail) ->
      (head :: tail)
      |> List.map (fun n -> { Name = SymbolName n; GenericParameters = [] })
      |> List.rev
    )

  let fsharpSignature, fsharpSignatureRef = createParserForwardedToRef()

  let userInputType =
    indexed partialName
    |>> (function (name, range) -> Identifier (UserInputType { Name = name }, atQuery range))
    <?> "type"
  let variable =
    indexed (skipChar ''' >>. pidentifier)
    |>> (function (name, range) -> Variable (VariableSource.Query, { Name = name; IsSolveAtCompileTime = false }, atQuery range))
    <?> "variable"
  let wildcard =
    indexed (skipChar '?' >>. opt pidentifier)
    |>> (fun (name, range) -> Wildcard (name, atQuery range))
    <?> "wildcard"

  let genericId =
    choice [
      attempt userInputType
      attempt variable
      wildcard
    ]

  let createGeneric id parameters pos =
    let parameterCount = List.length parameters
    let id =
      match id with
      | Identifier (UserInputType p, pos) ->
        let newName =
          match p.Name with
          | [] -> []
          | n :: tail -> { n with GenericParameters = List.init parameterCount (fun n -> { Name = sprintf "T%d" n; IsSolveAtCompileTime = false }) } :: tail
        Identifier (UserInputType { p with Name = newName }, pos)
      | other -> other
    Generic (id, parameters, pos)

  let dotNetGeneric =
    parse {
      let! begin' = getIndex
      let! id = genericId
      do! spaces
      do! skipChar '<'
      let! parameters = sepBy1 (trim fsharpSignature) (skipChar ',')
      do! skipChar '>'
      let! end' = getIndex
      return createGeneric id parameters (atQuery { Begin = begin'; End = end' })
    }

  let subtype =
    let prefix = skipChar '#'
    let t = attempt dotNetGeneric <|> userInputType
    indexed (prefix >>. t)
    |>> fun (t, range) -> Subtype (t, atQuery range)

  let ptype =
    choice [
      attempt (between (skipChar '(') (skipChar ')') (trim fsharpSignature))
      attempt dotNetGeneric
      attempt userInputType
      attempt variable
      attempt subtype
      wildcard
    ]
  
  let array _ typeParser =
    let arraySymbol =
      let t = { Name = "T"; IsSolveAtCompileTime = false }
      indexed parray
      |>> (fun (array, range) -> Identifier (UserInputType { Name = [ { Name = SymbolName array; GenericParameters = [ t ] } ] }, atQuery range))
    
    parse {
      let! begin' = getIndex
      let! t = typeParser
      let! arrays = many1 (trim arraySymbol)
      let end' = maxEndRange arrays
      return List.fold (fun x array -> createGeneric array [ x ] (atQuery { Begin = begin'; End = end' })) t arrays
    }

  let structTuple self typeParser =
    let elem =
      choice [
        attempt typeParser
        self
      ]
    parse {
      let! begin' = getIndex
      do! skipString struct'
      do! spaces
      do! skipChar '('
      let! xs = sepBy1 (trim elem) (skipChar '*')
      do! skipChar ')'
      let! end' = getIndex
      return Tuple ({ Elements = xs; IsStruct = true }, atQuery { Begin = begin'; End = end' })
    }

  let mlGeneric _ typeParser =
    let mlMultiGenericParameter = between (skipChar '(') (skipChar ')') (sepBy1 (trim fsharpSignature) (pchar ','))
    let mlSingleGenericParameter = typeParser |>> List.singleton

    let foldGeneric parameter ids begin' =
      let rec foldGeneric' acc = function
        | id :: rest ->
          let parameters = [ acc ]
          let generic = createGeneric id parameters (atQuery { Begin = begin'; End = maxEndRange [ id ] })
          foldGeneric' generic rest
        | [] -> acc
      let generic =
        let id = List.head ids
        createGeneric id parameter (atQuery { Begin = begin'; End = maxEndRange [ id ] })
      foldGeneric' generic (List.tail ids)
    
    parse {
      let! begin' = getIndex
      let! parameters = attempt mlMultiGenericParameter <|> mlSingleGenericParameter
      do! spaces
      let! ids = many1 (trim genericId)
      return foldGeneric parameters ids begin'
    }


  let tuple _ typeParser =
    sepBy2 (trim typeParser) (pstring "*")
    |>> fun xs ->
      let range = { Begin = minBeginRange xs; End = maxEndRange xs }
      Tuple ({ Elements = xs; IsStruct = false }, atQuery range)

  let arrow _ typeParser =
    sepBy2 (trim typeParser) (pstring "->")
    |>> fun xs ->
      let arrow = Arrow.ofLowTypeList xs
      let range = { Begin = minBeginRange xs; End = maxEndRange xs }
      Arrow (arrow, atQuery range)

  do fsharpSignatureRef := compose ptype [ array; structTuple; mlGeneric; tuple; arrow ]

  let activePatternKind =
    (skipString "(||)" >>% ActivePatternKind.ActivePattern)
    <|> (skipString "(|_|)" >>% ActivePatternKind.PartialActivePattern)
  let allPatterns =
    trim (skipString "...") >>. skipString "->" >>. fsharpSignature
    >>= function
        | Arrow (([ x ], y), _) -> preturn (ActivePatternSignature.AnyParameter (x, y))
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
    trim (sepBy1 name (pchar '.')) .>> pstring ":" .>>. trim anyOrSignature
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
    match runParserOnString (trim query .>> eof) () "" queryStr with
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

  let identifierHead = letter <|> pchar '_'
  let identifierTail = letter <|> digit <|> anyOf "_'"

  let pidentifier =
    identifierHead .>>. manyChars identifierTail |>> (fun (h, t) -> string h + t)
    >>= (fun x -> if keywords |> List.exists ((=)x) then fail (sprintf "%s is the keyword." x) else preturn x)
    <?> "identifier"
  
  let partialName = sepBy1 pidentifier (pchar '.') |>> (List.map (fun n -> { Name = SymbolName n; GenericParameters = [] } ) >> List.rev)
  let csharpSignature, csharpSignatureRef = createParserForwardedToRef()

  let lowerOnlyVariable =
    lower .>>. manyChars identifierTail
    >>= (fun (head, tail) ->
      let str = string head + tail
      if String.exists isUpper str then
        fail (sprintf "%s contains upper cases." str)
      elif SpecialTypes.Identifier.CSharp.aliases |> List.map fst |> List.contains str then
        fail (sprintf "%s is a C# alias." str)
      else
        preturn (Variable.create (VariableSource.Query, { Name = str; IsSolveAtCompileTime = false }))
    )
    |> trim
    <?> "variable"

  let userInputType = partialName |>> (function name -> Identifier.create (UserInputType { Name = name })) |> trim <?> "type"
  let wildcard = pchar '?' >>. opt pidentifier |>> Wildcard.create |> trim <?> "wildcard"

  let genericId =
    choice [
      attempt lowerOnlyVariable
      attempt userInputType
      wildcard
    ]

  let createGeneric id parameters =
    let parameterCount = List.length parameters
    let id =
      match id with
      | Identifier (UserInputType p, _) ->
        let newName =
          match p.Name with
          | [] -> []
          | n :: tail -> { n with GenericParameters = List.init parameterCount (fun n -> { Name = sprintf "T%d" n; IsSolveAtCompileTime = false }) } :: tail
        Identifier.create (UserInputType { p with Name = newName })
      | other -> other
    Generic.create (id, parameters)

  let generic = genericId .>>. between (pcharAndTrim '<') (pcharAndTrim '>') (sepBy1 csharpSignature (pchar ',')) |>> (fun (id, parameter) -> createGeneric id parameter)

  let subtype =
    let prefix = pcharAndTrim '#'
    let t = attempt generic <|> userInputType
    prefix >>. t |>> Subtype.create

  let ptype =
    choice [
      attempt punit
      attempt (between (pcharAndTrim '(') (pcharAndTrim ')') csharpSignature)
      attempt generic
      attempt lowerOnlyVariable
      attempt userInputType
      attempt subtype
      wildcard
    ]

  let array _ typeParser =
    let arraySymbol =
      let t = { Name = "T"; IsSolveAtCompileTime = false }
      parray |> trim |>> (fun array -> Identifier.create (UserInputType { Name = [ { Name = SymbolName array; GenericParameters = [ t ] } ] }))
    typeParser
    .>>. many1 arraySymbol
    |>> fun (t, xs) -> List.foldBack (fun array x -> createGeneric array [ x ]) xs t

  let structTuple self typeParser =
    let elems = attempt self <|> typeParser
    between (pcharAndTrim '(') (pcharAndTrim ')') (sepBy2 elems (pstring ",")) |>> fun xs -> Tuple.create { Elements = xs; IsStruct = true }

  let byref _ typeParser =
    trim ((pstring ref <|> pstring out) .>> spaces1 .>>. typeParser)
    |>> (fun (refType, t) -> let isOut = refType = out in ByRef.create (isOut, t))

  let arrow _ typeParser =
    let args =
      let elems = sepBy1 typeParser (pstring ",")
      let elemsWithParen = between (pcharAndTrim '(') (pcharAndTrim ')') elems
      attempt elemsWithParen <|> attempt elems <|> (typeParser |>> List.singleton)
      |>> function
        | [ x ] -> x
        | xs -> Tuple.create { Elements = xs; IsStruct = false }
    sepBy2 args (pstring "->") |>> (Arrow.ofLowTypeList >> Arrow.create)

  do csharpSignatureRef := compose ptype [ array; structTuple; byref; arrow ]

  let rec replaceWithVariable (variableNames: Set<string>) = function
    | Identifier (UserInputType { Name = [ { Name = SymbolName name } ] }, pos) when Set.contains name variableNames ->
        Variable (VariableSource.Query, { Name = name; IsSolveAtCompileTime = false }, pos)
    | Generic (id, args, pos) -> Generic (replaceWithVariable variableNames id, List.map (replaceWithVariable variableNames) args, pos)
    | Tuple (tpl, pos) -> Tuple ({ tpl with Elements = List.map (replaceWithVariable variableNames) tpl.Elements }, pos)
    | Arrow ((ps, ret), pos) -> Arrow ((List.map (replaceWithVariable variableNames) ps, replaceWithVariable variableNames ret), pos)
    | ByRef (isOut, t, pos) -> ByRef (isOut, replaceWithVariable variableNames t, pos)
    | Subtype (t, pos) -> Subtype (replaceWithVariable variableNames t, pos)
    | (Wildcard _ | Variable _ | Identifier _ | TypeAbbreviation _ | Delegate _ | Choice _) as t -> t
    | LoadingType _ -> Name.loadingNameError()

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