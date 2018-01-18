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
    >>= (fun (name, range) ->
      match name with
      | [ { Name = SymbolName "_" } ] -> fail "It is wildcard"
      | _ -> preturn (Identifier (UserInputType { Name = name }, atQuery range))
    )
    <?> "type"
  let variable =
    indexed (skipChar ''' >>. pidentifier)
    |>> (fun (name, range) -> Variable (VariableSource.Query, { Name = name; IsSolveAtCompileTime = false }, atQuery range))
    <?> "variable"
  let wildcard =
    let questionPrefix = skipChar '?' >>. opt pidentifier
    let underscore = pstring "_" |>> (fun _ -> None)
    indexed (questionPrefix <|> underscore)
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

  let computationExpressionSyntax =
    let syntax = manyChars (letter <|> pchar '/') .>>. opt (pstring "!")
    indexed syntax
    |>> (fun ((syntax, bang), range) ->
      {
        Syntax = match bang with Some bang -> syntax + bang | None -> syntax
        Position = AtQuery (None, range)
      }
    )  
  let computationExpressionQuery =
    let underScore = skipString "_" >>% []
    let syntaxes = sepBy1 (trim computationExpressionSyntax) (pchar ';') |>> List.filter (fun x -> x.Syntax <> "")
    let left = skipString "{" >>. trim (attempt underScore <|> syntaxes) .>> skipString "}"
    trim left .>> skipString ":" .>>. trim fsharpSignature
    |>> (fun (syntax, t) -> QueryMethod.ByComputationExpression { Syntaxes = syntax; Type = t })

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

  let punit =
    indexed (skipString "void" <|> skipString "()")
    |>> fun (_, range) -> SpecialTypes.LowType.Unit |> LowType.setPosition (fun _ -> atQuery range)

  let identifierHead = letter <|> pchar '_'
  let identifierTail = letter <|> digit <|> anyOf "_'"

  let pidentifier =
    identifierHead .>>. manyChars identifierTail |>> (fun (h, t) -> string h + t)
    >>= (fun x -> if keywords |> List.exists ((=)x) then fail (sprintf "%s is the keyword." x) else preturn x)
    <?> "identifier"
  
  let partialName =
    pidentifier .>>. many (attempt (spaces >>. skipChar '.' >>. pidentifier))
    |>> (fun (head, tail) ->
      head :: tail
      |> List.map (fun n -> { Name = SymbolName n; GenericParameters = [] })
      |> List.rev
    )

  let csharpSignature, csharpSignatureRef = createParserForwardedToRef()

  let lowerOnlyVariable =
    parse {
      let! begin' = getIndex
      let! head = lower
      let! tail = manyChars identifierTail
      let! end' = getIndex
      let str = string head + tail
      if String.exists isUpper str then
        return! fail (sprintf "%s contains upper cases." str)
      elif SpecialTypes.Identifier.CSharp.aliases |> List.map fst |> List.contains str then
        return! fail (sprintf "%s is a C# alias." str)
      else
        return (Variable (VariableSource.Query, { Name = str; IsSolveAtCompileTime = false }, atQuery { Begin = begin'; End = end' }))
    }
    <?> "variable"

  let userInputType =
    indexed partialName
    |>> (function (name, range) -> Identifier (UserInputType { Name = name }, atQuery range))
    <?> "type"

  let wildcard = indexed (pchar '?' >>. opt pidentifier) |>> (fun (name, range) -> Wildcard (name, atQuery range)) <?> "wildcard"

  let genericId =
    choice [
      attempt lowerOnlyVariable
      attempt userInputType
      wildcard
    ]

  let createGeneric id parameters pos =
    let parameterCount = List.length parameters
    let id =
      match id with
      | Identifier (UserInputType p, idPos) ->
        let newName =
          match p.Name with
          | [] -> []
          | n :: tail -> { n with GenericParameters = List.init parameterCount (fun n -> { Name = sprintf "T%d" n; IsSolveAtCompileTime = false }) } :: tail
        Identifier (UserInputType { p with Name = newName }, idPos)
      | other -> other
    Generic (id, parameters, pos)

  let generic = parse {
    let! begin' = getIndex
    let! id = genericId
    do! spaces
    do! skipChar '<'
    let! parameters = sepBy1 (trim csharpSignature) (skipChar ',')
    do! skipChar '>'
    let! end' = getIndex
    return createGeneric id parameters (atQuery { Begin = begin'; End = end' })
  }

  let subtype =
    let prefix = skipChar '#'
    let t = attempt generic <|> userInputType
    indexed (prefix >>. t) |>> (fun (t, range) -> Subtype (t, atQuery range))

  let ptype =
    choice [
      attempt punit
      attempt (between (skipChar '(') (skipChar ')') (trim csharpSignature))
      attempt generic
      attempt lowerOnlyVariable
      attempt userInputType
      attempt subtype
      wildcard
    ]

  let array _ typeParser =
    let arraySymbol =
      let t = { Name = "T"; IsSolveAtCompileTime = false }
      indexed parray |>> (fun (array, range) -> Identifier (UserInputType { Name = [ { Name = SymbolName array; GenericParameters = [ t ] } ] }, atQuery range))
    parse {
      let! begin' = getIndex
      let! t = typeParser
      let! xs = many1 (trim arraySymbol)
      let end' = maxEndRange xs
      return List.foldBack (fun array x -> createGeneric array [ x ] (atQuery { Begin = begin'; End = end' })) xs t
    }

  let structTuple self typeParser =
    let elems = attempt self <|> typeParser
    parse {
      do! skipChar '('
      let! xs = sepBy2 (trim elems) (skipChar ',')
      do! skipChar ')'
      let range = { Begin = minBeginRange xs; End = maxEndRange xs }
      return Tuple ({ Elements = xs; IsStruct = true }, atQuery range)
    }

  let byref _ typeParser =
    parse {
      let! begin' = getIndex
      let! refType = pstring ref <|> pstring out
      do! spaces1
      let! t = typeParser
      let! end' = getIndex
      let isOut = refType = out
      return ByRef (isOut, t, atQuery { Begin = begin'; End = end' })
    }

  let arrow _ typeParser =
    let args =
      let elems = sepBy1 (trim typeParser) (pstring ",")
      let elemsWithParen = between (skipChar '(') (skipChar ')') elems
      attempt elemsWithParen <|> attempt elems <|> (typeParser |>> List.singleton)
      |>> function
        | [ x ] -> x
        | xs -> Tuple ({ Elements = xs; IsStruct = false }, atQuery { Begin = minBeginRange xs; End = maxEndRange xs })
    sepBy2 (trim args) (pstring "->")
    |>> (fun xs ->
      let arrow = Arrow.ofLowTypeList xs
      let range = { Begin = minBeginRange xs; End = maxEndRange xs }
      Arrow (arrow, atQuery range)
    )

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

  let anyOrSignature = attempt signatureWildcard <|> (trim csharpSignature |>> SignatureQuery.Signature)
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
    genericPart .>> skipString ":" .>>. trim csharpSignature
    |>> (fun (variableNames, csharpSig) ->
      let variableNames = Set.ofList variableNames
      let csharpSig = replaceWithVariable variableNames csharpSig
      QueryMethod.BySignature (SignatureQuery.Signature csharpSig))

  let signatureQuery = csharpSignature |>> (SignatureQuery.Signature >> QueryMethod.BySignature)

  let query = choice [ attempt genericQuery ; attempt nameQuery; signatureQuery ]

  let parse (queryStr: string) =
    match runParserOnString (trim query .>> eof) () "" queryStr with
    | Success (queryMethod, _, _) -> { OriginalString = queryStr; Method = queryMethod }: Query
    | Failure (msg, _, _) -> failwithf "%s" msg