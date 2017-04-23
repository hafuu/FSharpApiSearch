namespace FSharpApiSearch

type LinkGenerator = Api -> string option

open FSharpApiSearch.Printer

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module LinkGenerator =
  open System.Web

  let internal genericParameters (api: Api) = api.Name |> Name.toDisplayName |> Seq.rev |> Seq.collect (fun x -> x.GenericParameters) |> Seq.distinct |> Seq.toList

  let internal toLower (str: string) = str.ToLower()
  let internal urlEncode (str: string) = HttpUtility.UrlEncode(str)

  let urlName (n: DisplayNameItem) =
    match n.Name with
    | SymbolName n -> n
    | OperatorName (n, _) -> n
    | WithCompiledName (n, _) -> n

  module internal FSharp =
    let fullOpReplaceTable =
      dict [
        "..", "[-..-]"
        ".. ..", "[-..-..-]"
        "-", "[-]"
        "=", "[=]"
        "%", "[r-]"
        ":=", "[-=-]"
        "?%", "[-qr]"
        "?%?", "[-qr-q]"
        "~%", "[-zr"
      ]

    let opReplaceTable =
      dict [
        '<', '['
        '>', ']'
        '|', 'h'
        '*', 'a'
        '&', 'n'
        '+', 'p'
        '%', 'r'
        '/', 's'
        '~', 'z'
        '?', 'q'
      ]

    let isActivePattern (api: Api) = match api.Signature with ApiSignature.ActivePatten _ -> true | _ -> false

    let replaceOp (name: string) =
      let op = name.[2..(name.Length - 3)]
      match fullOpReplaceTable.TryGetValue(op) with
      | true, replaced -> replaced
      | false, _ ->
        let replaced = op |> String.map (fun s -> match opReplaceTable.TryGetValue(s) with true, r -> r | false, _ -> s)
        "[-" + replaced + "-]"

    let isArray (n: DisplayNameItem) = (urlName n).StartsWith("[")

    let generate (api: Api) = option {
      let ns =
        let ns = Name.toDisplayName api.Name
        match api.Signature with
        | ApiSignature.Constructor _ -> ns.Tail // skip "new"
        | _ -> ns
      let namePart =
        let name =
          let name = urlName ns.Head
          if isActivePattern api then
            name.[3..(name.Length - 4)].Replace("|_", "").Replace("|", "h")
          elif name.StartsWith("( ") then
            replaceOp name
          else
            name
        let path = urlName (ns.Item 1)
        path + "." + name
      let genericParamsPart =
        if namePart = "Operators.[=]" then
          "'t"
        elif namePart = "ExtraTopLevelOperators.array2D" then
          "'t"
        else
          match genericParameters api with
          | [] -> ""
          | genericParams -> sprintf "[%s]" (genericParams |> Seq.map (fun v -> v.Print()) |> String.concat ",")
      let! kindPart =
        match api.Signature with
        | ApiSignature.ActivePatten _ -> Some "active-pattern"
        | ApiSignature.ModuleValue _ when ns.Head.GenericParameters.IsEmpty = false -> Some "type-function"
        | ApiSignature.ModuleValue _
        | ApiSignature.ModuleFunction _ -> Some "function"
        | ApiSignature.InstanceMember (_, m) | ApiSignature.StaticMember (_, m) when m.Kind = MemberKind.Method -> Some "method"
        | ApiSignature.InstanceMember (_, m) | ApiSignature.StaticMember (_, m) when m.Kind = MemberKind.Field -> None
        | ApiSignature.InstanceMember _ | ApiSignature.StaticMember _ -> Some "property"
        | ApiSignature.Constructor _ -> Some "constructor"
        | ApiSignature.ModuleDefinition _ -> Some "module"
        | ApiSignature.FullTypeDefinition td  ->
          if isArray td.Name.Head then
            None
          else
            match td.Kind with
            | TypeDefinitionKind.Class -> Some "class"
            | TypeDefinitionKind.Interface -> Some "interface"
            | TypeDefinitionKind.Type -> Some "type"
            | TypeDefinitionKind.Union -> Some "union"
            | TypeDefinitionKind.Record -> Some "record"
            | TypeDefinitionKind.Enumeration -> Some "enumeration"
        | ApiSignature.TypeAbbreviation _ -> Some "type-abbreviation"
        | ApiSignature.TypeExtension _ ->
          match urlName (Seq.last ns) with
          | "System" -> Some "extension-method"
          | _ -> Some "method"
        | ApiSignature.ExtensionMember _ -> Some "extension-method"
        | ApiSignature.UnionCase _ -> None
        | ApiSignature.ComputationExpressionBuilder _ -> Some "class"

      return sprintf "%s%s-%s-[fsharp]" namePart genericParamsPart kindPart |> toLower |> urlEncode
    }
  module internal Msdn =
    let isGeneric api = api.Name |> Name.toDisplayName |> List.exists (fun n -> List.isEmpty n.GenericParameters = false)
    let canGenerate (api: Api) =
      match api.Signature with
      | ApiSignature.ActivePatten _
      | ApiSignature.ModuleValue _
      | ApiSignature.ModuleFunction _
      | ApiSignature.Constructor _
      | ApiSignature.ModuleDefinition _
      | ApiSignature.FullTypeDefinition _
      | ApiSignature.TypeAbbreviation _
      | ApiSignature.TypeExtension _
      | ApiSignature.ExtensionMember _
      | ApiSignature.ComputationExpressionBuilder _
      | ApiSignature.UnionCase _ -> false

      | ApiSignature.InstanceMember _
      | ApiSignature.StaticMember _ -> true

    let generate (api: Api) =
      if isGeneric api then
        None
      elif canGenerate api = false then
        None
      else
        (Name.toDisplayName api.Name |> Seq.rev |> Seq.map urlName |> String.concat ".") + ".aspx"
        |> toLower
        |> Some

  module internal MicrosoftDocs =
    open SpecialTypes
    open FSharpImpl
    let isGeneric api = api.Name |> Name.toDisplayName |> List.exists (fun n -> List.isEmpty n.GenericParameters = false)

    let convertGenericUrl (api:Api) (dname : seq<DisplayNameItem>) =   
        let toParameterSeq (dname : DisplayNameItem list) =
            let mutable genericParameterIsWrite = false
            dname |> Seq.map ( fun n -> 
                                if genericParameterIsWrite = false && n.GenericParameters.IsEmpty = false then
                                    genericParameterIsWrite <- true
                                    let genericCounter = n.GenericParameters |> List.length
                                    (CSharpImpl.toDisplayName n.Name) + "-" + (genericCounter.ToString())
                                            
                                else
                                    (CSharpImpl.toDisplayName n.Name))
        
        match api.Signature with
        | ApiSignature.Constructor (_ , _) ->
            seq {
                let revdname = (Name.toDisplayName api.Name).Tail |> List.rev
                yield! revdname |> toParameterSeq
                yield "-ctor"
            }
        | _ ->
            dname |> Seq.rev |> Seq.toList |> toParameterSeq

    let convertGenericParameter (dname : seq<DisplayNameItem>) =   
        dname |> Seq.map (
            fun n -> 
                if n.GenericParameters.IsEmpty = false && n.GenericParameters |> List.exists(fun x -> x.Name = (CSharpImpl.toDisplayName n.Name)) then
                    let genericCounter = n.GenericParameters |> List.findIndex(fun x -> x.Name = (CSharpImpl.toDisplayName n.Name)) 
                    "_" + (genericCounter.ToString())
                                            
                else
                    (CSharpImpl.toDisplayName n.Name)
        ) 

    let generateMSDocsUrl (api :Api) (mem : Member) =
        let getParameterString (lt : LowType) =
            seq {
                    match lt with
                    | LowType.Patterns.AbbreviationRoot original -> 
                        match original with
                        | Identity originalID -> 
                            match originalID with
                            | FullIdentity foid  -> 
                                match foid.Name  with
                                | DisplayName dname -> 
                                    match lt with
                                    | LowType.Patterns.Unit -> ()
                                    | _ ->
                                        let revdname = dname |> List.rev
                                        for nameItem in revdname do
                                            yield (CSharpImpl.toDisplayName nameItem.Name)
                                | _ -> ()
                            | _ -> ()
                        | _ -> ()
                    | _ -> ()
            }
        
        let getParameterQuery (dname : seq<DisplayNameItem>) mem = 
                                    seq {
                                        for param in mem.Parameters do
                                            for param2 in param do
                                                match param2.Type with
                                                | TypeAbbreviation t -> 
                                                    yield! getParameterString param2.Type
                                                
                                                | LowType.Patterns.Array (name, elem) ->
                                                    yield! getParameterString elem
                                                    yield "_"
                                                
                                                | Variable (vs , tv) ->
                                                    for n in dname do
                                                        if n.GenericParameters.IsEmpty = false 
                                                                        && n.GenericParameters |> List.exists(fun x -> x.Name = tv.Name) then
                                                            let genericCounter = n.GenericParameters |> List.findIndex(fun x -> x.Name = tv.Name) 
                                                            yield "_" + (genericCounter.ToString())
                                                        else
                                                            ()
                                                | Generic (Identity id , [elem]) ->
                                                    for n in dname do
                                                        if n.GenericParameters.IsEmpty = false 
                                                                        && n.GenericParameters |> List.exists(fun x -> match elem with
                                                                                                                        | Variable (vs , tv) -> x.Name = tv.Name
                                                                                                                        | _ -> false
                                                                                                                ) then
                                                            let genericCounter = n.GenericParameters |> List.findIndex(fun x -> match elem with
                                                                                                                                | Variable (vs , tv) -> x.Name = tv.Name
                                                                                                                                | _ -> false
                                                                                                                        )
                                                            match id with
                                                            | FullIdentity fid -> 
                                                                if (CSharpImpl.toDisplayName ((Name.toDisplayName fid.Name).Head.Name)) = "byref" then
                                                                    yield "_" + (genericCounter.ToString()) + "_"
                                                                else
                                                                    yield "_" + (genericCounter.ToString())
                                                        else
                                                            ()
                                                    ()

                                                | _ -> ()
                                     } |> String.concat "_"

        if isGeneric api then

            let urlBase : list<string> = api.Name |> Name.toDisplayName
                                            |> convertGenericUrl api
                                            |> List.ofSeq
            let url = urlBase |> String.concat "." |> toLower
            
            let parameter = urlBase |> List.map(fun (x : string) -> x.Replace("-","_")) |> String.concat "_"
            
            let parameterQuery = getParameterQuery (Name.toDisplayName api.Name) mem

            if parameterQuery.Length > 0 then
                (url + "?#" + parameter + "_" + parameterQuery + "_") |> Some
            else
                (url + "?#" + parameter) |> Some
        else
            let basedname = seq {
                                match api.Signature with
                                | ApiSignature.Constructor (_ , _) ->
                                    let revdname = (Name.toDisplayName api.Name).Tail |> List.rev
                                    for nameItem in revdname do
                                        yield (CSharpImpl.toDisplayName nameItem.Name)
                                    yield "-ctor"
                                | _ ->
                                    let revdname = (Name.toDisplayName api.Name) |> List.rev
                                    for nameItem in revdname do
                                        yield (CSharpImpl.toDisplayName nameItem.Name)
            }
            let urlBase = basedname |> String.concat "."
                            |> toLower
            let nameParameter = basedname |>  Seq.map(fun (x : string) -> x.Replace("-","_")) |> String.concat "_"
            
            let parameterQuery = getParameterQuery (Name.toDisplayName api.Name) mem
    
            if parameterQuery.Length > 0 then
                (urlBase + "?#" + nameParameter + "_" + parameterQuery + "_") |> Some
            else
                (urlBase + "?#" + nameParameter) |> Some
    
    let generateMSDocsTypeUrl (api : Api) =
        if isGeneric api then
            let urlBase : list<string> = api.Name |> Name.toDisplayName 
                                            |> convertGenericUrl api
                                            |> List.ofSeq
            urlBase |> String.concat "." |> toLower |> Some
        else
            let urlBase = (Name.toDisplayName api.Name |> Seq.rev |> Seq.map (fun n -> (CSharpImpl.toDisplayName n.Name)) |> String.concat ".")
                            |> toLower
            urlBase |> Some
            
    let generate (api: Api) =
        match api.Signature with
            | ApiSignature.ActivePatten _
            | ApiSignature.ModuleValue _
            | ApiSignature.ModuleFunction _
            | ApiSignature.ModuleDefinition _            
            | ApiSignature.TypeAbbreviation _
            | ApiSignature.TypeExtension _
            | ApiSignature.ExtensionMember _
            | ApiSignature.ComputationExpressionBuilder _
            | ApiSignature.UnionCase _ -> None

            | ApiSignature.Constructor (_ , (x : Member)) ->
                generateMSDocsUrl api x

            | ApiSignature.FullTypeDefinition _ -> 
                generateMSDocsTypeUrl api

            | ApiSignature.InstanceMember (_ , (x : Member)) ->
                generateMSDocsUrl api x

            | ApiSignature.StaticMember (_ , (x : Member)) ->
                generateMSDocsUrl api x

  let fsharp baseUrl: LinkGenerator = fun api -> FSharp.generate api |> Option.map (fun apiUrl -> baseUrl + apiUrl)
  let msdn baseUrl: LinkGenerator = fun api -> Msdn.generate api |> Option.map (fun apiUrl -> baseUrl + apiUrl)
  let msdocs baseUrl:LinkGenerator = fun api -> MicrosoftDocs.generate api |> Option.map (fun apiUrl -> baseUrl + apiUrl)