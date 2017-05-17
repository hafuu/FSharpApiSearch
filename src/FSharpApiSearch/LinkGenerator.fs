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

  module internal DotNetApiBrowser =
    open SpecialTypes
    open SpecialTypes.LowType.Patterns
    open System.Collections.Generic
    open System.Text

    let isGenericMember (api: Api) = List.isEmpty ((Name.toDisplayName api.Name).Head.GenericParameters) = false
    
    let nameElementsAndVariableId (api: Api) =
      let wroteGeneric = ref false
      let variableId = ref 0
      let variableMemory = Dictionary<string, int>()
      let memory variable =
        if variableMemory.ContainsKey(variable) = false then
          variableMemory.[variable] <- !variableId
          incr variableId
      
      let convert (forcedFlag: bool) (modifiedString : string) (name: DisplayNameItem) =
        variableId := 0
        name.GenericParameters |> List.iter (fun p -> memory p.Name)
        if (forcedFlag) || (!wroteGeneric = false && name.GenericParameters.IsEmpty = false) then
          wroteGeneric := true
          urlName name + modifiedString + string name.GenericParameters.Length
        else
          urlName name
      
      let elems =
        //printfn "%A" api
        match api.Kind with
        | ApiKind.Constructor ->
          [|
            yield! Name.toDisplayName api.Name |> List.tail |> Seq.rev |> Seq.map (convert false "-")
            yield "-ctor"
          |]

        | ApiKind.ExtensionMember when isGenericMember api ->
          Name.toDisplayName api.Name |> Seq.rev |> Seq.map (convert false "--") |> Seq.toArray

        | ApiKind.Member(_ , _) when isGenericMember api ->
          Name.toDisplayName api.Name |> Seq.rev |> Seq.map (fun name ->
                                                                match api.Signature with
                                                                | ApiSignature.InstanceMember(_, mem)
                                                                | ApiSignature.StaticMember(_,mem)  -> 
                                                                    if mem.GenericParameters |> List.exists(fun x -> name.GenericParameters |> List.exists(fun y -> x.Name = y.Name)) then
                                                                        convert true "--" name
                                                                    else
                                                                        convert false "-" name
                                                                | _ -> 
                                                                    convert false "-" name
                                                    ) |> Seq.toArray

        | _ ->
          Name.toDisplayName api.Name |> Seq.rev |> Seq.map (convert false "-") |> Seq.toArray

      elems, variableMemory

    let urlPart elems (sb: StringBuilder) =
      let elems = elems |> Seq.map toLower
      sb.AppendJoin(".", elems)

    let rec parameterElement (api: Api) (variableMemory: Dictionary<string, int>) (t: LowType) (sb: StringBuilder) : StringBuilder =
      match t with
      | Unit -> sb
      | Identity (FullIdentity { Name = name }) ->
        let ns = Name.toDisplayName name |> Seq.rev
        sb.AppendJoin("_", ns, (fun n sb -> sb.Append(urlName n)))
      | Array (_, elem) -> sb.Append(parameterElement api variableMemory elem).Append("__")
      | ByRef (_, arg) -> sb.Append(parameterElement api variableMemory arg).Append("_")
      | Generic (id, args) ->
        sb.Append(parameterElement api variableMemory id) |> ignore
        
        sb.Append("_") |> ignore
        sb.AppendJoin("_", args, (parameterElement api variableMemory))
            .Append("_")
      | Variable (_, v) -> 
            match api.Kind with
            | ApiKind.ExtensionMember when isGenericMember api ->
                sb.Append("__").Append(variableMemory.[v.Name])

            | ApiKind.Member(_ , _) when isGenericMember api ->
                match api.Signature with
                | ApiSignature.InstanceMember(_, mem)
                | ApiSignature.StaticMember(_,mem)  -> 
                    if mem.GenericParameters |> List.exists(fun x -> x.Name = v.Name) then
                        sb.Append("__").Append(variableMemory.[v.Name])
                    else
                        sb.Append("_").Append(variableMemory.[v.Name])
                | _ -> 
                    sb.Append("__").Append(variableMemory.[v.Name])

            | _ -> sb.Append("_").Append(variableMemory.[v.Name])
      | Delegate (d, _) -> sb.Append(parameterElement api variableMemory d)
      | AbbreviationRoot root -> sb.Append(parameterElement api variableMemory root)
      | _ -> sb

    let hasParameter (member': Member) =
      match member'.Parameters with
      | [] | [ [ { Type = Unit } ] ] -> false
      | _ -> true

    let hashPart (nameElems: string[]) (variableMemory: Dictionary<_, _>) (member': Member) (api: Api) (sb: StringBuilder) =
      let nameElems = nameElems |> Seq.map (fun n -> n.Replace("-", "_"))
      
      sb.AppendJoin("_", nameElems) |> ignore

      if hasParameter member' then
        let parameters = member'.Parameters |> Seq.collect id |> Seq.map (fun p -> p.Type)
        sb.Append("_")
          .AppendJoin("_", parameters, (parameterElement api variableMemory))
          .Append("_")
        |> ignore

      sb

    let generate (view: string) (api: Api) =
      match api.Signature with
      | ApiSignature.ActivePatten _
      | ApiSignature.ModuleValue _
      | ApiSignature.ModuleFunction _
      | ApiSignature.ModuleDefinition _            
      | ApiSignature.TypeAbbreviation _
      | ApiSignature.TypeExtension _
      | ApiSignature.ComputationExpressionBuilder _
      | ApiSignature.UnionCase _ -> None

      | ApiSignature.FullTypeDefinition _ ->
        let nameElems, _ = nameElementsAndVariableId api
        let sb = StringBuilder().Append(urlPart nameElems).Append("?view=").Append(view)
        Some (string sb)

      | ApiSignature.ExtensionMember (member' : Member)
      | ApiSignature.Constructor (_ , (member' : Member))
      | ApiSignature.InstanceMember (_ , (member' : Member))
      | ApiSignature.StaticMember (_ ,(member' : Member)) ->
        let nameElems, variableMemory = nameElementsAndVariableId api
        let sb = StringBuilder().Append(urlPart nameElems).Append("?view=").Append(view).Append("#").Append(hashPart nameElems variableMemory member' api)
        Some (string sb)

  let fsharp baseUrl: LinkGenerator = fun api -> FSharp.generate api |> Option.map (fun apiUrl -> baseUrl + apiUrl)
  let msdn baseUrl: LinkGenerator = fun api -> Msdn.generate api |> Option.map (fun apiUrl -> baseUrl + apiUrl)
  let dotNetApiBrowser baseUrl (view: string) : LinkGenerator = fun api -> DotNetApiBrowser.generate view api |> Option.map (fun apiUrl -> baseUrl + apiUrl)