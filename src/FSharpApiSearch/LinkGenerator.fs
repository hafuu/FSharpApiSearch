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
    open System.Text

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

    let replaceOp (name: string) (sb:StringBuilder) =
      let op = name.[2..(name.Length - 3)]
      match fullOpReplaceTable.TryGetValue(op) with
      | true, replaced -> sb.Append(replaced)
      | false, _ ->
        let replaced = op |> String.map (fun s -> match opReplaceTable.TryGetValue(s) with true, r -> r | false, _ -> s)
        sb.Append("[-").Append(replaced).Append("-]")

    let isArray (n: DisplayNameItem) = (urlName n).StartsWith("[")

    let generate (api: Api) = option {
      let ns =
        let ns = Name.toDisplayName api.Name
        match api.Signature with
        | ApiSignature.Constructor _ -> ns.Tail // skip "new"
        | _ -> ns
      let namePart (sb: StringBuilder) =
        let path = urlName (ns.Item 1)
        sb.Append(path).Append(".") |> ignore
        let name =
          let name = urlName ns.Head
          if isActivePattern api then
            sb.Append(name.[3..(name.Length - 4)].Replace("|_", "").Replace("|", "h"))
          elif name.StartsWith("( ") then
            replaceOp name sb
          else
            sb.Append(name)
        sb
      
      let genericParamsPart (sb: StringBuilder) =
        let temp_sb = StringBuilder()
        let name = namePart temp_sb
        
        if string name = "Operators.[=]" then
          sb.Append("'t")
        elif string name = "ExtraTopLevelOperators.array2D" then
          sb.Append("['t]")
        else
          match genericParameters api with
          | [] -> sb.Append("")
          | genericParams ->
            sb.Append("[")
              .AppendJoin(",",(genericParams |> Seq.map (fun v -> v.Print())))
              .Append("]")
      
      let kindPart (sb: StringBuilder) =
        match api.Signature with
        | ApiSignature.ActivePatten _ -> sb.Append("active-pattern")
        | ApiSignature.ModuleValue _ when ns.Head.GenericParameters.IsEmpty = false -> sb.Append("type-function")
        | ApiSignature.ModuleValue _
        | ApiSignature.ModuleFunction _ -> sb.Append("function")
        | ApiSignature.InstanceMember (_, m) | ApiSignature.StaticMember (_, m) when m.Kind = MemberKind.Method -> sb.Append("method")
        | ApiSignature.InstanceMember (_, m) | ApiSignature.StaticMember (_, m) when m.Kind = MemberKind.Field -> sb
        | ApiSignature.InstanceMember _ | ApiSignature.StaticMember _ -> sb.Append("property")
        | ApiSignature.Constructor _ -> sb.Append("constructor")
        | ApiSignature.ModuleDefinition _ -> sb.Append("module")
        | ApiSignature.FullTypeDefinition td  ->
          if isArray td.Name.Head then
            sb
          else
            match td.Kind with
            | TypeDefinitionKind.Class -> sb.Append("class")
            | TypeDefinitionKind.Interface -> sb.Append("interface")
            | TypeDefinitionKind.Type -> sb.Append("type")
            | TypeDefinitionKind.Union -> sb.Append("union")
            | TypeDefinitionKind.Record -> sb.Append("record")
            | TypeDefinitionKind.Enumeration -> sb.Append("enumeration")
        | ApiSignature.TypeAbbreviation _ -> sb.Append("type-abbreviation")
        | ApiSignature.TypeExtension _ ->
          match urlName (Seq.last ns) with
          | "System" -> sb.Append("extension-method")
          | _ -> sb.Append("method")
        | ApiSignature.ExtensionMember _ -> sb.Append("extension-method")
        | ApiSignature.UnionCase _ -> sb
        | ApiSignature.ComputationExpressionBuilder _ -> sb.Append("class")

      let sb = StringBuilder().Append(namePart).Append(genericParamsPart).Append("-")
      let preLength = sb.Length
      do sb.Append(kindPart) |> ignore
      if preLength = sb.Length then
        return! None
      else
        return (string (sb.Append("-[fsharp]"))) |> toLower |> urlEncode
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

    type VariableMemory = Dictionary<string, string>

    let variableId (kind: ApiKind) (name: DisplayName) =
      let variableMemory = VariableMemory()
      let memory (prefix: string) (n: DisplayNameItem) =
        n.GenericParameters
        |> List.iteri (fun variableId p ->
          let variable = p.Name
          if variableMemory.ContainsKey(variable) = false then
            variableMemory.[variable] <- prefix + string variableId
        )
        
      match kind with
      | ApiKind.TypeDefinition ->
        name |> List.iter (memory "_")
      | _ ->
        name.Head |> memory "__"
        name.Tail |> List.iter (memory "_")
      variableMemory

    let nameElementsAndVariableId (api: Api) =
      let convert (modifiedString: string) ((wroteGeneric, result): bool * string list) (name: DisplayNameItem) =
        if wroteGeneric = false && name.GenericParameters.IsEmpty = false then
          let result = (urlName name + modifiedString + string name.GenericParameters.Length) :: result
          true, result
        else
          let result = urlName name :: result
          (wroteGeneric, result)
      
      let name = Name.toDisplayName api.Name

      let kind = api.Kind

      let elems =
        let initState = false, []
        [|
          match kind with
          | ApiKind.TypeDefinition ->
            yield! name |> Seq.rev |> Seq.fold (convert "-") initState |> snd |> Seq.rev
          | ApiKind.Constructor ->
            yield! name.Tail |> Seq.rev |> Seq.fold (convert "-") initState |> snd |> Seq.rev
            yield "-ctor"
          | _ ->
            yield! name.Tail |> Seq.rev |> Seq.fold (convert "-") initState |> snd |> Seq.rev
            yield! convert "--" initState name.Head |> snd
        |]

      let variableMemory = variableId kind name
      elems, variableMemory

    let urlPart elems (sb: StringBuilder) =
      let elems = elems |> Seq.map toLower
      sb.AppendJoin(".", elems)

    let rec parameterElement (api: Api) (variableMemory: VariableMemory) (t: LowType) (sb: StringBuilder) : StringBuilder =
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
      | Variable (_, v) -> sb.Append(variableMemory.[v.Name])
      | Delegate (d, _) -> sb.Append(parameterElement api variableMemory d)
      | AbbreviationRoot root -> sb.Append(parameterElement api variableMemory root)
      | _ -> sb

    let hasParameter (member': Member) =
      match member'.Parameters with
      | [] | [ [ { Type = Unit } ] ] -> false
      | _ -> true

    let hashPart (nameElems: string[]) (variableMemory: VariableMemory) (member': Member) (api: Api) (sb: StringBuilder) =
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