namespace FSharpApiSearch

type LinkGenerator = Api -> string option

open FSharpApiSearch.Printer

module LinkGenerator =
  open System.Web

  let internal genericParameters (api: Api) = api.Name |> Name.toDisplayName |> Seq.rev |> Seq.collect (fun x -> x.GenericParameters) |> Seq.distinct |> Seq.toList

  let internal toLower (str: string) = str.ToLower()
  let internal urlEncode (str: string) = HttpUtility.UrlEncode(str)

  let internal urlName (n: DisplayNameItem) =
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

    let namePart (api: Api) (ns: DisplayName) (sb: StringBuilder) =
      let path = urlName (List.item 1 ns)
      let name = urlName ns.Head
      sb.Append(path).Append(".") |> ignore
      if isActivePattern api then
        sb.Append(name.[3..(name.Length - 4)].Replace("|_", "").Replace("|", "h"))
      elif name.StartsWith("( ") then
        replaceOp name sb
      else
        sb.Append(name)

    let genericParamsPart (api: Api) (ns: DisplayName) (sb: StringBuilder) =
      match ns with
      | { Name = OperatorName ("( = )", _) } :: { Name = SymbolName "Operators" } :: _ -> sb.Append("'t")
      | { Name = WithCompiledName ("array2D", "CreateArray2D") } :: { Name = SymbolName "ExtraTopLevelOperators" } :: _ -> sb.Append("['t]")
      | _ ->
        match genericParameters api with
        | [] -> sb.Append("")
        | genericParams ->
          sb.Append("[")
            .AppendJoin(",",(genericParams |> Seq.map (fun v -> v.Print())))
            .Append("]")

    let kindPart (api: Api) (ns: DisplayName) : string option =
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

    let generate (api: Api) =
      let ns =
        let ns = Name.toDisplayName api.Name
        match api.Signature with
        | ApiSignature.Constructor _ -> ns.Tail // skip "new"
        | _ -> ns
      kindPart api ns
      |> Option.map (fun kind ->
        let sb = StringBuilder()
        sb.Append(namePart api ns).Append(genericParamsPart api ns).Append("-").Append(kind).Append("-[fsharp]") |> ignore
        string sb |> toLower |> urlEncode
      )

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
        seq {
          match kind with
          | ApiKind.TypeDefinition ->
            yield! name |> Seq.rev |> Seq.fold (convert "_") initState |> snd |> Seq.rev
          | ApiKind.Constructor ->
            yield! name.Tail |> Seq.rev |> Seq.fold (convert "_") initState |> snd |> Seq.rev
            yield "_ctor"
          | _ ->
            yield! name.Tail |> Seq.rev |> Seq.fold (convert "_") initState |> snd |> Seq.rev
            yield! convert "__" initState name.Head |> snd
        }

      let urlelems =
        let initState = false, []
        seq {
          match kind, api.Signature with
          | ApiKind.TypeDefinition, _ ->
            yield! name |> Seq.rev |> Seq.fold (convert "-") initState |> snd |> Seq.rev
          | ApiKind.Constructor, _ ->
            yield! name.Tail |> Seq.rev |> Seq.fold (convert "-") initState |> snd |> Seq.rev
            yield "-ctor"
          | _ ->
            yield! name.Tail |> Seq.rev |> Seq.fold (convert "-") initState |> snd |> Seq.rev
            yield urlName name.Head
        }

      let variableMemory = variableId kind name
      urlelems, elems, variableMemory

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
      | Generic (Identity(FullIdentity{Name = DisplayName({Name = SymbolName("nativeptr")}::{Name = SymbolName("Core")}::{Name = SymbolName("FSharp")}::{Name = SymbolName("Microsoft")}::[])}), args) ->
        sb.AppendJoin("_", args, (parameterElement api variableMemory))
            .Append("_")
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

    let hashPart (nameElems: seq<string>) (variableMemory: VariableMemory) (member': Member) (api: Api) (sb: StringBuilder) =
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
        let urlElems, _, _ = nameElementsAndVariableId api
        let sb = StringBuilder().Append(urlPart urlElems).Append("?view=").Append(view)
        Some (string sb)

      | ApiSignature.ExtensionMember (member' : Member)
      | ApiSignature.Constructor (_ , (member' : Member))
      | ApiSignature.InstanceMember (_ , (member' : Member))
      | ApiSignature.StaticMember (_ ,(member' : Member)) ->
        let urlElems, nameElems, variableMemory = nameElementsAndVariableId api
        let sb = StringBuilder().Append(urlPart urlElems).Append("?view=").Append(view).Append("#").Append(hashPart nameElems variableMemory member' api)
        Some (string sb)

  module internal FParsec =
    open System.Text
    
    let moduleName (api: Api) =
      let names = api.Name |> Name.toDisplayName |> List.rev
      match names with
      | { Name = SymbolName ("FParsec") } :: { Name = SymbolName moduleName } :: _ ->
        if moduleName = "Internals" || moduleName = "Emit" then
          None
        else
          Some moduleName
      | _ -> None

    let urlPart (moduleName: string) (sb: StringBuilder) =
      sb.Append(toLower moduleName).Append(".html")

    let opReplaceTable =
      dict [
        ' ', ""
        '(', ""
        ')', ""
        '.', ".."
      ]

    let opReplace (ns: DisplayNameItem) (sb: StringBuilder) =
      match ns.Name with
      | OperatorName (name, _) ->
        for ch in name do
          match opReplaceTable.TryGetValue(ch) with
          | true, replaced -> sb.Append(replaced) |> ignore
          | false, _ -> sb.Append(":").Append(int ch).Append(":") |> ignore
        sb
      | SymbolName name -> sb.Append(name)
      | WithCompiledName (name, _) -> sb.Append(name)

    let membersPart (ns: DisplayName) (sb: StringBuilder) =
      sb.Append("members.").Append(ns.Head |> opReplace)

    let staticMembersPart (ns: DisplayName) (sb: StringBuilder) =
      sb.Append("members.").Append(urlName ns.[1]).Append("..").Append(urlName ns.[0])

    let generate (api: Api) =
      moduleName api
      |> Option.bind (fun moduleName ->
        match api.Signature with
        | ApiSignature.ActivePatten _    
        | ApiSignature.TypeExtension _
        | ApiSignature.UnionCase _ 
        | ApiSignature.ExtensionMember _
        | ApiSignature.Constructor _ -> None
      
        | ApiSignature.ModuleDefinition _ ->
          let sb = StringBuilder().Append(urlPart moduleName)
          Some (string sb)  

        | ApiSignature.ComputationExpressionBuilder _
        | ApiSignature.ModuleFunction _   
        | ApiSignature.ModuleValue _
        | ApiSignature.TypeAbbreviation _
        | ApiSignature.FullTypeDefinition _ 
        | ApiSignature.InstanceMember _ ->
          let sb = StringBuilder().Append(urlPart moduleName).Append("#").Append(api.Name |> Name.toDisplayName |> membersPart)
          Some (string sb)

        | ApiSignature.StaticMember (_, mem) ->
          match mem.Kind with
          | MemberKind.Method ->
            let sb = StringBuilder().Append(urlPart moduleName).Append("#").Append(api.Name |> Name.toDisplayName |> staticMembersPart)
            Some (string sb)
          | _ -> None
      )

  let fsharp baseUrl: LinkGenerator = fun api -> FSharp.generate api |> Option.map (fun apiUrl -> baseUrl + apiUrl)
  let msdn baseUrl: LinkGenerator = fun api -> Msdn.generate api |> Option.map (fun apiUrl -> baseUrl + apiUrl)
  let dotNetApiBrowser baseUrl (view: string) : LinkGenerator = fun api -> DotNetApiBrowser.generate view api |> Option.map (fun apiUrl -> baseUrl + apiUrl)
  let fparsec baseUrl : LinkGenerator = fun api -> FParsec.generate api |> Option.map(fun apiUrl -> baseUrl + apiUrl)