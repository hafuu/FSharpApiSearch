namespace FSharpApiSearch

type LinkGenerator = Api -> string option

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module LinkGenerator =
  open System.Web

  let internal genericParameters (api: Api) = api.Name |> Name.displayName |> Seq.rev |> Seq.collect (fun x -> x.GenericParametersForDisplay) |> Seq.distinct |> Seq.toList

  let internal toLower (str: string) = str.ToLower()
  let internal urlEncode (str: string) = HttpUtility.UrlEncode(str)

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

    let isArray (n: NameItem) = n.FSharpName.StartsWith("[")

    let generate (api: Api) = option {
      let ns =
        let ns = Name.displayName api.Name
        match api.Signature with
        | ApiSignature.Constructor _ -> ns.Tail // skip "new"
        | _ -> ns
      let namePart =
        let name =
          let name = ns.Head.FSharpName
          if isActivePattern api then
            name.[3..(name.Length - 4)].Replace("|_", "").Replace("|", "h")
          elif name.StartsWith("( ") then
            replaceOp name
          else
            name
        let path = (ns.Item 1).FSharpName
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
        | ApiSignature.ModuleValue _ when ns.Head.GenericParametersForDisplay.IsEmpty = false -> Some "type-function"
        | ApiSignature.ModuleValue _
        | ApiSignature.ModuleFunction _ -> Some "function"
        | ApiSignature.InstanceMember (_, m) | ApiSignature.StaticMember (_, m) when m.Kind = MemberKind.Method -> Some "method"
        | ApiSignature.InstanceMember (_, m) | ApiSignature.StaticMember (_, m) when m.Kind = MemberKind.Field -> None
        | ApiSignature.InstanceMember _ | ApiSignature.StaticMember _ -> Some "property"
        | ApiSignature.Constructor _ -> Some "constructor"
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
          match (Seq.last ns).FSharpName with
          | "System" -> Some "extension-method"
          | _ -> Some "method"
        | ApiSignature.ExtensionMember _ -> Some "extension-method"
        | ApiSignature.UnionCase _ -> None

      return sprintf "%s%s-%s-[fsharp]" namePart genericParamsPart kindPart |> toLower |> urlEncode
    }

  module internal Msdn =
    let isGeneric api = api.Name |> Name.displayName |> List.exists (fun n -> List.isEmpty n.GenericParametersForDisplay = false)
    let canGenerate (api: Api) =
      match api.Signature with
      | ApiSignature.ActivePatten _
      | ApiSignature.ModuleValue _
      | ApiSignature.ModuleFunction _
      | ApiSignature.Constructor _
      | ApiSignature.FullTypeDefinition _
      | ApiSignature.TypeAbbreviation _
      | ApiSignature.TypeExtension _
      | ApiSignature.ExtensionMember _
      | ApiSignature.UnionCase _ -> false

      | ApiSignature.InstanceMember _
      | ApiSignature.StaticMember _ -> true

    let generate (api: Api) =
      if isGeneric api then
        None
      elif canGenerate api = false then
        None
      else
        (Name.displayName api.Name |> Seq.rev |> Seq.map (fun n -> n.FSharpName) |> String.concat ".") + ".aspx"
        |> toLower
        |> Some
        

  let fsharp baseUrl: LinkGenerator = fun api -> FSharp.generate api |> Option.map (fun apiUrl -> baseUrl + apiUrl)
  let msdn baseUrl: LinkGenerator = fun api -> Msdn.generate api |> Option.map (fun apiUrl -> baseUrl + apiUrl)