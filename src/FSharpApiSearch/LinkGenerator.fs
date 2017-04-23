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
          match (Seq.last ns).FSharpName with
          | "System" -> Some "extension-method"
          | _ -> Some "method"
        | ApiSignature.ExtensionMember _ -> Some "extension-method"
        | ApiSignature.UnionCase _ -> None
        | ApiSignature.ComputationExpressionBuilder _ -> Some "class"

      return sprintf "%s%s-%s-[fsharp]" namePart genericParamsPart kindPart |> toLower |> urlEncode
    }

  module internal Msdn =
    let isGeneric api = api.Name |> Name.displayName |> List.exists (fun n -> List.isEmpty n.GenericParametersForDisplay = false)

    let generateMSDNUrl (api :Api) (mem : Member) =
        if isGeneric api then
            let mutable genericParameterIsWrite = false
            let urlBase : list<string> = api.Name |> Name.displayName |> Seq.rev 
                                            |> Seq.map (
                                                fun n -> 
                                                    if genericParameterIsWrite = false && n.GenericParametersForDisplay.IsEmpty = false then
                                                        genericParameterIsWrite <- true
                                                        let genericCounter = n.GenericParametersForDisplay |> List.length
                                                        n.FSharpName + "-" + (genericCounter.ToString())
                                            
                                                    else
                                                        n.FSharpName
                                            ) 
                                            |> List.ofSeq
            let url = urlBase |> String.concat "." |> toLower
            let parameter = urlBase |> List.map(fun (x : string) -> x.Replace("-","_")) |> String.concat "_"
            url + "?#" + parameter  |> Some
        else
            let urlBase = (Name.displayName api.Name |> Seq.rev |> Seq.map (fun n -> n.FSharpName) |> String.concat ".")
                            |> toLower
            let nameParameter = (Name.displayName api.Name |> Seq.rev |> Seq.map (fun n -> n.FSharpName) |> String.concat "_")

            let isArray (id : Identity) =   match id with
                                            | FullIdentity fid ->
                                                match fid.Name with
                                                | DisplayName dname ->
                                                    dname |> List.exists(fun x -> x.FSharpName = "[]")
                                                | _ -> false
                                            | _ -> false
            
            let getParameterString (lt : LowType) =
                                        seq {
                                                match lt with
                                                | TypeAbbreviation t -> 
                                                    match t.Original with
                                                    | Identity originalID -> 
                                                        match originalID with
                                                        | FullIdentity foid  -> 
                                                            match foid.Name  with
                                                            | DisplayName dname -> 
                                                                if dname.Head.FSharpName = "Unit" then
                                                                    yield ""
                                                                else
                                                                    let revdname = dname |> List.rev
                                                                    for nameItem in revdname do
                                                                        yield nameItem.FSharpName
                                                            | _ -> yield ""
                                                        | _ -> yield ""
                                                    | _ -> yield ""
                                                | _ -> yield ""
                                        }
            let parameterQuery = seq {
                                        for param in mem.Parameters do
                                            for param2 in param do
                                                match param2.Type with
                                                | TypeAbbreviation t -> 
                                                    yield! getParameterString param2.Type
                                                
                                                | Generic (Identity id , [elem]) -> 
                                                    if (id |> isArray) then
                                                        yield! getParameterString elem
                                                        yield "_"
                                                
                                                | _ -> yield ""
                                     } |> String.concat "_"
    
            if parameterQuery.Length > 0 then
                (urlBase + "?#" + nameParameter + "_" + parameterQuery + "_") |> Some
            else
                (urlBase + "?#" + nameParameter) |> Some

    let generate (api: Api) =
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
            | ApiSignature.UnionCase _ -> None

            | ApiSignature.InstanceMember (_ , (x : Member)) ->
                generateMSDNUrl api x

            | ApiSignature.StaticMember (_ , (x : Member)) ->
                generateMSDNUrl api x

  let fsharp baseUrl: LinkGenerator = fun api -> FSharp.generate api |> Option.map (fun apiUrl -> baseUrl + apiUrl)
  let msdn baseUrl: LinkGenerator = fun api -> Msdn.generate api |> Option.map (fun apiUrl -> baseUrl + apiUrl)