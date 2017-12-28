module internal FSharpApiSearch.CSharpFormat

open SpecialTypes.LowType.Patterns

let toDisplayName = function
  | SymbolName n -> n
  | OperatorName (_, n) -> n
  | WithCompiledName (_, n) -> n

let printNameItem (n: NameItem) (p: SignaturePrinter<_>) =
  match n.GenericParameters with
  | [] -> p.Append(toDisplayName n.Name)
  | args ->
    p.Append(toDisplayName n.Name)
      .Append("<")
        .AppendJoin(", ", args, (fun arg p -> p.Append(arg.Name)))
      .Append(">")

let printDisplayName_full xs (p: SignaturePrinter<_>) = p.AppendJoin(".", List.rev xs, printNameItem)

let printName_full (name: Name) (p: SignaturePrinter<_>) = p.Append(printDisplayName_full name)

let printApiName (name: Name) (p: SignaturePrinter<_>) =
  p.Append(printNameItem name.Head)

let printAccessPath depth (name: Name) (p: SignaturePrinter<_>) =
  let depth = Option.defaultValue (name.Tail.Length) depth
    
  let pathes = List.truncate depth name.Tail |> List.rev
  p.AppendJoin(".", pathes, printNameItem)

let csharpAlias =
  SpecialTypes.Identifier.CSharp.aliases
  |> List.map (fun (alias, t) ->
    let alias = ConcreteType { AssemblyName = "dummy"; Name = Name.ofString alias }
    t, alias
  )
  |> dict

let printTypeInfo (info: Identifier) (p: SignaturePrinter<_>) =
  let info =
    match csharpAlias.TryGetValue(info) with
    | true, alias -> alias
    | false, _ -> info

  let printDisplayName_short (xs: Name) (p: SignaturePrinter<_>) =
    match xs with
    | [] -> p.Append("<empty>")
    | n :: _ -> p.Append(toDisplayName n.Name)

  let printName_short name (p: SignaturePrinter<_>) = p.Append(printDisplayName_short name)
    
  match info with
  | ConcreteType a -> p.Append(printName_short a.Name)
  | UserInputType u -> p.Append(printDisplayName_short u.Name)

let toFSharpFunc (ps, ret) = List.foldBack (fun id ret -> Generic.create (SpecialTypes.LowType.FSharpFunc, [ id; ret ])) ps ret

let rec nestedArray acc = function
  | Array (name, elem, _) -> nestedArray (name :: acc) elem
  | x -> acc, x

let printRef isOut = if isOut then "out" else "ref"

let printLoadingName (name: LoadingName) (p: SignaturePrinter<_>) =
  match name.MemberName with
  | [] -> p.Append(name.RawName)
  | n2 ->
    p.Append(name.RawName).Append(".").Append(printName_full n2)

let rec printLowType t (p: SignaturePrinter<_>) =
  p.BeginPrintType(t) |> ignore
  let ret =
    match t with
    | Wildcard (name, _) ->
      match name with
      | Some n -> p.Append("?").Append(n)
      | None -> p.Append("?")
    | Variable (_, v, _) -> p.Append(v.Name)
    | Identifier (i, _) -> p.Append(printTypeInfo i)
    | Arrow (arrow, _) -> printLowType (toFSharpFunc arrow) p
    | Tuple ({ Elements = xs; IsStruct = false }, _) -> p.Append("Tuple<").AppendJoin(", ", xs, printLowType).Append(">")
    | Tuple ({ Elements = xs; IsStruct = true }, _) -> p.Append("(").AppendJoin(", ", xs, printLowType).Append(")")
    | Array (array, elem, _) ->
      let arrays, elem = nestedArray [ array ] elem
      p.Append(printLowType elem) |> ignore
      arrays |> Seq.rev |> Seq.iter (fun a -> p.Append(a) |> ignore)
      p
    | Generic (id, args, _) -> p.Append(printLowType id).Append("<").AppendJoin(", ", args, printLowType).Append(">")
    | TypeAbbreviation (t, _) -> p.Append(printLowType t.Original)
    | Delegate (t, _, _) -> p.Append(printLowType t)
    | ByRef (isOut, t, _) -> p.Append(printRef isOut).Append(" ").Append(printLowType t)
    | Subtype (t, _) -> p.Append("#").Append(printLowType t)
    | Choice (_, xs, _) -> p.Append("(").AppendJoin(" or ", xs, printLowType).Append(")")
    | LoadingType (n, _) -> p.Append(printLoadingName n)
  p.EndPrintType(t) |> ignore
  ret

let printParameter (param: Parameter) (p: SignaturePrinter<_>) =
  if param.IsParamArray then p.Append("params ") |> ignore
  if param.IsOptional then p.Append("[") |> ignore
  p.Append(printLowType param.Type) |> ignore
  param.Name |> Option.iter (fun name -> p.Append(" ").Append(name) |> ignore)
  if param.IsOptional then p.Append("]") |> ignore
  p

let printPropertyParameter (m: Member) (p: SignaturePrinter<_>) =
  let parameters = m.Parameters |> List.collect id
  p.Append("[").AppendJoin(", ", parameters, printParameter).Append("]")

let printProperty (m: Member) (p: SignaturePrinter<_>) =
  if List.isEmpty m.Parameters = false then p.Append(printPropertyParameter m) |> ignore
  p.Append(" : ").Append(printLowType m.ReturnParameter.Type)

let printReturnParameter (param: Parameter) (p: SignaturePrinter<_>) =
  match param.Type with
  | Unit -> p.Append("void")
  | t -> p.Append(printLowType t)

let printMethodParameter (m: Member) (isExtension: bool) (p: SignaturePrinter<_>) =
  let parameters = m.Parameters |> List.collect id
  match parameters with
  | [ { Type = Unit } ] ->
    p.Append("()")
  | _ ->
    if isExtension then
      p.Append("(this ") |> ignore
    else
      p.Append("(") |> ignore
    p.AppendJoin(", ", parameters, printParameter).Append(")")

let printMethod (m: Member) (isExtension: bool) (p: SignaturePrinter<_>) =
  p.Append(printMethodParameter m isExtension)
    .Append(" : ")
    .Append(printReturnParameter m.ReturnParameter)

let printField (m: Member) (p: SignaturePrinter<_>) =
  p.Append(" : ").Append(printLowType m.ReturnParameter.Type)

let printMember (m: Member) (p: SignaturePrinter<_>) =
  match m.Kind with
  | MemberKind.Property _ -> p.Append(printProperty m)
  | MemberKind.Method -> p.Append(printMethod m false)
  | MemberKind.Field -> p.Append(printField m)

let printConstructor (m: Member) (p: SignaturePrinter<_>) =
  p.Append(printMethodParameter m false).Append(" : void")

let printModuleValue (t: LowType) (p: SignaturePrinter<_>) = p.Append(" : ").Append(printLowType t)

let printFunction (fn: Function) (p: SignaturePrinter<_>) =
  let m = {
    Name = "dummy"
    Kind = MemberKind.Method
    GenericParameters = []
    Parameters = fst fn
    ReturnParameter = snd fn
  }
  printMethod m false p

let printFullTypeDefinition (td: FullTypeDefinition) (p: SignaturePrinter<_>) =
  let kind =
    match td.Kind with
    | TypeDefinitionKind.Class
    | TypeDefinitionKind.Record
    | TypeDefinitionKind.Type
    | TypeDefinitionKind.Union -> "class"
    | TypeDefinitionKind.Interface -> "interface"
    | TypeDefinitionKind.Enumeration -> "enum"
  p.Append(" : ").Append(kind).Append(" ").Append(printNameItem td.Name.[0])

let printApiSignature (apiSig: ApiSignature) (p: SignaturePrinter<_>) =
  let error name = failwithf "%s is not C# api." name
  match apiSig with
  | ApiSignature.ModuleValue t -> p.Append(printModuleValue t)
  | ApiSignature.ModuleFunction fn -> p.Append(printFunction fn)
  | ApiSignature.ActivePatten (_, _) -> error "ActivePattern"
  | ApiSignature.InstanceMember (_, m) -> p.Append(printMember m)
  | ApiSignature.StaticMember (_, m) -> p.Append(printMember m)
  | ApiSignature.Constructor (_, m) -> p.Append(printConstructor m)
  | ApiSignature.ModuleDefinition _ -> error "Module"
  | ApiSignature.FullTypeDefinition td -> p.Append(printFullTypeDefinition td)
  | ApiSignature.TypeAbbreviation _ -> error "TypeAbbreviation"
  | ApiSignature.TypeExtension _ -> error "TypeExtension"
  | ApiSignature.ExtensionMember m -> p.Append(printMethod m true)
  | ApiSignature.UnionCase _ -> error "UnionCase"
  | ApiSignature.ComputationExpressionBuilder _ -> error "ComputationExpression"

let csharpTypeConstraintPred (tc: TypeConstraint) =
  match tc.Constraint with
  | Constraint.NullnessConstraints
  | Constraint.MemberConstraints _
  | Constraint.EnumerationConstraints
  | Constraint.DelegateConstraints
  | Constraint.UnmanagedConstraints
  | Constraint.EqualityConstraints
  | Constraint.ComparisonConstraints -> false

  | Constraint.SubtypeConstraints _
  | Constraint.DefaultConstructorConstraints
  | Constraint.ValueTypeConstraints
  | Constraint.ReferenceTypeConstraints -> true

let filterCSharpTypeConstraint (xs: TypeConstraint list) =
  xs |> List.filter csharpTypeConstraintPred

let printConstraints (xs: TypeConstraint list) (p: SignaturePrinter<_>) =
  let printConstraint (c: TypeConstraint) (p: SignaturePrinter<_>) =
    match c.Constraint with
    | Constraint.SubtypeConstraints s -> p.Append(printLowType s) |> ignore
    | Constraint.DefaultConstructorConstraints -> p.Append("new()") |> ignore
    | Constraint.ValueTypeConstraints -> p.Append("struct") |> ignore
    | Constraint.ReferenceTypeConstraints -> p.Append("class") |> ignore
    | _ -> failwith "It is not C# constraint."

    p

  let printVariable (variable: TypeVariable, constraints: TypeConstraint list) (p: SignaturePrinter<_>) =
    p.Append("where ").Append(variable.Name).Append(" : ").AppendJoin(", ", constraints, printConstraint)

  let constraints = xs |> List.groupBy (fun c -> c.Variables.Head)
  p.AppendJoin(" ", constraints, printVariable)

let printPropertyKind kind (p: SignaturePrinter<_>) =
  match kind with
  | PropertyKind.Get -> p.Append("get")
  | PropertyKind.Set -> p.Append("set")
  | PropertyKind.GetSet -> p.Append("get set")
let printMemberKind kind (p: SignaturePrinter<_>) =
  match kind with
  | MemberKind.Method -> p.Append("method")
  | MemberKind.Property prop -> p.Append("property with ").Append(printPropertyKind prop)
  | MemberKind.Field -> p.Append("field")
let printMemberModifier modi (p: SignaturePrinter<_>) =
  match modi with
  | MemberModifier.Instance -> p.Append("instance")
  | MemberModifier.Static -> p.Append("static")
let printApiKind kind (p: SignaturePrinter<_>) =
  match kind with
  | ApiKind.ModuleValue -> p.Append(printMemberModifier MemberModifier.Static).Append(" ").Append(printMemberKind MemberKind.Method)
  | ApiKind.Constructor -> p.Append("constructor")
  | ApiKind.Member (modifier, memberKind) -> p.Append(printMemberModifier modifier).Append(" ").Append(printMemberKind memberKind)
  | ApiKind.ExtensionMember -> p.Append("extension method")
  | ApiKind.TypeDefinition -> p.Append("type")

  | ApiKind.UnionCase
  | ApiKind.ModuleDefinition
  | ApiKind.TypeAbbreviation
  | ApiKind.ComputationExpressionBuilder
  | ApiKind.TypeExtension _ -> failwith "It is not C# api."