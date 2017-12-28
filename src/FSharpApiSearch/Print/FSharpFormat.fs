module internal FSharpApiSearch.FSharpFormat

open SpecialTypes

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
  | ApiKind.ModuleValue -> p.Append("module value")
  | ApiKind.Constructor -> p.Append("constructor")
  | ApiKind.Member (modifier, memberKind) -> p.Append(printMemberModifier modifier).Append(" ").Append(printMemberKind memberKind)
  | ApiKind.TypeExtension (modifier, memberKind) -> p.Append(printMemberModifier modifier).Append(" ").Append(printMemberKind memberKind)
  | ApiKind.ExtensionMember -> p.Append("extension member")
  | ApiKind.UnionCase -> p.Append("union case")
  | ApiKind.ModuleDefinition -> p.Append("module")
  | ApiKind.TypeDefinition -> p.Append("type")
  | ApiKind.TypeAbbreviation -> p.Append("type abbreviation")
  | ApiKind.ComputationExpressionBuilder -> p.Append("builder")

let typeVariablePrefix (v: TypeVariable) = if v.IsSolveAtCompileTime then "^" else "'"

let toDisplayName = function
  | SymbolName n -> n
  | OperatorName (n, _) -> n
  | WithCompiledName (n, _) -> n

let printNameItem (n: NameItem) (p: SignaturePrinter<_>) =
  match n.GenericParameters with
  | [] -> p.Append(toDisplayName n.Name)
  | args ->
    p.Append(toDisplayName n.Name)
      .Append("<")
        .AppendJoin(", ", args, (fun arg p -> p.Append(typeVariablePrefix arg).Append(arg.Name)))
      .Append(">")

let printDisplayName_full (xs: Name) (p: SignaturePrinter<_>) =
  match xs with
  | [] -> p.Append("<empty>")
  | ns ->
    ns.Tail
    |> List.rev
    |> List.iter (fun n -> p.Append(printNameItem n).Append(".") |> ignore)
    p.Append(toDisplayName ns.Head.Name)

let printName_full (name: Name) (p: SignaturePrinter<_>) = p.AppendJoin(".", List.rev name, printNameItem)

let printApiName (name: Name) (p: SignaturePrinter<_>) = p.Append(printNameItem name.Head)

let printAccessPath depth (name: Name) (p: SignaturePrinter<_>) =
  let depth = Option.defaultValue (name.Tail.Length) depth
    
  let pathes = List.truncate depth name.Tail |> List.rev
  p.AppendJoin(".", pathes, printNameItem)

let printTypeInfo_full (typeInfo: Identifier) (p: SignaturePrinter<_>) =
  match typeInfo with
  | ConcreteType a ->
    p.Append(printDisplayName_full a.Name)
  | UserInputType u -> p.Append(printDisplayName_full u.Name)

let printTypeInfo_short (typeInfo: Identifier) (p: SignaturePrinter<_>) =
  let printDisplayName_short (xs: Name) (p: SignaturePrinter<_>) =
    match xs with
    | [] -> p.Append("<empty>")
    | n :: _ -> p.Append(toDisplayName n.Name)

  let printName_short name (p: SignaturePrinter<_>) = p.Append(printDisplayName_short name)
    
  match typeInfo with
  | ConcreteType a -> p.Append(printName_short a.Name)
  | UserInputType u -> p.Append(printDisplayName_short u.Name)

let printVariableSource = function
  | VariableSource.Query -> "q"
  | VariableSource.Target -> "t"

let printTypeVariable isDebug source v (p: SignaturePrinter<_>) =
  if isDebug then
    p.Append(typeVariablePrefix v).Append(printVariableSource source).Append("_").Append(v.Name)
  else
    p.Append(typeVariablePrefix v).Append(v.Name)

let printLoadingName (name: LoadingName) (p: SignaturePrinter<_>) =
  match name.MemberName with
  | [] -> p.Append(name.RawName)
  | n2 ->
    p.Append(name.RawName).Append(".").Append(printName_full n2)

let rec printLowType isDebug (printTypeInfo: Identifier -> SignaturePrinter<_> -> SignaturePrinter<_>) lowType (p: SignaturePrinter<_>) =
  p.BeginPrintType(lowType) |> ignore
  let ret =
    match lowType with
    | Wildcard (name, _) ->
      match name with
      | Some n -> p.Append("?").Append(n)
      | None -> p.Append("?")
    | Variable (source, v, _) -> p.Append(printTypeVariable isDebug source v)
    | Identifier (i, _) -> p.Append(printTypeInfo i)
    | Arrow (arrow, _) -> p.Append(printArrow isDebug printTypeInfo arrow)
    | Tuple ({ Elements = xs; IsStruct = false }, _) -> p.Append(printTuple isDebug printTypeInfo xs)
    | Tuple ({ Elements = xs; IsStruct = true }, _) -> p.Append(printStructTuple isDebug printTypeInfo xs)
    | LowType.Patterns.Array (name, elem, _) ->
      match elem with
      | Tuple ({ IsStruct = false }, _) | Arrow _ ->
        p.Append("(")
          .Append(printLowType isDebug printTypeInfo elem)
          .Append(")")
          |> ignore
      | _ -> p.Append(printLowType isDebug printTypeInfo elem) |> ignore
      p.Append(name)
    | Generic (id, args, _) -> p.Append(printGeneric isDebug printTypeInfo id args)
    | TypeAbbreviation (t, _) -> p.Append(printLowType isDebug printTypeInfo t.Abbreviation)
    | Delegate (t, _, _) -> p.Append(printLowType isDebug printTypeInfo t)
    | ByRef (_, t, _) -> p.Append("byref<").Append(printLowType isDebug printTypeInfo t).Append(">")
    | Subtype (t, _) -> p.Append("#").Append(printLowType isDebug printTypeInfo t)
    | Choice (_, xs, _) -> p.Append(printChoice isDebug printTypeInfo xs)
    | LoadingType (name, _) -> p.Append(printLoadingName name)
  p.EndPrintType(lowType) |> ignore
  ret
and printGeneric isDebug printTypeInfo id (args: _ list) (p: SignaturePrinter<_>) =
  p.Append(printLowType isDebug printTypeInfo id)
    .Append("<")
    .AppendJoin(", ", args, (printLowType isDebug printTypeInfo))
    .Append(">")
and printArrowItem isDebug printTypeInfo (item: LowType) (p: SignaturePrinter<_>) =
  match item with
  | Arrow _ as a ->
    p.Append("(")
      .Append(printLowType isDebug printTypeInfo a)
      .Append(")")
  | x -> p.Append(printLowType isDebug printTypeInfo x)
and printArrow isDebug printTypeInfo (arrow: Arrow) (p: SignaturePrinter<_>) =
  let ps, ret = arrow
  p.AppendJoin(" -> ", ps, printArrowItem isDebug printTypeInfo).Append(" -> ").Append(printArrowItem isDebug printTypeInfo ret)
and printTuple isDebug printTypeInfo (xs: _ list) (p: SignaturePrinter<_>) =
  let printItem lowType (p: SignaturePrinter<_>) =
    match lowType with
    | Tuple _ as t ->
      p.Append("(")
        .Append(printLowType isDebug printTypeInfo t)
        .Append(")")
    | x -> p.Append(printLowType isDebug printTypeInfo x)
  p.AppendJoin(" * ", xs, printItem)
and printStructTuple isDebug printTypeInfo (xs: _ list) (p: SignaturePrinter<_>) =
  let printItem lowType (p: SignaturePrinter<_>) =
    match lowType with
    | Tuple ({ IsStruct = false  }, _) | Arrow _ ->
      p.Append("(")
        .Append(printLowType isDebug printTypeInfo lowType)
        .Append(")")
    | _ -> p.Append(printLowType isDebug printTypeInfo lowType)
  p.Append("struct (")
    .AppendJoin(" * ", xs, printItem)
    .Append(")")
and printChoice isDebug printTypeInfo (xs: _ list) (p: SignaturePrinter<_>) =
  p.Append("(")
    .AppendJoin(" or ", xs, printLowType isDebug printTypeInfo)
    .Append(")")

let printLowType_short isDebug t (p: SignaturePrinter<_>) = p.Append(printLowType isDebug printTypeInfo_short t)
let printLowType_full isDebug t (p: SignaturePrinter<_>) = p.Append(printLowType isDebug printTypeInfo_full t)

let printParameter tupleParen isDebug (param: Parameter) (p: SignaturePrinter<_>) =
  if param.IsParamArray then p.Append("[<ParamArray>]") |> ignore

  match param.IsOptional with
  | true -> p.Append("?") |> ignore
  | false -> ()

  match param.Name with
  | Some name -> p.Append(name).Append(":") |> ignore
  | None -> ()

  let hasName = param.Name |> Option.isSome

  match param with
  | { Type = Tuple _ } when tupleParen || hasName ->
    p.Append("(")
      .Append(printLowType_short isDebug param.Type)
      .Append(")")
  | { Type = Arrow _ } ->
    p.Append("(")
      .Append(printLowType_short isDebug param.Type)
      .Append(")")
  | _ -> p.Append(printLowType_short isDebug param.Type)

let printParameterGroups tupleParen isDebug (pg: ParameterGroups) (p: SignaturePrinter<_>) =
  p.AppendJoin(" -> ", pg, (fun ps p -> p.AppendJoin(" * ", ps, printParameter tupleParen isDebug)))

let printFunction tupleParen isDebug (func: Function) (p: SignaturePrinter<_>) =
  let ps, ret = func
  p.Append(printParameterGroups tupleParen isDebug ps).Append(" -> ").Append(printParameter tupleParen isDebug ret)

let printMember isDebug (m: Member) (p: SignaturePrinter<_>) =
  match m.Parameters with
  | [] -> p.Append(printLowType_short isDebug m.ReturnParameter.Type)
  | _ ->
    p.Append(printParameterGroups true isDebug m.Parameters)
      .Append(" -> ")
      .Append(printArrowItem isDebug printTypeInfo_short m.ReturnParameter.Type)

let printConstraint isDebug (c: TypeConstraint) (p: SignaturePrinter<_>) =
  let variableSource = VariableSource.Target

  match c.Variables with
  | [ v ] ->
    p.Append(printTypeVariable isDebug variableSource v) |> ignore
  | vs ->
    p.Append("(")
      .AppendJoin(" or ", vs, printTypeVariable isDebug variableSource)
      .Append(")")
    |> ignore

  p.Append(" ") |> ignore

  match c.Constraint with
  | Constraint.SubtypeConstraints s ->
    p.Append(":> ")
      .Append(printLowType_short isDebug s)
  | Constraint.NullnessConstraints -> p.Append(": null")
  | Constraint.MemberConstraints (modifier, member') ->
    let printMemberModifier modifier (p: SignaturePrinter<_>) =
      match modifier with
      | MemberModifier.Static -> p.Append("static member")
      | MemberModifier.Instance -> p.Append("member")
    p.Append(": (")
        .Append(printMemberModifier modifier)
        .Append(" ").Append(member'.Name).Append(" : ")
        .Append(printMember isDebug member')
      .Append(")")
  | Constraint.DefaultConstructorConstraints ->
    p.Append(": (new : unit -> ")
      .Append(printTypeVariable isDebug variableSource (c.Variables.Head))
      .Append(")")
  | Constraint.ValueTypeConstraints -> p.Append(": struct")
  | Constraint.ReferenceTypeConstraints -> p.Append(": not struct")
  | Constraint.EnumerationConstraints -> p.Append(": enum")
  | Constraint.DelegateConstraints -> p.Append(": delegate")
  | Constraint.UnmanagedConstraints -> p.Append(": unmanaged")
  | Constraint.EqualityConstraints -> p.Append(": equality")
  | Constraint.ComparisonConstraints -> p.Append(": comparison")
    
let printFullTypeDefinition isDebug (x: FullTypeDefinition) (p: SignaturePrinter<_>) =
  p.Append("type ")
    .Append(printLowType_short isDebug x.LowType)

let pringTypeAbbreviation isDebug (x: TypeAbbreviationDefinition) (p: SignaturePrinter<_>) =
  p.Append("type ")
    .Append(printLowType_short isDebug x.TypeAbbreviation.Abbreviation)
    .Append(" = ")
    .Append(printLowType_full isDebug x.Abbreviated)

let printUnionCaseField isDebug (uc: UnionCaseField) (p: SignaturePrinter<_>) =
  match uc.Name with
  | Some name ->
    p.Append(name).Append(":").Append(printLowType_short isDebug uc.Type)
  | None -> p.Append(printLowType_short isDebug uc.Type)

let printUnionCase isDebug (uc: UnionCase) (p: SignaturePrinter<_>) =
  if uc.Fields.IsEmpty then
    p.Append(printLowType_short isDebug uc.DeclaringType)
  else
    p.Append(printFunction true isDebug (UnionCase.toFunction uc))

let printModule (m: ModuleDefinition) (p: SignaturePrinter<_>) = p.Append("module ").Append(toDisplayName m.Name.Head.Name)

let printComputationExpressionBuilder isDebug (builder: ComputationExpressionBuilder) (p: SignaturePrinter<_>) =
  if isDebug then
    p.Append("type ")
      .Append(printLowType_short isDebug builder.BuilderType)
      .Append(", [ ")
      .AppendJoin("; ", builder.ComputationExpressionTypes, printLowType_short isDebug)
      .Append(" ], { ")
      .AppendJoin("; ", builder.Syntaxes, (fun syntax p -> p.Append(syntax)))
      .Append(" }")
  else
    p.Append("type ")
      .Append(printLowType_short isDebug builder.BuilderType)
      .Append(", { ")
      .AppendJoin("; ", builder.Syntaxes, (fun syntax p -> p.Append(syntax)))
      .Append(" }")

let printApiSignature isDebug apiSig (p: SignaturePrinter<_>) =
  match apiSig with
  | ApiSignature.ModuleValue t -> p.Append(printLowType_short isDebug t)
  | ApiSignature.ModuleFunction fn -> p.Append(printFunction false isDebug fn)
  | ApiSignature.ActivePatten (_, fn) -> p.Append(printFunction false isDebug fn)
  | ApiSignature.InstanceMember (declaringType, m) ->
    if isDebug then
      p.Append(printLowType_short isDebug declaringType)
        .Append(" => ")
        .Append(printMember isDebug m)
    else
      p.Append(printMember isDebug m)
  | ApiSignature.StaticMember (_, m) -> p.Append(printMember isDebug m)
  | ApiSignature.Constructor (_, m) -> p.Append(printMember isDebug m)
  | ApiSignature.ModuleDefinition m -> p.Append(printModule m)
  | ApiSignature.FullTypeDefinition x -> p.Append(printFullTypeDefinition isDebug x)
  | ApiSignature.TypeAbbreviation t -> p.Append(pringTypeAbbreviation isDebug t)
  | ApiSignature.TypeExtension t ->
    if isDebug then
      p.Append(printLowType_short isDebug t.ExistingType)
        .Append(" => ")
        .Append(printMember isDebug t.Member)
    else
      p.Append(printMember isDebug t.Member)
  | ApiSignature.ExtensionMember m -> p.Append(printMember isDebug m)
  | ApiSignature.UnionCase uc -> p.Append(printUnionCase isDebug uc)
  | ApiSignature.ComputationExpressionBuilder builder -> p.Append(printComputationExpressionBuilder isDebug builder)