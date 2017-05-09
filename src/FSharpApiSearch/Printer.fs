module FSharpApiSearch.Printer

open System.Text
open System

module internal FSharpImpl =
  open SpecialTypes

  let printPropertyKind = function
    | PropertyKind.Get -> "get"
    | PropertyKind.Set -> "set"
    | PropertyKind.GetSet -> "get set"
  let printMemberKind = function
    | MemberKind.Method -> "method"
    | MemberKind.Property p -> "property with " + printPropertyKind p
    | MemberKind.Field -> "field"
  let printMemberModifier = function
    | MemberModifier.Instance -> "instance"
    | MemberModifier.Static -> "static"
  let printApiKind kind (sb: StringBuilder) =
    match kind with
    | ApiKind.ModuleValue -> sb.Append("module value")
    | ApiKind.Constructor -> sb.Append("constructor")
    | ApiKind.Member (modifier, memberKind) -> sb.Append(printMemberModifier modifier).Append(" ").Append(printMemberKind memberKind)
    | ApiKind.TypeExtension (modifier, memberKind) -> sb.Append(printMemberModifier modifier).Append(" ").Append(printMemberKind memberKind)
    | ApiKind.ExtensionMember -> sb.Append("extension member")
    | ApiKind.UnionCase -> sb.Append("union case")
    | ApiKind.ModuleDefinition -> sb.Append("module")
    | ApiKind.TypeDefinition -> sb.Append("type")
    | ApiKind.TypeAbbreviation -> sb.Append("type abbreviation")
    | ApiKind.ComputationExpressionBuilder -> sb.Append("builder")

  let typeVariablePrefix (v: TypeVariable) = if v.IsSolveAtCompileTime then "^" else "'"

  let toDisplayName = function
    | SymbolName n -> n
    | OperatorName (n, _) -> n
    | WithCompiledName (n, _) -> n

  let printNameItem (n: DisplayNameItem) (sb: StringBuilder) =
    match n.GenericParameters with
    | [] -> sb.Append(toDisplayName n.Name)
    | args ->
      sb.Append(toDisplayName n.Name)
        .Append("<")
          .AppendJoin(", ", args, (fun arg sb -> sb.Append(typeVariablePrefix arg).Append(arg.Name)))
        .Append(">")

  let printDisplayName_full xs (sb: StringBuilder) =
    match xs with
    | [] -> sb.Append("<empty>")
    | ns ->
      ns.Tail
      |> Seq.rev
      |> Seq.iter (fun n -> sb.Append(printNameItem n).Append(".") |> ignore)
      sb.Append(toDisplayName ns.Head.Name)

  let printName_full (name: Name) (sb: StringBuilder) =
    match name with
    | LoadingName (_, n1, n2) ->
      match n2 with
      | [] -> sb.Append(n1)
      | n2 ->
        sb.Append(n1).Append(".").Append(printDisplayName_full n2)
    | DisplayName n -> sb.Append(printDisplayName_full n)

  let printIdentity_full (identity: Identity) (sb: StringBuilder) =
    match identity with
    | FullIdentity i -> sb.Append(printName_full i.Name)
    | PartialIdentity i -> sb.Append(printDisplayName_full i.Name)

  let printIdentity_short (identity: Identity) (sb: StringBuilder) =
    let printDisplayName_short (xs: DisplayName) (sb: StringBuilder) =
      match xs with
      | [] -> sb.Append("<empty>")
      | n :: _ -> sb.Append(toDisplayName n.Name)

    let printName_short name (sb: StringBuilder) =
      match name with
      | LoadingName (_, n1, n2) ->
        match n2 with
        | [] -> sb.Append(n1)
        | n2 -> sb.Append(printDisplayName_short n2)
      | DisplayName n -> sb.Append(printDisplayName_short n)
    
    match identity with
    | FullIdentity i -> sb.Append(printName_short i.Name)
    | PartialIdentity i -> sb.Append(printDisplayName_short i.Name)

  let printVariableSource = function
    | VariableSource.Query -> "q"
    | VariableSource.Target -> "t"

  let printTypeVariable isDebug source v (sb: StringBuilder) =
    if isDebug then
      sb.Append(typeVariablePrefix v).Append(printVariableSource source).Append("_").Append(v.Name)
    else
      sb.Append(typeVariablePrefix v).Append(v.Name)

  let rec printLowType isDebug (printIdentity: Identity -> StringBuilder -> StringBuilder) lowType (sb: StringBuilder) =
    match lowType with
    | Wildcard name ->
      match name with
      | Some n -> sb.Append("?").Append(n)
      | None -> sb.Append("?")
    | Variable (source, v) -> sb.Append(printTypeVariable isDebug source v)
    | Identity i -> sb.Append(printIdentity i)
    | Arrow xs -> sb.Append(printArrow isDebug printIdentity xs)
    | Tuple { Elements = xs; IsStruct = false } -> sb.Append(printTuple isDebug printIdentity xs)
    | Tuple { Elements = xs; IsStruct = true } -> sb.Append(printStructTuple isDebug printIdentity xs)
    | LowType.Patterns.Array (name, elem) ->
      match elem with
      | Tuple { IsStruct = false } | Arrow _ ->
        sb.Append("(")
          .Append(printLowType isDebug printIdentity elem)
          .Append(")")
          |> ignore
      | _ -> sb.Append(printLowType isDebug printIdentity elem) |> ignore
      sb.Append(name)
    | Generic (id, args) -> sb.Append(printGeneric isDebug printIdentity id args)
    | TypeAbbreviation t -> sb.Append(printLowType isDebug printIdentity t.Abbreviation)
    | Delegate (t, _) -> sb.Append(printLowType isDebug printIdentity t)
    | ByRef (_, t) -> sb.Append("byref<").Append(printLowType isDebug printIdentity t).Append(">")
    | Choice xs -> sb.Append(printChoice isDebug printIdentity xs)
  and printGeneric isDebug printIdentity id (args: _ list) (sb: StringBuilder) =
    sb.Append(printLowType isDebug printIdentity id)
      .Append("<")
      .AppendJoin(", ", args, (printLowType isDebug printIdentity))
      .Append(">")
  and printArrow isDebug printIdentity (xs: _ list) (sb: StringBuilder) =
    let printItem lowType (sb: StringBuilder) =
      match lowType with
      | Arrow _ as a ->
        sb.Append("(")
          .Append(printLowType isDebug printIdentity a)
          .Append(")")
      | x -> sb.Append(printLowType isDebug printIdentity x)
    sb.AppendJoin(" -> ", xs, printItem)
  and printTuple isDebug printIdentity (xs: _ list) (sb: StringBuilder) =
    let printItem lowType (sb: StringBuilder) =
      match lowType with
      | Tuple _ as t ->
        sb.Append("(")
          .Append(printLowType isDebug printIdentity t)
          .Append(")")
      | x -> sb.Append(printLowType isDebug printIdentity x)
    sb.AppendJoin(" * ", xs, printItem)
  and printStructTuple isDebug printIdentity (xs: _ list) (sb: StringBuilder) =
    let printItem lowType (sb: StringBuilder) =
      match lowType with
      | Tuple { IsStruct = false  } | Arrow _ ->
        sb.Append("(")
          .Append(printLowType isDebug printIdentity lowType)
          .Append(")")
      | _ -> sb.Append(printLowType isDebug printIdentity lowType)
    sb.Append("struct (")
      .AppendJoin(" * ", xs, printItem)
      .Append(")")
  and printChoice isDebug printIdentity (xs: _ list) (sb: StringBuilder) =
    sb.Append("(")
      .AppendJoin(" or ", xs, printLowType isDebug printIdentity)
      .Append(")")

  let printLowType_short isDebug t (sb: StringBuilder) = sb.Append(printLowType isDebug printIdentity_short t)
  let printLowType_full isDebug t (sb: StringBuilder) = sb.Append(printLowType isDebug printIdentity_full t)

  let printParameter tupleParen isDebug (p: Parameter) (sb: StringBuilder) =
    match p.IsOptional with
    | true -> sb.Append("?") |> ignore
    | false -> ()

    match p.Name with
    | Some name -> sb.Append(name).Append(":") |> ignore
    | None -> ()

    match p with
    | { Type = Tuple _ } when tupleParen ->
      sb.Append("(")
        .Append(printLowType_short isDebug p.Type)
        .Append(")")
    | { Type = Arrow _ } ->
      sb.Append("(")
        .Append(printLowType_short isDebug p.Type)
        .Append(")")
    | _ -> sb.Append(printLowType_short isDebug p.Type)

  let printParameterGroups tupleParen isDebug (func: Parameter list list) (sb: StringBuilder) =
    sb.AppendJoin(" -> ", func, (fun ps sb -> sb.AppendJoin(" * ", ps, printParameter tupleParen isDebug)))

  let printMember isDebug (m: Member) (sb: StringBuilder) =
    match m.Parameters with
    | [] -> sb.Append(printLowType_short isDebug m.ReturnParameter.Type)
    | _ ->
      sb.Append(printParameterGroups true isDebug m.Parameters)
        .Append(" -> ")
        .Append(printLowType_short isDebug m.ReturnParameter.Type)

  let printConstraint isDebug (c: TypeConstraint) (sb: StringBuilder) =
    let variableSource = VariableSource.Target

    match c.Variables with
    | [ v ] ->
      sb.Append(printTypeVariable isDebug variableSource v) |> ignore
    | vs ->
      sb.Append("(")
        .AppendJoin(" or ", vs, printTypeVariable isDebug variableSource)
        .Append(")")
      |> ignore

    sb.Append(" ") |> ignore

    match c.Constraint with
    | Constraint.SubtypeConstraints s ->
      sb.Append(":> ")
        .Append(printLowType_short isDebug s)
    | Constraint.NullnessConstraints -> sb.Append(": null")
    | Constraint.MemberConstraints (modifier, member') ->
      let printMemberModifier modifier (sb: StringBuilder) =
        match modifier with
        | MemberModifier.Static -> sb.Append("static member")
        | MemberModifier.Instance -> sb.Append("member")
      sb.Append(": (")
          .Append(printMemberModifier modifier)
          .Append(" ").Append(member'.Name).Append(" : ")
          .Append(printMember isDebug member')
        .Append(")")
    | Constraint.DefaultConstructorConstraints ->
      sb.Append(": (new : unit -> ")
        .Append(printTypeVariable isDebug variableSource (c.Variables.Head))
        .Append(")")
    | Constraint.ValueTypeConstraints -> sb.Append(": struct")
    | Constraint.ReferenceTypeConstraints -> sb.Append(": not struct")
    | Constraint.EnumerationConstraints -> sb.Append(": enum")
    | Constraint.DelegateConstraints -> sb.Append(": delegate")
    | Constraint.UnmanagedConstraints -> sb.Append(": unmanaged")
    | Constraint.EqualityConstraints -> sb.Append(": equality")
    | Constraint.ComparisonConstraints -> sb.Append(": comparison")
    
  let printFullTypeDefinition isDebug (x: FullTypeDefinition) (sb: StringBuilder) =
    sb.Append("type ")
      .Append(printLowType_short isDebug x.LowType)

  let pringTypeAbbreviation isDebug (x: TypeAbbreviationDefinition) (sb: StringBuilder) =
    sb.Append("type ")
      .Append(printLowType_short isDebug x.TypeAbbreviation.Abbreviation)
      .Append(" = ")
      .Append(printLowType_full isDebug x.Abbreviated)

  let printUnionCaseField isDebug (uc: UnionCaseField) (sb: StringBuilder) =
    match uc.Name with
    | Some name ->
      sb.Append(name).Append(":").Append(printLowType_short isDebug uc.Type)
    | None -> sb.Append(printLowType_short isDebug uc.Type)

  let printUnionCase isDebug (uc: UnionCase) (sb: StringBuilder) =
    if uc.Fields.IsEmpty then
      sb.Append(printLowType_short isDebug uc.DeclaringType)
    else
      sb.Append(printParameterGroups true isDebug (UnionCase.toFunction uc))

  let printModule (m: ModuleDefinition) (sb: StringBuilder) = sb.Append("module ").Append(toDisplayName m.Name.Head.Name)

  let printComputationExpressionBuilder isDebug (builder: ComputationExpressionBuilder) (sb: StringBuilder) =
    if isDebug then
      sb.Append("type ")
        .Append(printLowType_short isDebug builder.BuilderType)
        .Append(", [ ")
        .AppendJoin("; ", builder.ComputationExpressionTypes, printLowType_short isDebug)
        .Append(" ], { ")
        .AppendJoin("; ", builder.Syntaxes, (fun syntax sb -> sb.Append(syntax)))
        .Append(" }")
    else
      sb.Append("type ")
        .Append(printLowType_short isDebug builder.BuilderType)
        .Append(", { ")
        .AppendJoin("; ", builder.Syntaxes, (fun syntax sb -> sb.Append(syntax)))
        .Append(" }")

  let printApiSignature isDebug apiSig (sb: StringBuilder) =
    match apiSig with
    | ApiSignature.ModuleValue t -> sb.Append(printLowType_short isDebug t)
    | ApiSignature.ModuleFunction fn -> sb.Append(printParameterGroups false isDebug fn)
    | ApiSignature.ActivePatten (_, fn) -> sb.Append(printParameterGroups false isDebug fn)
    | ApiSignature.InstanceMember (declaringType, m) ->
      if isDebug then
        sb.Append(printLowType_short isDebug declaringType)
          .Append(" => ")
          .Append(printMember isDebug m)
      else
        sb.Append(printMember isDebug m)
    | ApiSignature.StaticMember (_, m) -> sb.Append(printMember isDebug m)
    | ApiSignature.Constructor (_, m) -> sb.Append(printMember isDebug m)
    | ApiSignature.ModuleDefinition m -> sb.Append(printModule m)
    | ApiSignature.FullTypeDefinition x -> sb.Append(printFullTypeDefinition isDebug x)
    | ApiSignature.TypeAbbreviation t -> sb.Append(pringTypeAbbreviation isDebug t)
    | ApiSignature.TypeExtension t ->
      if isDebug then
        sb.Append(printLowType_short isDebug t.ExistingType)
          .Append(" => ")
          .Append(printMember isDebug t.Member)
      else
        sb.Append(printMember isDebug t.Member)
    | ApiSignature.ExtensionMember m -> sb.Append(printMember isDebug m)
    | ApiSignature.UnionCase uc -> sb.Append(printUnionCase isDebug uc)
    | ApiSignature.ComputationExpressionBuilder builder -> sb.Append(printComputationExpressionBuilder isDebug builder)

module FSharp =
  let printName (api: Api) = StringBuilder().Append(FSharpImpl.printName_full api.Name).ToString()
  let printSignature (api: Api) = StringBuilder().Append(FSharpImpl.printApiSignature false api.Signature).ToString()
  let printKind (api: Api) =
    match api.Signature with
    | ApiSignature.TypeExtension { Declaration = declaration } ->
      let sb = StringBuilder()
      sb.Append(FSharpImpl.printApiKind api.Kind)
        .Append(" (").Append(FSharpImpl.printDisplayName_full declaration).Append(")")
        .ToString()
    | _ -> StringBuilder().Append(FSharpImpl.printApiKind api.Kind).ToString()

  let tryPrintTypeConstraints (api: Api) =
    match api.TypeConstraints with
    | [] -> None
    | xs ->
      let sb = StringBuilder()
      sb.Append("when ")
        .AppendJoin(" and ", xs, FSharpImpl.printConstraint false)
        .ToString()
      |> Some

module internal CSharpImpl =
  open SpecialTypes.LowType.Patterns

  let toDisplayName = function
    | SymbolName n -> n
    | OperatorName (_, n) -> n
    | WithCompiledName (_, n) -> n

  let printNameItem (n: DisplayNameItem) (sb: StringBuilder) =
    match n.GenericParameters with
    | [] -> sb.Append(toDisplayName n.Name)
    | args ->
      sb.Append(toDisplayName n.Name)
        .Append("<")
          .AppendJoin(", ", args, (fun arg sb -> sb.Append(arg.Name)))
        .Append(">")

  let csharpAlias =
    SpecialTypes.Identity.CSharp.aliases
    |> List.map (fun (alias, t) ->
      let alias = FullIdentity { AssemblyName = "dummy"; Name = Name.ofString alias; GenericParameterCount = 0 }
      t, alias
    )
    |> dict

  let printIdentity (identity: Identity) (sb: StringBuilder) =
    let identity =
      match csharpAlias.TryGetValue(identity) with
      | true, alias -> alias
      | false, _ -> identity

    let printDisplayName_short (xs: DisplayName) (sb: StringBuilder) =
      match xs with
      | [] -> sb.Append("<empty>")
      | n :: _ -> sb.Append(toDisplayName n.Name)

    let printName_short name (sb: StringBuilder) =
      match name with
      | LoadingName (_, n1, n2) ->
        match n2 with
        | [] -> sb.Append(n1)
        | n2 -> sb.Append(printDisplayName_short n2)
      | DisplayName n -> sb.Append(printDisplayName_short n)
    
    match identity with
    | FullIdentity i -> sb.Append(printName_short i.Name)
    | PartialIdentity i -> sb.Append(printDisplayName_short i.Name)

  let toFSharpFunc xs = xs |> List.reduceBack (fun id ret -> Generic(SpecialTypes.LowType.FSharpFunc, [ id; ret ]))

  let rec nestedArray acc = function
    | Array (name, elem) -> nestedArray (name :: acc) elem
    | x -> acc, x

  let printRef isOut = if isOut then "out" else "ref"

  let rec printLowType t (sb: StringBuilder) =
    match t with
    | Wildcard name ->
      match name with
      | Some n -> sb.Append("?").Append(n)
      | None -> sb.Append("?")
    | Variable (_, v) -> sb.Append(v.Name)
    | Identity i -> sb.Append(printIdentity i)
    | Arrow xs -> printLowType (toFSharpFunc xs) sb
    | Tuple { Elements = xs; IsStruct = false } -> sb.Append("Tuple<").AppendJoin(", ", xs, printLowType).Append(">")
    | Tuple { Elements = xs; IsStruct = true } -> sb.Append("(").AppendJoin(", ", xs, printLowType).Append(")")
    | Array (array, elem) ->
      let arrays, elem = nestedArray [ array ] elem
      sb.Append(printLowType elem) |> ignore
      arrays |> Seq.rev |> Seq.iter (fun a -> sb.Append(a) |> ignore)
      sb
    | Generic (id, args) -> sb.Append(printLowType id).Append("<").AppendJoin(", ", args, printLowType).Append(">")
    | TypeAbbreviation t -> sb.Append(printLowType t.Original)
    | Delegate (t, _) -> sb.Append(printLowType t)
    | ByRef (isOut, t) -> sb.Append(printRef isOut).Append(" ").Append(printLowType t)
    | Choice xs -> sb.Append("(").AppendJoin(" or ", xs, printLowType).Append(")")

  let printParameter (p: Parameter) (sb: StringBuilder) =
    sb.Append(printLowType p.Type) |> ignore
    p.Name |> Option.iter (fun name -> sb.Append(" ").Append(name) |> ignore)
    sb

  let printPropertyKind propKind (sb: StringBuilder) =
    match propKind with
    | PropertyKind.Get -> sb.Append("{ get; }")
    | PropertyKind.Set -> sb.Append("{ set; }")
    | PropertyKind.GetSet -> sb.Append("{ get; set; }")

  let printPropertyParameter (m: Member) (sb: StringBuilder) =
    let parameters = m.Parameters |> List.collect id
    sb.Append("[").AppendJoin(", ", parameters, printParameter).Append("]")

  let printProperty (name: DisplayName) (m: Member) (sb: StringBuilder) =
    sb.Append(printLowType m.ReturnParameter.Type)
      .Append(" ").Append(printNameItem name.[1]).Append(".").Append(printNameItem name.[0])
      |> ignore
    if List.isEmpty m.Parameters = false then sb.Append(printPropertyParameter m) |> ignore

    let propKind = match m.Kind with MemberKind.Property propKind -> propKind | _ -> failwith "it is not property."
    sb.Append(" ").Append(printPropertyKind propKind)

  let printReturnParameter (p: Parameter) (sb: StringBuilder) =
    match p.Type with
    | Unit -> sb.Append("void")
    | t -> sb.Append(printLowType t)

  let printMethodParameter (m: Member) (isExtension: bool) (sb: StringBuilder) =
    let parameters = m.Parameters |> List.collect id
    match parameters with
    | [ { Type = Unit } ] ->
      sb.Append("()")
    | _ ->
      if isExtension then
        sb.Append("(this ") |> ignore
      else
        sb.Append("(") |> ignore
      sb.AppendJoin(", ", parameters, printParameter).Append(")")

  let printMethod (name: DisplayName) (m: Member) (isExtension: bool) (sb: StringBuilder) =
    sb.Append(printReturnParameter m.ReturnParameter)
      .Append(" ").Append(printNameItem name.[1]).Append(".").Append(printNameItem name.[0])
      .Append(printMethodParameter m isExtension)

  let printField (name: DisplayName) (m: Member) (sb: StringBuilder) =
    sb.Append(printLowType m.ReturnParameter.Type)
      .Append(" ").Append(printNameItem name.[1]).Append(".").Append(printNameItem name.[0])

  let printMember (name: DisplayName) (m: Member) (sb: StringBuilder) =
    match m.Kind with
    | MemberKind.Property _ -> sb.Append(printProperty name m)
    | MemberKind.Method -> sb.Append(printMethod name m false)
    | MemberKind.Field -> sb.Append(printField name m)

  let printConstructor (name: DisplayName) (m: Member) (sb: StringBuilder) =
    let typeName = name.[1]
    sb.Append(printNameItem typeName).Append(".").Append(printNameItem typeName)
      .Append(printMethodParameter m false)

  let printFunction (name: DisplayName) (fn: Function) (sb: StringBuilder) =
    let m = {
      Name = "dummy"
      Kind = MemberKind.Method
      GenericParameters = []
      Parameters = fn |> List.take (List.length fn - 1)
      ReturnParameter = fn |> List.last |> List.head
    }
    printMethod name m false sb

  let printFullTypeDefinition (td: FullTypeDefinition) (sb: StringBuilder) =
    let kind =
      match td.Kind with
      | TypeDefinitionKind.Class
      | TypeDefinitionKind.Record
      | TypeDefinitionKind.Type
      | TypeDefinitionKind.Union -> "class"
      | TypeDefinitionKind.Interface -> "interface"
      | TypeDefinitionKind.Enumeration -> "enum"
    sb.Append(kind).Append(" ").Append(printNameItem td.Name.[0])

  let printApiSignature (name: Name) (apiSig: ApiSignature) (sb: StringBuilder) =
    let error name = failwithf "%s is not C# api." name
    let name = Name.toDisplayName name
    match apiSig with
    | ApiSignature.ModuleValue t -> sb.Append("static ").Append(printLowType t).Append(" ").Append(printNameItem name.[1]).Append(".").Append(printNameItem name.[0]).Append(" ").Append(printPropertyKind PropertyKind.Get)
    | ApiSignature.ModuleFunction fn -> sb.Append("static ").Append(printFunction name fn)
    | ApiSignature.ActivePatten (_, _) -> error "ActivePattern"
    | ApiSignature.InstanceMember (_, m) -> sb.Append(printMember name m)
    | ApiSignature.StaticMember (_, m) -> sb.Append("static ").Append(printMember name m)
    | ApiSignature.Constructor (_, m) -> sb.Append(printConstructor name m)
    | ApiSignature.ModuleDefinition _ -> error "Module"
    | ApiSignature.FullTypeDefinition td -> sb.Append(printFullTypeDefinition td)
    | ApiSignature.TypeAbbreviation _ -> error "TypeAbbreviation"
    | ApiSignature.TypeExtension _ -> error "TypeExtension"
    | ApiSignature.ExtensionMember m -> sb.Append(printMethod name m true)
    | ApiSignature.UnionCase _ -> error "UnionCase"
    | ApiSignature.ComputationExpressionBuilder _ -> error "ComputationExpression"

  let filterCSharpTypeConstraint (xs: TypeConstraint list) =
    xs
    |> List.filter (fun x ->
      match x.Constraint with
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
      | Constraint.ReferenceTypeConstraints -> true)

  let printConstraint (c: TypeConstraint) (sb: StringBuilder) =
    let print (v: TypeVariable) (sb: StringBuilder) =
      sb.Append("where ").Append(v.Name).Append(" : ") |> ignore

      match c.Constraint with
      | Constraint.SubtypeConstraints s -> sb.Append(printLowType s) |> ignore
      | Constraint.DefaultConstructorConstraints -> sb.Append("new()") |> ignore
      | Constraint.ValueTypeConstraints -> sb.Append("struct") |> ignore
      | Constraint.ReferenceTypeConstraints -> sb.Append("class") |> ignore
      | _ -> failwith "It is not C# constraint."

      sb

    sb.AppendJoin(" ", c.Variables, print)

  let printMemberKind = function
    | MemberKind.Method -> "method"
    | MemberKind.Property _ -> "property"
    | MemberKind.Field -> "field"
  let printMemberModifier = function
    | MemberModifier.Instance -> "instance"
    | MemberModifier.Static -> "static"
  let printApiKind kind (sb: StringBuilder) =
    match kind with
    | ApiKind.ModuleValue -> sb.Append(printMemberModifier MemberModifier.Static).Append(" ").Append(printMemberKind MemberKind.Method)
    | ApiKind.Constructor -> sb.Append("constructor")
    | ApiKind.Member (modifier, memberKind) -> sb.Append(printMemberModifier modifier).Append(" ").Append(printMemberKind memberKind)
    | ApiKind.ExtensionMember -> sb.Append("extension method")
    | ApiKind.TypeDefinition -> sb.Append("type")

    | ApiKind.UnionCase
    | ApiKind.ModuleDefinition
    | ApiKind.TypeAbbreviation
    | ApiKind.ComputationExpressionBuilder
    | ApiKind.TypeExtension _ -> failwith "It is not C# api."

module CSharp =
  let printSignatureAndName (api: Api) = StringBuilder().Append(CSharpImpl.printApiSignature api.Name api.Signature).ToString()

  let tryPrintTypeConstraints (api: Api) =
    let xs = api.TypeConstraints |> CSharpImpl.filterCSharpTypeConstraint
    match xs with
    | [] -> None
    | _ -> StringBuilder().AppendJoin(" ", xs, CSharpImpl.printConstraint).ToString() |> Some

  let printKind (api: Api) = StringBuilder().Append(CSharpImpl.printApiKind api.Kind).ToString()

type TypeVariable with
  member this.Print() = StringBuilder().Append(FSharpImpl.printTypeVariable false VariableSource.Target this).ToString()

type DisplayNameItem with
  member this.Print() = StringBuilder().Append(FSharpImpl.printNameItem this).ToString()

type Name with
  member this.Print() = StringBuilder().Append(FSharpImpl.printName_full this).ToString()

type LowType with
  member this.Print() = StringBuilder().Append(FSharpImpl.printLowType_short false this).ToString()
  member internal this.Debug() = StringBuilder().Append(FSharpImpl.printLowType_short true this).ToString()

type ApiSignature with
  member this.Print() = StringBuilder().Append(FSharpImpl.printApiSignature false this).ToString()
  member internal this.Debug() = StringBuilder().Append(FSharpImpl.printApiSignature true this).ToString()

type TypeConstraint with
  member this.Print() = StringBuilder().Append(FSharpImpl.printConstraint false this).ToString()
  member internal this.Debug() = StringBuilder().Append(FSharpImpl.printConstraint true this).ToString()
  
type FullTypeDefinition with
  member this.Print() = StringBuilder().Append(FSharpImpl.printFullTypeDefinition false this).ToString()
  member internal this.Debug() = StringBuilder().Append(FSharpImpl.printFullTypeDefinition true this).ToString()
  
module internal LowType =
  let debug (x: LowType) = x.Debug()

module internal ApiSignature =
  let debug (x: ApiSignature) = x.Debug()
  let print (x: ApiSignature) = x.Print()
  
module internal TypeConstraint =
  let debug (x: TypeConstraint) = x.Debug()
  
module internal FullTypeDefinition =
  let debug (x: FullTypeDefinition) = x.Debug()