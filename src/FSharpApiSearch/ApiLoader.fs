module FSharpApiSearch.ApiLoader

open Microsoft.FSharp.Compiler.SourceCodeServices
open FSharpApiSearch.OptionModule
open FSharpApiSearch.SpecialTypes
open System.Text.RegularExpressions
open System.IO
open Nessos.FsPickler
open System.Collections.Generic
open System.Xml.Linq

let inline internal tryGetXmlDoc (xml: XElement option) (symbol: ^a): string option =
  option {
    let! xml = xml
    let xmlSig: string = (^a : (member XmlDocSig : string) symbol)
    let members = xml.Element(XName.Get("members")).Elements(XName.Get("member"))
    let! sigMember = members |> Seq.tryFind (fun e -> e.Attribute(XName.Get("name")).Value = xmlSig)
    let! summary = sigMember.Element(XName.Get("summary")) |> Option.ofObj
    if System.String.IsNullOrWhiteSpace(summary.Value) then
      return! None
    else
      return summary.Value.Replace("\r", "").Replace("\n", "").Trim()
  }
  
type FSharpEntity with
  member this.TypeAbbreviationFullName = this.AccessPath + "." + this.CompiledName
  member this.LoadingFullIdentity =
    let assemblyName = this.Assembly.SimpleName
    let name =
      match this.TryFullName with
      | Some fullName -> fullName
      | None -> this.TypeAbbreviationFullName
    { AssemblyName = assemblyName; Name = LoadingName (assemblyName, name, []); GenericParameterCount = this.GenericParameters.Count }
  member this.Identity = Identity (FullIdentity this.LoadingFullIdentity)
  member this.IsTuple =
    match this.TryFullName with
    | Some fullName -> fullName.StartsWith("System.Tuple") && this.DisplayName = "Tuple"
    | None -> false
  member this.IsCompilerInternalModule = this.IsFSharpModule && (this.FullName = "Microsoft.FSharp.Core.LanguagePrimitives" || this.FullName = "Microsoft.FSharp.Core.Operators.OperatorIntrinsics")
  
type FSharpType with
  member this.TryIdentity = this.TryFullIdentity |> Option.map (fun x -> Identity (FullIdentity x))
  member this.TryFullIdentity =
    if Hack.isFloat this then
      Some { this.TypeDefinition.LoadingFullIdentity with GenericParameterCount = 0 }
    elif this.HasTypeDefinition then
      Some this.TypeDefinition.LoadingFullIdentity
    else
      None

type FSharpMemberOrFunctionOrValue with
  member this.IsStaticMember = not this.IsInstanceMember
  member this.IsMethod = this.FullType.IsFunctionType && not this.IsPropertyGetterMethod && not this.IsPropertySetterMethod
  member this.IsConstructor = this.CompiledName = ".ctor"
  member this.IsCSharpExtensionMember = this.Attributes |> Seq.exists (fun attr -> attr.AttributeType.TryFullName = Some "System.Runtime.CompilerServices.ExtensionAttribute")
  member this.MemberModifier = if this.IsStaticMember then MemberModifier.Static else MemberModifier.Instance
  member this.PropertyKind =
    if not this.IsProperty then
      failwith "It is not property."
    elif this.HasGetterMethod && this.HasSetterMethod then
      PropertyKind.GetSet
    elif this.HasGetterMethod then
      PropertyKind.Get
    else
      PropertyKind.Set
  member this.TargetSignatureConstructor = fun declaringType member' ->
    if this.IsCSharpExtensionMember then
      ApiSignature.ExtensionMember member'
    elif this.IsStaticMember then
      ApiSignature.StaticMember (declaringType, member')
    else
      ApiSignature.InstanceMember (declaringType, member')
  member this.GenericParametersAsTypeVariable =
    this.GenericParameters |> Seq.map (fun x -> x.DisplayName : TypeVariable) |> Seq.toList
  member this.GetDisplayName =
    let dn = this.DisplayName
    let isOperator =
      dn.StartsWith("(") && dn.EndsWith(")") && this.CompiledName.StartsWith("op_")
    if isOperator then
      { FSharpName = dn; InternalFSharpName = this.CompiledName; GenericParametersForDisplay = [] }
    else
      { FSharpName = dn; InternalFSharpName = dn; GenericParametersForDisplay = [] }

type FSharpField with
  member this.TargetSignatureConstructor = fun declaringType member' ->
    if this.IsStatic then
      ApiSignature.StaticMember (declaringType, member')
    else
      ApiSignature.InstanceMember (declaringType, member')

let genericParameters (e: FSharpEntity) =
  e.GenericParameters |> Seq.map (fun p -> p.DisplayName : TypeVariable) |> Seq.toList

let accessibility (e: FSharpEntity) =
  let a = e.Accessibility
  if a.IsPublic then
    Public
  else
    Private

let rec toSignature (t: FSharpType) =
  if Hack.isMeasure t then
    None
  elif t.IsFunctionType then
    option {
      let! xs = toFlatArrow t
      return Arrow xs
    }
  elif t.IsGenericParameter then
    Some (Variable (VariableSource.Target, t.GenericParameter.Name))
  elif t.IsTupleType then
    option {
      let! args = listSignature t.GenericArguments
      return Tuple args
    }
  elif Hack.isFloat t then
    Some SpecialTypes.LowType.float
  elif t.HasTypeDefinition then
    let signature =
      match Hack.genericArguments t with
      | [] ->
        option {
          let! id = t.TryIdentity
          return id
        }
      | xs -> 
        option {
          let! xs = listSignature xs
          let! id = t.TryIdentity
          return Generic (id, xs)
        }
    option {
      let! signature = signature
      if Hack.isAbbreviation t then
        let! original = abbreviationRoot t
        return TypeAbbreviation { Abbreviation = signature; Original = original }
      else
        return signature
    }
  else
    None
and abbreviationRoot (t: FSharpType) =
  if t.IsAbbreviation then
    abbreviationRoot t.AbbreviatedType
  elif Hack.isFloat t then
    Some SpecialTypes.LowType.Double
  elif t.IsFunctionType then
    option {
      let! xs = toFlatArrow t
      return Arrow xs
    }
  else
    toSignature t
and toFlatArrow (t: FSharpType): LowType list option =
  match Seq.toList t.GenericArguments with
  | [ x; y ] when y.IsFunctionType ->
    option {
      let! xSig = toSignature x
      let! ySigs = toFlatArrow y
      return xSig :: ySigs
    }
  | [ x; y ] ->
    option {
      let! xSig = toSignature x
      let! ySig = toSignature y
      return [ xSig; ySig ]
    }
  | _ -> None
and listSignature (ts: FSharpType seq) =
  let f (t: FSharpType) (acc: LowType list option) =
    option {
      let! acc = acc
      let! signature = toSignature t
      return signature :: acc
    }
  Seq.foldBack f ts (Some [])
and fsharpEntityToSignature (x: FSharpEntity) =
  let identity = x.Identity
  let args = x |> genericParameters |> List.map (fun v -> Variable (VariableSource.Target, v))
  match args with
  | [] -> identity
  | xs -> Generic (identity, xs)

let collectTypeConstraints (genericParamters: seq<FSharpGenericParameter>): TypeConstraint list =
  genericParamters
  |> Seq.collect (fun p ->
    let variable = p.Name
    p.Constraints
    |> Seq.choose (fun c -> 
      if c.IsCoercesToConstraint then
        option {
          let! parent = toSignature c.CoercesToTarget
          return { Variables = [ variable ]; Constraint = SubtypeConstraints parent }
        }
      elif c.IsSupportsNullConstraint then
        Some { Variables = [ variable ]; Constraint = NullnessConstraints }
      elif c.IsMemberConstraint then
        option {
          let data = c.MemberConstraintData
          let modifier = if data.MemberIsStatic then MemberModifier.Static else MemberModifier.Instance
          let! returnType = toSignature data.MemberReturnType
          let! args = listSignature data.MemberArgumentTypes
          let args =
            if data.MemberIsStatic then
              if args.Length = 0 then
                [ LowType.unit ] // Core.Unit is removed if the argument is only Core.Unit.
              else
                args
            else
              if args.Length = 1 then
                [ LowType.unit ] // Core.Unit is removed if the argument is only Core.Unit.
              else
                List.tail args // instance member contains receiver
          let variables = data.MemberSources |> Seq.map (fun x -> x.GenericParameter.Name) |> Seq.toList
          let name = data.MemberName
          let member' = { Name = name; Kind = MemberKind.Method; Arguments = args; ReturnType = returnType; IsCurried = false; GenericParameters = [] }
          return { Variables = variables; Constraint = MemberConstraints (modifier, member') }
        }
      elif c.IsNonNullableValueTypeConstraint then
        Some { Variables = [ variable ]; Constraint = ValueTypeConstraints }
      elif c.IsReferenceTypeConstraint then
        Some { Variables = [ variable ]; Constraint = ReferenceTypeConstraints }
      elif c.IsRequiresDefaultConstructorConstraint then
        Some { Variables = [ variable ]; Constraint = DefaultConstructorConstraints }
      elif c.IsEqualityConstraint then
        Some { Variables = [ variable ]; Constraint = EqualityConstraints }
      elif c.IsComparisonConstraint then
        Some { Variables = [ variable ]; Constraint = ComparisonConstraints }
      elif c.IsEnumConstraint then
        Some { Variables = [ variable ]; Constraint = EnumerationConstraints }
      elif c.IsDelegateConstraint then
        Some { Variables = [ variable ]; Constraint = DelegateConstraints }
      elif c.IsUnmanagedConstraint then
        Some { Variables = [ variable ]; Constraint = UnmanagedConstraints }
      else
        None
    )
  )
  |> Seq.toList
  |> List.distinct

let parameterSignature (t: FSharpMemberOrFunctionOrValue) =
  let xs = [
    for group in t.CurriedParameterGroups do
      for parameter in group do
        yield parameter.Type
  ]
  listSignature xs

let methodMember (x: FSharpMemberOrFunctionOrValue) =
  option {
    let name = x.GetDisplayName
    let! args =
      if x.CurriedParameterGroups.Count = 1 && x.CurriedParameterGroups.[0].Count = 0 then
        toSignature x.FullType.GenericArguments.[0] |> Option.map List.singleton
      else
        parameterSignature x

    let! returnType = toSignature x.ReturnParameter.Type
    let isCurried = x.CurriedParameterGroups.Count >= 2
    let genericParams = x.GenericParametersAsTypeVariable
    let member' = { Name = name.InternalFSharpName; Kind = MemberKind.Method; GenericParameters = genericParams; Arguments = args; IsCurried = isCurried; ReturnType = returnType }
    return (name, member')
  }

let propertyMember (x: FSharpMemberOrFunctionOrValue) =
  option {
    let memberKind = MemberKind.Property x.PropertyKind
    let! args = parameterSignature x
    let! returnType = toSignature x.ReturnParameter.Type
    let genericParams = x.GenericParametersAsTypeVariable
    let name = x.GetDisplayName
    let member' = { Name = name.InternalFSharpName; Kind = memberKind; GenericParameters = genericParams; Arguments = args; IsCurried = false; ReturnType = returnType }
    return (name, member')
  }

let toModuleValue xml (declaringModuleName: DisplayName) (x: FSharpMemberOrFunctionOrValue) =
  option {
    let name = x.GetDisplayName :: declaringModuleName
    let! signature = (toSignature x.FullType)
    let target =
      if x.IsActivePattern then
        let kind = if x.DisplayName.Contains("|_|") then ActivePatternKind.PartialActivePattern else ActivePatternKind.ActivePattern
        ApiSignature.ActivePatten (kind, signature)
      else
        match signature with
        | Arrow xs -> ApiSignature.ModuleFunction xs
        | x -> ApiSignature.ModuleValue x
    return { Name = DisplayName name; Signature = target; TypeConstraints = collectTypeConstraints x.GenericParameters; Document = tryGetXmlDoc xml x }
  }

let toTypeExtension xml (declaringModuleName: DisplayName) (x: FSharpMemberOrFunctionOrValue) =
  option {
    let existingType = fsharpEntityToSignature x.LogicalEnclosingEntity
    let modifier = x.MemberModifier

    let! _, member' =
      if x.IsPropertyGetterMethod || x.IsPropertySetterMethod then
        None
      elif x.IsProperty then
        propertyMember x
      else
        let existingTypeParameters = x.LogicalEnclosingEntity.GenericParameters |> Seq.map (fun x -> x.DisplayName) |> Seq.toArray
        let removeExistingTypeParameters xs = xs |> List.filter (fun p -> existingTypeParameters |> Array.exists ((=)p) = false)
        methodMember x
        |> Option.map (fun (n, m) -> (n, { m with GenericParameters = removeExistingTypeParameters m.GenericParameters }))

    let signature = ApiSignature.TypeExtension { ExistingType = existingType; Declaration = declaringModuleName; MemberModifier = modifier; Member = member' }
    let name =
      let memberAssemblyName = x.LogicalEnclosingEntity.Assembly.SimpleName
      let memberTypeName = x.LogicalEnclosingEntity.FullName
      let memberName =
        let name = member'.Name
        { FSharpName = name; InternalFSharpName = name; GenericParametersForDisplay = [] }
      LoadingName (memberAssemblyName, memberTypeName, [ memberName ])
    return { Name = name; Signature = signature; TypeConstraints = collectTypeConstraints x.GenericParameters; Document = tryGetXmlDoc xml x }
  }

let toFSharpApi xml (declaringModuleName: DisplayName) (x: FSharpMemberOrFunctionOrValue) =
  if x.IsExtensionMember then
    toTypeExtension xml declaringModuleName x
  else
    toModuleValue xml declaringModuleName x

let constructorSignature xml (declaringSignatureName: DisplayName) declaringSignature (x: FSharpMemberOrFunctionOrValue) =
  option {
    let! _, target = methodMember x
    let target = { target with Name = declaringSignatureName.Head.FSharpName; ReturnType = declaringSignature }
    return { Name = DisplayName declaringSignatureName; Signature = ApiSignature.Constructor (declaringSignature, target); TypeConstraints = collectTypeConstraints x.GenericParameters; Document = tryGetXmlDoc xml x }
  }

let memberSignature xml (loadMember: FSharpMemberOrFunctionOrValue -> (NameItem * Member) option) (declaringSignatureName: DisplayName) (declaringEntity: FSharpEntity) declaringSignature (x: FSharpMemberOrFunctionOrValue) =
  option {
    let! name, member' = loadMember x
    let name = name :: declaringSignatureName
    let typeConstraints = Seq.append declaringEntity.GenericParameters x.GenericParameters |> collectTypeConstraints
    return { Name = DisplayName name; Signature = x.TargetSignatureConstructor declaringSignature member'; TypeConstraints = typeConstraints; Document = tryGetXmlDoc xml x }
  }

let toTypeMemberApi xml (declaringSignatureName: DisplayName) (declaringEntity: FSharpEntity) (declaringSignature: LowType) (x: FSharpMemberOrFunctionOrValue) =
  if x.IsConstructor then
    constructorSignature xml declaringSignatureName declaringSignature x
  elif x.IsMethod then
    memberSignature xml methodMember declaringSignatureName declaringEntity declaringSignature x
  elif x.IsProperty then
    memberSignature xml propertyMember declaringSignatureName declaringEntity declaringSignature x
  else
    None

let toFieldApi xml (accessPath: DisplayName) (declaringEntity: FSharpEntity) (declaringSignature: LowType) (field: FSharpField) =
  option {
    let! returnType = toSignature field.FieldType
    let member' = { Name = field.Name; Kind = MemberKind.Field; GenericParameters = []; Arguments = []; IsCurried = false; ReturnType = returnType }
    let apiName =
      let name = field.Name
      { FSharpName = name; InternalFSharpName = name; GenericParametersForDisplay = [] } :: accessPath
    return { Name = DisplayName apiName; Signature = field.TargetSignatureConstructor declaringSignature member'; TypeConstraints = collectTypeConstraints declaringEntity.GenericParameters; Document = tryGetXmlDoc xml field }
  }

let resolveConflictGenericArgumnet (replacementVariables: LowType list) (m: FSharpMemberOrFunctionOrValue) =
  m.GenericParameters
  |> Seq.choose (fun p ->
    let name = p.Name.TrimStart(''')
    let isConflict = replacementVariables |> List.exists (function Variable (VariableSource.Target, n) -> n = name | _ -> false)
    if isConflict then
      let confrictVariable = name
      let newVariable = Variable (VariableSource.Target, name + "1")
      Some (confrictVariable, newVariable)
    else None
  )
  |> Seq.toList

let genericParametersAndArguments (t: FSharpType) =
  Seq.zip t.TypeDefinition.GenericParameters t.GenericArguments
  |> Seq.choose (fun (parameter, arg) -> option {
    let! s = toSignature arg
    let v = parameter.Name.TrimStart(''')
    return v, s
  })
  |> Seq.toList

let updateInterfaceDeclaringType (declaringSignatureName: DisplayName) declaringSignature api =
  let target =
    match api.Signature with
    | ApiSignature.InstanceMember (_, m) -> ApiSignature.InstanceMember (declaringSignature, m)
    | ApiSignature.StaticMember (_, m) -> ApiSignature.StaticMember (declaringSignature, m)
    | _ -> failwith "It is not a member of interface."
  let name =
    match api.Name with
    | LoadingName (_, _, name :: _) -> name :: declaringSignatureName
    | DisplayName (name :: _) -> name :: declaringSignatureName
    | _ -> declaringSignatureName
  { api with Name = DisplayName name; Signature = target }

let collectTypeAbbreviationDefinition xml (accessPath: DisplayName) (e: FSharpEntity): Api seq =
  let typeAbbreviationName =
    let name = e.DisplayName
    { FSharpName = name; InternalFSharpName = name; GenericParametersForDisplay = genericParameters e } :: accessPath
  option {
    let! abbreviatedAndOriginal = toSignature e.AbbreviatedType
    let abbreviated, original =
      match abbreviatedAndOriginal with
      | TypeAbbreviation t -> t.Abbreviation, t.Original
      | original -> original, original
    let abbreviationDef: TypeAbbreviationDefinition = {
      Name = typeAbbreviationName
      FullName = e.TypeAbbreviationFullName
      AssemblyName = e.Assembly.SimpleName
      Accessibility = accessibility e
      GenericParameters = e.GenericParameters |> Seq.map (fun p -> p.Name) |> Seq.toList
      Abbreviated = abbreviated
      Original = original
    }
    let target = ApiSignature.TypeAbbreviation abbreviationDef
    return { Name = DisplayName typeAbbreviationName; Signature = target; TypeConstraints = collectTypeConstraints e.GenericParameters; Document = tryGetXmlDoc xml e }
  }
  |> Option.toList
  |> List.toSeq

let boolToConstraintStatus = function
  | true -> Satisfy
  | false -> NotSatisfy

let supportNull (e: FSharpEntity) =
  let hasAllowLiteralAttribute (x: FSharpEntity) = x.Attributes |> Seq.exists (fun attr -> attr.AttributeType.TryFullName = Some "Microsoft.FSharp.Core.AllowNullLiteralAttribute")
  if e.IsArrayType then
    true
  elif e.IsFSharp then
    if e.IsInterface || e.IsClass then
      hasAllowLiteralAttribute e
    else
      false
  elif e.IsTuple then
    false
  else
    not (e.IsEnum || e.IsValueType)

let isStruct (e: FSharpEntity) = e.IsValueType

let hasDefaultConstructor (xs: Member seq) =
  xs
  |> Seq.exists (function
    | { Arguments = [ LowType.Patterns.Unit ] } -> true
    | _ -> false)

let conditionalEquality (e:FSharpEntity) =
  let vs =
    e.GenericParameters
    |> Seq.filter (fun x ->
      x.Attributes
      |> Seq.exists (fun attr -> attr.AttributeType.FullName = "Microsoft.FSharp.Core.EqualityConditionalOnAttribute")
    )
    |> Seq.map (fun p -> p.DisplayName)
    |> Seq.toList
  match vs with
  | [] -> Satisfy
  | _ -> Dependence vs

let rec equality (cache: Map<FullIdentity, ConstraintStatus>) (e: FSharpEntity) =
  let identity = e.LoadingFullIdentity
  let updateCache cache result =
    let cache = Map.add identity result cache
    (cache, result)
  match Map.tryFind identity cache with
  | Some x -> (cache, x)
  | None ->
    if e.Attributes |> Seq.exists (fun attr -> attr.AttributeType.FullName = "Microsoft.FSharp.Core.CustomEqualityAttribute") then
      updateCache cache (conditionalEquality e)
    elif e.Attributes |> Seq.exists (fun attr -> attr.AttributeType.FullName = "Microsoft.FSharp.Core.NoEqualityAttribute") then
      updateCache cache NotSatisfy
    elif e.IsTuple then
      let vs = e.GenericParameters |> Seq.map (fun p -> p.DisplayName) |> Seq.toList
      let eq =
        match vs with
        | [] -> Satisfy
        | _ -> Dependence vs
      updateCache cache eq
    elif e.IsArrayType then
      let v = e.GenericParameters |> Seq.map (fun p -> p.DisplayName) |> Seq.toList
      updateCache cache (Dependence v)
    else
      match updateCache cache (basicEquality e) with
      | cache, NotSatisfy -> (cache, NotSatisfy)
      | cache, basicResult ->
        match foldDependentTypeEquality cache e with
        | cache, Satisfy -> updateCache cache basicResult
        | cache, _ -> updateCache cache NotSatisfy
and basicEquality (e: FSharpEntity) =
  if e.IsFSharp && (e.IsFSharpRecord || e.IsFSharpUnion || e.IsValueType) then
    let vs = e.GenericParameters |> Seq.map (fun p -> p.DisplayName) |> Seq.toList
    match vs with
    | [] -> Satisfy
    | _ -> Dependence vs
  else
    conditionalEquality e
and foldDependentTypeEquality cache (e: FSharpEntity) =
  let fields =
    seq {
      if e.IsFSharp && (e.IsFSharpRecord || e.IsValueType) then
        yield! e.FSharpFields
      elif e.IsFSharpUnion then
        for unionCase in e.UnionCases do
          yield! unionCase.UnionCaseFields
    }
    |> Seq.map (fun field -> field.FieldType)
  foldFsharpTypeEquality cache fields
and foldFsharpTypeEquality cache (ts: FSharpType seq) =
  let cache, results =
    ts
    |> Seq.fold (fun (cache, results) t ->
      let newCache, result = fsharpTypeEquality cache t
      (newCache, result :: results)
    ) (cache, [])
  let result = if results |> Seq.forall ((=)Satisfy) then Satisfy else NotSatisfy
  (cache, result)
and fsharpTypeEquality cache (t: FSharpType) =
  if t.IsGenericParameter then
    cache, Satisfy
  elif t.IsFunctionType then
    cache, NotSatisfy
  else
    let rec getRoot (t: FSharpType) = if t.IsAbbreviation then getRoot t.AbbreviatedType else t
    let root = getRoot t
    if root.IsGenericParameter then
      cache, Satisfy
    elif Seq.isEmpty root.GenericArguments then
      equality cache root.TypeDefinition
    elif root.IsTupleType then
      foldFsharpTypeEquality cache root.GenericArguments
    elif root.IsFunctionType then
      cache, NotSatisfy
    else
      let cache, rootEquality = equality cache root.TypeDefinition
      match rootEquality with
      | Satisfy -> cache, Satisfy
      | NotSatisfy -> cache, NotSatisfy
      | Dependence dependenceVariables ->
        let testArgs =
          root.TypeDefinition.GenericParameters
          |> Seq.map (fun x -> x.DisplayName)
          |> Seq.zip root.GenericArguments
          |> Seq.choose (fun (t, v) -> if Seq.exists ((=)v) dependenceVariables then Some t else None)
        foldFsharpTypeEquality cache testArgs

let conditionalComparison (e:FSharpEntity) =
  let vs =
    e.GenericParameters
    |> Seq.filter (fun x ->
      x.Attributes
      |> Seq.exists (fun attr -> attr.AttributeType.FullName = "Microsoft.FSharp.Core.ComparisonConditionalOnAttribute")
    )
    |> Seq.map (fun p -> p.DisplayName)
    |> Seq.toList
  match vs with
  | [] -> Satisfy
  | _ -> Dependence vs

let rec existsInterface identity (e: FSharpEntity) =
  e.DeclaredInterfaces
  |> Seq.exists (fun i -> i.TypeDefinition.LoadingFullIdentity = identity || existsInterface identity i.TypeDefinition)

let rec comparison (cache: Map<FullIdentity, ConstraintStatus>) (e: FSharpEntity) =
  let identity = e.LoadingFullIdentity
  let updateCache cache result =
    let cache = Map.add identity result cache
    (cache, result)
  match Map.tryFind identity cache with
  | Some x -> (cache, x)
  | None ->
    if e.Attributes |> Seq.exists (fun attr -> attr.AttributeType.FullName = "Microsoft.FSharp.Core.CustomComparisonAttribute") then
      updateCache cache (conditionalComparison e)
    elif e.Attributes |> Seq.exists (fun attr -> attr.AttributeType.FullName = "Microsoft.FSharp.Core.NoComparisonAttribute") then
      updateCache cache NotSatisfy
    elif identity = LoadingFullIdentity.IntPtr || identity = LoadingFullIdentity.UIntPtr then
      updateCache cache Satisfy
    elif e.IsTuple then
      let vs = e.GenericParameters |> Seq.map (fun p -> p.DisplayName) |> Seq.toList
      let eq =
        match vs with
        | [] -> Satisfy
        | _ -> Dependence vs
      updateCache cache eq
    elif e.IsArrayType then
      let v = e.GenericParameters |> Seq.map (fun p -> p.DisplayName) |> Seq.toList
      updateCache cache (Dependence v)
    else
      match updateCache cache (basicComparison e) with
      | cache, NotSatisfy -> (cache, NotSatisfy)
      | cache, basicResult ->
        match foldDependentTypeComparison cache e with
        | cache, Satisfy -> updateCache cache basicResult
        | cache, _ -> updateCache cache NotSatisfy
and basicComparison (e: FSharpEntity) =
  if e.IsFSharp && (e.IsFSharpRecord || e.IsFSharpUnion || e.IsValueType) then
    let vs = e.GenericParameters |> Seq.map (fun p -> p.DisplayName) |> Seq.toList
    match vs with
    | [] -> Satisfy
    | _ -> Dependence vs
  elif existsInterface LoadingFullIdentity.IComparable e || existsInterface LoadingFullIdentity.IStructuralComparable e then
    conditionalComparison e
  else
    NotSatisfy
and foldDependentTypeComparison cache (e: FSharpEntity) =
  let fields =
    seq {
      if e.IsFSharp && (e.IsFSharpRecord || e.IsValueType) then
        yield! e.FSharpFields
      elif e.IsFSharpUnion then
        for unionCase in e.UnionCases do
          yield! unionCase.UnionCaseFields
    }
    |> Seq.map (fun field -> field.FieldType)
  foldFsharpTypeComparison cache fields
and foldFsharpTypeComparison cache (ts: FSharpType seq) =
  let cache, results =
    ts
    |> Seq.fold (fun (cache, results) t ->
      let newCache, result = fsharpTypeComparison cache t
      (newCache, result :: results)
    ) (cache, [])
  let result = if results |> Seq.forall ((=)Satisfy) then Satisfy else NotSatisfy
  (cache, result)
and fsharpTypeComparison cache (t: FSharpType) =
  if t.IsGenericParameter then
    cache, Satisfy
  elif t.IsFunctionType then
    cache, NotSatisfy
  else
    let rec getRoot (t: FSharpType) = if t.IsAbbreviation then getRoot t.AbbreviatedType else t
    let root = getRoot t
    if root.IsGenericParameter then
      cache, Satisfy
    elif Seq.isEmpty root.GenericArguments then
      comparison cache root.TypeDefinition
    elif root.IsTupleType then
      foldFsharpTypeComparison cache root.GenericArguments
    elif root.IsFunctionType then
      cache, NotSatisfy
    else
      let cache, rootComparison = comparison cache root.TypeDefinition
      match rootComparison with
      | Satisfy -> cache, Satisfy
      | NotSatisfy -> cache, NotSatisfy
      | Dependence dependenceVariables ->
        let testArgs =
          root.TypeDefinition.GenericParameters
          |> Seq.map (fun x -> x.DisplayName)
          |> Seq.zip root.GenericArguments
          |> Seq.choose (fun (t, v) -> if Seq.exists ((=)v) dependenceVariables then Some t else None)
        foldFsharpTypeComparison cache testArgs

let fullTypeDef xml (name: DisplayName) (e: FSharpEntity) members =
  option {
    let identity = { e.LoadingFullIdentity with Name = DisplayName name }
    let baseType =
      if not e.IsInterface then
        e.BaseType |> Option.bind toSignature
      else
        None
    let instanceMembers =
      members
      |> Seq.choose (function
        | { Signature = ApiSignature.InstanceMember (_, m) } -> Some m
        | _ -> None)
      |> Seq.toList
    let staticMembers =
      members
      |> Seq.choose (function
        | { Signature = ApiSignature.StaticMember (_, m) } -> Some m
        | _ -> None)
      |> Seq.toList

    let implicitInstanceMembers, implicitStaticMembers = CompilerOptimization.implicitMembers identity

    let fullName =
      match e.TryFullName with
      | Some fullName -> fullName
      | None -> e.AccessPath + "." + e.CompiledName

    let typeDef = {
      Name = name
      FullName = fullName
      AssemblyName = identity.AssemblyName
      Accessibility = accessibility e
      BaseType = baseType
      AllInterfaces = e.DeclaredInterfaces |> Seq.filter (fun x -> x.TypeDefinition.Accessibility.IsPublic) |> Seq.choose toSignature |> Seq.toList
      GenericParameters = genericParameters e
      TypeConstraints = e.GenericParameters |> collectTypeConstraints
      InstanceMembers = instanceMembers
      StaticMembers = staticMembers

      ImplicitInstanceMembers = implicitInstanceMembers
      ImplicitStaticMembers = implicitStaticMembers

      SupportNull = supportNull e |> boolToConstraintStatus
      ReferenceType = isStruct e |> not |> boolToConstraintStatus
      ValueType = isStruct e |> boolToConstraintStatus
      DefaultConstructor = 
        let constructors = members |> Seq.choose (function { Signature = ApiSignature.Constructor (_, m)} -> Some m | _ -> None)
        hasDefaultConstructor constructors
        |> boolToConstraintStatus
      Equality = equality Map.empty e |> snd
      Comparison = comparison Map.empty e |> snd
    }
    return { Name = DisplayName name; Signature = ApiSignature.FullTypeDefinition typeDef; TypeConstraints = typeDef.TypeConstraints; Document = tryGetXmlDoc xml e }
  }

let rec collectApi xml (accessPath: DisplayName) (e: FSharpEntity): Api seq =
  seq {
    if e.IsNamespace then
      let accessPath =
        let name = e.DisplayName
        { FSharpName = name; InternalFSharpName = name; GenericParametersForDisplay = [] } :: accessPath
      yield! collectFromNestedEntities xml accessPath e
    elif e.IsCompilerInternalModule then
      ()
    elif e.IsFSharpModule then
      yield! collectFromModule xml accessPath e
    elif e.IsFSharpAbbreviation && not e.IsMeasure then
      yield! collectTypeAbbreviationDefinition xml accessPath e
    elif e.IsClass || e.IsValueType || e.IsFSharpRecord || e.IsFSharpUnion || e.IsArrayType || e.IsDelegate then
      yield! collectFromType xml accessPath e
    elif e.IsInterface then
      yield! collectFromInterface xml accessPath e
    elif e.IsOpaque || e.HasAssemblyCodeRepresentation then
      yield! collectFromType xml accessPath e
  }
and collectFromNestedEntities xml (accessPath: DisplayName) (e: FSharpEntity): Api seq =
  e.NestedEntities
  |> Seq.collect (collectApi xml accessPath)
and collectFromModule xml (accessPath: DisplayName) (e: FSharpEntity): Api seq =
  let moduleName =
    let name = e.DisplayName
    { FSharpName = name; InternalFSharpName = name; GenericParametersForDisplay = [] } :: accessPath
  seq {
    yield! e.MembersFunctionsAndValues
            |> Seq.filter (fun x -> x.Accessibility.IsPublic)
            |> Seq.choose (toFSharpApi xml moduleName)
    yield! collectFromNestedEntities xml moduleName e
  }
and collectFromType xml (accessPath: DisplayName) (e: FSharpEntity): Api seq =
  let typeName =
    let name = e.DisplayName
    { FSharpName = name; InternalFSharpName = name; GenericParametersForDisplay = genericParameters e } :: accessPath
  seq {
    let declaringSignature = fsharpEntityToSignature e

    let members =
      e.MembersFunctionsAndValues
      |> Seq.filter (fun x -> x.Accessibility.IsPublic && not x.IsCompilerGenerated)
      |> Seq.choose (toTypeMemberApi xml typeName e declaringSignature)
      |> Seq.cache

    let fields =
      e.FSharpFields
      |> Seq.filter (fun x -> x.Accessibility.IsPublic && not x.IsCompilerGenerated)
      |> Seq.choose (toFieldApi xml typeName e declaringSignature)
      |> Seq.cache
    match fullTypeDef xml typeName e (Seq.append members fields) with
    | Some d ->
      yield d
      yield! members
      yield! fields
    | None -> ()
    yield! collectFromNestedEntities xml typeName e
  }
and collectInterfaceMembers xml (declaringSignatureName: DisplayName) (inheritArgs: (TypeVariable * LowType) list) (e: FSharpEntity): Api seq =
  let replaceVariable replacements = function
    | ApiSignature.InstanceMember (declaringType, member') ->
      let apply = LowType.applyVariable VariableSource.Target replacements
      let declaringType = apply declaringType
      let genericParameters =
        member'.GenericParameters
        |> List.map (fun p ->
          match replacements |> Map.tryFind p with
          | Some (Variable (_, replacement)) -> replacement
          | Some _ -> failwith "Generic parameter replacement should be variable."
          | None -> p)
      let member' =
        { member' with
            GenericParameters = genericParameters
            Arguments = member'.Arguments |> List.map apply
            ReturnType = member'.ReturnType |> apply
        }
      ApiSignature.InstanceMember (declaringType, member')
    | _ -> failwith "It is not interface member."
  
  seq {
    let declaringSignature = fsharpEntityToSignature e
    let replacementVariables = inheritArgs |> List.map snd |> List.collect LowType.collectVariables
    yield! e.MembersFunctionsAndValues
            |> Seq.filter (fun x -> x.Accessibility.IsPublic && not x.IsCompilerGenerated)
            |> Seq.choose (fun member' -> option {
              let! api = toTypeMemberApi xml declaringSignatureName e declaringSignature member'
              let variableReplacements =
                List.append (resolveConflictGenericArgumnet replacementVariables member') inheritArgs
                |> Map.ofList
              let api = { api with Signature = replaceVariable variableReplacements api.Signature }
              return api
            })

    for parentInterface in e.DeclaredInterfaces |> Seq.filter (fun x -> x.TypeDefinition.Accessibility.IsPublic) do
      let inheritArgs = genericParametersAndArguments parentInterface
      yield! collectInterfaceMembers xml declaringSignatureName inheritArgs parentInterface.TypeDefinition
              |> Seq.map (updateInterfaceDeclaringType declaringSignatureName declaringSignature)
  }
and collectFromInterface xml (accessPath: DisplayName) (e: FSharpEntity): Api seq =
  let interfaceName =
    let name = e.DisplayName
    { FSharpName = name; InternalFSharpName = name; GenericParametersForDisplay = genericParameters e } :: accessPath
  seq {
    let members = collectInterfaceMembers xml interfaceName [] e |> Seq.cache

    match fullTypeDef xml interfaceName e members with
    | Some d ->
      yield d
      yield! members
    | None -> ()
  }

let collectTypeAbbreviations (xs: Api seq) =
  xs
  |> Seq.choose (function { Signature = ApiSignature.TypeAbbreviation t } -> Some t | _ -> None)

let tryGetXml (assembly: FSharpAssembly) = option {
  let! assemblyFileName = assembly.FileName
  let xmlFileName = Path.ChangeExtension(assemblyFileName, "xml")
  if File.Exists(xmlFileName) then
    return XElement.Parse(File.ReadAllText(xmlFileName))
  else
    return! None
}

let load' (assembly: FSharpAssembly): ApiDictionary =
  let xml = tryGetXml assembly
  let api =
    assembly.Contents.Entities
    |> Seq.collect (fun e ->
      let accessPath =
        match e.AccessPath with
        | "global" -> []
        | a -> DisplayName.ofString a
      collectApi xml accessPath e)
    |> Seq.toArray
  let types =
    api
    |> Seq.choose (function { Signature = ApiSignature.FullTypeDefinition full } -> Some full | _ -> None)
    |> Seq.toArray
  let typeAbbreviations =
    collectTypeAbbreviations api
    |> Seq.toArray
  { AssemblyName = assembly.SimpleName; Api = api; TypeDefinitions = types; TypeAbbreviations = typeAbbreviations }

module NameResolve =
  type NameCache = IDictionary<string, IDictionary<string, DisplayName>>
  
  let resolve_Name (cache: NameCache) (name: Name) =
    match name with
    | LoadingName (assemblyName, fullName, displayName) -> DisplayName (displayName @ cache.[assemblyName].[fullName])
    | DisplayName _ as n -> n
  
  let rec resolve_LowType cache = function
    | Wildcard _ as w -> w
    | Variable _ as v -> v
    | Identity i ->
      match i with
      | PartialIdentity _ -> Identity i
      | FullIdentity full -> Identity (FullIdentity { full with Name = resolve_Name cache full.Name })
    | Arrow xs -> Arrow (List.map (resolve_LowType cache) xs)
    | Tuple xs -> Tuple (List.map (resolve_LowType cache) xs)
    | Generic (id, args) ->
      let id = resolve_LowType cache id
      let args = List.map (resolve_LowType cache) args
      Generic (id, args)
    | TypeAbbreviation a -> TypeAbbreviation { Abbreviation = resolve_LowType cache a.Abbreviation; Original = resolve_LowType cache a.Original }

  let resolve_Member cache (member': Member) =
    { member' with
        Arguments = List.map (resolve_LowType cache) member'.Arguments
        ReturnType = resolve_LowType cache member'.ReturnType
    }

  let resolve_TypeExtension cache (typeExtension: TypeExtension) =
    { typeExtension with
        ExistingType = resolve_LowType cache typeExtension.ExistingType
        Member = resolve_Member cache typeExtension.Member
    }

  let resolve_TypeConstraint cache (c: TypeConstraint) =
    let constraint' =
      match c.Constraint with
      | SubtypeConstraints x -> SubtypeConstraints (resolve_LowType cache x)
      | MemberConstraints (modifier, member') -> MemberConstraints (modifier, resolve_Member cache member')
      | other -> other
    { c with Constraint = constraint' }

  let resolve_FullTypeDefinition cache (fullTypeDef: FullTypeDefinition) =
    { fullTypeDef with
        BaseType = Option.map (resolve_LowType cache) fullTypeDef.BaseType
        AllInterfaces = List.map (resolve_LowType cache) fullTypeDef.AllInterfaces
        TypeConstraints = List.map (resolve_TypeConstraint cache) fullTypeDef.TypeConstraints
        InstanceMembers = List.map (resolve_Member cache) fullTypeDef.InstanceMembers
        StaticMembers = List.map (resolve_Member cache) fullTypeDef.StaticMembers
        ImplicitInstanceMembers = List.map (resolve_Member cache) fullTypeDef.ImplicitInstanceMembers
        ImplicitStaticMembers = List.map (resolve_Member cache) fullTypeDef.ImplicitStaticMembers
    }

  let resolve_TypeAbbreviationDefinition cache abbDef =
    { abbDef with
        Abbreviated = resolve_LowType cache abbDef.Abbreviated
        Original = resolve_LowType cache abbDef.Original
    }

  let resolve_Signature cache = function
    | ApiSignature.ModuleValue x -> ApiSignature.ModuleValue (resolve_LowType cache x)
    | ApiSignature.ModuleFunction xs -> ApiSignature.ModuleFunction (List.map (resolve_LowType cache) xs)
    | ApiSignature.ActivePatten (kind, x) -> ApiSignature.ActivePatten (kind, resolve_LowType cache x)
    | ApiSignature.InstanceMember (d, m) -> ApiSignature.InstanceMember (resolve_LowType cache d, resolve_Member cache m)
    | ApiSignature.StaticMember (d, m) -> ApiSignature.StaticMember (resolve_LowType cache d, resolve_Member cache m)
    | ApiSignature.Constructor (d, m) -> ApiSignature.Constructor (resolve_LowType cache d, resolve_Member cache m)
    | ApiSignature.FullTypeDefinition f -> ApiSignature.FullTypeDefinition (resolve_FullTypeDefinition cache f)
    | ApiSignature.TypeAbbreviation a -> ApiSignature.TypeAbbreviation (resolve_TypeAbbreviationDefinition cache a)
    | ApiSignature.TypeExtension e -> ApiSignature.TypeExtension (resolve_TypeExtension cache e)
    | ApiSignature.ExtensionMember m -> ApiSignature.ExtensionMember (resolve_Member cache m)

  let resolve_Api cache (api: Api) =
    { api with
        Name = resolve_Name cache api.Name
        Signature = resolve_Signature cache api.Signature
        TypeConstraints = List.map (resolve_TypeConstraint cache) api.TypeConstraints
    }

  let resolve_ApiDictionary cache (apiDic: ApiDictionary) =
    { apiDic with
        Api = Array.map (resolve_Api cache) apiDic.Api
        TypeDefinitions = Array.map (resolve_FullTypeDefinition cache) apiDic.TypeDefinitions
        TypeAbbreviations = Array.map (resolve_TypeAbbreviationDefinition cache) apiDic.TypeAbbreviations
    }

  let resolveLoadingName (dictionaries: ApiDictionary[]) =
    let cache: NameCache =
      dictionaries
      |> Seq.map (fun d ->
        let names =
          Seq.concat [
            d.TypeDefinitions |> Seq.map (fun x -> x.FullName, x.Name)
            d.TypeAbbreviations |> Seq.map (fun x -> x.FullName, x.Name)
          ]
          |> dict
        (d.AssemblyName, names)
      )
      |> dict
    dictionaries |> Array.map (resolve_ApiDictionary cache)

let load (assemblies: FSharpAssembly[]): ApiDictionary[] = Array.map load' assemblies |> NameResolve.resolveLoadingName

let databaseName = "database"

let save path (dictionaries: ApiDictionary[]) =
  use file = File.OpenWrite(path)
  let serializer = FsPickler.CreateBinarySerializer()
  serializer.Serialize(file, dictionaries)

let loadFromFile path =
  use file = File.OpenRead(path)
  let serializer = FsPickler.CreateBinarySerializer()
  serializer.Deserialize<ApiDictionary[]>(file)