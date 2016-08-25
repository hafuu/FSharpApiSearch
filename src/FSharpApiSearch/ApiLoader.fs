module FSharpApiSearch.ApiLoader

open Microsoft.FSharp.Compiler.SourceCodeServices
open FSharpApiSearch.SpecialTypes
open System.Text.RegularExpressions
open System.IO
open MBrace.FsPickler
open System.Collections.Generic
open System.Xml.Linq

module internal Impl =
  let inline tryGetXmlDoc (xml: XElement option) (symbol: ^a): string option =
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

  let rec fsharpTypeToLowType (t: FSharpType) =
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
        let! args = listLowType t.GenericArguments
        return Tuple args
      }
    elif Hack.isFloat t then
      Some SpecialTypes.LowType.float
    elif t.HasTypeDefinition then
      let signature =
        match Hack.genericArguments t with
        | [] -> t.TryIdentity
        | xs -> 
          option {
            let! xs = listLowType xs
            let! id = t.TryIdentity
            return Generic (id, xs)
          }
      if t.TypeDefinition.IsDelegate then
        option {
          let! arrow = delegateArrow t
          let! t = signature
          return Delegate (t, arrow)
        }
      elif Hack.isAbbreviation t then
        option {
          let! signature = signature
          let! original = abbreviationRoot t
          return TypeAbbreviation { Abbreviation = signature; Original = original }
        }
      else
        signature
    else
      None
  and delegateArrow (t: FSharpType) =
    let td = t.TypeDefinition
    option {
      let! invokeMethod = td.MembersFunctionsAndValues |> Seq.tryFind (fun m -> m.DisplayName = "Invoke")
      let! arrow = option {
        let! genericArguments = t.GenericArguments |> listLowType
        let replacements = Seq.zip (genericParameters td) genericArguments |> Map.ofSeq
        let! methodParameters =
          [
            for xs in invokeMethod.CurriedParameterGroups do
              for x in xs do
                yield x.Type
            yield invokeMethod.ReturnParameter.Type
          ]
          |> listLowType
        return LowType.applyVariableToTargetList VariableSource.Target replacements methodParameters
      }
      return arrow
    }
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
      fsharpTypeToLowType t
  and toFlatArrow (t: FSharpType): LowType list option =
    match Seq.toList t.GenericArguments with
    | [ x; y ] when y.IsFunctionType ->
      option {
        let! xSig = fsharpTypeToLowType x
        let! ySigs = toFlatArrow y
        return xSig :: ySigs
      }
    | [ x; y ] ->
      option {
        let! xSig = fsharpTypeToLowType x
        let! ySig = fsharpTypeToLowType y
        return [ xSig; ySig ]
      }
    | _ -> None
  and listLowType (ts: FSharpType seq) = ts |> Seq.foldOptionMapping fsharpTypeToLowType |> Option.map Seq.toList
  and fsharpEntityToLowType (x: FSharpEntity) =
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
            let! parent = fsharpTypeToLowType c.CoercesToTarget
            return { Variables = [ variable ]; Constraint = SubtypeConstraints parent }
          }
        elif c.IsSupportsNullConstraint then
          Some { Variables = [ variable ]; Constraint = NullnessConstraints }
        elif c.IsMemberConstraint then
          option {
            let data = c.MemberConstraintData
            let modifier = if data.MemberIsStatic then MemberModifier.Static else MemberModifier.Instance
            let! returnType = fsharpTypeToLowType data.MemberReturnType
            let returnParam = Parameter.ofLowType returnType
            let! parameters = listLowType data.MemberArgumentTypes
            let parameters =
              let ps =
                if data.MemberIsStatic then
                  if parameters.Length = 0 then
                    [ LowType.unit ] // Core.Unit is removed if the argument is only Core.Unit.
                  else
                    parameters
                else
                  if parameters.Length = 1 then
                    [ LowType.unit ] // Core.Unit is removed if the argument is only Core.Unit.
                  else
                    List.tail parameters // instance member contains receiver
              [ List.map Parameter.ofLowType ps ]
              
            let variables = data.MemberSources |> Seq.map (fun x -> x.GenericParameter.Name) |> Seq.toList
            let name = data.MemberName
            let member' = { Name = name; Kind = MemberKind.Method; Parameters = parameters; ReturnParameter = returnParam; GenericParameters = [] }
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
    listLowType xs

  let curriedParameterGroups (t: FSharpMemberOrFunctionOrValue) =
    seq {
      for group in t.CurriedParameterGroups do
        if Seq.length group > 0 then
          let group = seq {
              for p in group do
                yield option {
                  let! t = fsharpTypeToLowType p.Type
                  return { Name = p.Name; Type = t }
                }
            }
          yield Seq.foldOptionMapping id group |> Option.map Seq.toList
        else
          ()
    }
    |> Seq.foldOptionMapping id
    |> Option.map Seq.toList

  let complementUnitParameter (x: FSharpMemberOrFunctionOrValue) ps =
    if Hack.isUnitOnlyParameter x then
      [ [ Parameter.ofLowType LowType.unit ] ]
    else
      ps

  let methodMember (x: FSharpMemberOrFunctionOrValue) =
    option {
      let name = x.GetDisplayName
      let! parameters = curriedParameterGroups x
      let parameters = complementUnitParameter x parameters
      let! returnType = fsharpTypeToLowType x.ReturnParameter.Type
      let returnParam = Parameter.ofLowType returnType
      let genericParams = x.GenericParametersAsTypeVariable
      let member' = { Name = name.InternalFSharpName; Kind = MemberKind.Method; GenericParameters = genericParams; Parameters = parameters; ReturnParameter = returnParam }
      return (name, member')
    }

  let propertyMember (x: FSharpMemberOrFunctionOrValue) =
    option {
      let name = x.GetDisplayName
      let! parameters = curriedParameterGroups x
      let! returnType = fsharpTypeToLowType x.ReturnParameter.Type
      let returnParam = Parameter.ofLowType returnType
      let genericParams = x.GenericParametersAsTypeVariable
      let memberKind = MemberKind.Property x.PropertyKind
      let member' = { Name = name.InternalFSharpName; Kind = memberKind; GenericParameters = genericParams; Parameters = parameters; ReturnParameter = returnParam }
      return (name, member')
    }

  let toModuleValue xml (declaringModuleName: DisplayName) (x: FSharpMemberOrFunctionOrValue) =
    option {
      let name = x.GetDisplayName :: declaringModuleName
      let! parameters = curriedParameterGroups x
      let! returnType = fsharpTypeToLowType x.ReturnParameter.Type
      let returnParam = Parameter.ofLowType returnType
      let arrow = [ yield! parameters; yield [ returnParam ] ]
      let target =
        if x.IsActivePattern then
          let kind = if x.DisplayName.Contains("|_|") then ActivePatternKind.PartialActivePattern else ActivePatternKind.ActivePattern
          ApiSignature.ActivePatten (kind, arrow)
        else
          match arrow with
          | [ [ value ] ] -> ApiSignature.ModuleValue value.Type
          | _ -> ApiSignature.ModuleFunction arrow
      return { Name = DisplayName name; Signature = target; TypeConstraints = collectTypeConstraints x.GenericParameters; Document = tryGetXmlDoc xml x }
    }

  let toTypeExtension xml (declaringModuleName: DisplayName) (x: FSharpMemberOrFunctionOrValue) =
    option {
      let existingType = fsharpEntityToLowType x.LogicalEnclosingEntity
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

  let constructorSignature xml (declaringSignatureName: DisplayName) (declaringSignature: LowType) (x: FSharpMemberOrFunctionOrValue) =
    option {
      let! _, target = methodMember x
      let target = { target with Name = declaringSignatureName.Head.FSharpName; ReturnParameter = Parameter.ofLowType declaringSignature }
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
      let! returnType = fsharpTypeToLowType field.FieldType
      let returnParam = Parameter.ofLowType returnType
      let member' = { Name = field.Name; Kind = MemberKind.Field; GenericParameters = []; Parameters = []; ReturnParameter = returnParam }
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
      let! s = fsharpTypeToLowType arg
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
      let! abbreviatedAndOriginal = fsharpTypeToLowType e.AbbreviatedType
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
      | { Parameters = [ [ { Type = LowType.Patterns.Unit } ] ] } -> true
      | _ -> false)

  type EqualityAndComparisonLoaderBuilder = {
    ConditionalAttrName: string
    CustomAttrName: string
    NoAttrName: string
    SatisfyTypes: string list
    ExpectedInterfaces: string list
  }

  let loadEqualityAndComparison builder (e: FSharpEntity) =
    let loadConditional (e: FSharpEntity) =
      let vs =
        e.GenericParameters
        |> Seq.filter (fun x ->
          x.Attributes
          |> Seq.exists (fun attr -> attr.AttributeType.FullName = builder.ConditionalAttrName)
        )
        |> Seq.map (fun p -> p.DisplayName)
        |> Seq.toList
      match vs with
      | [] -> Satisfy
      | _ -> Dependence vs

    let rec implementsInterface (e: FSharpEntity) expectedInterface =
      e.DeclaredInterfaces
      |> Seq.exists (fun i -> i.TypeDefinition.TryFullName = Some expectedInterface || implementsInterface i.TypeDefinition expectedInterface)

    let rec load (cache: Map<FullIdentity, ConstraintStatus>) (e: FSharpEntity) =
      let identity = e.LoadingFullIdentity
      let fullName = e.TryFullName
      let updateCache cache result =
        let cache = Map.add identity result cache
        (cache, result)
      match Map.tryFind identity cache with
      | Some x -> (cache, x)
      | None ->
        if e.Attributes |> Seq.exists (fun attr -> attr.AttributeType.FullName = builder.CustomAttrName) then
          updateCache cache (loadConditional e)
        elif e.Attributes |> Seq.exists (fun attr -> attr.AttributeType.FullName = builder.NoAttrName) then
          updateCache cache NotSatisfy
        elif builder.SatisfyTypes |> Seq.exists (Some >> ((=) fullName)) then
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
          match updateCache cache (loadBasic e) with
          | cache, NotSatisfy -> (cache, NotSatisfy)
          | cache, basicResult ->
            match foldDependentType cache e with
            | cache, Satisfy -> updateCache cache basicResult
            | cache, _ -> updateCache cache NotSatisfy
    and loadBasic (e: FSharpEntity) =
      if e.IsFSharp && (e.IsFSharpRecord || e.IsFSharpUnion || e.IsValueType) then
        let vs = e.GenericParameters |> Seq.map (fun p -> p.DisplayName) |> Seq.toList
        match vs with
        | [] -> Satisfy
        | _ -> Dependence vs
      elif builder.ExpectedInterfaces |> Seq.isEmpty then
        loadConditional e
      elif builder.ExpectedInterfaces |> Seq.exists (implementsInterface e) then
        loadConditional e
      else
        NotSatisfy
    and foldDependentType cache (e: FSharpEntity) =
      let fields =
        seq {
          if e.IsFSharp && (e.IsFSharpRecord || e.IsValueType) then
            yield! e.FSharpFields
          elif e.IsFSharpUnion then
            for unionCase in e.UnionCases do
              yield! unionCase.UnionCaseFields
        }
        |> Seq.map (fun field -> field.FieldType)
      foldFsharpType cache fields
    and foldFsharpType cache (ts: FSharpType seq) =
      let cache, results =
        ts
        |> Seq.fold (fun (cache, results) t ->
          let newCache, result = loadFsharpType cache t
          (newCache, result :: results)
        ) (cache, [])
      let result = if results |> Seq.forall ((=)Satisfy) then Satisfy else NotSatisfy
      (cache, result)
    and loadFsharpType cache (t: FSharpType) =
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
          load cache root.TypeDefinition
        elif root.IsTupleType then
          foldFsharpType cache root.GenericArguments
        elif root.IsFunctionType then
          cache, NotSatisfy
        else
          let cache, rootResult = load cache root.TypeDefinition
          match rootResult with
          | Satisfy -> cache, Satisfy
          | NotSatisfy -> cache, NotSatisfy
          | Dependence dependenceVariables ->
            let testArgs =
              root.TypeDefinition.GenericParameters
              |> Seq.map (fun x -> x.DisplayName)
              |> Seq.zip root.GenericArguments
              |> Seq.choose (fun (t, v) -> if Seq.exists ((=)v) dependenceVariables then Some t else None)
            foldFsharpType cache testArgs

    load Map.empty e

  let equality (e: FSharpEntity) =
    let builder = {
      ConditionalAttrName = typeof<EqualityConditionalOnAttribute>.FullName
      CustomAttrName = typeof<CustomEqualityAttribute>.FullName
      NoAttrName = typeof<NoEqualityAttribute>.FullName
      SatisfyTypes = []
      ExpectedInterfaces = []
    }
    loadEqualityAndComparison builder e

  let comparison (e: FSharpEntity) =
    let builder = {
      ConditionalAttrName = typeof<ComparisonConditionalOnAttribute>.FullName
      CustomAttrName = typeof<CustomComparisonAttribute>.FullName
      NoAttrName = typeof<NoComparisonAttribute>.FullName
      SatisfyTypes = [ typeof<System.IntPtr>.FullName; typeof<System.UIntPtr>.FullName ]
      ExpectedInterfaces = [ typeof<System.IComparable>.FullName; typeof<System.Collections.IStructuralComparable>.FullName ]
    }
    loadEqualityAndComparison builder e

  let fullTypeDef xml (name: DisplayName) (e: FSharpEntity) members =
    option {
      let identity = { e.LoadingFullIdentity with Name = DisplayName name }
      let baseType =
        if not e.IsInterface then
          e.BaseType |> Option.bind fsharpTypeToLowType
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
        AllInterfaces = e.DeclaredInterfaces |> Seq.filter (fun x -> x.TypeDefinition.Accessibility.IsPublic) |> Seq.choose fsharpTypeToLowType |> Seq.toList
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
        Equality = equality e |> snd
        Comparison = comparison e |> snd
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
      let declaringSignature = fsharpEntityToLowType e

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
        let applyParam p = { p with Type = apply p.Type }
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
              Parameters = member'.Parameters |> List.map (List.map applyParam)
              ReturnParameter = member'.ReturnParameter |> applyParam
          }
        ApiSignature.InstanceMember (declaringType, member')
      | _ -> failwith "It is not interface member."
  
    seq {
      let declaringSignature = fsharpEntityToLowType e
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

  let load (assembly: FSharpAssembly): ApiDictionary =
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
      | LoadingName (assemblyName, fullName, displayName) ->
        match cache.TryGetValue(assemblyName) with
        | true, assembly ->
            match assembly.TryGetValue(fullName) with
            | true, namespace' -> DisplayName (displayName @ namespace')
            | false, _ -> failwithf "Namespace %s is not found in %s" assemblyName assemblyName
        | false, _ -> failwithf "Assembly %s is not found" assemblyName
      | DisplayName _ as n -> n
  
    let rec resolve_LowType cache = function
      | Wildcard _ as w -> w
      | Variable _ as v -> v
      | Identity i -> Identity (resolve_Identity cache i)
      | Arrow xs -> Arrow (List.map (resolve_LowType cache) xs)
      | Tuple xs -> Tuple (List.map (resolve_LowType cache) xs)
      | Generic (id, args) ->
        let id = resolve_LowType cache id
        let args = List.map (resolve_LowType cache) args
        Generic (id, args)
      | TypeAbbreviation a -> TypeAbbreviation { Abbreviation = resolve_LowType cache a.Abbreviation; Original = resolve_LowType cache a.Original }
      | Delegate (t, arrow) ->
        let t = resolve_LowType cache t
        let arrow = List.map (resolve_LowType cache) arrow
        Delegate (t, arrow)
    and resolve_Identity cache = function
      | PartialIdentity _ as i -> i
      | FullIdentity full -> FullIdentity { full with Name = resolve_Name cache full.Name }

    let resolve_Parameter cache (p: Parameter) = { p with Type = resolve_LowType cache p.Type }
    let resolve_ParameterGroups cache (groups: ParameterGroups) = List.map (List.map (resolve_Parameter cache)) groups

    let resolve_Member cache (member': Member) =
      { member' with
          Parameters = resolve_ParameterGroups cache member'.Parameters
          ReturnParameter = resolve_Parameter cache member'.ReturnParameter
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

    let resolve_Function cache fn = resolve_ParameterGroups cache fn

    let resolve_Signature cache = function
      | ApiSignature.ModuleValue x -> ApiSignature.ModuleValue (resolve_LowType cache x)
      | ApiSignature.ModuleFunction fn -> ApiSignature.ModuleFunction (resolve_Function cache fn)
      | ApiSignature.ActivePatten (kind, fn) -> ApiSignature.ActivePatten (kind, resolve_Function cache fn)
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

let load (assemblies: FSharpAssembly[]): ApiDictionary[] = Array.map Impl.load assemblies |> Impl.NameResolve.resolveLoadingName

let databaseName = "database"

let save path (dictionaries: ApiDictionary[]) =
  use file = File.OpenWrite(path)
  let serializer = FsPickler.CreateBinarySerializer()
  serializer.Serialize(file, dictionaries)

let loadFromFile path =
  use file = File.OpenRead(path)
  let serializer = FsPickler.CreateBinarySerializer()
  serializer.Deserialize<ApiDictionary[]>(file)