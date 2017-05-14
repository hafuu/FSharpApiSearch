module FSharpApiSearch.ApiLoader

open Microsoft.FSharp.Compiler.SourceCodeServices
open FSharpApiSearch.SpecialTypes
open System.Text.RegularExpressions
open System.IO
open MBrace.FsPickler
open System.Collections.Generic
open System.Xml.Linq
open FSharp.Collections.ParallelSeq

type TypeForward = {
  Type: string
  From: string
  To: string
}

module internal Impl =
  type XmlDocCache = IDictionary<string, string>
  let createXmlDocCache (xml: XElement) =
    let members =
      let membersElem = xml.Element(XName.Get("members"))
      if membersElem = null then
        Seq.empty
      else
        seq {
          for m in membersElem.Elements(XName.Get("member")) do
            match m.Element(XName.Get("summary")) with
            | null -> ()
            | summary when System.String.IsNullOrWhiteSpace(summary.Value) = false ->
              let name = m.Attribute(XName.Get("name")).Value
              let summaryValue = summary.Value.Replace("\r", "").Replace("\n", "").Trim()
              yield (name, summaryValue)
            | _ -> ()
        }
    dict members

  let VariableSource = VariableSource.Target
  let inline tryGetXmlDoc (cache: XmlDocCache option) (symbol: ^a): string option =
    option {
      let! cache = cache
      let xmlSig: string = (^a : (member XmlDocSig : string) symbol)
      match cache.TryGetValue(xmlSig) with
      | true, v -> return v
      | false, _ -> return! None
    }

  let genericSuffix = Regex(@"`\d+$")
  let inline compiledName (symbol: ^a) =
    let name = (^a : (member CompiledName : string) symbol)
    genericSuffix.Replace(name, "")
  
  type FSharpGenericParameter with
    member this.IsAutoGeneric = this.Name.StartsWith("?")
    member this.TypeVariable: TypeVariable =
      let name = this.Name
      { Name = name; IsSolveAtCompileTime = this.IsSolveAtCompileTime }

  let genericParameters (e: FSharpEntity) =
    e.GenericParameters |> Seq.map (fun p -> p.TypeVariable) |> Seq.toList

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
      | Some fullName ->
        (fullName.StartsWith("System.Tuple") && this.DisplayName = "Tuple")
        || (fullName.StartsWith("System.ValueTuple") && this.DisplayName = "ValueTuple")
      | None -> false
    member this.IsCompilerInternalModule = this.IsFSharpModule && (this.FullName = "Microsoft.FSharp.Core.LanguagePrimitives" || this.FullName = "Microsoft.FSharp.Core.Operators.OperatorIntrinsics")
    member this.GetDisplayName() =
      let name =
        let name = this.DisplayName
        let compiledName = compiledName this
        if name <> compiledName then WithCompiledName (name, compiledName) else SymbolName name
      { Name = name; GenericParameters = genericParameters this }

  type FSharpType with
    member this.TryIdentity = this.TryFullIdentity |> Option.map (fun x -> Identity (FullIdentity x))
    member this.TryFullIdentity =
      if Hack.isFloat this then
        Some { this.TypeDefinition.LoadingFullIdentity with GenericParameterCount = 0 }
      elif this.HasTypeDefinition then
        Some this.TypeDefinition.LoadingFullIdentity
      else
        None

  let compiledNameOfProperty (x: FSharpMemberOrFunctionOrValue) =
    let compiledNameAttr = x.Attributes |> Seq.tryFind (fun attr -> attr.AttributeType.TryFullName = Some "Microsoft.FSharp.Core.CompiledNameAttribute")
    match compiledNameAttr with
    | None -> x.DisplayName
    | Some attr -> attr.ConstructorArguments |> Seq.head |> snd |> unbox<string>

  type FSharpMemberOrFunctionOrValue with
    member this.IsStaticMember = not this.IsInstanceMember
    member this.IsMethod = this.FullType.IsFunctionType && not this.IsPropertyGetterMethod && not this.IsPropertySetterMethod
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
    member this.TargetSignatureConstructor = fun (declaringType: LowType) member' ->
      if this.IsCSharpExtensionMember then
        ApiSignature.ExtensionMember member'
      elif this.IsStaticMember then
        ApiSignature.StaticMember (declaringType, member')
      else
        ApiSignature.InstanceMember (declaringType, member')
    member this.GenericParametersAsTypeVariable =
      this.GenericParameters |> Seq.map (fun x -> x.TypeVariable) |> Seq.toList
    member this.GetDisplayName =
      let displayName = this.DisplayName
      let compiledName =
        if this.IsProperty then
          compiledNameOfProperty this
        else
          compiledName this
      let genericParameters = this.GenericParameters |> Seq.map (fun p -> p.TypeVariable) |> Seq.toList
      let isOperator =
        displayName.StartsWith("(") && displayName.EndsWith(")") && compiledName.StartsWith("op_")
      if isOperator then
        { Name = OperatorName (displayName, compiledName); GenericParameters = genericParameters }
      else
        let name =
          if displayName <> compiledName then
            WithCompiledName (displayName, compiledName)
          else
            SymbolName displayName
        { Name = name; GenericParameters = genericParameters }

  type FSharpField with
    member this.TargetSignatureConstructor = fun (declaringType: LowType) member' ->
      if this.IsStatic then
        ApiSignature.StaticMember (declaringType, member')
      else
        ApiSignature.InstanceMember (declaringType, member')

  let accessibility (e: FSharpEntity) =
    let a = e.Accessibility
    if a.IsPublic then
      Accessibility.Public
    else
      Accessibility.Private

  let autoGenericVariableLen = "type '".Length

  let isByRef (t: FSharpType) = t.HasTypeDefinition && t.TypeDefinition.IsByRef

  let rec fsharpTypeToLowType (t: FSharpType) =
    if Hack.isMeasure t then
      None
    elif t.IsFunctionType then
      option {
        let! xs = toFlatArrow t
        return Arrow xs
      }
    elif t.IsGenericParameter then
      Some (Variable (VariableSource, t.GenericParameter.TypeVariable))
    elif Hack.isTupleType t then
      option {
        let! args = listLowType t.GenericArguments
        return Tuple { Elements = args; IsStruct = Hack.isStructTupleType t }
      }
    elif Hack.isFloat t then
      Some SpecialTypes.LowType.float
    elif isByRef t then
      option {
        let! x = fsharpTypeToLowType (Hack.genericArguments t |> List.head)
        return ByRef(false, x)
      }
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
        let! methodParameters =
          [
            for xs in invokeMethod.CurriedParameterGroups do
              for x in xs do
                yield x.Type
            yield invokeMethod.ReturnParameter.Type
          ]
          |> listLowType
        let! genericArguments = t.GenericArguments |> listLowType
        let replacements = Seq.zip (genericParameters td) genericArguments |> Map.ofSeq
        return LowType.applyVariableToTargetList VariableSource replacements methodParameters
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
  and listLowType (ts: FSharpType seq) : (LowType list) option = ts |> Seq.foldOptionMapping fsharpTypeToLowType |> Option.map Seq.toList
  and fsharpEntityToLowType (x: FSharpEntity) =
    let identity = x.Identity
    let args = x |> genericParameters |> List.map (fun v -> Variable (VariableSource, v))
    match args with
    | [] -> identity
    | xs -> Generic (identity, xs)

  let collectTypeConstraints (genericParamters: seq<FSharpGenericParameter>): TypeConstraint list =
    genericParamters
    |> Seq.collect (fun p ->
      let variable = p.TypeVariable
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
              
            let variables = data.MemberSources |> Seq.map (fun x -> x.GenericParameter.TypeVariable) |> Seq.toList
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

  let private (|Fs_option|_|) = function
    | Generic (Identity (FullIdentity { AssemblyName = "FSharp.Core"; Name = LoadingName ("FSharp.Core", "Microsoft.FSharp.Core.option`1", []); GenericParameterCount = 1 }), [ p ]) -> Some p
    | _ -> None
  let private (|Fs_Option|_|) = function
    | Generic (Identity (FullIdentity { AssemblyName = "FSharp.Core"; Name = LoadingName ("FSharp.Core", "Microsoft.FSharp.Core.FSharpOption`1", []); GenericParameterCount = 1 }), [ p ]) -> Some p
    | _ -> None
  let private (|IsOption|_|) = function
    | TypeAbbreviation { Abbreviation = Fs_option _; Original = Fs_Option p } -> Some p
    | Fs_Option p -> Some p
    | _ -> None
  let unwrapFsOptionalParameter = function
    | IsOption p -> p
    | p -> p

  let loadByRef (p: FSharpParameter) (t: LowType) =
    match t with
    | ByRef (_, t) -> ByRef (p.IsOutArg, t)
    | _ -> t

  let curriedParameterGroups isFSharp (t: FSharpMemberOrFunctionOrValue) =
    seq {
      for group in t.CurriedParameterGroups do
        if Seq.length group > 0 then
          let group = seq {
              for p in group do
                yield option {
                  let! t = fsharpTypeToLowType p.Type
                  let t =
                    if isFSharp && p.IsOptionalArg then
                      unwrapFsOptionalParameter t
                    else
                      t
                  let t = loadByRef p t
                  return { Name = p.Name; Type = t; IsOptional = p.IsOptionalArg }
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

  let toMemberName (name: DisplayNameItem) =
    match name.Name with
    | SymbolName n -> n
    | OperatorName (_, n) -> n
    | WithCompiledName (n, _) -> n

  let methodMember isFSharp (x: FSharpMemberOrFunctionOrValue) =
    option {
      let! parameters = curriedParameterGroups isFSharp x
      let parameters = complementUnitParameter x parameters
      let! returnType = fsharpTypeToLowType x.ReturnParameter.Type
      let returnParam = Parameter.ofLowType returnType
      let name = x.GetDisplayName
      let genericParams = x.GenericParametersAsTypeVariable
      let member' = { Name = toMemberName name; Kind = MemberKind.Method; GenericParameters = genericParams; Parameters = parameters; ReturnParameter = returnParam }
      return (name, member')
    }

  let propertyMember isFSharp (x: FSharpMemberOrFunctionOrValue) =
    option {
      let! parameters = curriedParameterGroups isFSharp x
      let! returnType = fsharpTypeToLowType x.ReturnParameter.Type
      let returnParam = Parameter.ofLowType returnType
      let name = x.GetDisplayName
      let genericParams = x.GenericParametersAsTypeVariable
      let memberKind = MemberKind.Property x.PropertyKind
      let member' = { Name = toMemberName name; Kind = memberKind; GenericParameters = genericParams; Parameters = parameters; ReturnParameter = returnParam }
      return (name, member')
    }

  let toModuleValue isFSharp xml (declaringModuleName: DisplayName) (x: FSharpMemberOrFunctionOrValue) =
    option {
      
      let! parameters = curriedParameterGroups isFSharp x
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
      let name = x.GetDisplayName :: declaringModuleName
      return { Name = DisplayName name; Signature = target; TypeConstraints = collectTypeConstraints x.GenericParameters; Document = tryGetXmlDoc xml x }
    }

  let toTypeExtension isFSharp xml (declaringModuleName: DisplayName) (x: FSharpMemberOrFunctionOrValue) =
    option {
      let! _, member' =
        if x.IsPropertyGetterMethod || x.IsPropertySetterMethod then
          None
        elif x.IsProperty then
          propertyMember isFSharp x
        else
          let existingTypeParameters = x.LogicalEnclosingEntity.GenericParameters |> Seq.map (fun x -> x.TypeVariable) |> Seq.toArray
          let removeExistingTypeParameters xs = xs |> List.filter (fun p -> existingTypeParameters |> Array.exists ((=)p) = false)
          methodMember isFSharp x
          |> Option.map (fun (n, m) -> (n, { m with GenericParameters = removeExistingTypeParameters m.GenericParameters }))

      let modifier = x.MemberModifier
      let existingType = fsharpEntityToLowType x.LogicalEnclosingEntity
      let signature = ApiSignature.TypeExtension { ExistingType = existingType; Declaration = declaringModuleName; MemberModifier = modifier; Member = member' }
      let name =
        let memberAssemblyName = x.LogicalEnclosingEntity.Assembly.SimpleName
        let memberTypeName = x.LogicalEnclosingEntity.FullName
        let memberName =
          let name = member'.Name
          { Name = SymbolName name; GenericParameters = [] }
        LoadingName (memberAssemblyName, memberTypeName, [ memberName ])
      return { Name = name; Signature = signature; TypeConstraints = collectTypeConstraints x.GenericParameters; Document = tryGetXmlDoc xml x }
    }

  let toFSharpApi isFSharp xml (declaringModuleName: DisplayName) (x: FSharpMemberOrFunctionOrValue) =
    if x.IsExtensionMember then
      toTypeExtension isFSharp xml declaringModuleName x
    else
      toModuleValue isFSharp xml declaringModuleName x

  let constructorSignature isFSharp xml (declaringSignatureName: DisplayName) (declaringSignature: LowType) (x: FSharpMemberOrFunctionOrValue) =
    let constructorName = "new"
    option {
      let! _, target = methodMember isFSharp x
      let target = { target with Name = constructorName; ReturnParameter = Parameter.ofLowType declaringSignature }
      let name = { Name = WithCompiledName (constructorName, compiledName x); GenericParameters = [] } :: declaringSignatureName
      return { Name = DisplayName name; Signature = ApiSignature.Constructor (declaringSignature, target); TypeConstraints = collectTypeConstraints x.GenericParameters; Document = tryGetXmlDoc xml x }
    }

  let memberSignature xml (loadMember: FSharpMemberOrFunctionOrValue -> (DisplayNameItem * Member) option) (declaringSignatureName: DisplayName) (declaringEntity: FSharpEntity) declaringSignature (x: FSharpMemberOrFunctionOrValue) =
    option {
      let! name, member' = loadMember x
      let name = name :: declaringSignatureName
      let typeConstraints = Seq.append declaringEntity.GenericParameters x.GenericParameters |> collectTypeConstraints
      return { Name = DisplayName name; Signature = x.TargetSignatureConstructor declaringSignature member'; TypeConstraints = typeConstraints; Document = tryGetXmlDoc xml x }
    }

  let isConstructor (x: FSharpMemberOrFunctionOrValue) = x.CompiledName = ".ctor"

  let toTypeMemberApi xml (declaringSignatureName: DisplayName) (declaringEntity: FSharpEntity) (declaringSignature: LowType) (x: FSharpMemberOrFunctionOrValue) =
    let isFSharp = declaringEntity.IsFSharp
    if isConstructor x then
      constructorSignature isFSharp xml declaringSignatureName declaringSignature x
    elif x.IsMethod then
      memberSignature xml (methodMember isFSharp) declaringSignatureName declaringEntity declaringSignature x
    elif x.IsProperty then
      memberSignature xml (propertyMember isFSharp) declaringSignatureName declaringEntity declaringSignature x
    else
      None

  let toFieldApi xml (accessPath: DisplayName) (declaringEntity: FSharpEntity) (declaringSignature: LowType) (field: FSharpField) =
    option {
      let! returnType = fsharpTypeToLowType field.FieldType
      let returnParam = Parameter.ofLowType returnType
      let member' = { Name = field.Name; Kind = MemberKind.Field; GenericParameters = []; Parameters = []; ReturnParameter = returnParam }
      let apiName =
        let name = field.Name
        { Name = SymbolName name; GenericParameters = [] } :: accessPath
      return { Name = DisplayName apiName; Signature = field.TargetSignatureConstructor declaringSignature member'; TypeConstraints = collectTypeConstraints declaringEntity.GenericParameters; Document = tryGetXmlDoc xml field }
    }

  let toUnionCaseField length (n, field: FSharpField) = option {
    let name =
      if length = 1 && field.Name = "Item" then None
      elif field.Name = sprintf "Item%d" n then None
      else Some field.Name
    let! t = fsharpTypeToLowType field.FieldType
    return ({ Name = name; Type = t } : UnionCaseField)
  }

  let toUnionCaseApi xml (accessPath: DisplayName) (declaringEntity: FSharpEntity) (declaringSignature: LowType) (unionCase: FSharpUnionCase) =
    option {
      let caseName = unionCase.Name
      let! fields = unionCase.UnionCaseFields |> Seq.mapi (fun i field -> (i + 1, field)) |> Seq.foldOptionMapping (toUnionCaseField unionCase.UnionCaseFields.Count)
      let returnType = declaringSignature
      let signature = { DeclaringType = returnType; Name = caseName; Fields = List.ofSeq fields } : UnionCase
      let apiName = { Name = SymbolName caseName; GenericParameters = [] } :: accessPath
      return { Name = DisplayName apiName; Signature = ApiSignature.UnionCase signature; TypeConstraints = collectTypeConstraints declaringEntity.GenericParameters; Document = tryGetXmlDoc xml unionCase }
    }

  let resolveConflictGenericArgumnet (replacementVariables: LowType list) (m: FSharpMemberOrFunctionOrValue) =
    m.GenericParameters
    |> Seq.choose (fun p ->
      let name = p.TypeVariable
      let isConflict = replacementVariables |> List.exists (function Variable (VariableSource.Target, n) -> n = name | _ -> false)
      if isConflict then
        let confrictVariable = name
        let newVariable = Variable (VariableSource, { confrictVariable with Name = confrictVariable.Name + "1" })
        Some (confrictVariable, newVariable)
      else None
    )
    |> Seq.toList

  let genericParametersAndArguments (t: FSharpType) =
    Seq.zip t.TypeDefinition.GenericParameters t.GenericArguments
    |> Seq.choose (fun (parameter, arg) -> option {
      let! s = fsharpTypeToLowType arg
      let v = parameter.TypeVariable
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
    option {
      let! abbreviatedAndOriginal = fsharpTypeToLowType e.AbbreviatedType
      let abbreviated, original =
        match abbreviatedAndOriginal with
        | TypeAbbreviation t -> t.Abbreviation, t.Original
        | original -> original, original

      let typeAbbreviationName =
        let name = e.DisplayName
        let compiledName = compiledName e
        let name = if name <> compiledName then WithCompiledName (name, compiledName) else SymbolName name
        { Name = name; GenericParameters = genericParameters e } :: accessPath
      let abbreviationDef: TypeAbbreviationDefinition = {
        Name = typeAbbreviationName
        FullName = e.TypeAbbreviationFullName
        AssemblyName = e.Assembly.SimpleName
        Accessibility = accessibility e
        GenericParameters = e.GenericParameters |> Seq.map (fun p -> p.TypeVariable) |> Seq.toList
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
        |> Seq.map (fun p -> p.TypeVariable)
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
        elif e.IsTuple  then
          let vs = e.GenericParameters |> Seq.map (fun p -> p.TypeVariable) |> Seq.toList
          let eq =
            match vs with
            | [] -> Satisfy
            | _ -> Dependence vs
          updateCache cache eq
        elif e.IsArrayType then
          let v = e.GenericParameters |> Seq.map (fun p -> p.TypeVariable) |> Seq.toList
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
        let vs = e.GenericParameters |> Seq.map (fun p -> p.TypeVariable) |> Seq.toList
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
              |> Seq.map (fun x -> x.TypeVariable)
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

  let typeDefKind (e: FSharpEntity) =
    if e.IsEnum then
      TypeDefinitionKind.Enumeration
    elif e.IsFSharpRecord then
      TypeDefinitionKind.Record
    elif e.IsFSharpUnion then
      TypeDefinitionKind.Union
    elif e.IsClass then
      TypeDefinitionKind.Class
    elif e.IsInterface then
      TypeDefinitionKind.Interface
    else
      TypeDefinitionKind.Type

  let fullTypeDef xml (name: DisplayName) (e: FSharpEntity) members =
    option {
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

      let identity = { e.LoadingFullIdentity with Name = DisplayName name }
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
        Kind = typeDefKind e
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
      let api = { Name = DisplayName name; Signature = ApiSignature.FullTypeDefinition typeDef; TypeConstraints = typeDef.TypeConstraints; Document = tryGetXmlDoc xml e }
      return (api, typeDef)
    }

  let moduleDef xml name (e: FSharpEntity) =
    let def: ModuleDefinition = { Name = name; AssemblyName = e.Assembly.SimpleName; Accessibility = accessibility e }
    { Name = DisplayName name; Signature = ApiSignature.ModuleDefinition def; TypeConstraints = []; Document = tryGetXmlDoc xml e }

  let tryExtractSyntaxes typeDef customOperations =
    let defaultSyntaxes = ComputationExpressionLoader.extractSyntaxes typeDef
    let customOperationSyntaxes = customOperations |> Seq.map fst |> Set.ofSeq
    let syntaxes = defaultSyntaxes + customOperationSyntaxes
    if Set.isEmpty syntaxes then
      None
    else
      Some syntaxes

  let computationExpression xml (typeDef: FullTypeDefinition) customOperations =
    option {
      let! syntaxes = tryExtractSyntaxes typeDef customOperations
      let ceTypes = ComputationExpressionLoader.extractTypes typeDef customOperations |> Seq.toList
      let apiSig = ApiSignature.ComputationExpressionBuilder { BuilderType = typeDef.LowType; ComputationExpressionTypes = ceTypes; Syntaxes = Set.toList syntaxes }
      return { Name = DisplayName typeDef.Name; Signature = apiSig; TypeConstraints = typeDef.TypeConstraints; Document = xml }
    }

  let isCustomOperation (x: FSharpMemberOrFunctionOrValue) =
    x.Attributes |> Seq.tryPick (fun attr ->
      if attr.AttributeType.TryFullName = Some "Microsoft.FSharp.Core.CustomOperationAttribute" then
        let (_, name) = attr.ConstructorArguments |> Seq.head
        tryUnbox<string> name
      else
        None)

  let rec collectApi xml (accessPath: DisplayName) (e: FSharpEntity): Api seq =
    seq {
      if e.IsNamespace then
        let accessPath = e.GetDisplayName() :: accessPath
        yield! collectFromNestedEntities xml accessPath e
      elif e.IsCompilerInternalModule then
        ()
      elif e.IsFSharpModule then
        yield! collectFromModule xml accessPath e
      elif e.IsFSharpAbbreviation && not e.IsMeasure then
        yield! collectTypeAbbreviationDefinition xml accessPath e
      elif e.IsClass || e.IsInterface || e.IsValueType || e.IsFSharpRecord || e.IsFSharpUnion || e.IsArrayType || e.IsDelegate then
        yield! collectFromType xml accessPath e
      elif e.IsOpaque || e.HasAssemblyCodeRepresentation then
        yield! collectFromType xml accessPath e
    }
  and collectFromNestedEntities xml (accessPath: DisplayName) (e: FSharpEntity): Api seq =
    e.NestedEntities
    |> Seq.collect (collectApi xml accessPath)
  and collectFromModule xml (accessPath: DisplayName) (e: FSharpEntity): Api seq =
    let moduleName = e.GetDisplayName() :: accessPath
    seq {
      yield moduleDef xml moduleName e
      yield! e.MembersFunctionsAndValues
              |> Seq.filter (fun x -> x.Accessibility.IsPublic)
              |> Seq.choose (toFSharpApi e.IsFSharp xml moduleName)
      yield! collectFromNestedEntities xml moduleName e
    }
  and collectFromType xml (accessPath: DisplayName) (e: FSharpEntity): Api seq =
    let typeName = e.GetDisplayName() :: accessPath

    seq {
      let declaringSignature = fsharpEntityToLowType e

      let membersAndCustomOperations =
        e.MembersFunctionsAndValues
        |> Seq.filter (fun x -> x.Accessibility.IsPublic && not x.IsCompilerGenerated)
        |> Seq.choose (fun x ->
          toTypeMemberApi xml typeName e declaringSignature x
          |> Option.map (fun m -> (m, isCustomOperation x)))
        |> Seq.cache

      let members = membersAndCustomOperations |> Seq.map fst

      let customOperations =
        membersAndCustomOperations
        |> Seq.choose (function
          | { Signature = ApiSignature.InstanceMember (_, m) }, Some name -> Some (name, m)
          | _ -> None)
        |> Seq.toList

      let fields =
        e.FSharpFields
        |> Seq.filter (fun x -> x.Accessibility.IsPublic && not x.IsCompilerGenerated)
        |> Seq.choose (toFieldApi xml typeName e declaringSignature)
        |> Seq.cache

      let unionCases =
        e.UnionCases
        |> Seq.filter (fun x -> x.Accessibility.IsPublic)
        |> Seq.choose (toUnionCaseApi xml typeName e declaringSignature)

      match fullTypeDef xml typeName e (Seq.append members fields) with
      | Some (typeDefApi, typeDef) ->
        yield typeDefApi
        yield! members
        yield! fields
        yield! unionCases

        yield! computationExpression typeDefApi.Document typeDef customOperations |> Option.toList
      | None -> ()
      yield! collectFromNestedEntities xml typeName e
    }

  let tryGetXml (assembly: FSharpAssembly) = option {
    let! assemblyFileName = assembly.FileName
    let xmlFileName = Path.ChangeExtension(assemblyFileName, "xml")
    if File.Exists(xmlFileName) then
      return XElement.Parse(File.ReadAllText(xmlFileName))
    else
      return! None
  }

  let typeDefsDict (xs: FullTypeDefinition seq) =
    let d =
      xs
      |> Seq.map (fun x -> x.FullIdentity, x)
      |> dict
    Dictionary(d, Identity.fullIdentityComparer)

  let makeDefAndAbb (api: ApiDictionary) =
    let types =
      api.Api
      |> Seq.choose (function { Signature = ApiSignature.FullTypeDefinition full } -> Some full | _ -> None)
      |> typeDefsDict
    let typeAbbreviations =
      api.Api
      |> Seq.choose (function { Signature = ApiSignature.TypeAbbreviation t } -> Some t | _ -> None)
      |> Seq.toArray
    { api with TypeDefinitions = types; TypeAbbreviations = typeAbbreviations }
      

  let load (assembly: FSharpAssembly): ApiDictionary =
    let xml = tryGetXml assembly |> Option.map createXmlDocCache
    let api =
      assembly.Contents.Entities
      |> Seq.collect (fun e ->
        let accessPath =
          match e.AccessPath with
          | "global" -> []
          | a -> DisplayName.ofString a
        collectApi xml accessPath e)
      |> Seq.toArray
    
    let apiDict = { AssemblyName = assembly.SimpleName; Api = api; TypeDefinitions = IDictionary.empty; TypeAbbreviations = Array.empty }
    makeDefAndAbb apiDict

  module NameResolve =
    type AssemblyCache = IDictionary<string, DisplayName>
    type NameCache = (string * AssemblyCache)[]

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module NameCache =
      let tryGetValue key (cache: NameCache) = cache |> Array.tryFind (fun (k, _) -> k = key) |> Option.map snd
      let getValue key (cache: NameCache) = cache |> Array.find (fun (k, _) -> k = key) |> snd

    type Context = {
      Cache: NameCache
      ForwardingLogs: IDictionary<string, TypeForward>
    }

    let tryGetValue key (dict: IDictionary<_, _>) =
      match dict.TryGetValue(key) with
      | true, value -> Some value
      | false, _ -> None

    let tryResolve_Name (name: Name) (assemblyCache: AssemblyCache) =
      match name with
      | LoadingName (_, accessPath, apiName) ->
        assemblyCache |> tryGetValue accessPath |> Option.map (fun accessPath -> DisplayName (apiName @ accessPath))
      | DisplayName _ as n -> Some n

    let typeForwarding (context: Context) fromAssemblyName (name: Name) =
      match name with
      | LoadingName (_, accessPath, _) ->
        context.Cache
        |> Seq.tryPick (fun (toAssemblyName, assemblyCache) ->
          let result = tryResolve_Name name assemblyCache
          result |> Option.iter (fun _ -> context.ForwardingLogs.[accessPath] <- { Type = accessPath; From = fromAssemblyName; To = toAssemblyName })
          result)
      | DisplayName _ as n -> Some n

    let resolve_Name context (name: Name) =
      match name with
      | LoadingName (assemblyName, accessPath, _) ->
        let resolved = NameCache.tryGetValue assemblyName context.Cache |> Option.bind (tryResolve_Name name)
        match resolved with
        | Some n -> n
        | None ->
          match typeForwarding context assemblyName name with
          | Some n -> n
          | None -> failwithf "%s(%s) is not found." accessPath assemblyName
      | DisplayName _ as n -> n
  
    let rec resolve_LowType context = function
      | Wildcard _ as w -> w
      | Variable _ as v -> v
      | Identity i -> Identity (resolve_Identity context i)
      | Arrow xs -> Arrow (List.map (resolve_LowType context) xs)
      | Tuple x -> Tuple { x with Elements = List.map (resolve_LowType context) x.Elements }
      | Generic (id, args) ->
        let id = resolve_LowType context id
        let args = List.map (resolve_LowType context) args
        Generic (id, args)
      | TypeAbbreviation a -> TypeAbbreviation { Abbreviation = resolve_LowType context a.Abbreviation; Original = resolve_LowType context a.Original }
      | Delegate (t, arrow) ->
        let t = resolve_LowType context t
        let arrow = List.map (resolve_LowType context) arrow
        Delegate (t, arrow)
      | ByRef (out, t) -> ByRef(out, resolve_LowType context t)
      | Flexible t -> Flexible(resolve_LowType context t)
      | Choice (xs) -> Choice (List.map (resolve_LowType context) xs)
    and resolve_Identity cache = function
      | PartialIdentity _ as i -> i
      | FullIdentity full -> FullIdentity { full with Name = resolve_Name cache full.Name }

    let resolve_Signature context apiSig = LowTypeVisitor.accept_ApiSignature (resolve_LowType context) apiSig
    let resolve_TypeConstraint context constraint' = LowTypeVisitor.accept_TypeConstraint (resolve_LowType context) constraint'

    let resolve_Api context (api: Api) =
      { api with
          Name = resolve_Name context api.Name
          Signature = resolve_Signature context api.Signature
          TypeConstraints = List.map (resolve_TypeConstraint context) api.TypeConstraints
      }

    let resolve_ApiDictionary cache (apiDic: ApiDictionary) =
      let context = {
        Cache = cache
        ForwardingLogs = Dictionary<_, _>() :> IDictionary<_, _>
      }
      let resolved = { apiDic with Api = Array.map (resolve_Api context) apiDic.Api }
      let forwardingLogs = context.ForwardingLogs.Values :> seq<_>
      (resolved, forwardingLogs)

    let resolveLoadingName (dictionaries: ApiDictionary[]) =
      let cache: NameCache =
        dictionaries
        |> Array.map (fun d ->
          let names =
            Seq.concat [
              d.TypeDefinitions |> Seq.map (fun (KeyValue(_, x)) -> x.FullName, x.Name)
              d.TypeAbbreviations |> Seq.map (fun x -> x.FullName, x.Name)
            ]
            |> dict
          (d.AssemblyName, names)
        )
      dictionaries |> Array.map (resolve_ApiDictionary cache)

  module AutoGenericResolve =
    let variables (name: Name) = name |> Name.toDisplayName |> List.collect (fun n -> n.GenericParameters) |> List.distinct

    let replaceVariables (table: Map<TypeVariable, TypeVariable>) (variables: TypeVariable list) =
      variables
      |> List.map (fun p ->
        match Map.tryFind p table with
        | Some r -> r
        | None -> p
      )

    let replaceName (table: Map<TypeVariable, TypeVariable>) (name: Name) =
      Name.toDisplayName name
      |> List.map (fun n ->
        let genericParams = n.GenericParameters |> replaceVariables table
        { n with GenericParameters = genericParams }
      )
      |> DisplayName

    let resolve_TypeConstraint variableTable lowTypeTable constraint' =
      let c = LowTypeVisitor.accept_TypeConstraint (LowType.applyVariable VariableSource lowTypeTable) constraint'
      { c with Variables = replaceVariables variableTable c.Variables }

    let resolve_ApiSignature table apiSig = LowTypeVisitor.accept_ApiSignature (LowType.applyVariable VariableSource table) apiSig

    let resolve_Api (api: Api) : Api =
      let variables = variables api.Name
      let autoGenericVariables = variables |> List.filter (fun x -> x.Name.StartsWith("?")) |> List.sortBy (fun x -> x.Name)
      if List.isEmpty autoGenericVariables then
        api
      else
        let replaceTable =
          let rec calcNewName i (n: int option) =
            let suffix, next =
              match n with
              | Some n -> string n, Some (n + 1)
              | None -> "", Some 0
            let newName = string ('a' + char i) + suffix
            if variables |> List.exists (fun v -> v.Name = newName) then
              calcNewName i next
            else
              newName
          autoGenericVariables
          |> Seq.mapi (fun i autoGeneric ->
            let newName = calcNewName i None
            let replacement = { autoGeneric with Name = newName }
            (autoGeneric, replacement)
          )
          |> Map.ofSeq
        let lowTypeReplaceTable = replaceTable |> Map.map (fun _ value -> Variable (VariableSource, value))
        { api with
            Name = replaceName replaceTable api.Name
            Signature = resolve_ApiSignature lowTypeReplaceTable api.Signature
            TypeConstraints = List.map (resolve_TypeConstraint replaceTable lowTypeReplaceTable) api.TypeConstraints
        }

    let resolveAutoGeneric (apiDict: ApiDictionary) =
      { apiDict with
          Api = Array.map resolve_Api apiDict.Api
      }

let loadWithLogs (assemblies: FSharpAssembly[]) =
  PSeq.map Impl.load assemblies
  |> PSeq.toArray
  |> Impl.NameResolve.resolveLoadingName
  |> Array.map (fun (apiDict, logs) ->
    let apiDict =
      apiDict
      |> Impl.AutoGenericResolve.resolveAutoGeneric
      |> Impl.makeDefAndAbb
    (apiDict, logs)
  )

let load (assemblies: FSharpAssembly[]): ApiDictionary[] = loadWithLogs assemblies |> Array.map fst

let databaseName = "database"

module internal Serialization =
  type T = (string * Api[])[]
  let toDumpObj (xs: ApiDictionary[]) : T = xs |> Array.map (fun x -> x.AssemblyName, x.Api)

  let fromDumpObj (xs: T) =
    xs
    |> Array.map (fun (name, apis) ->
      { AssemblyName = name; Api = apis; TypeDefinitions = IDictionary.empty; TypeAbbreviations = Array.empty }
      |> Impl.makeDefAndAbb
    )

let save (path: string) (dictionaries: ApiDictionary[]) : unit =
  use file = File.OpenWrite(path)
  let serializer = FsPickler.CreateBinarySerializer()
  serializer.Serialize<Serialization.T>(file, Serialization.toDumpObj dictionaries)

let loadFromFile (path: string) : ApiDictionary[] =
  use file = File.OpenRead(path)
  let serializer = FsPickler.CreateBinarySerializer()
  serializer.Deserialize<Serialization.T>(file) |> Serialization.fromDumpObj