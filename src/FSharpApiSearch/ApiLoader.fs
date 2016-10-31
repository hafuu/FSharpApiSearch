module FSharpApiSearch.ApiLoader

open Microsoft.FSharp.Compiler.SourceCodeServices
open FSharpApiSearch.SpecialTypes
open System.Text.RegularExpressions
open System.IO
open MBrace.FsPickler
open System.Collections.Generic
open System.Xml.Linq

module internal Impl =
  type VariableCache = IDictionary<string, string>

  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module VariableCache =
    let carete() = Dictionary<_, _>() :> VariableCache

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

  type FSharpGenericParameter with
    member this.IsAutoGeneric = this.Name.StartsWith("?")
    member this.TypeVariable(cache: VariableCache): TypeVariable =
      let name =
        if this.IsAutoGeneric then
          try cache.[this.Name] with ex -> raise (System.Exception(sprintf "%s is not found" this.Name, ex))
        else
          this.Name
      { Name = name; IsSolveAtCompileTime = this.IsSolveAtCompileTime }

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
    member this.TargetSignatureConstructor = fun (declaringType: Lazy<LowType>) member' ->
      if this.IsCSharpExtensionMember then
        ApiSignature.ExtensionMember member'
      elif this.IsStaticMember then
        ApiSignature.StaticMember (declaringType.Value, member')
      else
        ApiSignature.InstanceMember (declaringType.Value, member')
    member this.GenericParametersAsTypeVariable(cache: VariableCache) =
      this.GenericParameters |> Seq.map (fun x -> x.TypeVariable(cache)) |> Seq.toList
    member this.GetDisplayName(cache: VariableCache) =
      let dn = this.DisplayName
      let genericParameters = this.GenericParameters |> Seq.map (fun p -> p.TypeVariable(cache)) |> Seq.toList
      let isOperator =
        dn.StartsWith("(") && dn.EndsWith(")") && this.CompiledName.StartsWith("op_")
      if isOperator then
        { FSharpName = dn; InternalFSharpName = this.CompiledName; GenericParametersForDisplay = genericParameters }
      else
        { FSharpName = dn; InternalFSharpName = dn; GenericParametersForDisplay = genericParameters }

  type FSharpField with
    member this.TargetSignatureConstructor = fun (declaringType: Lazy<LowType>) member' ->
      if this.IsStatic then
        ApiSignature.StaticMember (declaringType.Value, member')
      else
        ApiSignature.InstanceMember (declaringType.Value, member')

  let genericParameters (cache: VariableCache) (e: FSharpEntity) =
    e.GenericParameters |> Seq.map (fun p -> p.TypeVariable(cache)) |> Seq.toList

  let accessibility (e: FSharpEntity) =
    let a = e.Accessibility
    if a.IsPublic then
      Public
    else
      Private

  let autoGenericVariableLen = "type '".Length

  let cacheVariableName (cache: VariableCache) (t: FSharpType) =
    let p = t.GenericParameter
    if p.IsAutoGeneric then
      let name = t.ToString().Substring(autoGenericVariableLen)
      cache.[t.GenericParameter.Name] <- name
    else
      ()

  let rec fsharpTypeToLowType (cache: VariableCache) (t: FSharpType) =
    if Hack.isMeasure t then
      None
    elif t.IsFunctionType then
      option {
        let! xs = toFlatArrow cache t
        return Arrow xs
      }
    elif t.IsGenericParameter then
      cacheVariableName cache t
      Some (Variable (VariableSource.Target, t.GenericParameter.TypeVariable(cache)))
    elif t.IsTupleType then
      option {
        let! args = listLowType cache t.GenericArguments
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
            let! xs = listLowType cache xs
            let! id = t.TryIdentity
            return Generic (id, xs)
          }
      if t.TypeDefinition.IsDelegate then
        option {
          let! arrow = delegateArrow cache t
          let! t = signature
          return Delegate (t, arrow)
        }
      elif Hack.isAbbreviation t then
        option {
          let! signature = signature
          let! original = abbreviationRoot cache t
          return TypeAbbreviation { Abbreviation = signature; Original = original }
        }
      else
        signature
    else
      None
  and delegateArrow (cache: VariableCache) (t: FSharpType) =
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
          |> listLowType cache
        let! genericArguments = t.GenericArguments |> listLowType cache
        let replacements = Seq.zip (genericParameters cache td) genericArguments |> Map.ofSeq
        return LowType.applyVariableToTargetList VariableSource.Target replacements methodParameters
      }
      return arrow
    }
  and abbreviationRoot (cache: VariableCache) (t: FSharpType) =
    if t.IsAbbreviation then
      abbreviationRoot cache t.AbbreviatedType
    elif Hack.isFloat t then
      Some SpecialTypes.LowType.Double
    elif t.IsFunctionType then
      option {
        let! xs = toFlatArrow cache t
        return Arrow xs
      }
    else
      fsharpTypeToLowType cache t
  and toFlatArrow (cache: VariableCache) (t: FSharpType): LowType list option =
    match Seq.toList t.GenericArguments with
    | [ x; y ] when y.IsFunctionType ->
      option {
        let! xSig = fsharpTypeToLowType cache x
        let! ySigs = toFlatArrow cache y
        return xSig :: ySigs
      }
    | [ x; y ] ->
      option {
        let! xSig = fsharpTypeToLowType cache x
        let! ySig = fsharpTypeToLowType cache y
        return [ xSig; ySig ]
      }
    | _ -> None
  and listLowType (cache: VariableCache) (ts: FSharpType seq) = ts |> Seq.foldOptionMapping (fsharpTypeToLowType cache) |> Option.map Seq.toList
  and fsharpEntityToLowType (cache: VariableCache) (x: FSharpEntity) =
    let identity = x.Identity
    let args = x |> genericParameters cache |> List.map (fun v -> Variable (VariableSource.Target, v))
    match args with
    | [] -> identity
    | xs -> Generic (identity, xs)

  let collectTypeConstraints (cache: VariableCache) (genericParamters: seq<FSharpGenericParameter>): TypeConstraint list =
    genericParamters
    |> Seq.collect (fun p ->
      let variable = p.TypeVariable(cache)
      p.Constraints
      |> Seq.choose (fun c -> 
        if c.IsCoercesToConstraint then
          option {
            let! parent = fsharpTypeToLowType cache c.CoercesToTarget
            return { Variables = [ variable ]; Constraint = SubtypeConstraints parent }
          }
        elif c.IsSupportsNullConstraint then
          Some { Variables = [ variable ]; Constraint = NullnessConstraints }
        elif c.IsMemberConstraint then
          option {
            let data = c.MemberConstraintData
            let modifier = if data.MemberIsStatic then MemberModifier.Static else MemberModifier.Instance
            let! returnType = fsharpTypeToLowType cache data.MemberReturnType
            let returnParam = Parameter.ofLowType returnType
            let! parameters = listLowType cache data.MemberArgumentTypes
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
              
            let variables = data.MemberSources |> Seq.map (fun x -> x.GenericParameter.TypeVariable(cache)) |> Seq.toList
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

  let parameterSignature (cache: VariableCache) (t: FSharpMemberOrFunctionOrValue) =
    let xs = [
      for group in t.CurriedParameterGroups do
        for parameter in group do
          yield parameter.Type
    ]
    listLowType cache xs

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

  let curriedParameterGroups (cache: VariableCache) isFSharp (t: FSharpMemberOrFunctionOrValue) =
    seq {
      for group in t.CurriedParameterGroups do
        if Seq.length group > 0 then
          let group = seq {
              for p in group do
                yield option {
                  let! t = fsharpTypeToLowType cache p.Type
                  let t =
                    if isFSharp && p.IsOptionalArg then
                      unwrapFsOptionalParameter t
                    else
                      t
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

  let methodMember (cache: VariableCache) isFSharp (x: FSharpMemberOrFunctionOrValue) =
    option {
      let! parameters = curriedParameterGroups cache isFSharp x
      let parameters = complementUnitParameter x parameters
      let! returnType = fsharpTypeToLowType cache x.ReturnParameter.Type
      let returnParam = Parameter.ofLowType returnType
      let name = x.GetDisplayName(cache)
      let genericParams = x.GenericParametersAsTypeVariable(cache)
      let member' = { Name = name.InternalFSharpName; Kind = MemberKind.Method; GenericParameters = genericParams; Parameters = parameters; ReturnParameter = returnParam }
      return (name, member')
    }

  let propertyMember (cache: VariableCache) isFSharp (x: FSharpMemberOrFunctionOrValue) =
    option {
      let! parameters = curriedParameterGroups cache isFSharp x
      let! returnType = fsharpTypeToLowType cache x.ReturnParameter.Type
      let returnParam = Parameter.ofLowType returnType
      let name = x.GetDisplayName(cache)
      let genericParams = x.GenericParametersAsTypeVariable(cache)
      let memberKind = MemberKind.Property x.PropertyKind
      let member' = { Name = name.InternalFSharpName; Kind = memberKind; GenericParameters = genericParams; Parameters = parameters; ReturnParameter = returnParam }
      return (name, member')
    }

  let toModuleValue (cache: VariableCache) isFSharp xml (declaringModuleName: Lazy<DisplayName>) (x: FSharpMemberOrFunctionOrValue) =
    option {
      
      let! parameters = curriedParameterGroups cache isFSharp x
      let! returnType = fsharpTypeToLowType cache x.ReturnParameter.Type
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
      let name = x.GetDisplayName(cache) :: declaringModuleName.Value
      return { Name = DisplayName name; Signature = target; TypeConstraints = collectTypeConstraints cache x.GenericParameters; Document = tryGetXmlDoc xml x }
    }

  let toTypeExtension (cache: VariableCache) isFSharp xml (declaringModuleName: Lazy<DisplayName>) (x: FSharpMemberOrFunctionOrValue) =
    option {
      let! _, member' =
        if x.IsPropertyGetterMethod || x.IsPropertySetterMethod then
          None
        elif x.IsProperty then
          propertyMember cache isFSharp x
        else
          let existingTypeParameters = x.LogicalEnclosingEntity.GenericParameters |> Seq.map (fun x -> x.TypeVariable(cache)) |> Seq.toArray
          let removeExistingTypeParameters xs = xs |> List.filter (fun p -> existingTypeParameters |> Array.exists ((=)p) = false)
          methodMember cache isFSharp x
          |> Option.map (fun (n, m) -> (n, { m with GenericParameters = removeExistingTypeParameters m.GenericParameters }))

      let modifier = x.MemberModifier
      let existingType = fsharpEntityToLowType cache x.LogicalEnclosingEntity
      let signature = ApiSignature.TypeExtension { ExistingType = existingType; Declaration = declaringModuleName.Value; MemberModifier = modifier; Member = member' }
      let name =
        let memberAssemblyName = x.LogicalEnclosingEntity.Assembly.SimpleName
        let memberTypeName = x.LogicalEnclosingEntity.FullName
        let memberName =
          let name = member'.Name
          { FSharpName = name; InternalFSharpName = name; GenericParametersForDisplay = [] }
        LoadingName (memberAssemblyName, memberTypeName, [ memberName ])
      return { Name = name; Signature = signature; TypeConstraints = collectTypeConstraints cache x.GenericParameters; Document = tryGetXmlDoc xml x }
    }

  let toFSharpApi (cache: VariableCache) isFSharp xml (declaringModuleName: Lazy<DisplayName>) (x: FSharpMemberOrFunctionOrValue) =
    if x.IsExtensionMember then
      toTypeExtension cache isFSharp xml declaringModuleName x
    else
      toModuleValue cache isFSharp xml declaringModuleName x

  let constructorSignature (cache: VariableCache) isFSharp xml (declaringSignatureName: Lazy<DisplayName>) (declaringSignature: Lazy<LowType>) (x: FSharpMemberOrFunctionOrValue) =
    let constructorName = "new"
    option {
      let! _, target = methodMember cache isFSharp x
      let target = { target with Name = constructorName; ReturnParameter = Parameter.ofLowType declaringSignature.Value }
      let name = { FSharpName = constructorName; InternalFSharpName = constructorName; GenericParametersForDisplay = [] } :: declaringSignatureName.Value
      return { Name = DisplayName name; Signature = ApiSignature.Constructor (declaringSignature.Value, target); TypeConstraints = collectTypeConstraints cache x.GenericParameters; Document = tryGetXmlDoc xml x }
    }

  let memberSignature (cache: VariableCache) xml (loadMember: FSharpMemberOrFunctionOrValue -> (NameItem * Member) option) (declaringSignatureName: Lazy<DisplayName>) (declaringEntity: FSharpEntity) declaringSignature (x: FSharpMemberOrFunctionOrValue) =
    option {
      let! name, member' = loadMember x
      let name = name :: declaringSignatureName.Value
      let typeConstraints = Seq.append declaringEntity.GenericParameters x.GenericParameters |> collectTypeConstraints cache
      return { Name = DisplayName name; Signature = x.TargetSignatureConstructor declaringSignature member'; TypeConstraints = typeConstraints; Document = tryGetXmlDoc xml x }
    }

  let toTypeMemberApi (cache: VariableCache) xml (declaringSignatureName: Lazy<DisplayName>) (declaringEntity: FSharpEntity) (declaringSignature: Lazy<LowType>) (x: FSharpMemberOrFunctionOrValue) =
    let isFSharp = declaringEntity.IsFSharp
    if x.IsConstructor then
      constructorSignature cache isFSharp xml declaringSignatureName declaringSignature x
    elif x.IsMethod then
      memberSignature cache xml (methodMember cache isFSharp) declaringSignatureName declaringEntity declaringSignature x
    elif x.IsProperty then
      memberSignature cache xml (propertyMember cache isFSharp) declaringSignatureName declaringEntity declaringSignature x
    else
      None

  let toFieldApi (cache: VariableCache) xml (accessPath: Lazy<DisplayName>) (declaringEntity: FSharpEntity) (declaringSignature: Lazy<LowType>) (field: FSharpField) =
    option {
      let! returnType = fsharpTypeToLowType cache field.FieldType
      let returnParam = Parameter.ofLowType returnType
      let member' = { Name = field.Name; Kind = MemberKind.Field; GenericParameters = []; Parameters = []; ReturnParameter = returnParam }
      let apiName =
        let name = field.Name
        { FSharpName = name; InternalFSharpName = name; GenericParametersForDisplay = [] } :: accessPath.Value
      return { Name = DisplayName apiName; Signature = field.TargetSignatureConstructor declaringSignature member'; TypeConstraints = collectTypeConstraints cache declaringEntity.GenericParameters; Document = tryGetXmlDoc xml field }
    }

  let toUnionCaseField (cache: VariableCache) length (n, field: FSharpField) = option {
    let name =
      if length = 1 && field.Name = "Item" then None
      elif field.Name = sprintf "Item%d" n then None
      else Some field.Name
    let! t = fsharpTypeToLowType cache field.FieldType
    return ({ Name = name; Type = t } : UnionCaseField)
  }

  let toUnionCaseApi (cache: VariableCache) xml (accessPath: Lazy<DisplayName>) (declaringEntity: FSharpEntity) (declaringSignature: Lazy<LowType>) (unionCase: FSharpUnionCase) =
    option {
      let caseName = unionCase.Name
      let! fields = unionCase.UnionCaseFields |> Seq.mapi (fun i field -> (i + 1, field)) |> Seq.foldOptionMapping (toUnionCaseField cache unionCase.UnionCaseFields.Count)
      let returnType = declaringSignature.Value
      let signature = { DeclaringType = returnType; Name = caseName; Fields = List.ofSeq fields } : UnionCase
      let apiName = { FSharpName = caseName; InternalFSharpName = caseName; GenericParametersForDisplay = [] } :: accessPath.Value
      return { Name = DisplayName apiName; Signature = ApiSignature.UnionCase signature; TypeConstraints = collectTypeConstraints cache declaringEntity.GenericParameters; Document = tryGetXmlDoc xml unionCase }
    }

  let resolveConflictGenericArgumnet (cache: VariableCache) (replacementVariables: LowType list) (m: FSharpMemberOrFunctionOrValue) =
    m.GenericParameters
    |> Seq.choose (fun p ->
      let name = p.TypeVariable(cache)
      let isConflict = replacementVariables |> List.exists (function Variable (VariableSource.Target, n) -> n = name | _ -> false)
      if isConflict then
        let confrictVariable = name
        let newVariable = Variable (VariableSource.Target, { confrictVariable with Name = confrictVariable.Name + "1" })
        Some (confrictVariable, newVariable)
      else None
    )
    |> Seq.toList

  let genericParametersAndArguments (cache: VariableCache) (t: FSharpType) =
    Seq.zip t.TypeDefinition.GenericParameters t.GenericArguments
    |> Seq.choose (fun (parameter, arg) -> option {
      let! s = fsharpTypeToLowType cache arg
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

  let collectTypeAbbreviationDefinition (cache: VariableCache) xml (accessPath: Lazy<DisplayName>) (e: FSharpEntity): Api seq =
    option {
      let! abbreviatedAndOriginal = fsharpTypeToLowType cache e.AbbreviatedType
      let abbreviated, original =
        match abbreviatedAndOriginal with
        | TypeAbbreviation t -> t.Abbreviation, t.Original
        | original -> original, original

      let typeAbbreviationName =
        let name = e.DisplayName
        { FSharpName = name; InternalFSharpName = name; GenericParametersForDisplay = genericParameters cache e } :: accessPath.Value
      let abbreviationDef: TypeAbbreviationDefinition = {
        Name = typeAbbreviationName
        FullName = e.TypeAbbreviationFullName
        AssemblyName = e.Assembly.SimpleName
        Accessibility = accessibility e
        GenericParameters = e.GenericParameters |> Seq.map (fun p -> p.TypeVariable(cache)) |> Seq.toList
        Abbreviated = abbreviated
        Original = original
      }
      let target = ApiSignature.TypeAbbreviation abbreviationDef
      return { Name = DisplayName typeAbbreviationName; Signature = target; TypeConstraints = collectTypeConstraints cache e.GenericParameters; Document = tryGetXmlDoc xml e }
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

  let loadEqualityAndComparison (variableCache: VariableCache) builder (e: FSharpEntity) =
    let loadConditional (e: FSharpEntity) =
      let vs =
        e.GenericParameters
        |> Seq.filter (fun x ->
          x.Attributes
          |> Seq.exists (fun attr -> attr.AttributeType.FullName = builder.ConditionalAttrName)
        )
        |> Seq.map (fun p -> p.TypeVariable(variableCache))
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
          let vs = e.GenericParameters |> Seq.map (fun p -> p.TypeVariable(variableCache)) |> Seq.toList
          let eq =
            match vs with
            | [] -> Satisfy
            | _ -> Dependence vs
          updateCache cache eq
        elif e.IsArrayType then
          let v = e.GenericParameters |> Seq.map (fun p -> p.TypeVariable(variableCache)) |> Seq.toList
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
        let vs = e.GenericParameters |> Seq.map (fun p -> p.TypeVariable(variableCache)) |> Seq.toList
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
              |> Seq.map (fun x -> x.TypeVariable(variableCache))
              |> Seq.zip root.GenericArguments
              |> Seq.choose (fun (t, v) -> if Seq.exists ((=)v) dependenceVariables then Some t else None)
            foldFsharpType cache testArgs

    load Map.empty e

  let equality (cache: VariableCache) (e: FSharpEntity) =
    let builder = {
      ConditionalAttrName = typeof<EqualityConditionalOnAttribute>.FullName
      CustomAttrName = typeof<CustomEqualityAttribute>.FullName
      NoAttrName = typeof<NoEqualityAttribute>.FullName
      SatisfyTypes = []
      ExpectedInterfaces = []
    }
    loadEqualityAndComparison cache builder e

  let comparison (cache: VariableCache) (e: FSharpEntity) =
    let builder = {
      ConditionalAttrName = typeof<ComparisonConditionalOnAttribute>.FullName
      CustomAttrName = typeof<CustomComparisonAttribute>.FullName
      NoAttrName = typeof<NoComparisonAttribute>.FullName
      SatisfyTypes = [ typeof<System.IntPtr>.FullName; typeof<System.UIntPtr>.FullName ]
      ExpectedInterfaces = [ typeof<System.IComparable>.FullName; typeof<System.Collections.IStructuralComparable>.FullName ]
    }
    loadEqualityAndComparison cache builder e

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

  let fullTypeDef (cache: VariableCache) xml (name: Lazy<DisplayName>) (e: FSharpEntity) members =
    option {
      let baseType =
        if not e.IsInterface then
          e.BaseType |> Option.bind (fsharpTypeToLowType cache)
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

      let identity = { e.LoadingFullIdentity with Name = DisplayName name.Value }
      let implicitInstanceMembers, implicitStaticMembers = CompilerOptimization.implicitMembers identity

      let fullName =
        match e.TryFullName with
        | Some fullName -> fullName
        | None -> e.AccessPath + "." + e.CompiledName

      let typeDef = {
        Name = name.Value
        FullName = fullName
        AssemblyName = identity.AssemblyName
        Accessibility = accessibility e
        Kind = typeDefKind e
        BaseType = baseType
        AllInterfaces = e.DeclaredInterfaces |> Seq.filter (fun x -> x.TypeDefinition.Accessibility.IsPublic) |> Seq.choose (fsharpTypeToLowType cache) |> Seq.toList
        GenericParameters = genericParameters cache e
        TypeConstraints = e.GenericParameters |> collectTypeConstraints cache
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
        Equality = equality cache e |> snd
        Comparison = comparison cache e |> snd
      }
      let api = { Name = DisplayName name.Value; Signature = ApiSignature.FullTypeDefinition typeDef; TypeConstraints = typeDef.TypeConstraints; Document = tryGetXmlDoc xml e }
      return (api, typeDef)
    }

  let moduleDef xml (name: Lazy<_>) (e: FSharpEntity) =
    let def: ModuleDefinition = { Name = name.Value; Accessibility = accessibility e }
    { Name = DisplayName name.Value; Signature = ApiSignature.ModuleDefinition def; TypeConstraints = []; Document = tryGetXmlDoc xml e }

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

  let rec collectApi (cache: VariableCache) xml (accessPath: Lazy<DisplayName>) (e: FSharpEntity): Api seq =
    seq {
      if e.IsNamespace then
        let accessPath = Lazy.Create (fun () ->
          let name = e.DisplayName
          { FSharpName = name; InternalFSharpName = name; GenericParametersForDisplay = [] } :: accessPath.Value
        )
        yield! collectFromNestedEntities cache xml accessPath e
      elif e.IsCompilerInternalModule then
        ()
      elif e.IsFSharpModule then
        yield! collectFromModule cache xml accessPath e
      elif e.IsFSharpAbbreviation && not e.IsMeasure then
        yield! collectTypeAbbreviationDefinition cache xml accessPath e
      elif e.IsClass || e.IsInterface || e.IsValueType || e.IsFSharpRecord || e.IsFSharpUnion || e.IsArrayType || e.IsDelegate then
        yield! collectFromType cache xml accessPath e
      elif e.IsOpaque || e.HasAssemblyCodeRepresentation then
        yield! collectFromType cache xml accessPath e
    }
  and collectFromNestedEntities  (cache: VariableCache) xml (accessPath: Lazy<DisplayName>) (e: FSharpEntity): Api seq =
    e.NestedEntities
    |> Seq.collect (collectApi cache xml accessPath)
  and collectFromModule (cache: VariableCache) xml (accessPath: Lazy<DisplayName>) (e: FSharpEntity): Api seq =
    let moduleName = Lazy.Create (fun () ->
      let name = e.DisplayName
      { FSharpName = name; InternalFSharpName = name; GenericParametersForDisplay = [] } :: accessPath.Value
    )
    seq {
      yield moduleDef xml moduleName e
      yield! e.MembersFunctionsAndValues
              |> Seq.filter (fun x -> x.Accessibility.IsPublic)
              |> Seq.choose (toFSharpApi cache e.IsFSharp xml moduleName)
      yield! collectFromNestedEntities cache xml moduleName e
    }
  and collectFromType (cache: VariableCache) xml (accessPath: Lazy<DisplayName>) (e: FSharpEntity): Api seq =
    let typeName = Lazy.Create (fun () ->
      let name = e.DisplayName
      { FSharpName = name; InternalFSharpName = name; GenericParametersForDisplay = genericParameters cache e } :: accessPath.Value
    )

    seq {
      let declaringSignature = lazy(fsharpEntityToLowType cache e)

      let membersAndCustomOperations =
        e.MembersFunctionsAndValues
        |> Seq.filter (fun x -> x.Accessibility.IsPublic && not x.IsCompilerGenerated)
        |> Seq.choose (fun x ->
          toTypeMemberApi cache xml typeName e declaringSignature x
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
        |> Seq.choose (toFieldApi cache xml typeName e declaringSignature)
        |> Seq.cache

      let unionCases =
        e.UnionCases
        |> Seq.filter (fun x -> x.Accessibility.IsPublic)
        |> Seq.choose (toUnionCaseApi cache xml typeName e declaringSignature)
        |> Seq.cache

      match fullTypeDef cache xml typeName e (Seq.append members fields) with
      | Some (typeDefApi, typeDef) ->
        yield typeDefApi
        yield! members
        yield! fields
        yield! unionCases

        yield! computationExpression typeDefApi.Document typeDef customOperations |> Option.toList
      | None -> ()
      yield! collectFromNestedEntities cache xml typeName e
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
    let cache = VariableCache.carete()
    let api =
      assembly.Contents.Entities
      |> Seq.collect (fun e ->
        let accessPath =
          match e.AccessPath with
          | "global" -> []
          | a -> DisplayName.ofString a
        collectApi cache xml (lazy accessPath) e)
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
    type AssemblyCache = IDictionary<string, DisplayName>
    type NameCache = IDictionary<string, AssemblyCache>

    let tryGetValue key (dict: IDictionary<_, _>) =
      match dict.TryGetValue(key) with
      | true, value -> Some value
      | false, _ -> None

    let tryResolve_Name (name: Name) (assemblyCache: AssemblyCache) =
      match name with
      | LoadingName (_, accessPath, apiName) ->
        // TODO: Output warning log
        assemblyCache |> tryGetValue accessPath |> Option.map (fun accessPath -> DisplayName (apiName @ accessPath))
      | DisplayName _ as n -> Some n

    let tryResolve_Name' (cache: NameCache) (name: Name) =
      match name with
      | LoadingName _ ->
        cache.Values |> Seq.tryPick (tryResolve_Name name)
      | DisplayName _ as n -> Some n

    let resolve_Name (cache: NameCache) (name: Name) =
      match name with
      | LoadingName (assemblyName, accessPath, _) ->
        let resolved = tryGetValue assemblyName cache |> Option.bind (tryResolve_Name name)
        match resolved with
        | Some n -> n
        | None ->
          match tryResolve_Name' cache name with
          | Some n -> n
          | None -> failwithf "%s(%s) is not found." accessPath assemblyName
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
      | Choice (xs) -> Choice (List.map (resolve_LowType cache) xs)
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

    let resolve_UnionCaseField cache (field: UnionCaseField) = { field with Type = resolve_LowType cache field.Type }

    let resolve_UnionCase cache (uc: UnionCase) =
      { uc with
          DeclaringType = resolve_LowType cache uc.DeclaringType
          Fields = List.map (resolve_UnionCaseField cache) uc.Fields
      }

    let resolve_ComputationExpressionBuilder cache (builder: ComputationExpressionBuilder) =
      { builder with
          BuilderType = resolve_LowType cache builder.BuilderType
          ComputationExpressionTypes = List.map (resolve_LowType cache) builder.ComputationExpressionTypes
      }
      
    let resolve_Signature cache = function
      | ApiSignature.ModuleValue x -> ApiSignature.ModuleValue (resolve_LowType cache x)
      | ApiSignature.ModuleFunction fn -> ApiSignature.ModuleFunction (resolve_Function cache fn)
      | ApiSignature.ActivePatten (kind, fn) -> ApiSignature.ActivePatten (kind, resolve_Function cache fn)
      | ApiSignature.InstanceMember (d, m) -> ApiSignature.InstanceMember (resolve_LowType cache d, resolve_Member cache m)
      | ApiSignature.StaticMember (d, m) -> ApiSignature.StaticMember (resolve_LowType cache d, resolve_Member cache m)
      | ApiSignature.Constructor (d, m) -> ApiSignature.Constructor (resolve_LowType cache d, resolve_Member cache m)
      | ApiSignature.ModuleDefinition m -> ApiSignature.ModuleDefinition m
      | ApiSignature.FullTypeDefinition f -> ApiSignature.FullTypeDefinition (resolve_FullTypeDefinition cache f)
      | ApiSignature.TypeAbbreviation a -> ApiSignature.TypeAbbreviation (resolve_TypeAbbreviationDefinition cache a)
      | ApiSignature.TypeExtension e -> ApiSignature.TypeExtension (resolve_TypeExtension cache e)
      | ApiSignature.ExtensionMember m -> ApiSignature.ExtensionMember (resolve_Member cache m)
      | ApiSignature.UnionCase uc -> ApiSignature.UnionCase (resolve_UnionCase cache uc)
      | ApiSignature.ComputationExpressionBuilder b -> ApiSignature.ComputationExpressionBuilder (resolve_ComputationExpressionBuilder cache b)

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