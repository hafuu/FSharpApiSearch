module TestHelper

open FSharpApiSearch
open FSharpApiSearch.Printer

let skipAll xs = xs |> Seq.map (Persimmon.Syntax.skip "skip")

let defaultTestOptions =
  SearchOptions.defaultOptions
  |> SearchOptions.SwapOrderDepth.Set 0
  |> SearchOptions.ComplementDepth.Set 0
  |> SearchOptions.ShortLetterAsVariable.Set 0
  |> SearchOptions.Parallel.Set Disabled

module DSL =

  let createType' name args =
    let id = ConcreteType { AssemblyName = "test"; Name = name; }
    match args with
    | [] -> Identifier.create id
    | args -> Generic.create (Identifier.create id, args)

  let createType name args = createType' (Name.ofString name) args

  let updateAssembly name = function
    | Identifier (ConcreteType id, pos) -> Identifier (ConcreteType { id with AssemblyName = name }, pos)
    | Generic (Identifier (ConcreteType id, identPos), args, genPos) -> Generic (Identifier (ConcreteType { id with AssemblyName = name }, identPos), args, genPos)
    | other -> other

  let typeAbbreviation abbreviated abbreviation = TypeAbbreviation.create { Abbreviation = abbreviation; Original = abbreviated; }

  let tv v = TypeVariable.ofString v
  let tv' vs = List.map tv vs

  let userInput name = Identifier.create (UserInputType { Name = Name.ofString name })
  let variable name = Variable.create (VariableSource.Target, tv name)
  let queryVariable name = Variable.create (VariableSource.Query, tv name)

  let wildcard = Wildcard.create None
  let wildcardGroup name = Wildcard.create (Some name)

  let generic id args =
    match id with
    | Identifier (UserInputType id, _) ->
      let parameterCount = List.length args
      let name =
        match id.Name with
        | [] -> []
        | n :: tail -> { n with GenericParameters = List.init parameterCount (fun n -> { Name = sprintf "T%d" n; IsSolveAtCompileTime = false }) } :: tail
      let id = { id with Name = name }
      Generic.create (Identifier.create (UserInputType id), args)
    | _ -> Generic.create (id, args)

  let arrow xs = Arrow.create (Arrow.ofLowTypeList xs)

  let delegate' t xs = Delegate.create (t, Arrow.ofLowTypeList xs)

  let byref t = ByRef.create (false, t)
  let out t = ByRef.create (true, t)

  let subtype t = Subtype.create t

  let pos n t = LowType.setPosition (fun _ -> Position.AtSignature (SignatureId n)) t

  let ptype t (x: Parameter) = { x with Type = t }
  let pname n (x: Parameter) = { x with Name = Some n }
  let popt (x: Parameter) = { x with IsOptional = true }
  let pparams (x: Parameter) = { x with IsParamArray = true }

  let private defaultParameterValue = { Name = None; Type = SpecialTypes.LowType.unit; IsOptional = false; IsParamArray = false }

  let createParameterGroups xs : ParameterGroups = xs |> List.map (List.map (fun f -> f defaultParameterValue))
    
  let createFunction fn : Function =
    let parameters = List.take (List.length fn - 1) fn |> createParameterGroups
    let ret =
      let f = fn |> List.last |> List.last
      f defaultParameterValue
    parameters, ret

  let member' name kind parameters returnType =
    { Name = name; Kind = kind; GenericParameters = []; Parameters = (createParameterGroups parameters); ReturnParameter = Parameter.ofLowType returnType }

  let property' name kind parameters returnType = member' name (MemberKind.Property kind) parameters returnType
  let method' name parameters returnType = member' name MemberKind.Method parameters returnType
  let field name returnType = member' name MemberKind.Field [] returnType

  let choice original xs = Choice.create (original, xs)

  let private memberGenericParameters (declaring: LowType) (member': Member) =
    let toTypeVariable = function Variable (_, v, _) -> v | _ -> failwith "it is not variable."
    let declaringVariables = LowType.collectVariables declaring |> Array.map toTypeVariable |> Set.ofArray
    let memberVariables =
      [
        yield member'.ReturnParameter.Type

        for group in member'.Parameters do
          for p in group do
            yield p.Type
      ]
      |> Seq.collect LowType.collectVariables |> Seq.map toTypeVariable |> Seq.distinct |> Set.ofSeq
    (memberVariables - declaringVariables) |> Set.toList

  let moduleFunction' fn = ApiSignature.ModuleFunction (createFunction fn)
  let moduleValue x = ApiSignature.ModuleValue x
  let instanceMember receiver member' =
    let genericParams = memberGenericParameters receiver member'
    let m = { member' with GenericParameters = genericParams }
    ApiSignature.InstanceMember (receiver, m)
  let staticMember declaringSignature member' =
    let genericParams = memberGenericParameters declaringSignature member'
    let m = { member' with GenericParameters = genericParams }
    ApiSignature.StaticMember (declaringSignature, m)
  let constructor' declaringSignature member' =
    let genericParams = memberGenericParameters declaringSignature member'
    let m = { member' with GenericParameters = genericParams }
    ApiSignature.Constructor (declaringSignature, m)

  let activePattern xs = ApiSignature.ActivePatten (ActivePatternKind.ActivePattern, createFunction xs)
  let partialActivePattern xs = ApiSignature.ActivePatten (ActivePatternKind.PartialActivePattern, createFunction xs)

  let typeExtension existingType declaration modifier member' =
    let genericParams = memberGenericParameters existingType member'
    let m = { member' with GenericParameters = genericParams }
    ApiSignature.TypeExtension { ExistingType = existingType; Declaration = declaration; MemberModifier = modifier; Member = m }
  let extensionMember member' = ApiSignature.ExtensionMember member'

  let unionCase declaration name fields =
    let fields = fields |> List.map (fun (name, t) -> { Name = name; Type = t } : UnionCaseField)
    let uc = { DeclaringType = declaration; Name = name; Fields = fields } : UnionCase
    ApiSignature.UnionCase uc

  let module' name assemblyName accessibility = ApiSignature.ModuleDefinition { Name = name; AssemblyName = assemblyName; Accessibility = accessibility }

  let constraint' vs c = { Variables = List.map tv vs; Constraint = c }

  let typeAbbreviationApi def = ApiSignature.TypeAbbreviation def

  let api name apiSig = { Name = ApiName name; Signature = apiSig; TypeConstraints = []; Document = None }

  let arrayType = "Microsoft.FSharp.Core.[]<'T>"
  let array2DType = "Microsoft.FSharp.Core.[,]<'T>"
  let array t = createType arrayType [ t ]
  let array2D t = createType array2DType [ t ]

  let queryArray t = generic (userInput "[]<'T>") [ t ]
  let queryArray2D t = generic (userInput "[,]<'T>") [ t ]

  let tuple xs = Tuple.create { Elements = xs; IsStruct = false }
  let structTuple xs = Tuple.create { Elements = xs; IsStruct = true }

  let typeAbbreviationDef name original =
    let defName = Name.ofString name
    let fullName =
      let toFullName (x: NameItem) =
        let genericSuffix =
          match x.GenericParameters with
          | [] -> ""
          | args -> "`" + string args.Length
        let name = FSharpFormat.toDisplayName x.Name
        name + genericSuffix
      defName |> List.rev |> List.map toFullName |> String.concat "."
    { Name = defName; FullName = fullName; AssemblyName = "test"; Accessibility = Public; GenericParameters = defName.Head.GenericParameters; Abbreviated = original; Original = original }

  let Compare = NameMatchMethod.StringCompare
  let Regex = NameMatchMethod.Regex
  let StartsWith = NameMatchMethod.StartsWith
  let EndsWith = NameMatchMethod.EndsWith
  let Contains = NameMatchMethod.Contains
  let Any = NameMatchMethod.Any

  let byGenericName expected genericParameters method = { Expected = expected; GenericParameters = genericParameters; MatchMethod = method }
  let byName expected method = byGenericName expected [] method
  

open DSL

type FullTypeDefinition with
  member this.AsPublic = { this with Accessibility = Public }
  member this.AsPrivate = { this with Accessibility = Private }

type TypeAbbreviationDefinition with
  member this.AsPublic = { this with Accessibility = Public }
  member this.AsPrivate = { this with Accessibility = Private }

let fsharpAbbreviationTable: TypeAbbreviationDefinition[] = [|
    typeAbbreviationDef "Microsoft.FSharp.Core.int" (createType "System.Int32" [])
    typeAbbreviationDef "Microsoft.FSharp.Core.float" (createType "System.Double" [])
    typeAbbreviationDef "Microsoft.FSharp.Core.single" (createType "System.Single" [])
    typeAbbreviationDef "Microsoft.FSharp.Core.string" (createType "System.String" [])
    typeAbbreviationDef "Microsoft.FSharp.Core.unit" SpecialTypes.LowType.Unit
    typeAbbreviationDef "Microsoft.FSharp.Collections.list<'a>" (createType "Microsoft.FSharp.Collections.List<'a>" [ variable "'a" ])
    typeAbbreviationDef "Microsoft.FSharp.Core.option<'a>" (createType "Microsoft.FSharp.Core.Option<'a>" [ variable "'a" ])
  |]

module Types =
  let mscorlib = "mscorlib"
  let fscore = "FSharp.Core"

  let object' = createType "System.Object" [] |> updateAssembly mscorlib
  let obj =
    let obj = createType "Microsoft.FSharp.Core.obj" [] |> updateAssembly fscore
    typeAbbreviation object' obj

  let int32 = createType "System.Int32" [] |> updateAssembly mscorlib
  let int =
    let int = createType "Microsoft.FSharp.Core.int" [] |> updateAssembly fscore
    typeAbbreviation int32 int

  let double = createType "System.Double" [] |> updateAssembly mscorlib
  let float =
    let float = createType "Microsoft.FSharp.Core.float" [] |> updateAssembly fscore
    typeAbbreviation double float

  let Unit = createType "Microsoft.FSharp.Core.Unit" [] |> updateAssembly fscore

  let unit =
    let unit = createType "Microsoft.FSharp.Core.unit" [] |> updateAssembly fscore
    typeAbbreviation Unit unit

  let bool =
    let Boolean = createType "System.Boolean" [] |> updateAssembly mscorlib
    let bool = createType "Microsoft.FSharp.Core.bool" [] |> updateAssembly fscore
    typeAbbreviation Boolean bool

  let ienumerable t = createType "System.Collections.Generic.IEnumerable<'T>" [ t ] |> updateAssembly mscorlib
  let seq t =
    let seq = createType "Microsoft.FSharp.Collections.seq<'T>" [ t ] |> updateAssembly fscore
    typeAbbreviation (ienumerable t) seq

  let fsharpList t =
    let name = "Microsoft.FSharp.Collections.List<'T>"
    let compiledName = "Microsoft.FSharp.Collections.FSharpList<'T>"
    createType' (Name.ofCompiledName name compiledName) [ t] |> updateAssembly fscore
  let list t =
    let list = createType "Microsoft.FSharp.Collections.list<'T>" [ t ] |> updateAssembly fscore
    typeAbbreviation (fsharpList t) list

  let fsharpOption t =
    let name = "Microsoft.FSharp.Core.Option<'T>"
    let compiledName = "Microsoft.FSharp.Core.FSharpOption<'T>"
    createType' (Name.ofCompiledName name compiledName) [ t ] |> updateAssembly fscore
  let option t =
    let opt = createType "Microsoft.FSharp.Core.option<'T>" [ t ] |> updateAssembly fscore
    typeAbbreviation (fsharpOption t) opt

  let string =
    let String = createType "System.String" [] |> updateAssembly mscorlib
    let string = createType "Microsoft.FSharp.Core.string" [] |> updateAssembly fscore
    typeAbbreviation String string

  let map k v =
    let name = "Microsoft.FSharp.Collections.Map<'Key, 'Value>"
    let compiledName = "Microsoft.FSharp.Collections.FSharpMap<'Key, 'Value>"
    createType' (Name.ofCompiledName name compiledName) [ k; v ] |> updateAssembly fscore

  let array = array >> updateAssembly fscore
  let array2D = array2D >> updateAssembly fscore

  let istructualEquatable = createType "System.Collections.IStructuralEquatable" [] |> updateAssembly mscorlib
  let iequatable x = createType "System.IEquatable<'t>" [ x ] |> updateAssembly mscorlib
  let genericIComparable x = createType "System.IComparable<'t>" [ x ] |> updateAssembly mscorlib
  let icomparable = createType "System.IComparable" [] |> updateAssembly mscorlib
  let istructuralComparable = createType "System.IStructuralComparable" [] |> updateAssembly mscorlib

  let valuetype = createType "System.ValueType" [] |> updateAssembly mscorlib