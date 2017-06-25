module TestHelper

open FSharpApiSearch
open FSharpApiSearch.Printer

let skipAll xs = xs |> Seq.map (Persimmon.Syntax.skip "skip")

let defaultTestOptions =
  SearchOptions.defaultOptions
  |> SearchOptions.SwapOrderDepth.Set 0
  |> SearchOptions.ComplementDepth.Set 0
  |> SearchOptions.Parallel.Set Disabled

module DSL =

  let createType' name args =
    let id = FullIdentity { AssemblyName = "test"; Name = name; GenericParameterCount = List.length args }
    match args with
    | [] -> Identity id
    | args -> Generic (Identity id, args)

  let createType name args = createType' (Name.ofString name) args

  let rec updateAssembly name = function
    | Identity (FullIdentity id) -> Identity (FullIdentity { id with AssemblyName = name })
    | Generic (Identity (FullIdentity id), args) -> Generic (Identity (FullIdentity { id with AssemblyName = name }), args)
    | other -> other

  let typeAbbreviation abbreviated abbreviation = TypeAbbreviation { Abbreviation = abbreviation; Original = abbreviated; }

  let tv v = TypeVariable.ofString v
  let tv' vs = List.map tv vs

  let identity name = Identity (PartialIdentity { Name = DisplayName.ofString name; GenericParameterCount = 0 })
  let variable name = Variable (VariableSource.Target, tv name)
  let queryVariable name = Variable (VariableSource.Query, tv name)

  let wildcard = Wildcard None
  let wildcardGroup name = Wildcard (Some name)

  let generic id args =
    match id with
    | Identity (PartialIdentity id) ->
      let parameterCount = List.length args
      let name =
        match id.Name with
        | [] -> []
        | n :: tail -> { n with GenericParameters = List.init parameterCount (fun n -> { Name = sprintf "T%d" n; IsSolveAtCompileTime = false }) } :: tail
      let id = { id with Name = name; GenericParameterCount = parameterCount }
      Generic (Identity (PartialIdentity id), args)
    | _ -> Generic (id, args)

  let arrow xs = Arrow (Arrow.ofLowTypeList xs)

  let delegate' t xs = Delegate (t, Arrow.ofLowTypeList xs)

  let byref t = ByRef(false, t)
  let out t = ByRef(true, t)

  let flexible t = Flexible t

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

  let choice xs = Choice xs

  let private memberGenericParameters (declaring: LowType) (member': Member) =
    let toTypeVariable = function Variable (_, v) -> v | _ -> failwith "it is not variable."
    let declaringVariables = LowType.collectVariables declaring |> List.map toTypeVariable |> Set.ofList
    let memberVariables =
      [
        yield member'.ReturnParameter.Type

        for group in member'.Parameters do
          for p in group do
            yield p.Type
      ]
      |> List.collect LowType.collectVariables |> List.map toTypeVariable |> List.distinct |> Set.ofList
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

  let typeExtension existingType declaration modifier member' = ApiSignature.TypeExtension { ExistingType = existingType; Declaration = declaration; MemberModifier = modifier; Member = member' }
  let extensionMember member' = ApiSignature.ExtensionMember member'

  let unionCase declaration name fields =
    let fields = fields |> List.map (fun (name, t) -> { Name = name; Type = t } : UnionCaseField)
    let uc = { DeclaringType = declaration; Name = name; Fields = fields } : UnionCase
    ApiSignature.UnionCase uc

  let module' name assemblyName accessibility = ApiSignature.ModuleDefinition { Name = name; AssemblyName = assemblyName; Accessibility = accessibility }

  let constraint' vs c = { Variables = List.map tv vs; Constraint = c }

  let typeAbbreviationApi def = ApiSignature.TypeAbbreviation def

  let api name apiSig = { Name = name; Signature = apiSig; TypeConstraints = []; Document = None }

  let arrayType = "Microsoft.FSharp.Core.[]<'T>"
  let array2DType = "Microsoft.FSharp.Core.[,]<'T>"
  let array t = createType arrayType [ t ]
  let array2D t = createType array2DType [ t ]

  let queryArray t = generic (identity "[]<'T>") [ t ]
  let queryArray2D t = generic (identity "[,]<'T>") [ t ]

  let tuple xs = Tuple { Elements = xs; IsStruct = false }
  let structTuple xs = Tuple { Elements = xs; IsStruct = true }

  let typeAbbreviationDef name original =
    let defName = DisplayName.ofString name
    let fullName =
      let toFullName (x: DisplayNameItem) =
        let genericSuffix =
          match x.GenericParameters with
          | [] -> ""
          | args -> "`" + string args.Length
        let name = FSharpImpl.toDisplayName x.Name
        name + genericSuffix
      defName |> List.rev |> List.map toFullName |> String.concat "."
    { Name = defName; FullName = fullName; AssemblyName = "test"; Accessibility = Public; GenericParameters = defName.Head.GenericParameters; Abbreviated = original; Original = original }

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