module TestHelper

open FSharpApiSearch

module DSL =

  let createType name args =
    let id = FullIdentity { AssemblyName = "test"; Name = Name.displayNameOfString name; GenericParameterCount = List.length args }
    match args with
    | [] -> Identity id
    | args -> Generic (Identity id, args)

  let rec updateAssembly name = function
    | Identity (FullIdentity id) -> Identity (FullIdentity { id with AssemblyName = name })
    | Generic (Identity (FullIdentity id), args) -> Generic (Identity (FullIdentity { id with AssemblyName = name }), args)
    | other -> other

  let typeAbbreviation abbreviated abbreviation = TypeAbbreviation { Abbreviation = abbreviation; Original = abbreviated; }

  let identity name = Identity (PartialIdentity { Name = DisplayName.ofString name; GenericParameterCount = 0 })
  let variable name = Variable (VariableSource.Target, name)
  let queryVariable name = Variable (VariableSource.Query, name)

  let wildcard = Wildcard None
  let wildcardGroup name = Wildcard (Some name)

  let generic id args =
    match id with
    | Identity (PartialIdentity id) ->
      let parameterCount = List.length args
      let name =
        match id.Name with
        | [] -> []
        | n :: tail -> { n with GenericParametersForDisplay = List.init parameterCount (sprintf "T%d") } :: tail
      let id = { id with Name = name; GenericParameterCount = parameterCount }
      Generic (Identity (PartialIdentity id), args)
    | _ -> Generic (id, args)

  let arrow xs = Arrow xs

  let delegate' t xs = Delegate (t, xs)

  let ptype t (x: Parameter) = { x with Type = t }
  let pname n (x: Parameter) = { x with Name = Some n }

  let createFunction fn = (List.map (List.map (fun f -> f { Name = None; Type = SpecialTypes.LowType.unit })) fn)

  let member' name kind parameters returnType =
    { Name = name; Kind = kind; GenericParameters = []; Parameters = (createFunction parameters); ReturnParameter = Parameter.ofLowType returnType }

  let property' name kind parameters returnType = member' name (MemberKind.Property kind) parameters returnType
  let method' name parameters returnType = member' name MemberKind.Method parameters returnType
  let field name returnType = member' name MemberKind.Field [] returnType

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

  let constraint' vs c = { Variables = vs; Constraint = c }

  let arrayType = "Microsoft.FSharp.Core.[]<'T>"
  let array2DType = "Microsoft.FSharp.Core.[,]<'T>"
  let array t = createType arrayType [ t ]
  let array2D t = createType array2DType [ t ]

  let queryArray t = generic (identity "[]<'T>") [ t ]
  let queryArray2D t = generic (identity "[,]<'T>") [ t ]

  let tuple xs = Tuple xs

  let typeAbbreviationDef name original =
    let defName = DisplayName.ofString name
    let fullName =
      let toFullName (x: NameItem) =
        match x.GenericParametersForDisplay with
        | [] -> x.FSharpName
        | args -> sprintf "%s`%d" x.FSharpName args.Length
      defName |> List.rev |> List.map toFullName |> String.concat "."
    { Name = defName; FullName = fullName; AssemblyName = "test"; Accessibility = Public; GenericParameters = defName.Head.GenericParametersForDisplay; Abbreviated = original; Original = original }

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
    typeAbbreviationDef "Microsoft.FSharp.Collections.list<'a>" (createType "Microsoft.FSharp.Collections.List<'a>" [ variable "a" ])
    typeAbbreviationDef "Microsoft.FSharp.Core.option<'a>" (createType "Microsoft.FSharp.Core.Option<'a>" [ variable "a" ])
  |]