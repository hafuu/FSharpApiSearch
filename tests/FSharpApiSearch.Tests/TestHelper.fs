module TestHelper

open FSharpApiSearch

module DSL =

  let createType name args =
    let id = FullIdentity { AssemblyName = "test"; Name = ReverseName.ofString name; GenericParameterCount = List.length args }
    match args with
    | [] -> Identity id
    | args -> Generic (Identity id, args)

  let rec updateAssembly name = function
    | Identity (FullIdentity id) -> Identity (FullIdentity { id with AssemblyName = name })
    | Generic (Identity (FullIdentity id), args) -> Generic (Identity (FullIdentity { id with AssemblyName = name }), args)
    | other -> other

  let typeAbbreviation abbreviated abbreviation = TypeAbbreviation { Abbreviation = abbreviation; Original = abbreviated; }

  let identity name = Identity (PartialIdentity { Name = ReverseName.ofString name; GenericParameterCount = 0 })
  let variable name = Variable (VariableSource.Target, name)
  let queryVariable name = Variable (VariableSource.Query, name)

  let wildcard = Wildcard None
  let wildcardGroup name = Wildcard (Some name)

  let generic id args =
    match id with
    | Identity (PartialIdentity id) ->
      let id = { id with GenericParameterCount = List.length args }
      Generic (Identity (PartialIdentity id), args)
    | _ -> Generic (id, args)

  let arrow xs = Arrow xs

  let member' name kind arguments returnType = { Name = name; Kind = kind; GenericParameters = []; Arguments = arguments; IsCurried = false; ReturnType = returnType }
  let property' name kind arguments returnType = { Name = name; Kind = MemberKind.Property kind; GenericParameters = []; Arguments = arguments; IsCurried = false; ReturnType = returnType }
  let method' name arguments returnType = member' name MemberKind.Method arguments returnType
  let curriedMethod name arguments returnType = { method' name arguments returnType with IsCurried = true }
  let field name returnType = { Name = name; Kind = MemberKind.Field; GenericParameters = []; Arguments = []; IsCurried = false; ReturnType = returnType }

  let private memberGenericParameters (declaring: LowType) (member': Member) =
    let toTypeVariable = function Variable (_, v) -> v | _ -> failwith "it is not variable."
    let declaringVariables = LowType.collectVariables declaring |> List.map toTypeVariable |> Set.ofList
    let memberVariables = (member'.ReturnType :: member'.Arguments) |> List.collect LowType.collectVariables |> List.map toTypeVariable |> List.distinct |> Set.ofList
    (memberVariables - declaringVariables) |> Set.toList

  let moduleFunction xs = ApiSignature.ModuleFunction xs
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

  let constraint' vs c = { Variables = vs; Constraint = c }

  let arrayType = "Microsoft.FSharp.Core.[]"
  let array2DType = "Microsoft.FSharp.Core.[,]"
  let array t = createType arrayType [ t ]
  let array2D t = createType array2DType [ t ]

  let queryArray t = generic (identity "[]") [ t ]
  let queryArray2D t = generic (identity "[,]") [ t ]

  let tuple xs = Tuple xs

open DSL

let fsharpAbbreviationTable = [|
    { Abbreviation = createType "Microsoft.FSharp.Core.int" []; Original = createType "System.Int32" [] }
    { Abbreviation = createType "Microsoft.FSharp.Core.float" []; Original = createType "System.Double" [] }
    { Abbreviation = createType "Microsoft.FSharp.Core.single" []; Original = createType "System.Single" [] }
    { Abbreviation = createType "Microsoft.FSharp.Core.string" []; Original = createType "System.String" [] }
    { Abbreviation = createType "Microsoft.FSharp.Core.unit" []; Original = SpecialTypes.LowType.Unit }
    { Abbreviation = createType "Microsoft.FSharp.Collections.list" [ variable "a" ]; Original = createType "Microsoft.FSharp.Collections.List" [ variable "a" ] }
    { Abbreviation = createType "Microsoft.FSharp.Core.option" [ variable "a" ]; Original = createType "Microsoft.FSharp.Core.Option" [ variable "a" ] }
  |]