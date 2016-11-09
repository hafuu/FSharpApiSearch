namespace FSharpApiSearch

type TypeVariable = {
  Name: string
  IsSolveAtCompileTime: bool
}

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module TypeVariable =
  let ofString (v: string) =
    if List.exists (fun prefix -> v.StartsWith(prefix)) [ "'"; "^" ] = false then failwithf "wrong variable name: %s" v
    { Name = v.TrimStart(''', '^'); IsSolveAtCompileTime = v.StartsWith("^") }

type NameItem = {
  FSharpName: string
  InternalFSharpName: string
  // CompiledName: string
  GenericParametersForDisplay: TypeVariable list
}

type FullName = string
type DisplayName = NameItem list

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module internal DisplayName =
  open System.Text.RegularExpressions

  let ofString (name: string) =
    name.Split('.')
    |> Seq.map (fun n ->
      if n.Contains("<") then
        let xs = n.Split([| '<' |], 2)
        let name = xs.[0]
        let args = [ for m in Regex.Matches(xs.[1], @"(['^]\w+)") -> TypeVariable.ofString m.Groups.[1].Value ]
        { FSharpName = name; InternalFSharpName = name; GenericParametersForDisplay = args }
      else
        { FSharpName = n; InternalFSharpName = n; GenericParametersForDisplay = [] })
    |> Seq.toList
    |> List.rev
  
  let ofOperatorString (name: string) =
    let name = ofString name
    let head = { name.Head with InternalFSharpName = Microsoft.FSharp.Compiler.PrettyNaming.CompileOpName (name.Head.FSharpName.Trim('(', ')', ' ')) }
    head :: name.Tail

type Name =
  | LoadingName of assemblyName: string * FullName * DisplayName
  | DisplayName of DisplayName

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module internal Name =
  let displayNameOfString (name: string) = DisplayName (DisplayName.ofString name)
  let displayNameOfOperatorString (name: string) = DisplayName (DisplayName.ofOperatorString name)

  let loadingNameError() = failwith "Loading name at run time is invalid data."
  let displayName = function
    | LoadingName _ -> loadingNameError()
    | DisplayName ns -> ns

type PartialIdentity = {
  Name: DisplayName
  GenericParameterCount: int
}

type FullIdentity = {
  AssemblyName: string
  Name: Name
  GenericParameterCount: int
}

type Identity =
  | PartialIdentity of PartialIdentity
  | FullIdentity of FullIdentity

[<RequireQualifiedAccess>]
type VariableSource = Query | Target

type LowType =
  | Wildcard of string option
  | Variable of VariableSource * TypeVariable
  | Identity of Identity
  | Arrow of LowType list
  | Tuple of LowType list
  | Generic of LowType * LowType list
  | TypeAbbreviation of TypeAbbreviation
  | Delegate of delegateType: LowType * LowType list
  | Choice of LowType list
and TypeAbbreviation = {
  Abbreviation: LowType
  Original: LowType
}

type Accessibility =
  | Public
  | Private
  //| Internal

[<RequireQualifiedAccess>]
type PropertyKind = Get | Set | GetSet

[<RequireQualifiedAccess>]
type MemberKind =
  | Method
  | Property of PropertyKind
  | Field

[<RequireQualifiedAccess>]
type MemberModifier = Instance | Static

type Parameter = {
  Type: LowType
  Name: string option
  IsOptional: bool
}

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module internal Parameter =
  let ofLowType t = { Name = None; Type = t; IsOptional = false }

type ParameterGroups = Parameter list list
type Function = Parameter list list

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module internal Function =
  let toLowTypeList (fn: Parameter list list) =
    [
      for ps in fn do
        match ps with
        | [] -> ()
        | [ one ] -> yield one.Type
        | many -> yield Tuple (List.map (fun x -> x.Type) many)
    ]
  let toArrow (fn: Parameter list list) =
    let xs = toLowTypeList fn
    match xs with
    | [ one ] -> one
    | xs -> Arrow xs

type Member = {
  Name: string
  Kind: MemberKind
  GenericParameters: TypeVariable list
  Parameters: ParameterGroups
  ReturnParameter: Parameter
}
with
  member this.IsCurried = List.length this.Parameters > 1

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module internal Member =
  let toArrow m = Function.toArrow [ yield! m.Parameters; yield [ m.ReturnParameter ] ]
  let toFunction m =
    match m.Parameters with
    | [] -> [ [ m.ReturnParameter ] ]
    | _ -> [ yield! m.Parameters; yield [ m.ReturnParameter ] ]

[<RequireQualifiedAccess>]
type TypeDefinitionKind =
  | Class
  | Interface
  | Type
  | Union
  | Record
  | Enumeration

type Constraint =
  | SubtypeConstraints of LowType
  | NullnessConstraints
  | MemberConstraints of MemberModifier * Member
  | DefaultConstructorConstraints
  | ValueTypeConstraints
  | ReferenceTypeConstraints
  | EnumerationConstraints
  | DelegateConstraints
  | UnmanagedConstraints
  | EqualityConstraints
  | ComparisonConstraints

type TypeConstraint = {
  Variables: TypeVariable list
  Constraint: Constraint
}

type ConstraintStatus =
  | Satisfy
  | NotSatisfy
  | Dependence of TypeVariable list

type FullTypeDefinition = {
  Name: DisplayName
  FullName: FullName
  AssemblyName: string
  Accessibility: Accessibility
  Kind: TypeDefinitionKind
  BaseType: LowType option
  AllInterfaces: LowType list
  GenericParameters: TypeVariable list
  TypeConstraints: TypeConstraint list
  InstanceMembers: Member list
  StaticMembers: Member list
  
  ImplicitInstanceMembers: Member list  
  ImplicitStaticMembers: Member list

  // pre-compute for type constraints
  SupportNull: ConstraintStatus
  ReferenceType: ConstraintStatus
  ValueType: ConstraintStatus
  DefaultConstructor: ConstraintStatus
  Equality: ConstraintStatus
  Comparison: ConstraintStatus
}
with
  member internal this.FullIdentity = { AssemblyName = this.AssemblyName; Name = DisplayName this.Name; GenericParameterCount = this.GenericParameters.Length }
  member internal this.LowType =
    match this.GenericParameters with
    | [] -> Identity (FullIdentity this.FullIdentity)
    | gps ->
      let gps = gps |> List.map (fun v -> Variable (VariableSource.Target, v))
      let id = Identity (FullIdentity this.FullIdentity)
      Generic (id, gps) 

type TypeAbbreviationDefinition = {
  Name: DisplayName
  FullName: FullName
  AssemblyName: string
  Accessibility: Accessibility
  GenericParameters: TypeVariable list
  Abbreviated: LowType
  Original: LowType
}
with
  member internal this.FullIdentity = { AssemblyName = this.AssemblyName; Name = DisplayName this.Name; GenericParameterCount = this.GenericParameters.Length }
  member internal this.TypeAbbreviation =
    let abbreviation =
      match this.GenericParameters with
      | [] -> Identity (FullIdentity this.FullIdentity)
      | args ->
        let id = Identity (FullIdentity this.FullIdentity)
        let args = args |> List.map (fun a -> Variable (VariableSource.Target, a))
        Generic (id, args)
    {
      Abbreviation = abbreviation
      Original = this.Original
    }: TypeAbbreviation

type TypeExtension = {
  ExistingType: LowType
  Declaration: DisplayName
  MemberModifier: MemberModifier
  Member: Member
}

[<RequireQualifiedAccess>]
type ApiKind =
  | ModuleValue
  | Constructor
  | Member of MemberModifier * MemberKind
  | TypeExtension of MemberModifier * MemberKind
  | ExtensionMember
  | UnionCase
  | ModuleDefinition
  | TypeDefinition
  | TypeAbbreviation
  | ComputationExpressionBuilder

[<RequireQualifiedAccess>]
type ActivePatternKind =
  | ActivePattern
  | PartialActivePattern

[<RequireQualifiedAccess>]
type UnionCaseField = {
  Name: string option
  Type: LowType
}

[<RequireQualifiedAccess>]
type UnionCase = {
  DeclaringType: LowType
  Name: string
  Fields: UnionCaseField list
}

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module UnionCase =
  let toFunction (uc: UnionCase) =
    let fields = uc.Fields |> List.map (fun field -> { Name = field.Name; Type = field.Type; IsOptional = false })
    let ret = Parameter.ofLowType uc.DeclaringType |> List.singleton
    [ fields; ret ]

type ModuleDefinition = {
  Name: DisplayName
  Accessibility: Accessibility
}
with
  member internal this.LowType = Identity (FullIdentity { Name = DisplayName this.Name; AssemblyName = "dummy assembly"; GenericParameterCount = 0 })

type ComputationExpressionBuilder = {
  BuilderType: LowType
  ComputationExpressionTypes: LowType list
  Syntaxes: string list
}

[<RequireQualifiedAccess>]
type ApiSignature =
  | ModuleValue of LowType
  | ModuleFunction of Function
  | ActivePatten of ActivePatternKind * Function
  | InstanceMember of LowType * Member
  | StaticMember of LowType * Member
  | Constructor of LowType * Member
  | ModuleDefinition of ModuleDefinition
  | FullTypeDefinition of FullTypeDefinition
  | TypeAbbreviation of TypeAbbreviationDefinition
  /// F# Type Extension
  | TypeExtension of TypeExtension
  /// C# Extension Member
  | ExtensionMember of Member
  | UnionCase of UnionCase
  | ComputationExpressionBuilder of ComputationExpressionBuilder

type Api = {
  Name: Name
  Signature: ApiSignature
  TypeConstraints: TypeConstraint list
  Document: string option
}
with
  member this.Kind =
    match this.Signature with
    | ApiSignature.ModuleValue _ -> ApiKind.ModuleValue
    | ApiSignature.ModuleFunction _ -> ApiKind.ModuleValue
    | ApiSignature.ActivePatten _ -> ApiKind.ModuleValue
    | ApiSignature.Constructor _ -> ApiKind.Constructor
    | ApiSignature.InstanceMember (_, m) -> ApiKind.Member (MemberModifier.Instance, m.Kind)
    | ApiSignature.StaticMember (_, m) -> ApiKind.Member (MemberModifier.Static, m.Kind)
    | ApiSignature.ModuleDefinition _ -> ApiKind.ModuleDefinition
    | ApiSignature.FullTypeDefinition _ -> ApiKind.TypeDefinition
    | ApiSignature.TypeAbbreviation _ -> ApiKind.TypeAbbreviation
    | ApiSignature.TypeExtension t -> ApiKind.Member (t.MemberModifier, t.Member.Kind)
    | ApiSignature.ExtensionMember _ -> ApiKind.ExtensionMember
    | ApiSignature.UnionCase _ -> ApiKind.UnionCase
    | ApiSignature.ComputationExpressionBuilder _ -> ApiKind.ComputationExpressionBuilder

type ApiDictionary = {
  AssemblyName: string
  Api: Api[]
  TypeDefinitions: FullTypeDefinition[]
  TypeAbbreviations: TypeAbbreviationDefinition[]
}
with
  member this.PublicApiNumber =
    this.Api
    |> Seq.filter (function
      | { Signature = ApiSignature.FullTypeDefinition { Accessibility = Private } }
      | { Signature = ApiSignature.ModuleDefinition { Accessibility = Private } }
      | { Signature = ApiSignature.TypeAbbreviation { Accessibility = Private } } -> false
      | _ -> true)
    |> Seq.length

[<RequireQualifiedAccess>]
type ActivePatternSignature =
  | AnyParameter of LowType * LowType // (||) => ... -> a -> b
  | Specified of LowType // (||) => a -> b
[<RequireQualifiedAccess>]
type ActivePatternQuery = {
  Kind: ActivePatternKind
  Signature: ActivePatternSignature
}
[<RequireQualifiedAccess>]
type ComputationExpressionQuery = {
  Syntaxes: string list
  Type: LowType
}

[<RequireQualifiedAccess>]
type SignatureQuery =
  | Wildcard
  | Signature of LowType
  | InstanceMember of Receiver: LowType * Parameters: LowType list * ReturnType: LowType

[<RequireQualifiedAccess>]
type NameMatchMethod =
  | StringCompare
  | Regex

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module NameMatchMethod =
  let ofString (str: string) =
    if str.Contains("*") then
      NameMatchMethod.Regex
    else
      NameMatchMethod.StringCompare

[<RequireQualifiedAccess>]
type QueryMethod =
  | ByName of (string * NameMatchMethod) list * SignatureQuery
  | BySignature of SignatureQuery
  | ByActivePattern of ActivePatternQuery
  | ByComputationExpression of ComputationExpressionQuery

[<RequireQualifiedAccess>]
type Query = {
  OriginalString: string
  Method: QueryMethod
}

type OptionStatus = Enabled | Disabled

type SearchOptions = internal {
  GreedyMatching: OptionStatus
  RespectNameDifference: OptionStatus
  IgnoreParameterStyle: OptionStatus
  IgnoreCase: OptionStatus
  SwapOrderDepth: int
  ComplementDepth: int
  Parallel: OptionStatus
}

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module SearchOptions =
  let defaultOptions = { GreedyMatching = Disabled; RespectNameDifference = Enabled; IgnoreParameterStyle = Enabled; IgnoreCase = Enabled; SwapOrderDepth = 2; ComplementDepth = 2; Parallel = Disabled }

  let GreedyMatching = { Get = (fun x -> x.GreedyMatching); Set = (fun value x -> { x with GreedyMatching = value }) }
  let RespectNameDifference = { Get = (fun x -> x.RespectNameDifference); Set = (fun value x -> { x with RespectNameDifference = value }) }
  let IgnoreParameterStyle = { Get = (fun x -> x.IgnoreParameterStyle); Set = (fun value x -> { x with IgnoreParameterStyle = value }) }
  let IgnoreCase = { Get = (fun x -> x.IgnoreCase); Set = (fun value x -> { x with IgnoreCase = value }) }
  let SwapOrderDepth = { Get = (fun x -> x.SwapOrderDepth); Set = (fun value x -> { x with SwapOrderDepth = max 0 value }) }
  let ComplementDepth = { Get = (fun x -> x.ComplementDepth); Set = (fun value x -> { x with ComplementDepth = max 0 value }) }
  let Parallel = { Get = (fun x -> x.Parallel); Set = (fun value x -> { x with Parallel = value }) }

type Result = {
  Api: Api
  AssemblyName: string
  Distance: int
}

module internal SpecialTypes =
  open System
  open System.Text.RegularExpressions

  let arrayRegexPattern = @"\[,*\]"

  let mscorlib = "mscorlib"
  let fscore = "FSharp.Core"

  let UnitLoadingName = LoadingName (fscore, typeof<Unit>.FullName, [])
  let UnitDisplayName = DisplayName (DisplayName.ofString typeof<Unit>.FullName)

  module LoadingFullIdentity =
    open System.Collections

    let ofDotNetType (t: Type) =
      if t.IsGenericType then failwith "It is not support generic type."
      let assemblyName = t.Assembly.GetName().Name
      { AssemblyName = assemblyName; Name = LoadingName (assemblyName, t.FullName, []); GenericParameterCount = 0 }

  module FullIdentity =
    open System.Collections

    let ofDotNetType (t: Type) =
      if t.IsGenericType then failwith "It is not support generic type."
      { AssemblyName = t.Assembly.GetName().Name; Name = Name.displayNameOfString t.FullName; GenericParameterCount = 0 }

    let tupleName n =
      let name =
        let genericParams = List.init n (fun n -> { Name = sprintf "T%d" n; IsSolveAtCompileTime = false })
        { FSharpName = "Tuple"; InternalFSharpName = "Tuple"; GenericParametersForDisplay = genericParams } :: { FSharpName = "System"; InternalFSharpName = "System"; GenericParametersForDisplay = [] } :: []
      DisplayName name

  module Identity =
    let ofDotNetType (t: Type) = FullIdentity (FullIdentity.ofDotNetType t)

    let tupleN n = FullIdentity { AssemblyName = mscorlib; Name = FullIdentity.tupleName n; GenericParameterCount = n }

  module LowType =
    let ofDotNetType (t: Type) = LowType.Identity (Identity.ofDotNetType t)
    let Unit = ofDotNetType typeof<Unit>
    let unit =
      let unit = LowType.Identity (FullIdentity { AssemblyName = fscore; Name = Name.displayNameOfString "Microsoft.FSharp.Core.unit"; GenericParameterCount = 0 })
      TypeAbbreviation { Abbreviation = unit; Original = Unit }

    let Double = ofDotNetType typeof<Double>
    let float =
      let float = LowType.Identity (FullIdentity { AssemblyName = fscore; Name = Name.displayNameOfString "Microsoft.FSharp.Core.float"; GenericParameterCount = 0 })
      TypeAbbreviation { Abbreviation = float; Original = Double }

    let rec isUnit (x: LowType) =
      match x with
      | Identity (FullIdentity { Name = name }) -> name = UnitLoadingName || name = UnitDisplayName
      | TypeAbbreviation { Original = o } -> isUnit o
      | _ -> false

    let Boolean = ofDotNetType typeof<Boolean>

    module Patterns =
      let (|Unit|_|) x = if isUnit x then Some () else None
      let (|Array|_|) x =
        match x with
        | Generic (Identity id, [ elem ]) ->
          match id with
          | FullIdentity { Name = DisplayName name }
          | PartialIdentity { Name = name } ->
            match name with
            | { FSharpName = name; GenericParametersForDisplay = [ _ ] } :: _ ->
              if Regex.IsMatch(name, arrayRegexPattern) then
                Some (name, elem)
              else
                None
            | _ -> None
          | FullIdentity { Name = LoadingName _ } -> Name.loadingNameError()
        | _ -> None

      let private b = Boolean
      let (|Boolean|_|) (TypeAbbreviation { Original = t } | t ) =
        if t = b then
          Some ()
        else
          None
      let (|NonTuple|_|) x =
        match x with
        | Tuple _ -> None
        | _ -> Some x
      let rec (|AbbreviationRoot|_|) x =
        match x with
        | TypeAbbreviation { Original = original } ->
          match original with
          | TypeAbbreviation _ -> (|AbbreviationRoot|_|) original
          | _ -> Some original
        | _ -> None

module internal Print =
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
  let printApiKind = function
    | ApiKind.ModuleValue -> "module value"
    | ApiKind.Constructor -> "constructor"
    | ApiKind.Member (modifier, memberKind) -> sprintf "%s %s" (printMemberModifier modifier) (printMemberKind memberKind)
    | ApiKind.TypeExtension (modifier, memberKind) -> sprintf "%s %s" (printMemberModifier modifier) (printMemberKind memberKind)
    | ApiKind.ExtensionMember -> "extension member"
    | ApiKind.UnionCase -> "union case"
    | ApiKind.ModuleDefinition -> "module"
    | ApiKind.TypeDefinition -> "type"
    | ApiKind.TypeAbbreviation -> "type abbreviation"
    | ApiKind.ComputationExpressionBuilder -> "builder"

  let typeVariablePrefix (v: TypeVariable) = if v.IsSolveAtCompileTime then "^" else "'"

  let printNameItem (n: NameItem) =
    match n.GenericParametersForDisplay with
    | [] -> n.FSharpName
    | args -> sprintf "%s<%s>" n.FSharpName (args |> List.map (fun a -> typeVariablePrefix a +  a.Name) |> String.concat ", ")

  let printDisplayName_full = function
    | [] -> "<empty>"
    | ns ->
      seq {
        yield ns.Head.FSharpName
        for path in ns.Tail do
          yield printNameItem path
      }
      |> Seq.rev
      |> String.concat "."

  let printName_full = function
    | LoadingName (_, n1, n2) ->
      match n2 with
      | [] -> n1
      | n2 -> n1 + "." + printDisplayName_full n2
    | DisplayName n -> printDisplayName_full n

  let printIdentity_full (identity: Identity): string =
    match identity with
    | FullIdentity i -> printName_full i.Name
    | PartialIdentity i -> printDisplayName_full i.Name

  let printIdentity_short (identity: Identity): string =
    let printDisplayName_short = function
      | [] -> "<empty>"
      | n :: _ -> n.FSharpName

    let printName_short = function
      | LoadingName (_, n1, n2) ->
        match n2 with
        | [] -> n1
        | n2 -> printDisplayName_short n2
      | DisplayName n -> printDisplayName_short n
    
    match identity with
    | FullIdentity i -> printName_short i.Name
    | PartialIdentity i -> printDisplayName_short i.Name

  let printVariableSource = function
    | VariableSource.Query -> "q"
    | VariableSource.Target -> "t"

  let printTypeVariable isDebug source v =
    if isDebug then
      sprintf "%s%s_%s" (typeVariablePrefix v) (printVariableSource source) v.Name
    else
      typeVariablePrefix v + v.Name

  let rec printLowType isDebug (printIdentity: Identity -> string) = function
    | Wildcard name ->
      match name with
      | Some n -> sprintf "?%s" n
      | None -> "?"
    | Variable (source, v) -> printTypeVariable isDebug source v
    | Identity i -> printIdentity i
    | Arrow xs -> printArrow isDebug printIdentity xs
    | Tuple xs -> printTuple isDebug printIdentity xs
    | LowType.Patterns.Array (name, elem) ->
      let paramPart =
        match elem with
        | Tuple _ | Arrow _ ->
          sprintf "(%s)" (printLowType isDebug printIdentity elem)
        | _ -> printLowType isDebug printIdentity elem
      paramPart + name
    | Generic (id, args) -> printGeneric isDebug printIdentity id args
    | TypeAbbreviation t -> printLowType isDebug printIdentity t.Abbreviation
    | Delegate (t, _) -> printLowType isDebug printIdentity t
    | Choice xs -> sprintf "(%s)" (List.map (printLowType isDebug printIdentity) xs |> String.concat " or ")
  and printGeneric isDebug printIdentity id args =
    let args = args |> Seq.map (printLowType isDebug printIdentity) |> String.concat ", "
    sprintf "%s<%s>" (printLowType isDebug printIdentity id) args
  and printArrow isDebug printIdentity xs =
    xs
    |> Seq.map (function
      | Arrow _ as a -> sprintf "(%s)" (printLowType isDebug printIdentity a)
      | x -> printLowType isDebug printIdentity x)
    |> String.concat " -> "
  and printTuple isDebug printIdentity xs =
    xs
    |> Seq.map (function
      | Tuple _ as t -> sprintf "(%s)" (printLowType isDebug printIdentity t)
      | x -> printLowType isDebug printIdentity x)
    |> String.concat " * "

  let printLowType_short isDebug t = printLowType isDebug printIdentity_short t
  let printLowType_full isDebug t = printLowType isDebug printIdentity_full t

  let printAccessPath' (i: Identity) =
    let print (name: DisplayName) =
      List.tail name
      |> Seq.rev
      |> Seq.map printNameItem
      |> String.concat "."
    match i with
    | PartialIdentity p -> print p.Name
    | FullIdentity f ->
      let name = Name.displayName f.Name
      print name

  let rec printAccessPath = function
    | Wildcard _ -> ""
    | Variable _ -> ""
    | Identity i -> printAccessPath' i
    | Arrow _ -> ""
    | Tuple _ -> ""
    | Generic (id, _) -> printAccessPath id
    | TypeAbbreviation t -> printAccessPath t.Abbreviation
    | Delegate _ -> ""
    | Choice _ -> ""

  let printParameter tupleParen isDebug (p: Parameter) =
    let optPart =
      match p.IsOptional with
      | true -> "?"
      | false -> ""
    let namePart =
      match p.Name with
      | Some name -> sprintf "%s:" name
      | None -> ""
    let sigPart =
      match p with
      | { Type = Tuple _ } when tupleParen -> sprintf "(%s)" (printLowType_short isDebug p.Type)
      | { Type = Arrow _ } -> sprintf "(%s)" (printLowType_short isDebug p.Type)
      | _ -> printLowType_short isDebug p.Type
    optPart + namePart + sigPart

  let printParameterGroups tupleParen isDebug (f: Parameter list list) =
    f
    |> Seq.map (fun ps ->
      ps
      |> Seq.map (printParameter tupleParen isDebug)
      |> String.concat " * "
    )
    |> String.concat " -> "

  let printMember isDebug (m: Member) =
    match m.Parameters with
    | [] -> printLowType_short isDebug m.ReturnParameter.Type
    | _ -> sprintf "%s -> %s" (printParameterGroups true isDebug m.Parameters) (printLowType_short isDebug m.ReturnParameter.Type)

  let printConstraint isDebug (c: TypeConstraint) =
    let variableSource = VariableSource.Target
    let variablePart =
      match c.Variables with
      | [ v ] -> printTypeVariable isDebug variableSource v
      | vs -> sprintf "(%s)" (List.map (printTypeVariable isDebug variableSource) vs |> String.concat " or ")
    let constraintPart =
      match c.Constraint with
      | Constraint.SubtypeConstraints s -> sprintf ":> %s" (printLowType_short isDebug s)
      | Constraint.NullnessConstraints -> ": null"
      | Constraint.MemberConstraints (modifier, member') ->
        let modifierPart =
          match modifier with
          | MemberModifier.Static -> "static member"
          | MemberModifier.Instance -> "member"
        sprintf ": (%s %s : %s)" modifierPart member'.Name (printMember isDebug member')
      | Constraint.DefaultConstructorConstraints -> let v = printTypeVariable isDebug variableSource (c.Variables.Head) in sprintf ": (new : unit -> %s)" v
      | Constraint.ValueTypeConstraints -> ": struct"
      | Constraint.ReferenceTypeConstraints -> ": not struct"
      | Constraint.EnumerationConstraints -> ": enum"
      | Constraint.DelegateConstraints -> ": delegate"
      | Constraint.UnmanagedConstraints -> ": unmanaged"
      | Constraint.EqualityConstraints -> ": equality"
      | Constraint.ComparisonConstraints -> ": comparison"
    sprintf "%s %s" variablePart constraintPart
    
  let printFullTypeDefinition isDebug (x: FullTypeDefinition) = sprintf "type %s" (printLowType_short isDebug x.LowType)

  let pringTypeAbbreviation isDebug (x: TypeAbbreviationDefinition) =
    sprintf "type %s = %s"
      (printLowType_short isDebug x.TypeAbbreviation.Abbreviation)
      (printLowType_full isDebug x.Abbreviated)

  let printUnionCaseField isDebug (uc: UnionCaseField) =
    match uc.Name with
    | Some name -> sprintf "%s:%s" name (printLowType_short isDebug uc.Type)
    | None -> printLowType_short isDebug uc.Type

  let printUnionCase isDebug (uc: UnionCase) =
    if uc.Fields.IsEmpty then
      printLowType_short isDebug uc.DeclaringType
    else
      UnionCase.toFunction uc
      |> printParameterGroups true isDebug

  let printModule (m: ModuleDefinition) = sprintf "module %s" m.Name.Head.FSharpName

  let printComputationExpressionBuilder isDebug (builder: ComputationExpressionBuilder)=
    if isDebug then
      sprintf "type %s, [ %s ], { %s }"
        (printLowType_short isDebug builder.BuilderType)
        (List.map (printLowType_short isDebug) builder.ComputationExpressionTypes |> String.concat "; ")
        (String.concat "; " builder.Syntaxes)
    else
      sprintf "type %s, { %s }" (printLowType_short isDebug builder.BuilderType) (String.concat "; " builder.Syntaxes)

  let printApiSignature isDebug = function
    | ApiSignature.ModuleValue t -> printLowType_short isDebug t
    | ApiSignature.ModuleFunction fn -> printParameterGroups false isDebug fn
    | ApiSignature.ActivePatten (_, fn) -> printParameterGroups false isDebug fn
    | ApiSignature.InstanceMember (declaringType, m) ->
      if isDebug then
        sprintf "%s => %s" (printLowType_short isDebug declaringType) (printMember isDebug m)
      else
        printMember isDebug m
    | ApiSignature.StaticMember (_, m) -> printMember isDebug m
    | ApiSignature.Constructor (_, m) -> printMember isDebug m
    | ApiSignature.ModuleDefinition m -> printModule m
    | ApiSignature.FullTypeDefinition x -> printFullTypeDefinition isDebug x
    | ApiSignature.TypeAbbreviation t -> pringTypeAbbreviation isDebug t
    | ApiSignature.TypeExtension t ->
      if isDebug then
        sprintf "%s => %s" (printLowType_short isDebug t.ExistingType) (printMember isDebug t.Member)
      else
        printMember isDebug t.Member
    | ApiSignature.ExtensionMember m -> printMember isDebug m
    | ApiSignature.UnionCase uc -> printUnionCase isDebug uc
    | ApiSignature.ComputationExpressionBuilder builder -> printComputationExpressionBuilder isDebug builder

type TypeVariable with
  member this.Print() = Print.printTypeVariable false VariableSource.Target this

type NameItem with
  member this.Print() = Print.printNameItem this

type Name with
  member this.Print() = Print.printName_full this

type LowType with
  member this.Print() = Print.printLowType_short false this
  member this.Debug() = Print.printLowType_short true this

type ApiSignature with
  member this.Print() = Print.printApiSignature false this
  member this.Debug() = Print.printApiSignature true this

type TypeConstraint with
  member this.Print() = Print.printConstraint false this
  member this.Debug() = Print.printConstraint true this
  
type FullTypeDefinition with
  member this.Print() = Print.printFullTypeDefinition false this
  member this.Debug() = Print.printFullTypeDefinition true this

type Api with
  member this.PrintSignature() = this.Signature.Print()
  member this.PrintTypeConstraints() =
    sprintf "when %s" (this.TypeConstraints |> List.map (fun c -> c.Print()) |> String.concat " and ")
  member this.PrintKind() =
    match this.Signature with
    | ApiSignature.TypeExtension { Declaration = declaration } ->
      sprintf "%s (%s)" (Print.printApiKind this.Kind) (Print.printDisplayName_full declaration)
    | _ -> Print.printApiKind this.Kind

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module internal Identity =
  open System

  let private testDisplayName cmp (xs: DisplayName) (ys: DisplayName) =
    Seq.zip xs ys |> Seq.forall (fun (x, y) -> String.equalsWithComparer cmp x.InternalFSharpName y.InternalFSharpName && x.GenericParametersForDisplay.Length = y.GenericParametersForDisplay.Length)

  let testFullIdentity (x: FullIdentity) (y: FullIdentity) =
    match x.Name, y.Name with
    | (LoadingName _, _) | (_, LoadingName _) -> Name.loadingNameError()
    | DisplayName xName, DisplayName yName -> x.AssemblyName = y.AssemblyName && testDisplayName StringComparer.InvariantCulture xName yName

  let private testPartialAndFullIdentity cmp (partial: PartialIdentity) (full: FullIdentity) =
    let strEqual x y = String.equalsWithComparer cmp x y
    let testNameItem (p: NameItem, f: NameItem) =
      match p.GenericParametersForDisplay, f.GenericParametersForDisplay with
      | [], _ -> strEqual p.InternalFSharpName f.InternalFSharpName
      | _ -> strEqual p.InternalFSharpName f.InternalFSharpName && p.GenericParametersForDisplay.Length = f.GenericParametersForDisplay.Length
    match full.Name with
    | DisplayName fullName ->
      partial.GenericParameterCount = full.GenericParameterCount
      && (Seq.zip partial.Name fullName |> Seq.forall testNameItem)
    | LoadingName _ -> Name.loadingNameError()

  let private sameName' cmp x y =
    match x, y with
    | FullIdentity left, FullIdentity right -> testFullIdentity left right
    | FullIdentity full, PartialIdentity partial
    | PartialIdentity partial, FullIdentity full -> testPartialAndFullIdentity cmp partial full
    | PartialIdentity left, PartialIdentity right ->
      left.GenericParameterCount = right.GenericParameterCount
      && testDisplayName cmp left.Name right.Name

  type Equality = Identity -> Identity -> bool

  let sameName x y = sameName' StringComparer.InvariantCulture x y
  let sameNameIgnoreCase x y = sameName' StringComparer.InvariantCultureIgnoreCase x y

  let equalityFromOptions opt : Equality =
    match opt.IgnoreCase with
    | Enabled -> sameNameIgnoreCase
    | Disabled -> sameName

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module internal LowType =
  let debug (x: LowType) = x.Debug()

  let rec applyVariable source (replacements: Map<TypeVariable, LowType>) = function
    | Variable (s, name) as oldValue when s = source ->
      match Map.tryFind name replacements with
      | Some newValue -> newValue
      | None -> oldValue
    | Generic (baseType, args) ->
      let baseType = applyVariable source replacements baseType
      let args = applyVariableToTargetList source replacements args
      Generic (baseType, args)
    | Tuple xs -> Tuple (applyVariableToTargetList source replacements xs)
    | Arrow xs -> Arrow (applyVariableToTargetList source replacements xs)
    | TypeAbbreviation t -> TypeAbbreviation { Abbreviation = applyVariable source replacements t.Abbreviation; Original = applyVariable source replacements t.Original }
    | Delegate (t, xs) ->
      let delegateType = applyVariable source replacements t
      let xs = applyVariableToTargetList source replacements xs
      Delegate (delegateType, xs)
    | other -> other
  and applyVariableToTargetList source replacements xs = xs |> List.map (applyVariable source replacements)

  let collectVariables x =
    let rec f = function
      | Variable _ as v -> [ v ]
      | Arrow xs -> List.collect f xs
      | Tuple xs -> List.collect f xs
      | Generic (id, args) -> List.concat [ f id; List.collect f args ]
      | TypeAbbreviation t -> List.append (f t.Abbreviation) (f t.Original)
      | Delegate (t, _) -> f t
      | _ -> []
    f x |> List.distinct

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module internal ApiSignature =
  let debug (x: ApiSignature) = x.Debug()
  let print (x: ApiSignature) = x.Print()

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module internal TypeConstraint =
  let debug (x: TypeConstraint) = x.Debug()

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module internal FullTypeDefinition =
  let debug (x: FullTypeDefinition) = x.Debug()