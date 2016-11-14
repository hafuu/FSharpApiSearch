namespace FSharpApiSearch

open System.Text

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
  | Any

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module NameMatchMethod =
  let ofString (str: string) : string * NameMatchMethod =
    if str = "*" then
      str, NameMatchMethod.Any
    elif str.Contains("*") then
      let pattern = sprintf "^%s$" (str.Replace("*",".*"))
      pattern, NameMatchMethod.Regex
    else
      str, NameMatchMethod.StringCompare

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

  let private statusToInt enabledValue = function
    | Enabled -> enabledValue
    | Disabled -> 0

  let private intToStatus = function
    | n when n > 0 -> Enabled
    | _ -> Disabled

  let GreedyMatching = { Get = (fun x -> x.GreedyMatching); Set = (fun value x -> { x with GreedyMatching = value }) }
  let RespectNameDifference = { Get = (fun x -> x.RespectNameDifference); Set = (fun value x -> { x with RespectNameDifference = value }) }
  let IgnoreParameterStyle = { Get = (fun x -> x.IgnoreParameterStyle); Set = (fun value x -> { x with IgnoreParameterStyle = value }) }
  let IgnoreCase = { Get = (fun x -> x.IgnoreCase); Set = (fun value x -> { x with IgnoreCase = value }) }
  let SwapOrderDepth = { Get = (fun x -> x.SwapOrderDepth); Set = (fun value x -> { x with SwapOrderDepth = max 0 value }) }
  let SwapOrder = { Get = SwapOrderDepth.Get >> intToStatus; Set = statusToInt defaultOptions.SwapOrderDepth >> SwapOrderDepth.Set }
  let ComplementDepth = { Get = (fun x -> x.ComplementDepth); Set = (fun value x -> { x with ComplementDepth = max 0 value }) }
  let Complement = { Get = ComplementDepth.Get >> intToStatus; Set = statusToInt defaultOptions.ComplementDepth >> ComplementDepth.Set }
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

  let join (sep: string) (sb: StringBuilder) (f: StringBuilder -> 'a -> unit) (xs: 'a list) : unit =
    if xs.IsEmpty then
      ()
    else
      f sb xs.Head
      xs.Tail
      |> List.iter (fun x ->
        sb.Append(sep) |> ignore
        f sb x  
      )

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

  let printNameItem (sb: StringBuilder) (n: NameItem) =
    match n.GenericParametersForDisplay with
    | [] -> sb.Append(n.FSharpName) |> ignore
    | args ->
      sb.Append(n.FSharpName).Append("<") |> ignore
      join ", " sb (fun sb arg -> sb.Append(typeVariablePrefix arg).Append(arg.Name) |> ignore) args
      sb.Append(">") |> ignore  

  let printDisplayName_full (sb: StringBuilder) = function
    | [] -> sb.Append("<empty>") |> ignore
    | ns ->
      ns.Tail
      |> Seq.rev
      |> Seq.iter (fun n -> printNameItem sb n; sb.Append(".") |> ignore)
      sb.Append(ns.Head.FSharpName) |> ignore

  let printName_full (sb: StringBuilder) = function
    | LoadingName (_, n1, n2) ->
      match n2 with
      | [] -> sb.Append(n1) |> ignore
      | n2 ->
        sb.Append(n1).Append(".") |> ignore
        printDisplayName_full sb n2
    | DisplayName n -> printDisplayName_full sb n

  let printIdentity_full (sb: StringBuilder) (identity: Identity) =
    match identity with
    | FullIdentity i -> printName_full sb i.Name
    | PartialIdentity i -> printDisplayName_full sb i.Name

  let printIdentity_short (sb: StringBuilder) (identity: Identity) =
    let printDisplayName_short (sb: StringBuilder) = function
      | [] -> sb.Append("<empty>") |> ignore
      | n :: _ -> sb.Append(n.FSharpName) |> ignore

    let printName_short (sb: StringBuilder) = function
      | LoadingName (_, n1, n2) ->
        match n2 with
        | [] -> sb.Append(n1) |> ignore
        | n2 -> printDisplayName_short sb n2
      | DisplayName n -> printDisplayName_short sb n
    
    match identity with
    | FullIdentity i -> printName_short sb i.Name
    | PartialIdentity i -> printDisplayName_short sb i.Name

  let printVariableSource = function
    | VariableSource.Query -> "q"
    | VariableSource.Target -> "t"

  let printTypeVariable isDebug (sb: StringBuilder) source v =
    if isDebug then
      sb.Append(typeVariablePrefix v).Append(printVariableSource source).Append("_").Append(v.Name) |> ignore
    else
      sb.Append(typeVariablePrefix v).Append(v.Name) |> ignore

  let rec printLowType isDebug (sb: StringBuilder) (printIdentity: StringBuilder -> Identity -> unit) = function
    | Wildcard name ->
      match name with
      | Some n -> sb.Append("?").Append(n) |> ignore
      | None -> sb.Append("?") |> ignore
    | Variable (source, v) -> printTypeVariable isDebug sb source v
    | Identity i -> printIdentity sb i
    | Arrow xs -> printArrow isDebug sb printIdentity xs
    | Tuple xs -> printTuple isDebug sb printIdentity xs
    | LowType.Patterns.Array (name, elem) ->
      match elem with
      | Tuple _ | Arrow _ ->
        sb.Append("(") |> ignore
        printLowType isDebug sb printIdentity elem
        sb.Append(")") |> ignore
      | _ -> printLowType isDebug sb printIdentity elem
      sb.Append(name) |> ignore
    | Generic (id, args) -> printGeneric isDebug sb printIdentity id args
    | TypeAbbreviation t -> printLowType isDebug sb printIdentity t.Abbreviation
    | Delegate (t, _) -> printLowType isDebug sb printIdentity t
    | Choice xs -> printChoice isDebug sb printIdentity xs
  and printGeneric isDebug (sb: StringBuilder) printIdentity id (args: _ list) =
    printLowType isDebug sb printIdentity id
    sb.Append("<") |> ignore
    join ", " sb (fun sb arg -> printLowType isDebug sb printIdentity arg) args
    sb.Append(">") |> ignore
  and printArrow isDebug (sb: StringBuilder) printIdentity (xs: _ list) =
    let printItem (sb: StringBuilder) = function
      | Arrow _ as a ->
        sb.Append("(") |> ignore
        printLowType isDebug sb printIdentity a
        sb.Append(")") |> ignore
      | x -> printLowType isDebug sb printIdentity x
    join " -> " sb printItem xs
  and printTuple isDebug (sb: StringBuilder) printIdentity (xs: _ list) =
    let printItem (sb: StringBuilder) = function
      | Tuple _ as t ->
        sb.Append("(") |> ignore
        printLowType isDebug sb printIdentity t
        sb.Append(")") |> ignore
      | x -> printLowType isDebug sb printIdentity x
    join " * " sb printItem xs
  and printChoice isDebug (sb: StringBuilder) printIdentity (xs: _ list) =
    sb.Append("(") |> ignore
    join " or " sb (fun sb x -> printLowType isDebug sb printIdentity x) xs
    sb.Append(")") |> ignore

  let printLowType_short isDebug (sb: StringBuilder) t = printLowType isDebug sb printIdentity_short t
  let printLowType_full isDebug (sb: StringBuilder) t = printLowType isDebug sb printIdentity_full t

  let printAccessPath' (sb: StringBuilder) (i: Identity) =
    let print (name: DisplayName) =
      List.tail name
      |> List.rev
      |> join "." sb (fun sb x -> printNameItem sb x)
    match i with
    | PartialIdentity p -> print p.Name
    | FullIdentity f ->
      let name = Name.displayName f.Name
      print name

  let rec printAccessPath (sb: StringBuilder) = function
    | Wildcard _ -> ()
    | Variable _ -> ()
    | Identity i -> printAccessPath' sb i
    | Arrow _ -> ()
    | Tuple _ -> ()
    | Generic (id, _) -> printAccessPath sb id
    | TypeAbbreviation t -> printAccessPath sb t.Abbreviation
    | Delegate _ -> ()
    | Choice _ -> ()

  let printParameter tupleParen isDebug (sb: StringBuilder) (p: Parameter) =
    match p.IsOptional with
    | true -> sb.Append("?") |> ignore
    | false -> ()

    match p.Name with
    | Some name -> sb.Append(name).Append(":") |> ignore
    | None -> ()

    match p with
    | { Type = Tuple _ } when tupleParen ->
      sb.Append("(") |> ignore
      printLowType_short isDebug sb p.Type
      sb.Append(")") |> ignore
    | { Type = Arrow _ } ->
      sb.Append("(") |> ignore
      printLowType_short isDebug sb p.Type
      sb.Append(")") |> ignore
    | _ -> printLowType_short isDebug sb p.Type

  let printParameterGroups tupleParen isDebug (sb: StringBuilder) (f: Parameter list list) =
    f
    |> join " -> " sb (fun sb ps ->
      ps |> join " * " sb (fun sb p -> printParameter tupleParen isDebug sb p)
    )

  let printMember isDebug (sb: StringBuilder) (m: Member) =
    match m.Parameters with
    | [] -> printLowType_short isDebug sb m.ReturnParameter.Type
    | _ ->
      printParameterGroups true isDebug sb m.Parameters
      sb.Append(" -> ") |> ignore
      printLowType_short isDebug sb m.ReturnParameter.Type

  let printConstraint isDebug (sb: StringBuilder) (c: TypeConstraint) =
    let variableSource = VariableSource.Target

    match c.Variables with
    | [ v ] ->
      printTypeVariable isDebug sb variableSource v
    | vs ->
      sb.Append("(") |> ignore
      join " or " sb (fun sb v -> printTypeVariable isDebug sb variableSource v) vs
      sb.Append(")") |> ignore

    sb.Append(" ") |> ignore

    match c.Constraint with
    | Constraint.SubtypeConstraints s ->
      sb.Append(":> ") |> ignore
      printLowType_short isDebug sb s
    | Constraint.NullnessConstraints -> sb.Append(": null") |> ignore
    | Constraint.MemberConstraints (modifier, member') ->
      sb.Append(": (") |> ignore
      match modifier with
      | MemberModifier.Static -> sb.Append("static member") |> ignore
      | MemberModifier.Instance -> sb.Append("member") |> ignore
      sb.Append(" ").Append(member'.Name).Append(" : ") |> ignore
      printMember isDebug sb member'
      sb.Append(")") |> ignore
    | Constraint.DefaultConstructorConstraints ->
      sb.Append(": (new : unit -> ") |> ignore
      printTypeVariable isDebug sb variableSource (c.Variables.Head)
      sb.Append(")") |> ignore
    | Constraint.ValueTypeConstraints -> sb.Append(": struct") |> ignore
    | Constraint.ReferenceTypeConstraints -> sb.Append(": not struct") |> ignore
    | Constraint.EnumerationConstraints -> sb.Append(": enum") |> ignore
    | Constraint.DelegateConstraints -> sb.Append(": delegate") |> ignore
    | Constraint.UnmanagedConstraints -> sb.Append(": unmanaged") |> ignore
    | Constraint.EqualityConstraints -> sb.Append(": equality") |> ignore
    | Constraint.ComparisonConstraints -> sb.Append(": comparison") |> ignore
    
  let printFullTypeDefinition isDebug (sb: StringBuilder) (x: FullTypeDefinition) =
    sb.Append("type ") |> ignore
    printLowType_short isDebug sb x.LowType

  let pringTypeAbbreviation isDebug (sb: StringBuilder) (x: TypeAbbreviationDefinition) =
    sb.Append("type ") |> ignore
    printLowType_short isDebug sb x.TypeAbbreviation.Abbreviation
    sb.Append(" = ") |> ignore
    printLowType_full isDebug sb x.Abbreviated

  let printUnionCaseField isDebug (sb: StringBuilder) (uc: UnionCaseField) =
    match uc.Name with
    | Some name ->
      sb.Append(name).Append(":") |> ignore
      printLowType_short isDebug sb uc.Type
    | None -> printLowType_short isDebug sb uc.Type

  let printUnionCase isDebug (sb: StringBuilder) (uc: UnionCase) =
    if uc.Fields.IsEmpty then
      printLowType_short isDebug sb uc.DeclaringType
    else
      printParameterGroups true isDebug sb (UnionCase.toFunction uc)

  let printModule (sb: StringBuilder) (m: ModuleDefinition) = sb.Append("module ").Append(m.Name.Head.FSharpName) |> ignore

  let printComputationExpressionBuilder isDebug (sb: StringBuilder) (builder: ComputationExpressionBuilder)=
    if isDebug then
      sb.Append("type ") |> ignore
      printLowType_short isDebug sb builder.BuilderType
      sb.Append(", [ ") |> ignore
      join "; " sb (fun sb x -> printLowType_short isDebug sb x) builder.ComputationExpressionTypes
      sb.Append(" ], { ") |> ignore
      join "; " sb (fun sb (x: string) -> sb.Append(x) |> ignore) builder.Syntaxes
      sb.Append(" }") |> ignore
    else
      sb.Append("type ") |> ignore
      printLowType_short isDebug sb builder.BuilderType
      sb.Append(", { ") |> ignore
      join "; " sb (fun sb (x: string) -> sb.Append(x) |> ignore) builder.Syntaxes
      sb.Append(" }") |> ignore

  let printApiSignature isDebug (sb: StringBuilder) = function
    | ApiSignature.ModuleValue t -> printLowType_short isDebug sb t
    | ApiSignature.ModuleFunction fn -> printParameterGroups false isDebug sb fn
    | ApiSignature.ActivePatten (_, fn) -> printParameterGroups false isDebug sb fn
    | ApiSignature.InstanceMember (declaringType, m) ->
      if isDebug then
        printLowType_short isDebug sb declaringType
        sb.Append(" => ") |> ignore
        printMember isDebug sb m
      else
        printMember isDebug sb m
    | ApiSignature.StaticMember (_, m) -> printMember isDebug sb m
    | ApiSignature.Constructor (_, m) -> printMember isDebug sb m
    | ApiSignature.ModuleDefinition m -> printModule sb m
    | ApiSignature.FullTypeDefinition x -> printFullTypeDefinition isDebug sb x
    | ApiSignature.TypeAbbreviation t -> pringTypeAbbreviation isDebug sb t
    | ApiSignature.TypeExtension t ->
      if isDebug then
        printLowType_short isDebug sb t.ExistingType
        sb.Append(" => ") |> ignore
        printMember isDebug sb t.Member
      else
        printMember isDebug sb t.Member
    | ApiSignature.ExtensionMember m -> printMember isDebug sb m
    | ApiSignature.UnionCase uc -> printUnionCase isDebug sb uc
    | ApiSignature.ComputationExpressionBuilder builder -> printComputationExpressionBuilder isDebug sb builder

type TypeVariable with
  member this.Print() =
    let sb = StringBuilder()
    Print.printTypeVariable false sb VariableSource.Target this
    string sb

type NameItem with
  member this.Print() =
    let sb = StringBuilder()
    Print.printNameItem sb this
    string sb

type Name with
  member this.Print() =
    let sb = StringBuilder()
    Print.printName_full sb this
    string sb

type LowType with
  member this.Print() =
    let sb = StringBuilder()
    Print.printLowType_short false sb this
    string sb
  member this.Debug() =
    let sb = StringBuilder()
    Print.printLowType_short true sb this
    string sb

type ApiSignature with
  member this.Print() =
    let sb = StringBuilder()
    Print.printApiSignature false sb this
    string sb
  member this.Debug() =
    let sb = StringBuilder()
    Print.printApiSignature true sb this
    string sb

type TypeConstraint with
  member this.Print() =
    let sb = StringBuilder()
    Print.printConstraint false sb this
    string sb
  member this.Debug() =
    let sb = StringBuilder()
    Print.printConstraint true sb this
    string sb
  
type FullTypeDefinition with
  member this.Print() =
    let sb = StringBuilder()
    Print.printFullTypeDefinition false sb this
    string sb
  member this.Debug() =
    let sb = StringBuilder()
    Print.printFullTypeDefinition true sb this
    string sb

type Api with
  member this.PrintSignature() = this.Signature.Print()
  member this.PrintTypeConstraints() =
    let sb = StringBuilder()
    sb.Append("when ") |> ignore
    Print.join " and " sb (fun sb (c: TypeConstraint) -> Print.printConstraint false sb c) this.TypeConstraints
    string sb
  member this.PrintKind() =
    match this.Signature with
    | ApiSignature.TypeExtension { Declaration = declaration } ->
      let sb = StringBuilder()
      sb.Append(Print.printApiKind this.Kind).Append(" (") |> ignore
      Print.printDisplayName_full sb declaration
      sb.Append(")") |> ignore
      string sb
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