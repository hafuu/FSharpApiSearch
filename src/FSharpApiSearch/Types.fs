namespace FSharpApiSearch

open System.Text
open System

type TypeVariable = {
  Name: string
  IsSolveAtCompileTime: bool
}

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module TypeVariable =
  let ofString (v: string) =
    if List.exists (fun prefix -> v.StartsWith(prefix)) [ "'"; "^" ] = false then failwithf "wrong variable name: %s" v
    { Name = v.TrimStart(''', '^'); IsSolveAtCompileTime = v.StartsWith("^") }

type NamePart =
  | SymbolName of name:string
  | OperatorName of name:string * compiledName:string
  | WithCompiledName of name:string * compiledName:string

type DisplayNameItem = {
  Name: NamePart
  GenericParameters: TypeVariable list
}

type DisplayName = DisplayNameItem list

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module internal DisplayName =
  open System.Text.RegularExpressions

  let private splitName (name: string) =
    name.Split('.')
    |> Array.map (fun n ->
      if n.Contains("<") then
        let xs = n.Split([| '<' |], 2)
        let name = xs.[0]
        let args = [ for m in Regex.Matches(xs.[1], @"(['^]\w+)") -> TypeVariable.ofString m.Groups.[1].Value ]
        (name, args)
      else
        (n, [])
    )
    |> Array.rev
    |> Array.toList

  let ofString name =
    splitName name |> List.map (fun (name, parameters) -> { Name = SymbolName name; GenericParameters = parameters })
  
  let ofString2 name compiledName =
    List.zip (splitName name) (splitName compiledName)
    |> List.map (fun ((name, parameters), (compiledName, _)) ->
      let n = if name <> compiledName then WithCompiledName (name, compiledName) else SymbolName name
      { Name = n; GenericParameters = parameters })

  let ofOperatorString (name: string) =
    let name = ofString name
    let headName =
      match name.Head.Name with
      | SymbolName n -> n
      | _ -> failwith "It is not symbol name"
    let compiledOpName =
      Microsoft.FSharp.Compiler.PrettyNaming.CompileOpName (headName.Trim('(', ')', ' '))
    let head = { name.Head with Name = OperatorName (headName, compiledOpName) }
    head :: name.Tail

type Name =
  | LoadingName of assemblyName:string * accessPath:string * DisplayName // only api loading
  | DisplayName of DisplayName

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module internal Name =
  let ofString (name: string) = DisplayName (DisplayName.ofString name)
  let ofCompiledName name compiledName = DisplayName (DisplayName.ofString2 name compiledName)
  let ofOperatorName (name: string) = DisplayName (DisplayName.ofOperatorString name)

  let loadingNameError() = failwith "Loading name at run time is invalid data."
  let toDisplayName = function
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
  | Tuple of TupleType
  | Generic of LowType * LowType list
  | TypeAbbreviation of TypeAbbreviation
  | Delegate of delegateType: LowType * LowType list
  | ByRef of isOut:bool * LowType
  | Choice of LowType list
and TypeAbbreviation = {
  Abbreviation: LowType
  Original: LowType
}
and TupleType = {
  Elements: LowType list
  IsStruct: bool
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
        | many -> yield Tuple { Elements = List.map (fun x -> x.Type) many; IsStruct = false }
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
  FullName: string
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
  FullName: string
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

type ByName = {
  Expected: string
  GenericParameters: string list
  MatchMethod: NameMatchMethod
}

[<RequireQualifiedAccess>]
type QueryMethod =
  | ByName of ByName list * SignatureQuery
  | BySignature of SignatureQuery
  | ByActivePattern of ActivePatternQuery
  | ByComputationExpression of ComputationExpressionQuery

[<RequireQualifiedAccess>]
type Query = {
  OriginalString: string
  Method: QueryMethod
}

type OptionStatus = Enabled | Disabled

type Language = FSharp | CSharp

module Language =
  let tryParse (str: string) =
    match str.ToLower() with
    | "f#" | "fsharp" -> Some FSharp
    | "c#" | "csharp" -> Some CSharp
    | _ -> None

type SearchOptions = internal {
  GreedyMatching: OptionStatus
  RespectNameDifference: OptionStatus
  IgnoreParameterStyle: OptionStatus
  IgnoreCase: OptionStatus
  SwapOrderDepth: int
  ComplementDepth: int
  Parallel: OptionStatus
  Language: Language
}

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module SearchOptions =
  let defaultOptions = { GreedyMatching = Disabled; RespectNameDifference = Enabled; IgnoreParameterStyle = Enabled; IgnoreCase = Enabled; SwapOrderDepth = 2; ComplementDepth = 2; Parallel = Disabled; Language = FSharp }

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
  let Language = { Get = (fun x -> x.Language); Set = (fun value x -> { x with Language = value }) }

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
      { AssemblyName = t.Assembly.GetName().Name; Name = Name.ofString t.FullName; GenericParameterCount = 0 }

    let private tuple' typeName n =
      let name =
        let genericParams = List.init n (fun n -> { Name = sprintf "T%d" n; IsSolveAtCompileTime = false })
        let ns = { Name = SymbolName "System"; GenericParameters = [] } :: []
        { Name = SymbolName typeName; GenericParameters = genericParams } :: ns
      DisplayName name

    let tupleName n = tuple' "Tuple" n

    let valueTupleName n = tuple' "ValueTuple" n

  module Identity =
    let ofDotNetType (t: Type) = FullIdentity (FullIdentity.ofDotNetType t)

    let tupleN n = FullIdentity { AssemblyName = mscorlib; Name = FullIdentity.tupleName n; GenericParameterCount = n }

    let tuples = [1..8] |> List.map (fun n -> tupleN n)

    let valueTupleN n = FullIdentity { AssemblyName = mscorlib; Name = FullIdentity.valueTupleName n; GenericParameterCount = n }

    let valueTuples = [1..8] |> List.map (fun n -> valueTupleN n)

    let byref = FullIdentity { AssemblyName = fscore; Name = Name.ofString "Microsoft.FSharp.Core.byref<'T>"; GenericParameterCount = 1 }

    module CSharp =
      let aliases =
        [
          "bool", typeof<System.Boolean>
          "byte", typeof<System.Byte>
          "sbyte", typeof<System.SByte>
          "char", typeof<System.Char>
          "decimal", typeof<System.Decimal>
          "double", typeof<System.Double>
          "float", typeof<System.Single>
          "int", typeof<System.Int32>
          "uint", typeof<System.UInt32>
          "long", typeof<System.Int64>
          "ulong", typeof<System.UInt64>
          "object", typeof<System.Object>
          "short", typeof<System.Int16>
          "ushort", typeof<System.UInt16>
          "string", typeof<System.String>
        ]
        |> List.map (fun (alias, t) -> alias, ofDotNetType t)

  module LowType =
    let ofDotNetType (t: Type) = LowType.Identity (Identity.ofDotNetType t)
    let Unit = ofDotNetType typeof<Unit>
    let unit =
      let unit = LowType.Identity (FullIdentity { AssemblyName = fscore; Name = Name.ofString "Microsoft.FSharp.Core.unit"; GenericParameterCount = 0 })
      TypeAbbreviation { Abbreviation = unit; Original = Unit }

    let Double = ofDotNetType typeof<Double>
    let float =
      let float = LowType.Identity (FullIdentity { AssemblyName = fscore; Name = Name.ofString "Microsoft.FSharp.Core.float"; GenericParameterCount = 0 })
      TypeAbbreviation { Abbreviation = float; Original = Double }

    let rec isUnit (x: LowType) =
      match x with
      | Identity (FullIdentity { Name = name }) -> name = UnitLoadingName || name = UnitDisplayName
      | TypeAbbreviation { Original = o } -> isUnit o
      | _ -> false

    let Boolean = ofDotNetType typeof<Boolean>

    let FSharpFunc = LowType.Identity (FullIdentity { AssemblyName= fscore; Name = Name.ofString "Microsoft.FSharp.Core.FSharpFunc<'T, 'U>"; GenericParameterCount = 2 })

    module Patterns =
      let (|Unit|_|) x = if isUnit x then Some () else None
      let (|Array|_|) x =
        match x with
        | Generic (Identity id, [ elem ]) ->
          match id with
          | FullIdentity { Name = DisplayName name }
          | PartialIdentity { Name = name } ->
            match name with
            | { Name = SymbolName typeName; GenericParameters = [ _ ] } :: _ ->
              if Regex.IsMatch(typeName, arrayRegexPattern) then
                Some (typeName, elem)
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

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module internal Identity =
  open System

  let testee (x: DisplayNameItem) =
    match x.Name with
    | SymbolName n -> n
    | OperatorName (_, n) -> n
    | WithCompiledName (n, _) -> n

  let private testDisplayName cmp (xs: DisplayName) (ys: DisplayName) =
    Seq.zip xs ys |> Seq.forall (fun (x, y) -> String.equalsWithComparer cmp (testee x) (testee y) && x.GenericParameters.Length = y.GenericParameters.Length)

  let testFullIdentity (x: FullIdentity) (y: FullIdentity) =
    match x.Name, y.Name with
    | (LoadingName _, _) | (_, LoadingName _) -> Name.loadingNameError()
    | DisplayName xName, DisplayName yName -> x.AssemblyName = y.AssemblyName && testDisplayName StringComparer.InvariantCulture xName yName

  let private testPartialAndFullIdentity cmp (partial: PartialIdentity) (full: FullIdentity) =
    let strEqual x y = String.equalsWithComparer cmp x y
    let testNameItem (p: DisplayNameItem, f: DisplayNameItem) =
      match p.GenericParameters, f.GenericParameters with
      | [], _ -> strEqual (testee p) (testee f)
      | _ -> strEqual (testee p) (testee f) && p.GenericParameters.Length = f.GenericParameters.Length
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

module LowTypeVisitor =
  type Visitor = LowType -> LowType
    
  let accept_Parameter visitor (p: Parameter) = { p with Type = visitor p.Type }

  let accept_ParameterGroups visitor (groups: ParameterGroups) = groups |> List.map (List.map (accept_Parameter visitor))

  let accept_Function visitor func = accept_ParameterGroups visitor func

  let accept_Member visitor (member': Member) =
    { member' with
        Parameters = accept_ParameterGroups visitor member'.Parameters
        ReturnParameter = accept_Parameter visitor member'.ReturnParameter
    }

  let accept_TypeConstraint visitor (c: TypeConstraint) =
    let constraint' =
      match c.Constraint with
      | SubtypeConstraints x -> SubtypeConstraints (visitor x)
      | MemberConstraints (modifier, member') -> MemberConstraints (modifier, accept_Member visitor member')
      | other -> other
    { c with Constraint = constraint' }

  let accept_FullTypeDefinition visitor (fullTypeDef: FullTypeDefinition) =
    { fullTypeDef with
        BaseType = Option.map visitor fullTypeDef.BaseType
        AllInterfaces = List.map visitor fullTypeDef.AllInterfaces
        TypeConstraints = List.map (accept_TypeConstraint visitor) fullTypeDef.TypeConstraints
        InstanceMembers = List.map (accept_Member visitor) fullTypeDef.InstanceMembers
        StaticMembers = List.map (accept_Member visitor) fullTypeDef.StaticMembers
        ImplicitInstanceMembers = List.map (accept_Member visitor) fullTypeDef.ImplicitInstanceMembers
        ImplicitStaticMembers = List.map (accept_Member visitor) fullTypeDef.ImplicitStaticMembers
    }

  let accept_TypeAbbreviationDefinition visitor (abbDef: TypeAbbreviationDefinition) =
    { abbDef with
        Abbreviated = visitor abbDef.Abbreviated
        Original = visitor abbDef.Original
    }

  let accept_TypeExtension visitor (typeExtension: TypeExtension) =
    { typeExtension with
        ExistingType = visitor typeExtension.ExistingType
        Member = accept_Member visitor typeExtension.Member
    }

  let accept_UnionCaseField visitor (field: UnionCaseField) = { field with Type = visitor field.Type }

  let accept_UnionCase visitor (uc: UnionCase) =
    { uc with
        DeclaringType = visitor uc.DeclaringType
        Fields = List.map (accept_UnionCaseField visitor) uc.Fields
    }

  let accept_ComputationExpressionBuilder visitor (builder: ComputationExpressionBuilder) =
    { builder with
        BuilderType = visitor builder.BuilderType
        ComputationExpressionTypes = List.map visitor builder.ComputationExpressionTypes
    }

  let accept_ApiSignature (visitor: Visitor) = function
    | ApiSignature.ModuleValue v -> ApiSignature.ModuleValue (visitor v)
    | ApiSignature.ModuleFunction func -> ApiSignature.ModuleFunction (accept_Function visitor func)
    | ApiSignature.ActivePatten (kind, func) -> ApiSignature.ActivePatten (kind, accept_Function visitor func)
    | ApiSignature.InstanceMember (d, m) -> ApiSignature.InstanceMember (visitor d, accept_Member visitor m)
    | ApiSignature.StaticMember (d, m) -> ApiSignature.StaticMember (visitor d, accept_Member visitor m)
    | ApiSignature.Constructor (d, m) -> ApiSignature.Constructor (visitor d, accept_Member visitor m)
    | ApiSignature.ModuleDefinition m -> ApiSignature.ModuleDefinition m
    | ApiSignature.FullTypeDefinition d -> ApiSignature.FullTypeDefinition (accept_FullTypeDefinition visitor d)
    | ApiSignature.TypeAbbreviation a -> ApiSignature.TypeAbbreviation (accept_TypeAbbreviationDefinition visitor a)
    | ApiSignature.TypeExtension t -> ApiSignature.TypeExtension (accept_TypeExtension visitor t)
    | ApiSignature.ExtensionMember m -> ApiSignature.ExtensionMember (accept_Member visitor m)
    | ApiSignature.UnionCase uc -> ApiSignature.UnionCase (accept_UnionCase visitor uc)
    | ApiSignature.ComputationExpressionBuilder b -> ApiSignature.ComputationExpressionBuilder (accept_ComputationExpressionBuilder visitor b)

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module internal LowType =
  let rec applyVariable source (replacements: Map<TypeVariable, LowType>) = function
    | Variable (s, name) as oldValue when s = source ->
      match Map.tryFind name replacements with
      | Some newValue -> newValue
      | None -> oldValue
    | Generic (baseType, args) ->
      let baseType = applyVariable source replacements baseType
      let args = applyVariableToTargetList source replacements args
      Generic (baseType, args)
    | Tuple x -> Tuple { x with Elements = applyVariableToTargetList source replacements x.Elements }
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
      | Tuple { Elements = xs } -> List.collect f xs
      | Generic (id, args) -> List.concat [ f id; List.collect f args ]
      | TypeAbbreviation t -> List.append (f t.Abbreviation) (f t.Original)
      | Delegate (t, _) -> f t
      | _ -> []
    f x |> List.distinct