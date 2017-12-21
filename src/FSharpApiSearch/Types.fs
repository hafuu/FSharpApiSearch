namespace FSharpApiSearch

open System.Text
open System
open System.Collections.Generic
open MessagePack

type internal DotNetType = System.Type

[<MessagePackObject>]
type TypeVariable = {
  [<Key(0)>]
  Name: string
  [<Key(1)>]
  IsSolveAtCompileTime: bool
}

module TypeVariable =
  let ofString (v: string) =
    if List.exists (fun prefix -> v.StartsWith(prefix)) [ "'"; "^" ] = false then failwithf "wrong variable name: %s" v
    { Name = v.TrimStart(''', '^'); IsSolveAtCompileTime = v.StartsWith("^") }

[<MessagePackObject>]
type NamePart =
  | SymbolName of name:string
  | OperatorName of name:string * compiledName:string
  | WithCompiledName of name:string * compiledName:string

[<MessagePackObject>]
type NameItem = {
  [<Key(0)>]
  Name: NamePart
  [<Key(1)>]
  GenericParameters: TypeVariable list
}

type Name = NameItem list

module internal Name =
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

  let ofString name : Name =
    splitName name |> List.map (fun (name, parameters) -> { Name = SymbolName name; GenericParameters = parameters })
  
  let ofCompiledName name compiledName : Name =
    List.zip (splitName name) (splitName compiledName)
    |> List.map (fun ((name, parameters), (compiledName, _)) ->
      let n = if name <> compiledName then WithCompiledName (name, compiledName) else SymbolName name
      { Name = n; GenericParameters = parameters })

  let ofOperatorString (name: string) : Name =
    let name = ofString name
    let headName =
      match name.Head.Name with
      | SymbolName n -> n
      | _ -> failwith "It is not symbol name"
    let compiledOpName =
      Microsoft.FSharp.Compiler.PrettyNaming.CompileOpName (headName.Trim('(', ')', ' '))
    let head = { name.Head with Name = OperatorName (headName, compiledOpName) }
    head :: name.Tail

  let loadingNameError() = failwith "Loading name at run time is invalid data."

[<RequireQualifiedAccess>]
[<MessagePackObject>]
type VariableSource = Query | Target

[<MessagePackObject>]
type ActualType = {
  [<Key(0)>]
  AssemblyName: string
  [<Key(1)>]
  Name: Name
}
with
  [<IgnoreMember>]
  member this.GenericParameterCount = this.Name.Head.GenericParameters.Length

[<MessagePackObject>]
type UserInputType = {
  [<Key(0)>]
  Name: Name
}
with
  [<IgnoreMember>]
  member this.GenericParameterCount = this.Name.Head.GenericParameters.Length

[<MessagePackObject>]
type TypeInfo =
  | ActualType of ActualType
  | UserInputType of UserInputType

[<MessagePackObject>]
type LoadingName = {
  [<Key(0)>]
  AssemblyName: string
  [<Key(1)>]
  RawName: string
  [<Key(2)>]
  MemberName: Name
}

[<MessagePackObject>]
type LowType =
  | Wildcard of string option
  | Variable of VariableSource * TypeVariable
  | Type of TypeInfo
  | Arrow of Arrow
  | Tuple of TupleType
  | Generic of LowType * LowType list
  | TypeAbbreviation of TypeAbbreviation
  | Delegate of delegateType: LowType * Arrow
  | ByRef of isOut:bool * LowType
  | Subtype of LowType
  | Choice of LowType list
  | LoadingType of LoadingName
and [<MessagePackObject>] TypeAbbreviation = {
  [<Key(0)>]
  Abbreviation: LowType
  [<Key(1)>]
  Original: LowType
}
and [<MessagePackObject>] TupleType = {
  [<Key(0)>]
  Elements: LowType list
  [<Key(1)>]
  IsStruct: bool
}
and Arrow = LowType list * LowType // parameters and return type

module internal Arrow =
  let ofLowTypeList xs =
    let ps = List.take (List.length xs - 1) xs
    let ret = List.last xs
    ps, ret

[<MessagePackObject>]
type Accessibility =
  | Public
  | Private
  //| Internal

[<RequireQualifiedAccess>]
[<MessagePackObject>]
type PropertyKind = Get | Set | GetSet

[<RequireQualifiedAccess>]
[<MessagePackObject>]
type MemberKind =
  | Method
  | Property of PropertyKind
  | Field

[<RequireQualifiedAccess>]
[<MessagePackObject>]
type MemberModifier = Instance | Static

[<MessagePackObject>]
type Parameter = {
  [<Key(0)>]
  Type: LowType
  [<Key(1)>]
  Name: string option
  [<Key(2)>]
  IsOptional: bool
  [<Key(3)>]
  IsParamArray: bool
}

module internal Parameter =
  let ofLowType t = { Name = None; Type = t; IsOptional = false; IsParamArray = false }

type ParameterGroups = Parameter list list
type Function = ParameterGroups * Parameter

module internal ParameterGroups =
  let toLowTypeList (pg: ParameterGroups) =
    [
      for ps in pg do
        match ps with
        | [] -> ()
        | [ one ] -> yield one.Type
        | many -> yield Tuple { Elements = List.map (fun x -> x.Type) many; IsStruct = false }
    ]
    
module internal Function =
  let toArrow (fn: Function) : Arrow =
    let ps, ret = fn
    let ps = ParameterGroups.toLowTypeList ps
    let ret = ret.Type
    ps, ret

[<MessagePackObject>]
type Member = {
  [<Key(0)>]
  Name: string
  [<Key(1)>]
  Kind: MemberKind
  [<Key(2)>]
  GenericParameters: TypeVariable list
  [<Key(3)>]
  Parameters: ParameterGroups
  [<Key(4)>]
  ReturnParameter: Parameter
}
with
  [<IgnoreMember>]
  member this.IsCurried = List.length this.Parameters > 1
  
module internal Member =
  let toFunction m = m.Parameters, m.ReturnParameter
  let toArrow m = Function.toArrow (toFunction m)

[<RequireQualifiedAccess>]
[<MessagePackObject>]
type TypeDefinitionKind =
  | Class
  | Interface
  | Type
  | Union
  | Record
  | Enumeration

[<MessagePackObject>]
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

[<MessagePackObject>]
type TypeConstraint = {
  [<Key(0)>]
  Variables: TypeVariable list
  [<Key(1)>]
  Constraint: Constraint
}

[<MessagePackObject>]
type ConstraintStatus =
  | Satisfy
  | NotSatisfy
  | Dependence of TypeVariable list

[<MessagePackObject>]
type FullTypeDefinition = {
  [<Key(0)>]
  Name: Name
  [<Key(1)>]
  FullName: string
  [<Key(2)>]
  AssemblyName: string
  [<Key(3)>]
  Accessibility: Accessibility
  [<Key(4)>]
  Kind: TypeDefinitionKind
  [<Key(5)>]
  BaseType: LowType option
  [<Key(6)>]
  AllInterfaces: LowType list
  [<Key(7)>]
  GenericParameters: TypeVariable list
  [<Key(8)>]
  TypeConstraints: TypeConstraint list
  [<Key(9)>]
  InstanceMembers: Member list
  [<Key(10)>]
  StaticMembers: Member list
  
  [<Key(11)>]
  ImplicitInstanceMembers: Member list  
  [<Key(12)>]
  ImplicitStaticMembers: Member list

  // pre-compute for type constraints
  [<Key(13)>]
  SupportNull: ConstraintStatus
  [<Key(14)>]
  ReferenceType: ConstraintStatus
  [<Key(15)>]
  ValueType: ConstraintStatus
  [<Key(16)>]
  DefaultConstructor: ConstraintStatus
  [<Key(17)>]
  Equality: ConstraintStatus
  [<Key(18)>]
  Comparison: ConstraintStatus
}
with
  [<IgnoreMember>]
  member internal this.ActualType : ActualType = { AssemblyName = this.AssemblyName; Name = this.Name; }
  [<IgnoreMember>]
  member internal this.LowType =
    match this.GenericParameters with
    | [] -> Type (ActualType this.ActualType)
    | gps ->
      let gps = gps |> List.map (fun v -> Variable (VariableSource.Target, v))
      let id = Type (ActualType this.ActualType)
      Generic (id, gps) 

[<MessagePackObject>]
type TypeAbbreviationDefinition = {
  [<Key(0)>]
  Name: Name
  [<Key(1)>]
  FullName: string
  [<Key(2)>]
  AssemblyName: string
  [<Key(3)>]
  Accessibility: Accessibility
  [<Key(4)>]
  GenericParameters: TypeVariable list
  [<Key(5)>]
  Abbreviated: LowType
  [<Key(6)>]
  Original: LowType
}
with
  [<IgnoreMember>]
  member internal this.ActualType = { AssemblyName = this.AssemblyName; Name = this.Name; }
  [<IgnoreMember>]
  member internal this.TypeAbbreviation =
    let abbreviation =
      match this.GenericParameters with
      | [] -> Type (ActualType this.ActualType)
      | args ->
        let id = Type (ActualType this.ActualType)
        let args = args |> List.map (fun a -> Variable (VariableSource.Target, a))
        Generic (id, args)
    {
      Abbreviation = abbreviation
      Original = this.Original
    }: TypeAbbreviation

[<MessagePackObject>]
type TypeExtension = {
  [<Key(0)>]
  ExistingType: LowType
  [<Key(1)>]
  Declaration: Name
  [<Key(2)>]
  MemberModifier: MemberModifier
  [<Key(3)>]
  Member: Member
}

[<RequireQualifiedAccess>]
[<MessagePackObject>]
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
[<MessagePackObject>]
type ActivePatternKind =
  | ActivePattern
  | PartialActivePattern

[<RequireQualifiedAccess>]
[<MessagePackObject>]
type UnionCaseField = {
  [<Key(0)>]
  Name: string option
  [<Key(1)>]
  Type: LowType
}

[<RequireQualifiedAccess>]
[<MessagePackObject>]
type UnionCase = {
  [<Key(0)>]
  DeclaringType: LowType
  [<Key(1)>]
  Name: string
  [<Key(2)>]
  Fields: UnionCaseField list
}

module UnionCase =
  let toFunction (uc: UnionCase) : Function =
    let fields = uc.Fields |> List.map (fun field -> { Name = field.Name; Type = field.Type; IsOptional = false; IsParamArray = false })
    let ret = Parameter.ofLowType uc.DeclaringType
    [ fields ], ret

[<MessagePackObject>]
type ModuleDefinition = {
  [<Key(0)>]
  Name: Name
  [<Key(1)>]
  AssemblyName: string
  [<Key(2)>]
  Accessibility: Accessibility
}
with
  [<IgnoreMember>]
  member internal this.LowType = Type (ActualType { Name = this.Name; AssemblyName = this.AssemblyName; })

[<MessagePackObject>]
type ComputationExpressionBuilder = {
  [<Key(0)>]
  BuilderType: LowType
  [<Key(1)>]
  ComputationExpressionTypes: LowType list
  [<Key(2)>]
  Syntaxes: string list
}

[<RequireQualifiedAccess>]
[<MessagePackObject>]
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

[<MessagePackObject>]
type ApiName =
  | ApiName of Name
  | LoadingApiName of LoadingName

module ApiName =
  let ofString name = ApiName (Name.ofString name)

  let toName = function
    | ApiName name -> name
    | LoadingApiName _ -> Name.loadingNameError()

[<MessagePackObject>]
type Api = {
  [<Key(0)>]
  Name: ApiName
  [<Key(1)>]
  Signature: ApiSignature
  [<Key(2)>]
  TypeConstraints: TypeConstraint list
  [<Key(3)>]
  Document: string option
}
with
  [<IgnoreMember>]
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

[<MessagePackObject>]
type ApiDictionary = {
  [<Key(0)>]
  AssemblyName: string
  [<Key(1)>]
  Api: Api[]
  [<Key(2)>]
  TypeDefinitions: IDictionary<ActualType, FullTypeDefinition>
  [<Key(3)>]
  TypeAbbreviations: TypeAbbreviationDefinition[]
}
with
  [<IgnoreMember>]
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
  | StartsWith
  | EndsWith
  | Contains
  | Any
  
module NameMatchMethod =
  let ofString (str: string) : string * NameMatchMethod =
    let asteriskNumber = Seq.filter ((=)'*') str |> Seq.length
    if str = "*" then
      str, NameMatchMethod.Any
    elif asteriskNumber = 1 && str.EndsWith("*") then
      str.TrimEnd('*'), NameMatchMethod.StartsWith
    elif asteriskNumber = 1 && str.StartsWith("*") then
      str.TrimStart('*'), NameMatchMethod.EndsWith
    elif asteriskNumber = 2 && str.StartsWith("*") && str.EndsWith("*") then
      str.Trim('*'), NameMatchMethod.Contains
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
  | ByNameOrSignature of ByName list * SignatureQuery
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
  ShortLetterAsVariable: int
  Parallel: OptionStatus
  Language: Language
}

module SearchOptions =
  let defaultOptions = {
    GreedyMatching = Disabled
    RespectNameDifference = Enabled
    IgnoreParameterStyle = Enabled
    IgnoreCase = Enabled
    SwapOrderDepth = 2
    ComplementDepth = 2
    ShortLetterAsVariable = 1
    Parallel = Disabled
    Language = FSharp
  }

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
  
  let internal SwapOrderDepth = { Get = (fun x -> x.SwapOrderDepth); Set = (fun value x -> { x with SwapOrderDepth = max 0 value }) }
  let SwapOrder = { Get = SwapOrderDepth.Get >> intToStatus; Set = statusToInt defaultOptions.SwapOrderDepth >> SwapOrderDepth.Set }
  
  let internal ComplementDepth = { Get = (fun x -> x.ComplementDepth); Set = (fun value x -> { x with ComplementDepth = max 0 value }) }
  let Complement = { Get = ComplementDepth.Get >> intToStatus; Set = statusToInt defaultOptions.ComplementDepth >> ComplementDepth.Set }
  
  let internal ShortLetterAsVariable = { Get = (fun x -> x.ShortLetterAsVariable); Set = (fun value x -> { x with ShortLetterAsVariable = max 0 value }) }
  let SingleLetterAsVariable = { Get = ShortLetterAsVariable.Get >> intToStatus; Set = statusToInt defaultOptions.ShortLetterAsVariable >> ShortLetterAsVariable.Set }

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

  let UnitLoadingType = { AssemblyName = fscore; RawName = typeof<Unit>.FullName; MemberName = [] }
  let UnitName = Name.ofString typeof<Unit>.FullName

  module LoadingType =
    open System.Collections

    let ofDotNetType (t: DotNetType) =
      if t.IsGenericType then failwith "It is not support generic type."
      let assemblyName = t.Assembly.GetName().Name
      { AssemblyName = assemblyName; RawName = t.FullName; MemberName = [] }

  module TypeInfo =
    let ofDotNetType (t: DotNetType) =
      if t.IsGenericType then failwith "It is not support generic type."
      ActualType { AssemblyName = t.Assembly.GetName().Name; Name = Name.ofString t.FullName }

    let private tuple' typeName n =
      let name =
        let genericParams = List.init n (fun n -> { Name = sprintf "T%d" n; IsSolveAtCompileTime = false })
        let ns = { Name = SymbolName "System"; GenericParameters = [] } :: []
        { Name = SymbolName typeName; GenericParameters = genericParams } :: ns
      name

    let private tuple n = tuple' "Tuple" n
    let private valueTuple n = tuple' "ValueTuple" n

    let tupleN n = ActualType { AssemblyName = mscorlib; Name = tuple n }

    let tuples = [1..8] |> List.map (fun n -> tupleN n)

    let valueTupleN n = ActualType { AssemblyName = mscorlib; Name = valueTuple n }

    let valueTuples = [1..8] |> List.map (fun n -> valueTupleN n)

    let byref = ActualType { AssemblyName = fscore; Name = Name.ofString "Microsoft.FSharp.Core.byref<'T>" }

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
    let ofDotNetType (t: DotNetType) = LowType.Type (TypeInfo.ofDotNetType t)
    let Unit = ofDotNetType typeof<Unit>
    let unit =
      let unit = LowType.Type (ActualType { AssemblyName = fscore; Name = Name.ofString "Microsoft.FSharp.Core.unit" })
      TypeAbbreviation { Abbreviation = unit; Original = Unit }

    let Double = ofDotNetType typeof<Double>
    let float =
      let float = LowType.Type (ActualType { AssemblyName = fscore; Name = Name.ofString "Microsoft.FSharp.Core.float" })
      TypeAbbreviation { Abbreviation = float; Original = Double }

    let rec isUnit (x: LowType) =
      match x with
      | Type (ActualType { Name = name }) -> name = UnitName
      | LoadingType t -> t = UnitLoadingType
      | TypeAbbreviation { Original = o } -> isUnit o
      | _ -> false

    let Boolean = ofDotNetType typeof<Boolean>

    let FSharpFunc = LowType.Type (ActualType { AssemblyName= fscore; Name = Name.ofString "Microsoft.FSharp.Core.FSharpFunc<'T, 'U>" })

    module Patterns =
      let (|Unit|_|) x = if isUnit x then Some () else None
      let (|Array|_|) x =
        match x with
        | Generic (Type id, [ elem ]) ->
          match id with
          | ActualType { Name = name }
          | UserInputType { Name = name } ->
            match name with
            | { Name = SymbolName typeName; GenericParameters = [ _ ] } :: _ ->
              if Regex.IsMatch(typeName, arrayRegexPattern) then
                Some (typeName, elem)
              else
                None
            | _ -> None
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

module LowTypeVisitor =
  type Visitor = LowType -> LowType
    
  let accept_Parameter visitor (p: Parameter) = { p with Type = visitor p.Type }

  let accept_ParameterGroups visitor (groups: ParameterGroups) : ParameterGroups = groups |> List.map (List.map (accept_Parameter visitor))

  let accept_Function visitor ((pg, ret): Function) : Function = (accept_ParameterGroups visitor pg, accept_Parameter visitor ret)

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

  let accept_SignatureQuery (visitor: Visitor) = function
    | SignatureQuery.Signature s -> SignatureQuery.Signature (visitor s)
    | SignatureQuery.Wildcard as x -> x

  let accept_ActivePatternQuery (visitor: Visitor) (query: ActivePatternQuery) =
    let apsig =
      match query.Signature with
      | ActivePatternSignature.AnyParameter (x, y) -> ActivePatternSignature.AnyParameter (visitor x, visitor y)
      | ActivePatternSignature.Specified x -> ActivePatternSignature.Specified (visitor x)
    { query with Signature = apsig }

  let accept_ComputationExpressionQuery (visitor: Visitor) (query: ComputationExpressionQuery) =
    { query with Type = visitor query.Type }

  let accept_QueryMethod (visitor: Visitor) = function
    | QueryMethod.ByName (ns, sigQuery) -> QueryMethod.ByName (ns, accept_SignatureQuery visitor sigQuery)
    | QueryMethod.BySignature sigQuery -> QueryMethod.BySignature (accept_SignatureQuery visitor sigQuery)
    | QueryMethod.ByNameOrSignature (ns, sigQuery) -> QueryMethod.ByNameOrSignature (ns, accept_SignatureQuery visitor sigQuery)
    | QueryMethod.ByActivePattern a -> QueryMethod.ByActivePattern (accept_ActivePatternQuery visitor a)
    | QueryMethod.ByComputationExpression ce -> QueryMethod.ByComputationExpression (accept_ComputationExpressionQuery visitor ce)

  let accept_Query (visitor: Visitor) (query: Query) = { query with Method = accept_QueryMethod visitor query.Method }
  
module internal LowType =
  let rec applyVariable source (replacements: Map<TypeVariable, LowType>) = function
    | Wildcard _ as w -> w
    | Variable (s, name) as oldValue when s = source ->
      match Map.tryFind name replacements with
      | Some newValue -> newValue
      | None -> oldValue
    | Variable _ as v -> v
    | Type _ as i -> i
    | Generic (baseType, args) ->
      let baseType = applyVariable source replacements baseType
      let args = applyVariableToTargetList source replacements args
      Generic (baseType, args)
    | Tuple x -> Tuple { x with Elements = applyVariableToTargetList source replacements x.Elements }
    | Arrow arrow -> Arrow (applyVariableToArrow source replacements arrow)
    | TypeAbbreviation t -> TypeAbbreviation { Abbreviation = applyVariable source replacements t.Abbreviation; Original = applyVariable source replacements t.Original }
    | Delegate (t, arrow) ->
      let delegateType = applyVariable source replacements t
      let arrow = applyVariableToArrow source replacements arrow
      Delegate (delegateType, arrow)
    | ByRef (isOut, t) -> ByRef (isOut, applyVariable source replacements t)
    | Subtype t -> Subtype (applyVariable source replacements t)
    | Choice xs -> Choice (applyVariableToTargetList source replacements xs)
    | LoadingType _ as l -> l 
  and applyVariableToTargetList source replacements xs = xs |> List.map (applyVariable source replacements)
  and applyVariableToArrow source replacements arrow =
    let ps, ret = arrow
    (applyVariableToTargetList source replacements ps, applyVariable source replacements ret)

  let collectWildcardGroup x =
    let result = ResizeArray()
    let add x = result.Add(x)
    let rec f = function
      | Wildcard (Some _) as w -> add w
      | Wildcard None -> ()
      | Variable _ -> ()
      | Type _ -> ()
      | Arrow (ps, ret) -> List.iter f ps; f ret
      | Tuple { Elements = xs } -> List.iter f xs
      | Generic (id, args) -> f id; List.iter f args
      | TypeAbbreviation t -> f t.Original
      | Delegate (t, _) -> f t
      | ByRef (_, t) -> f t
      | Subtype t -> f t
      | Choice xs -> List.iter f xs
      | LoadingType _ -> Name.loadingNameError()
    f x
    result.ToArray()

  let collectVariables x =
    let result = ResizeArray()
    let add x = result.Add(x)
    let rec f = function
      | Wildcard _ -> ()
      | Variable _ as v -> add v
      | Type _ -> ()
      | Arrow (ps, ret) -> List.iter f ps; f ret
      | Tuple { Elements = xs } -> List.iter f xs
      | Generic (id, args) -> f id; List.iter f args
      | TypeAbbreviation t -> f t.Original
      | Delegate (t, _) -> f t
      | ByRef (_, t) -> f t
      | Subtype t -> f t
      | Choice xs -> List.iter f xs
      | LoadingType _ -> Name.loadingNameError()
    f x
    result.ToArray()

  let collectVariableOrWildcardGroup x =
    let result = ResizeArray()
    let add x = result.Add(x)
    let rec f = function
      | Wildcard (Some _) as w -> add w
      | Wildcard None -> ()
      | Variable _ as v -> add v
      | Type _ -> ()
      | Arrow (ps, ret) -> List.iter f ps; f ret
      | Tuple { Elements = xs } -> List.iter f xs
      | Generic (id, args) -> f id; List.iter f args
      | TypeAbbreviation t -> f t.Original
      | Delegate (t, _) -> f t
      | ByRef (_, t) -> f t
      | Subtype t -> f t
      | Choice xs -> List.iter f xs
      | LoadingType _ -> Name.loadingNameError()
    f x
    result.ToArray()