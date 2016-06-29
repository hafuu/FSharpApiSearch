namespace FSharpApiSearch

type TypeVariable = string

type NameItem = {
  FSharpName: string
  InternalFSharpName: string
  // CompiledName: string
  GenericParametersForDisplay: TypeVariable list
}

type FullName = string
type DisplayName = NameItem list

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module DisplayName =
  open System.Text.RegularExpressions

  let ofString (name: string) =
    name.Split('.')
    |> Seq.map (fun n ->
      if n.Contains("<") then
        let xs = n.Split([| '<' |], 2)
        let name = xs.[0]
        let args = [ for m in Regex.Matches(xs.[1], @"'(\w+)") -> m.Groups.[1].Value ]
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
module Name =
  let displayNameOfString (name: string) = DisplayName (DisplayName.ofString name)
  let displayNameOfOperatorString (name: string) = DisplayName (DisplayName.ofOperatorString name)

  let loadingNameError() = failwith "Loading name at run time is invalid data."

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

type Member = {
  Name: string
  Kind: MemberKind
  GenericParameters: TypeVariable list
  Arguments: LowType list
  IsCurried: bool
  ReturnType: LowType
}

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
  member this.FullIdentity = { AssemblyName = this.AssemblyName; Name = DisplayName this.Name; GenericParameterCount = this.GenericParameters.Length }

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
  member this.FullIdentity = { AssemblyName = this.AssemblyName; Name = DisplayName this.Name; GenericParameterCount = this.GenericParameters.Length }
  member this.TypeAbbreviation =
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

[<RequireQualifiedAccess>]
type ActivePatternKind =
  | ActivePattern
  | PartialActivePattern

[<RequireQualifiedAccess>]
type ApiSignature =
  | ModuleValue of LowType
  | ModuleFunction of LowType list
  | ActivePatten of ActivePatternKind * LowType
  | InstanceMember of LowType * Member
  | StaticMember of LowType * Member
  | Constructor of LowType * Member
  | FullTypeDefinition of FullTypeDefinition
  | TypeAbbreviation of TypeAbbreviationDefinition
  | TypeExtension of TypeExtension
  | ExtensionMember of Member

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
    | ApiSignature.FullTypeDefinition _ -> failwith "not implemented"
    | ApiSignature.TypeAbbreviation _ -> failwith "not implemeneted"
    | ApiSignature.TypeExtension t -> ApiKind.Member (t.MemberModifier, t.Member.Kind)
    | ApiSignature.ExtensionMember _ -> ApiKind.ExtensionMember

type ApiDictionary = {
  AssemblyName: string
  Api: Api[]
  TypeDefinitions: FullTypeDefinition[]
  TypeAbbreviations: TypeAbbreviationDefinition[]
}

[<RequireQualifiedAccess>]
type ActivePatternSignature =
  | AnyParameter of LowType * LowType
  | Specified of LowType
[<RequireQualifiedAccess>]
type ActivePatternQuery = {
  Kind: ActivePatternKind
  Signature: ActivePatternSignature
}

[<RequireQualifiedAccess>]
type SignatureQuery =
  | Wildcard
  | Signature of LowType
  | InstanceMember of Receiver: LowType * Arguments: LowType list * ReturnType: LowType

[<RequireQualifiedAccess>]
type QueryMethod =
  | ByName of string list * SignatureQuery
  | BySignature of SignatureQuery
  | ByActivePattern of ActivePatternQuery

[<RequireQualifiedAccess>]
type Query = {
  OriginalString: string
  Method: QueryMethod
}

type OptionStatus = Enabled | Disabled

type SearchOptions = {
  SimilaritySearching: OptionStatus
  StrictQueryVariable: OptionStatus
  IgnoreArgumentStyle: OptionStatus
}

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module SearchOptions =
  let defaultOptions = { SimilaritySearching = Disabled; StrictQueryVariable = Enabled; IgnoreArgumentStyle = Enabled }

type Result = {
  Api: Api
  Distance: int
}

module SpecialTypes =
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

    let IntPtr = ofDotNetType typeof<IntPtr>
    let UIntPtr = ofDotNetType typeof<UIntPtr>

    let IComparable = ofDotNetType typeof<IComparable>
    let IStructuralComparable = ofDotNetType typeof<IStructuralComparable>

  module FullIdentity =
    open System.Collections

    let ofDotNetType (t: Type) =
      if t.IsGenericType then failwith "It is not support generic type."
      { AssemblyName = t.Assembly.GetName().Name; Name = Name.displayNameOfString t.FullName; GenericParameterCount = 0 }

    let tupleName n =
      let name = { FSharpName = "Tuple"; InternalFSharpName = "Tuple"; GenericParametersForDisplay = List.init n (sprintf "T%d") } :: { FSharpName = "System"; InternalFSharpName = "System"; GenericParametersForDisplay = [] } :: []
      DisplayName name

    let Boolean = ofDotNetType typeof<Boolean>
    let Byte = ofDotNetType typeof<Byte>
    let Char = ofDotNetType typeof<Char>
    let Decimal = ofDotNetType typeof<Decimal>
    let Double = ofDotNetType typeof<Double>
    let Single = ofDotNetType typeof<Single>
    let Int32 = ofDotNetType typeof<Int32>
    let Int16 = ofDotNetType typeof<Int16>
    let Int64 = ofDotNetType typeof<Int64>
    let IntPtr = ofDotNetType typeof<IntPtr>
    let SByte = ofDotNetType typeof<SByte>
    let String = ofDotNetType typeof<String>
    let UInt16 = ofDotNetType typeof<UInt16>
    let UInt32 = ofDotNetType typeof<UInt32>
    let UInt64 = ofDotNetType typeof<UInt64>
    let UIntPtr = ofDotNetType typeof<UIntPtr>

  module Identity =
    let ofDotNetType (t: Type) = FullIdentity (FullIdentity.ofDotNetType t)

    let tupleN n = FullIdentity { AssemblyName = mscorlib; Name = FullIdentity.tupleName n; GenericParameterCount = n }

  module LowType =
    let ofDotNetType (t: Type) = LowType.Identity (Identity.ofDotNetType t)
    let Unit = ofDotNetType typeof<Unit>
    let unit =
      let unit = LowType.Identity (FullIdentity { AssemblyName = fscore; Name = Name.displayNameOfString "Microsoft.FSharp.Core.unit"; GenericParameterCount = 0 })
      TypeAbbreviation { Abbreviation = unit; Original = Unit }

    let Boolean = ofDotNetType typeof<Boolean>
    let Byte = ofDotNetType typeof<Byte>
    let Char = ofDotNetType typeof<Char>
    let Decimal = ofDotNetType typeof<Decimal>
    let Double = ofDotNetType typeof<Double>
    let Single = ofDotNetType typeof<Single>
    let Int32 = ofDotNetType typeof<Int32>
    let Int16 = ofDotNetType typeof<Int16>
    let Int64 = ofDotNetType typeof<Int64>
    let IntPtr = ofDotNetType typeof<IntPtr>
    let SByte = ofDotNetType typeof<SByte>
    let String = ofDotNetType typeof<String>
    let UInt16 = ofDotNetType typeof<UInt16>
    let UInt32 = ofDotNetType typeof<UInt32>
    let UInt64 = ofDotNetType typeof<UInt64>
    let UIntPtr = ofDotNetType typeof<UIntPtr>

    let float =
      let float = LowType.Identity (FullIdentity { AssemblyName = fscore; Name = Name.displayNameOfString "Microsoft.FSharp.Core.float"; GenericParameterCount = 0 })
      TypeAbbreviation { Abbreviation = float; Original = Double }

    let rec isUnit (x: LowType) =
      match x with
      | Identity (FullIdentity { Name = name }) -> name = UnitLoadingName || name = UnitDisplayName
      | TypeAbbreviation { Original = o } -> isUnit o
      | _ -> false

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

  let printDisplayName = function
    | [] -> "<empty>"
    | ns ->
      let print (x: NameItem) =
        match x.GenericParametersForDisplay with
        | [] -> x.FSharpName
        | args -> sprintf "%s<%s>" x.FSharpName (args |> List.map (sprintf "'%s") |> String.concat ", ")
      ns |> Seq.map print |> Seq.rev |> String.concat "."

  let printName = function
    | LoadingName (_, n1, n2) ->
      match n2 with
      | [] -> n1
      | n2 -> n1 + "." + printDisplayName n2
    | DisplayName n -> printDisplayName n

  let printDisplayName_short = function
    | [] -> "<empty>"
    | n :: _ -> n.FSharpName

  let printName_short = function
    | LoadingName (_, n1, n2) ->
      match n2 with
      | [] -> n1
      | n2 -> printDisplayName_short n2
    | DisplayName n -> printDisplayName_short n

  let printIdentity = function
    | FullIdentity i -> printName_short i.Name
    | PartialIdentity i -> printDisplayName_short i.Name

  let printVariableSource = function
    | VariableSource.Query -> "q"
    | VariableSource.Target -> "t"

  let printVariable isDebug source v =
    if isDebug then
      sprintf "'%s_%s" (printVariableSource source) v
    else
      sprintf "'%s" v

  let rec printLowType isDebug = function
    | Wildcard name ->
      match name with
      | Some n -> sprintf "?%s" n
      | None -> "?"
    | Variable (source, v) -> printVariable isDebug source v
    | Identity i -> printIdentity i
    | Arrow xs -> printArrow isDebug xs
    | Tuple xs -> printTuple isDebug xs
    | LowType.Patterns.Array (name, elem) ->
      let paramPart =
        match elem with
        | Tuple _ | Arrow _ ->
          sprintf "(%s)" (printLowType isDebug elem)
        | _ -> printLowType isDebug elem
      paramPart + name
    | Generic (id, args) ->
      let args = args |> Seq.map (printLowType isDebug) |> String.concat ", "
      sprintf "%s<%s>" (printLowType isDebug id) args
    | TypeAbbreviation t -> printLowType isDebug t.Abbreviation
  and printArrow isDebug xs =
    xs
    |> Seq.map (function
      | Arrow _ as a -> sprintf "(%s)" (printLowType isDebug a)
      | x -> printLowType isDebug x)
    |> String.concat " -> "
  and printTuple isDebug xs =
    xs
    |> Seq.map (function
      | Tuple _ as t -> sprintf "(%s)" (printLowType isDebug t)
      | x -> printLowType isDebug x)
    |> String.concat " * "

  let printMember isDebug (m: Member) =
    let argPart =
      match m.Arguments with
      | [ Tuple _ as t ] -> Some (sprintf "(%s)" (printLowType isDebug t))
      | [] -> None
      | args ->
        if m.IsCurried then
          Some (printLowType isDebug (Arrow args))
        else
          Some (printLowType isDebug (Tuple args))
    match argPart with
    | Some argPart -> sprintf "%s -> %s" argPart (printLowType isDebug m.ReturnType)
    | None -> printLowType isDebug m.ReturnType

  let printConstraint isDebug (c: TypeConstraint) =
    let variableSource = VariableSource.Target
    let variablePart =
      match c.Variables with
      | [ v ] -> printVariable isDebug variableSource v
      | vs -> sprintf "(%s)" (List.map (printVariable isDebug variableSource) vs |> String.concat " or ")
    let constraintPart =
      match c.Constraint with
      | Constraint.SubtypeConstraints s -> sprintf ":> %s" (printLowType isDebug s)
      | Constraint.NullnessConstraints -> ": null"
      | Constraint.MemberConstraints (modifier, member') ->
        let modifierPart =
          match modifier with
          | MemberModifier.Static -> "static member"
          | MemberModifier.Instance -> "member"
        sprintf ": (%s %s : %s)" modifierPart member'.Name (printMember isDebug member')
      | Constraint.DefaultConstructorConstraints -> let v = printVariable isDebug variableSource (c.Variables.Head) in sprintf ": (new : unit -> %s)" v
      | Constraint.ValueTypeConstraints -> ": struct"
      | Constraint.ReferenceTypeConstraints -> ": not struct"
      | Constraint.EnumerationConstraints -> ": enum"
      | Constraint.DelegateConstraints -> ": delegate"
      | Constraint.UnmanagedConstraints -> ": unmanaged"
      | Constraint.EqualityConstraints -> ": equality"
      | Constraint.ComparisonConstraints -> ": comparison"
    sprintf "%s %s" variablePart constraintPart
    
  let printFullTypeDefinition isDebug (x: FullTypeDefinition) =
    match x.GenericParameters with
    | [] -> sprintf "%s.%s" x.AssemblyName (printDisplayName x.Name)
    | args -> sprintf "%s.%s<%s>" x.AssemblyName (printDisplayName x.Name) (args |> Seq.map (printVariable isDebug VariableSource.Target) |> String.concat ", ")

  let pringTypeAbbreviation isDebug (x: TypeAbbreviationDefinition) =
    match x.GenericParameters with
    | [] -> sprintf "%s.%s" x.AssemblyName (printDisplayName x.Name)
    | args -> sprintf "%s.%s<%s>" x.AssemblyName (printDisplayName x.Name) (args |> Seq.map (printVariable isDebug VariableSource.Target) |> String.concat ", ")

  let printApiSignature isDebug = function
    | ApiSignature.ModuleValue t -> printLowType isDebug t
    | ApiSignature.ModuleFunction xs -> printLowType isDebug (Arrow xs)
    | ApiSignature.ActivePatten (_, t) -> printLowType isDebug t
    | ApiSignature.InstanceMember (declaringType, m) ->
      if isDebug then
        sprintf "%s => %s" (printLowType isDebug declaringType) (printMember isDebug m)
      else
        printMember isDebug m
    | ApiSignature.StaticMember (_, m) -> printMember isDebug m
    | ApiSignature.Constructor (_, m) -> printMember isDebug m
    | ApiSignature.FullTypeDefinition x -> printFullTypeDefinition isDebug x
    | ApiSignature.TypeAbbreviation t -> pringTypeAbbreviation isDebug t
    | ApiSignature.TypeExtension t ->
      if isDebug then
        sprintf "%s => %s" (printLowType isDebug t.ExistingType) (printMember isDebug t.Member)
      else
        printMember isDebug t.Member
    | ApiSignature.ExtensionMember m -> printMember isDebug m

type Name with
  member this.Print() = Print.printName this

type LowType with
  member this.Print() = Print.printLowType false this
  member this.Debug() = Print.printLowType true this

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
      sprintf "%s (%s)" (Print.printApiKind this.Kind) (Print.printDisplayName declaration)
    | _ -> Print.printApiKind this.Kind

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Identity =
  let private testDisplayName (xs: DisplayName) (ys: DisplayName) =
    Seq.zip xs ys |> Seq.forall (fun (x, y) -> x.InternalFSharpName = y.InternalFSharpName && x.GenericParametersForDisplay.Length = y.GenericParametersForDisplay.Length)

  let testFullIdentity (x: FullIdentity) (y: FullIdentity) =
    match x.Name, y.Name with
    | (LoadingName _, _) | (_, LoadingName _) -> Name.loadingNameError()
    | DisplayName xName, DisplayName yName -> x.AssemblyName = y.AssemblyName && testDisplayName xName yName

  let private testPartialAndFullIdentity (partial: PartialIdentity) (full: FullIdentity) =
    let testNameItem (p: NameItem, f: NameItem) =
      match p.GenericParametersForDisplay, f.GenericParametersForDisplay with
      | [], _ -> p.InternalFSharpName = f.InternalFSharpName
      | _ -> p.InternalFSharpName = f.InternalFSharpName && p.GenericParametersForDisplay.Length = f.GenericParametersForDisplay.Length
    match full.Name with
    | DisplayName fullName ->
      partial.GenericParameterCount = full.GenericParameterCount
      && (Seq.zip partial.Name fullName |> Seq.forall testNameItem)
    | LoadingName _ -> Name.loadingNameError()

  let sameName x y =
    match x, y with
    | FullIdentity left, FullIdentity right -> testFullIdentity left right
    | FullIdentity full, PartialIdentity partial
    | PartialIdentity partial, FullIdentity full -> testPartialAndFullIdentity partial full
    | PartialIdentity left, PartialIdentity right ->
      left.GenericParameterCount = right.GenericParameterCount
      && testDisplayName left.Name right.Name

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module LowType =
  let debug (x: LowType) = x.Debug()

  let rec applyVariable source (replacements: Map<TypeVariable, LowType>) = function
    | Variable (s, name) as oldValue when s = source ->
      match Map.tryFind name replacements with
      | Some newValue -> newValue
      | None -> oldValue
    | Generic (baseType, args) ->
      let baseType = applyVariable source replacements baseType
      let args = replaceTargetVariablesList source replacements args
      Generic (baseType, args)
    | Tuple xs -> Tuple (replaceTargetVariablesList source replacements xs)
    | Arrow xs -> Arrow (replaceTargetVariablesList source replacements xs)
    | TypeAbbreviation t -> TypeAbbreviation { Abbreviation = applyVariable source replacements t.Abbreviation; Original = applyVariable source replacements t.Original }
    | other -> other
  and private replaceTargetVariablesList source replacements xs = xs |> List.map (applyVariable source replacements)

  let collectVariables x =
    let rec f = function
      | Variable _ as v -> [ v ]
      | Arrow xs -> List.collect f xs
      | Tuple xs -> List.collect f xs
      | Generic (id, args) -> List.concat [ f id; List.collect f args ]
      | TypeAbbreviation t -> List.append (f t.Abbreviation) (f t.Original)
      | _ -> []
    f x |> List.distinct

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ApiSignature =
  let debug (x: ApiSignature) = x.Debug()
  let print (x: ApiSignature) = x.Print()

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module TypeConstraint =
  let debug (x: TypeConstraint) = x.Debug()

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module FullTypeDefinition =
  let debug (x: FullTypeDefinition) = x.Debug()