

namespace System
  module internal AssemblyVersionInformation = begin
    [<LiteralAttribute ()>]
    val AssemblyTitle : string
    [<LiteralAttribute ()>]
    val AssemblyProduct : string
    [<LiteralAttribute ()>]
    val AssemblyDescription : string
    [<LiteralAttribute ()>]
    val AssemblyVersion : string
    [<LiteralAttribute ()>]
    val AssemblyFileVersion : string
    [<LiteralAttribute ()>]
    val AssemblyConfiguration : string
  end



namespace FSharpApiSearch
  type Lens<'a,'b> =
    {Get: 'a -> 'b;
     Set: 'b -> 'a -> 'a;}
  module internal OptionModule = begin
    type OptionBuilder =
      class
        new : unit -> OptionBuilder
        member Bind : x:'c option * f:('c -> 'd option) -> 'd option
        member Return : x:'b -> 'b option
        member ReturnFrom : x:'a -> 'a
      end
    val option : OptionBuilder
  end
  module internal Seq = begin
    val foldOptionMapping : f:('a -> 'b option) -> xs:seq<'a> -> seq<'b> option
  end
  module internal String = begin
    val equalsWithComparer :
      cmp:System.StringComparer -> x:string -> y:string -> bool
  end
  module internal Extensions = begin
    type StringBuilder with
      member
        Append : print:(System.Text.StringBuilder -> System.Text.StringBuilder) ->
                   System.Text.StringBuilder
    type StringBuilder with
      member
        AppendJoin : sep:string * xs:seq<'a> *
                     print:('a -> System.Text.StringBuilder ->
                              System.Text.StringBuilder) ->
                       System.Text.StringBuilder
    type StringBuilder with
      member
        AppendJoin : sep:string * xs:seq<string> -> System.Text.StringBuilder
  end
  module internal IDictionary = begin
    val empty<'k,'v when 'k : equality> :
      System.Collections.Generic.IDictionary<'k,'v> when 'k : equality
  end

namespace FSharpApiSearch
  module internal Hack = begin
    val genericArguments :
      t:Compiler.SourceCodeServices.FSharpType ->
        Compiler.SourceCodeServices.FSharpType list
    val inferredFloatFullName : string option
    val measureOneFullName : string option
    val isFloat : t:Compiler.SourceCodeServices.FSharpType -> bool
    val isAbbreviation : t:Compiler.SourceCodeServices.FSharpType -> bool
    val isMeasure : t:Compiler.SourceCodeServices.FSharpType -> bool
    val isUnitOnlyParameter :
      x:Compiler.SourceCodeServices.FSharpMemberOrFunctionOrValue -> bool
    val isTupleType : t:Compiler.SourceCodeServices.FSharpType -> bool
    val isStructTupleType : t:Compiler.SourceCodeServices.FSharpType -> bool
  end

namespace FSharpApiSearch
  [<MessagePack.MessagePackObject (false)>]
  type TypeVariable =
    {Name: string;
     IsSolveAtCompileTime: bool;}
  module TypeVariable = begin
    val ofString : v:string -> TypeVariable
  end
  [<MessagePack.MessagePackObject (false)>]
  type NamePart =
    | SymbolName of name: string
    | OperatorName of name: string * compiledName: string
    | WithCompiledName of name: string * compiledName: string
  [<MessagePack.MessagePackObject (false)>]
  type DisplayNameItem =
    {Name: NamePart;
     GenericParameters: TypeVariable list;}
  type DisplayName = DisplayNameItem list
  module internal DisplayName = begin
    val private splitName : name:string -> (string * TypeVariable list) list
    val ofString : name:string -> DisplayNameItem list
    val ofString2 : name:string -> compiledName:string -> DisplayNameItem list
    val ofOperatorString : name:string -> DisplayNameItem list
  end
  [<MessagePack.MessagePackObject (false)>]
  type Name =
    | LoadingName of assemblyName: string * accessPath: string * DisplayName
    | DisplayName of DisplayName
  module internal Name = begin
    val ofString : name:string -> Name
    val ofCompiledName : name:string -> compiledName:string -> Name
    val ofOperatorName : name:string -> Name
    val loadingNameError : unit -> 'a
    val toDisplayName : _arg1:Name -> DisplayName
  end
  [<MessagePack.MessagePackObject (false)>]
  type PartialIdentity =
    {Name: DisplayName;
     GenericParameterCount: int;}
  [<MessagePack.MessagePackObject (false)>]
  type FullIdentity =
    {AssemblyName: string;
     Name: Name;
     GenericParameterCount: int;}
  [<MessagePack.MessagePackObject (false)>]
  type Identity =
    | PartialIdentity of PartialIdentity
    | FullIdentity of FullIdentity
  [<RequireQualifiedAccessAttribute (); MessagePack.MessagePackObject (false)>]
  type VariableSource =
    | Query
    | Target
  [<MessagePack.MessagePackObject (false)>]
  type LowType =
    | Wildcard of string option
    | Variable of VariableSource * TypeVariable
    | Identity of Identity
    | Arrow of LowType list
    | Tuple of TupleType
    | Generic of LowType * LowType list
    | TypeAbbreviation of TypeAbbreviation
    | Delegate of delegateType: LowType * LowType list
    | ByRef of isOut: bool * LowType
    | Flexible of LowType
    | Choice of LowType list
  [<MessagePack.MessagePackObject (false)>]
  and TypeAbbreviation =
    {Abbreviation: LowType;
     Original: LowType;}
  [<MessagePack.MessagePackObject (false)>]
  and TupleType =
    {Elements: LowType list;
     IsStruct: bool;}
  [<MessagePack.MessagePackObject (false)>]
  type Accessibility =
    | Public
    | Private
  [<RequireQualifiedAccessAttribute (); MessagePack.MessagePackObject (false)>]
  type PropertyKind =
    | Get
    | Set
    | GetSet
  [<RequireQualifiedAccessAttribute (); MessagePack.MessagePackObject (false)>]
  type MemberKind =
    | Method
    | Property of PropertyKind
    | Field
  [<RequireQualifiedAccessAttribute (); MessagePack.MessagePackObject (false)>]
  type MemberModifier =
    | Instance
    | Static
  [<MessagePack.MessagePackObject (false)>]
  type Parameter =
    {Type: LowType;
     Name: string option;
     IsOptional: bool;
     IsParamArray: bool;}
  module internal Parameter = begin
    val ofLowType : t:LowType -> Parameter
  end
  type ParameterGroups = Parameter list list
  type Function = Parameter list list
  module internal Function = begin
    val toLowTypeList : fn:Parameter list list -> LowType list
    val toArrow : fn:Parameter list list -> LowType
  end
  [<MessagePack.MessagePackObject (false)>]
  type Member =
    {Name: string;
     Kind: MemberKind;
     GenericParameters: TypeVariable list;
     Parameters: ParameterGroups;
     ReturnParameter: Parameter;}
    with
      [<MessagePack.IgnoreMember ()>]
      member IsCurried : bool
    end
  module internal Member = begin
    val toArrow : m:Member -> LowType
    val toFunction : m:Member -> Parameter list list
  end
  [<RequireQualifiedAccessAttribute (); MessagePack.MessagePackObject (false)>]
  type TypeDefinitionKind =
    | Class
    | Interface
    | Type
    | Union
    | Record
    | Enumeration
  [<MessagePack.MessagePackObject (false)>]
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
  [<MessagePack.MessagePackObject (false)>]
  type TypeConstraint =
    {Variables: TypeVariable list;
     Constraint: Constraint;}
  [<MessagePack.MessagePackObject (false)>]
  type ConstraintStatus =
    | Satisfy
    | NotSatisfy
    | Dependence of TypeVariable list
  [<MessagePack.MessagePackObject (false)>]
  type FullTypeDefinition =
    {Name: DisplayName;
     FullName: string;
     AssemblyName: string;
     Accessibility: Accessibility;
     Kind: TypeDefinitionKind;
     BaseType: LowType option;
     AllInterfaces: LowType list;
     GenericParameters: TypeVariable list;
     TypeConstraints: TypeConstraint list;
     InstanceMembers: Member list;
     StaticMembers: Member list;
     ImplicitInstanceMembers: Member list;
     ImplicitStaticMembers: Member list;
     SupportNull: ConstraintStatus;
     ReferenceType: ConstraintStatus;
     ValueType: ConstraintStatus;
     DefaultConstructor: ConstraintStatus;
     Equality: ConstraintStatus;
     Comparison: ConstraintStatus;}
    with
      [<MessagePack.IgnoreMember ()>]
      member internal FullIdentity : FullIdentity
      [<MessagePack.IgnoreMember ()>]
      member internal LowType : LowType
    end
  [<MessagePack.MessagePackObject (false)>]
  type TypeAbbreviationDefinition =
    {Name: DisplayName;
     FullName: string;
     AssemblyName: string;
     Accessibility: Accessibility;
     GenericParameters: TypeVariable list;
     Abbreviated: LowType;
     Original: LowType;}
    with
      [<MessagePack.IgnoreMember ()>]
      member internal FullIdentity : FullIdentity
      [<MessagePack.IgnoreMember ()>]
      member internal TypeAbbreviation : TypeAbbreviation
    end
  [<MessagePack.MessagePackObject (false)>]
  type TypeExtension =
    {ExistingType: LowType;
     Declaration: DisplayName;
     MemberModifier: MemberModifier;
     Member: Member;}
  [<RequireQualifiedAccessAttribute (); MessagePack.MessagePackObject (false)>]
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
  [<RequireQualifiedAccessAttribute (); MessagePack.MessagePackObject (false)>]
  type ActivePatternKind =
    | ActivePattern
    | PartialActivePattern
  [<RequireQualifiedAccessAttribute (); MessagePack.MessagePackObject (false)>]
  type UnionCaseField =
    {Name: string option;
     Type: LowType;}
  [<RequireQualifiedAccessAttribute (); MessagePack.MessagePackObject (false)>]
  type UnionCase =
    {DeclaringType: LowType;
     Name: string;
     Fields: UnionCaseField list;}
  module UnionCase = begin
    val toFunction : uc:UnionCase -> Parameter list list
  end
  [<MessagePack.MessagePackObject (false)>]
  type ModuleDefinition =
    {Name: DisplayName;
     AssemblyName: string;
     Accessibility: Accessibility;}
    with
      [<MessagePack.IgnoreMember ()>]
      member internal LowType : LowType
    end
  [<MessagePack.MessagePackObject (false)>]
  type ComputationExpressionBuilder =
    {BuilderType: LowType;
     ComputationExpressionTypes: LowType list;
     Syntaxes: string list;}
  [<RequireQualifiedAccessAttribute (); MessagePack.MessagePackObject (false)>]
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
    | TypeExtension of TypeExtension
    | ExtensionMember of Member
    | UnionCase of UnionCase
    | ComputationExpressionBuilder of ComputationExpressionBuilder
  [<MessagePack.MessagePackObject (false)>]
  type Api =
    {Name: Name;
     Signature: ApiSignature;
     TypeConstraints: TypeConstraint list;
     Document: string option;}
    with
      [<MessagePack.IgnoreMember ()>]
      member Kind : ApiKind
    end
  [<MessagePack.MessagePackObject (false)>]
  type ApiDictionary =
    {AssemblyName: string;
     Api: Api [];
     TypeDefinitions:
       System.Collections.Generic.IDictionary<FullIdentity,FullTypeDefinition>;
     TypeAbbreviations: TypeAbbreviationDefinition [];}
    with
      [<MessagePack.IgnoreMember ()>]
      member PublicApiNumber : int
    end
  [<RequireQualifiedAccessAttribute ()>]
  type ActivePatternSignature =
    | AnyParameter of LowType * LowType
    | Specified of LowType
  [<RequireQualifiedAccessAttribute ()>]
  type ActivePatternQuery =
    {Kind: ActivePatternKind;
     Signature: ActivePatternSignature;}
  [<RequireQualifiedAccessAttribute ()>]
  type ComputationExpressionQuery =
    {Syntaxes: string list;
     Type: LowType;}
  [<RequireQualifiedAccessAttribute ()>]
  type SignatureQuery =
    | Wildcard
    | Signature of LowType
  [<RequireQualifiedAccessAttribute ()>]
  type NameMatchMethod =
    | StringCompare
    | Regex
    | StartsWith
    | EndsWith
    | Contains
    | Any
  module NameMatchMethod = begin
    val ofString : str:string -> string * NameMatchMethod
  end
  type ByName =
    {Expected: string;
     GenericParameters: string list;
     MatchMethod: NameMatchMethod;}
  [<RequireQualifiedAccessAttribute ()>]
  type QueryMethod =
    | ByName of ByName list * SignatureQuery
    | BySignature of SignatureQuery
    | ByActivePattern of ActivePatternQuery
    | ByComputationExpression of ComputationExpressionQuery
  [<RequireQualifiedAccessAttribute ()>]
  type Query =
    {OriginalString: string;
     Method: QueryMethod;}
  type OptionStatus =
    | Enabled
    | Disabled
  type Language =
    | FSharp
    | CSharp
  module Language = begin
    val tryParse : str:string -> Language option
  end
  type SearchOptions =
    internal {GreedyMatching: OptionStatus;
              RespectNameDifference: OptionStatus;
              IgnoreParameterStyle: OptionStatus;
              IgnoreCase: OptionStatus;
              SwapOrderDepth: int;
              ComplementDepth: int;
              Parallel: OptionStatus;
              Language: Language;}
  module SearchOptions = begin
    val defaultOptions : SearchOptions
    val private statusToInt : enabledValue:int -> _arg1:OptionStatus -> int
    val private intToStatus : _arg1:int -> OptionStatus
    val GreedyMatching : Lens<SearchOptions,OptionStatus>
    val RespectNameDifference : Lens<SearchOptions,OptionStatus>
    val IgnoreParameterStyle : Lens<SearchOptions,OptionStatus>
    val IgnoreCase : Lens<SearchOptions,OptionStatus>
    val SwapOrderDepth : Lens<SearchOptions,int>
    val SwapOrder : Lens<SearchOptions,OptionStatus>
    val ComplementDepth : Lens<SearchOptions,int>
    val Complement : Lens<SearchOptions,OptionStatus>
    val Parallel : Lens<SearchOptions,OptionStatus>
    val Language : Lens<SearchOptions,Language>
  end
  type Result =
    {Api: Api;
     AssemblyName: string;
     Distance: int;}
  module internal SpecialTypes = begin
    val arrayRegexPattern : string
    val mscorlib : string
    val fscore : string
    val UnitLoadingName : Name
    val UnitDisplayName : Name
    module LoadingFullIdentity = begin
      val ofDotNetType : t:System.Type -> FullIdentity
    end
    module FullIdentity = begin
      val ofDotNetType : t:System.Type -> FullIdentity
      val private tuple' : typeName:string -> n:int -> Name
      val tupleName : n:int -> Name
      val valueTupleName : n:int -> Name
    end
    module Identity = begin
      val ofDotNetType : t:System.Type -> Identity
      val tupleN : n:int -> Identity
      val tuples : Identity list
      val valueTupleN : n:int -> Identity
      val valueTuples : Identity list
      val byref : Identity
      module CSharp = begin
        val aliases : (string * Identity) list
      end
    end
    module LowType = begin
      val ofDotNetType : t:System.Type -> LowType
      val Unit : LowType
      val unit : LowType
      val Double : LowType
      val float : LowType
      val isUnit : x:LowType -> bool
      val Boolean : LowType
      val FSharpFunc : LowType
      module Patterns = begin
        val ( |Unit|_| ) : x:LowType -> unit option
        val ( |Array|_| ) : x:LowType -> (string * LowType) option
        val private b : LowType
        val ( |Boolean|_| ) : LowType -> unit option
        val ( |NonTuple|_| ) : x:LowType -> LowType option
        val ( |AbbreviationRoot|_| ) : x:LowType -> LowType option
      end
    end
  end
  module internal Identity = begin
    [<RequireQualifiedAccessAttribute ()>]
    type IdentityEqualityResult =
      | Matched
      | GenericParameter of int * int
      | Name
      | AssemblyName
    val ( <&&> ) :
      x:(unit -> Identity.IdentityEqualityResult) ->
        y:(unit -> Identity.IdentityEqualityResult) ->
          Identity.IdentityEqualityResult
    val testee : x:DisplayNameItem -> string
    val private forall :
      f:('a -> Identity.IdentityEqualityResult) ->
        xs:seq<'a> -> unit -> Identity.IdentityEqualityResult
    val private testGenericParameterCount :
      x:int -> y:int -> unit -> Identity.IdentityEqualityResult
    val private testString :
      cmp:System.StringComparer ->
        x:string -> y:string -> unit -> Identity.IdentityEqualityResult
    val private testDisplayName :
      cmp:System.StringComparer ->
        xs:DisplayName ->
          ys:DisplayName -> unit -> Identity.IdentityEqualityResult
    val private testAssemblyName :
      x:FullIdentity ->
        y:FullIdentity -> unit -> Identity.IdentityEqualityResult
    val testFullIdentity :
      x:FullIdentity ->
        y:FullIdentity -> unit -> Identity.IdentityEqualityResult
    val fullIdentityComparer :
      System.Collections.Generic.IEqualityComparer<FullIdentity>
    val private testPartialAndFullIdentity :
      cmp:System.StringComparer ->
        partial:PartialIdentity ->
          full:FullIdentity -> unit -> Identity.IdentityEqualityResult
    val private sameName' :
      cmp:System.StringComparer ->
        x:Identity -> y:Identity -> Identity.IdentityEqualityResult
    type Equality = Identity -> Identity -> Identity.IdentityEqualityResult
    val sameName : x:Identity -> y:Identity -> Identity.IdentityEqualityResult
    val sameNameIgnoreCase :
      x:Identity -> y:Identity -> Identity.IdentityEqualityResult
    val equalityFromOptions : opt:SearchOptions -> Identity.Equality
  end
  module LowTypeVisitor = begin
    type Visitor = LowType -> LowType
    val accept_Parameter :
      visitor:(LowType -> LowType) -> p:Parameter -> Parameter
    val accept_ParameterGroups :
      visitor:(LowType -> LowType) ->
        groups:ParameterGroups -> Parameter list list
    val accept_Function :
      visitor:(LowType -> LowType) ->
        func:ParameterGroups -> Parameter list list
    val accept_Member : visitor:(LowType -> LowType) -> member':Member -> Member
    val accept_TypeConstraint :
      visitor:(LowType -> LowType) -> c:TypeConstraint -> TypeConstraint
    val accept_FullTypeDefinition :
      visitor:(LowType -> LowType) ->
        fullTypeDef:FullTypeDefinition -> FullTypeDefinition
    val accept_TypeAbbreviationDefinition :
      visitor:(LowType -> LowType) ->
        abbDef:TypeAbbreviationDefinition -> TypeAbbreviationDefinition
    val accept_TypeExtension :
      visitor:(LowType -> LowType) ->
        typeExtension:TypeExtension -> TypeExtension
    val accept_UnionCaseField :
      visitor:(LowType -> LowType) -> field:UnionCaseField -> UnionCaseField
    val accept_UnionCase :
      visitor:(LowType -> LowType) -> uc:UnionCase -> UnionCase
    val accept_ComputationExpressionBuilder :
      visitor:(LowType -> LowType) ->
        builder:ComputationExpressionBuilder -> ComputationExpressionBuilder
    val accept_ApiSignature :
      visitor:Visitor -> _arg1:ApiSignature -> ApiSignature
  end
  module internal LowType = begin
    val applyVariable :
      source:VariableSource ->
        replacements:Map<TypeVariable,LowType> -> _arg1:LowType -> LowType
    val applyVariableToTargetList :
      source:VariableSource ->
        replacements:Map<TypeVariable,LowType> ->
          xs:LowType list -> LowType list
    val collectWildcardGroup : x:LowType -> LowType list
    val collectVariables : x:LowType -> LowType list
    val collectVariableOrWildcardGroup : x:LowType -> LowType list
  end

namespace FSharpApiSearch
  module Printer = begin
    module internal FSharpImpl = begin
      val printPropertyKind : _arg1:PropertyKind -> string
      val printMemberKind : _arg1:MemberKind -> string
      val printMemberModifier : _arg1:MemberModifier -> string
      val printApiKind :
        kind:ApiKind ->
          sb:System.Text.StringBuilder -> System.Text.StringBuilder
      val typeVariablePrefix : v:TypeVariable -> string
      val toDisplayName : _arg1:NamePart -> string
      val printNameItem :
        n:DisplayNameItem ->
          sb:System.Text.StringBuilder -> System.Text.StringBuilder
      val printDisplayName_full :
        xs:DisplayNameItem list ->
          sb:System.Text.StringBuilder -> System.Text.StringBuilder
      val printName_full :
        name:Name -> sb:System.Text.StringBuilder -> System.Text.StringBuilder
      val printApiName :
        name:Name -> sb:System.Text.StringBuilder -> System.Text.StringBuilder
      val printAccessPath :
        depth:int option ->
          name:Name -> sb:System.Text.StringBuilder -> System.Text.StringBuilder
      val printIdentity_full :
        identity:Identity ->
          sb:System.Text.StringBuilder -> System.Text.StringBuilder
      val printIdentity_short :
        identity:Identity ->
          sb:System.Text.StringBuilder -> System.Text.StringBuilder
      val printVariableSource : _arg1:VariableSource -> string
      val printTypeVariable :
        isDebug:bool ->
          source:VariableSource ->
            v:TypeVariable ->
              sb:System.Text.StringBuilder -> System.Text.StringBuilder
      val printLowType :
        isDebug:bool ->
          printIdentity:(Identity -> System.Text.StringBuilder ->
                           System.Text.StringBuilder) ->
            lowType:LowType ->
              sb:System.Text.StringBuilder -> System.Text.StringBuilder
      val printGeneric :
        isDebug:bool ->
          printIdentity:(Identity -> System.Text.StringBuilder ->
                           System.Text.StringBuilder) ->
            id:LowType ->
              args:LowType list ->
                sb:System.Text.StringBuilder -> System.Text.StringBuilder
      val printArrow :
        isDebug:bool ->
          printIdentity:(Identity -> System.Text.StringBuilder ->
                           System.Text.StringBuilder) ->
            xs:LowType list ->
              sb:System.Text.StringBuilder -> System.Text.StringBuilder
      val printTuple :
        isDebug:bool ->
          printIdentity:(Identity -> System.Text.StringBuilder ->
                           System.Text.StringBuilder) ->
            xs:LowType list ->
              sb:System.Text.StringBuilder -> System.Text.StringBuilder
      val printStructTuple :
        isDebug:bool ->
          printIdentity:(Identity -> System.Text.StringBuilder ->
                           System.Text.StringBuilder) ->
            xs:LowType list ->
              sb:System.Text.StringBuilder -> System.Text.StringBuilder
      val printChoice :
        isDebug:bool ->
          printIdentity:(Identity -> System.Text.StringBuilder ->
                           System.Text.StringBuilder) ->
            xs:LowType list ->
              sb:System.Text.StringBuilder -> System.Text.StringBuilder
      val printLowType_short :
        isDebug:bool ->
          t:LowType -> sb:System.Text.StringBuilder -> System.Text.StringBuilder
      val printLowType_full :
        isDebug:bool ->
          t:LowType -> sb:System.Text.StringBuilder -> System.Text.StringBuilder
      val printParameter :
        tupleParen:bool ->
          isDebug:bool ->
            p:Parameter ->
              sb:System.Text.StringBuilder -> System.Text.StringBuilder
      val printParameterGroups :
        tupleParen:bool ->
          isDebug:bool ->
            func:Parameter list list ->
              sb:System.Text.StringBuilder -> System.Text.StringBuilder
      val printMember :
        isDebug:bool ->
          m:Member -> sb:System.Text.StringBuilder -> System.Text.StringBuilder
      val printConstraint :
        isDebug:bool ->
          c:TypeConstraint ->
            sb:System.Text.StringBuilder -> System.Text.StringBuilder
      val printFullTypeDefinition :
        isDebug:bool ->
          x:FullTypeDefinition ->
            sb:System.Text.StringBuilder -> System.Text.StringBuilder
      val pringTypeAbbreviation :
        isDebug:bool ->
          x:TypeAbbreviationDefinition ->
            sb:System.Text.StringBuilder -> System.Text.StringBuilder
      val printUnionCaseField :
        isDebug:bool ->
          uc:UnionCaseField ->
            sb:System.Text.StringBuilder -> System.Text.StringBuilder
      val printUnionCase :
        isDebug:bool ->
          uc:UnionCase ->
            sb:System.Text.StringBuilder -> System.Text.StringBuilder
      val printModule :
        m:ModuleDefinition ->
          sb:System.Text.StringBuilder -> System.Text.StringBuilder
      val printComputationExpressionBuilder :
        isDebug:bool ->
          builder:ComputationExpressionBuilder ->
            sb:System.Text.StringBuilder -> System.Text.StringBuilder
      val printApiSignature :
        isDebug:bool ->
          apiSig:ApiSignature ->
            sb:System.Text.StringBuilder -> System.Text.StringBuilder
    end
    module FSharp = begin
      val printFullName : api:Api -> string
      val printApiName : api:Api -> string
      val printAccessPath : depth:int option -> api:Api -> string
      val printSignature : api:Api -> string
      val printKind : api:Api -> string
      val tryPrintTypeConstraints : api:Api -> string option
    end
    module internal CSharpImpl = begin
      val toDisplayName : _arg1:NamePart -> string
      val printNameItem :
        n:DisplayNameItem ->
          sb:System.Text.StringBuilder -> System.Text.StringBuilder
      val printDisplayName_full :
        xs:DisplayNameItem list ->
          sb:System.Text.StringBuilder -> System.Text.StringBuilder
      val printName_full :
        name:Name -> sb:System.Text.StringBuilder -> System.Text.StringBuilder
      val printApiName :
        name:Name -> sb:System.Text.StringBuilder -> System.Text.StringBuilder
      val printAccessPath :
        depth:int option ->
          name:Name -> sb:System.Text.StringBuilder -> System.Text.StringBuilder
      val csharpAlias :
        System.Collections.Generic.IDictionary<Identity,Identity>
      val printIdentity :
        identity:Identity ->
          sb:System.Text.StringBuilder -> System.Text.StringBuilder
      val toFSharpFunc : xs:LowType list -> LowType
      val nestedArray :
        acc:string list -> _arg1:LowType -> string list * LowType
      val printRef : isOut:bool -> string
      val printLowType :
        t:LowType -> sb:System.Text.StringBuilder -> System.Text.StringBuilder
      val printParameter :
        p:Parameter -> sb:System.Text.StringBuilder -> System.Text.StringBuilder
      val printPropertyParameter :
        m:Member -> sb:System.Text.StringBuilder -> System.Text.StringBuilder
      val printProperty :
        m:Member -> sb:System.Text.StringBuilder -> System.Text.StringBuilder
      val printReturnParameter :
        p:Parameter -> sb:System.Text.StringBuilder -> System.Text.StringBuilder
      val printMethodParameter :
        m:Member ->
          isExtension:bool ->
            sb:System.Text.StringBuilder -> System.Text.StringBuilder
      val printMethod :
        m:Member ->
          isExtension:bool ->
            sb:System.Text.StringBuilder -> System.Text.StringBuilder
      val printField :
        m:Member -> sb:System.Text.StringBuilder -> System.Text.StringBuilder
      val printMember :
        m:Member -> sb:System.Text.StringBuilder -> System.Text.StringBuilder
      val printConstructor :
        m:Member -> sb:System.Text.StringBuilder -> System.Text.StringBuilder
      val printModuleValue :
        t:LowType -> sb:System.Text.StringBuilder -> System.Text.StringBuilder
      val printFunction :
        fn:Function -> sb:System.Text.StringBuilder -> System.Text.StringBuilder
      val printFullTypeDefinition :
        td:FullTypeDefinition ->
          sb:System.Text.StringBuilder -> System.Text.StringBuilder
      val printApiSignature :
        apiSig:ApiSignature ->
          sb:System.Text.StringBuilder -> System.Text.StringBuilder
      val filterCSharpTypeConstraint :
        xs:TypeConstraint list -> TypeConstraint list
      val printConstraints :
        xs:TypeConstraint list ->
          sb:System.Text.StringBuilder -> System.Text.StringBuilder
      val printPropertyKind : _arg1:PropertyKind -> string
      val printMemberKind : _arg1:MemberKind -> string
      val printMemberModifier : _arg1:MemberModifier -> string
      val printApiKind :
        kind:ApiKind ->
          sb:System.Text.StringBuilder -> System.Text.StringBuilder
    end
    module CSharp = begin
      val printFullName : api:Api -> string
      val printApiName : api:Api -> string
      val printAccessPath : depth:int option -> api:Api -> string
      val printSignature : api:Api -> string
      val tryPrintTypeConstraints : api:Api -> string option
      val printKind : api:Api -> string
    end
    type TypeVariable with
      member Print : unit -> string
    type DisplayNameItem with
      member Print : unit -> string
    type Name with
      member Print : unit -> string
    type LowType with
      member Print : unit -> string
    type LowType with
      member internal Debug : unit -> string
    type ApiSignature with
      member Print : unit -> string
    type ApiSignature with
      member internal Debug : unit -> string
    type TypeConstraint with
      member Print : unit -> string
    type TypeConstraint with
      member internal Debug : unit -> string
    type FullTypeDefinition with
      member Print : unit -> string
    type FullTypeDefinition with
      member internal Debug : unit -> string
    module internal LowType = begin
      val debug : x:LowType -> string
    end
    module internal ApiSignature = begin
      val debug : x:ApiSignature -> string
      val print : x:ApiSignature -> string
    end
    module internal TypeConstraint = begin
      val debug : x:TypeConstraint -> string
    end
    module internal FullTypeDefinition = begin
      val debug : x:FullTypeDefinition -> string
    end
  end

namespace FSharpApiSearch
  module internal QueryParser = begin
    val inline trim :
      p:FParsec.Primitives.Parser<'a,'b> -> FParsec.Primitives.Parser<'a,'b>
    val inline pcharAndTrim : c:char -> FParsec.Primitives.Parser<char,'a>
    val inline pstringAndTrim : s:string -> FParsec.Primitives.Parser<string,'a>
    val inline sepBy2 :
      p:FParsec.Primitives.Parser<'a,'b> ->
        sep:FParsec.Primitives.Parser<'c,'b> ->
          FParsec.Primitives.Parser<'a list,'b>
    val compose :
      firstParser:FParsec.Primitives.Parser<'a,'b> ->
        ps:(FParsec.Primitives.Parser<'a,'b> ->
              FParsec.Primitives.Parser<'a,'b> ->
              FParsec.Primitives.Parser<'a,'b>) list ->
          FParsec.Primitives.Parser<'a,'b>
    val parray : FParsec.Primitives.Parser<string,unit>
    module FSharp = begin
      val struct' : string
      val keywords : string list
      val pidentifier : FParsec.Primitives.Parser<string,unit>
      val partialName : FParsec.Primitives.Parser<DisplayNameItem list,unit>
      val fsharpSignatureRef : FParsec.Primitives.Parser<LowType,unit> ref
      val fsharpSignature : FParsec.Primitives.Parser<LowType,unit>
      val identity : FParsec.Primitives.Parser<LowType,unit>
      val variable : FParsec.Primitives.Parser<LowType,unit>
      val wildcard : FParsec.Primitives.Parser<LowType,unit>
      val genericId : (FParsec.CharStream<unit> -> FParsec.Reply<LowType>)
      val createGeneric : id:LowType -> parameters:LowType list -> LowType
      val dotNetGeneric : FParsec.Primitives.Parser<LowType,unit>
      val flexible : FParsec.Primitives.Parser<LowType,unit>
      val ptype : (FParsec.CharStream<unit> -> FParsec.Reply<LowType>)
      val array :
        'a ->
          typeParser:FParsec.Primitives.Parser<LowType,unit> ->
            FParsec.Primitives.Parser<LowType,unit>
      val structTuple :
        self:FParsec.Primitives.Parser<LowType,'a> ->
          typeParser:FParsec.Primitives.Parser<LowType,'a> ->
            FParsec.Primitives.Parser<LowType,'a>
      val mlGeneric :
        'a ->
          typeParser:FParsec.Primitives.Parser<LowType,unit> ->
            FParsec.Primitives.Parser<LowType,unit>
      val tuple :
        'a ->
          typeParser:FParsec.Primitives.Parser<LowType,'b> ->
            FParsec.Primitives.Parser<LowType,'b>
      val arrow :
        'a ->
          typeParser:FParsec.Primitives.Parser<LowType,'b> ->
            FParsec.Primitives.Parser<LowType,'b>
      val activePatternKind : FParsec.Primitives.Parser<ActivePatternKind,unit>
      val allPatterns : FParsec.Primitives.Parser<ActivePatternSignature,unit>
      val activePattern : FParsec.Primitives.Parser<ActivePatternSignature,unit>
      val activePatternQuery : FParsec.Primitives.Parser<QueryMethod,unit>
      val opName : FParsec.Primitives.Parser<ByName,unit>
      val memberNamePartial : FParsec.Primitives.Parser<ByName,unit>
      val signatureWildcard : FParsec.Primitives.Parser<SignatureQuery,unit>
      val anyOrSignature : FParsec.Primitives.Parser<SignatureQuery,unit>
      val nameQuery : FParsec.Primitives.Parser<QueryMethod,unit>
      val signatureQuery : FParsec.Primitives.Parser<QueryMethod,unit>
      val computationExpressionSyntax : FParsec.Primitives.Parser<string,unit>
      val computationExpressionQuery :
        FParsec.Primitives.Parser<QueryMethod,unit>
      val query : (FParsec.CharStream<unit> -> FParsec.Reply<QueryMethod>)
      val parse : queryStr:string -> Query
    end
    module CSharp = begin
      val ref : string
      val out : string
      val keywords : string list
      val punit : FParsec.Primitives.Parser<LowType,unit>
      val pidentifier : FParsec.Primitives.Parser<string,unit>
      val partialName : FParsec.Primitives.Parser<DisplayNameItem list,unit>
      val csharpSignatureRef : FParsec.Primitives.Parser<LowType,unit> ref
      val csharpSignature : FParsec.Primitives.Parser<LowType,unit>
      val identity : FParsec.Primitives.Parser<LowType,unit>
      val wildcard : FParsec.Primitives.Parser<LowType,unit>
      val genericId : (FParsec.CharStream<unit> -> FParsec.Reply<LowType>)
      val createGeneric : id:LowType -> parameters:LowType list -> LowType
      val generic : FParsec.Primitives.Parser<LowType,unit>
      val flexible : FParsec.Primitives.Parser<LowType,unit>
      val ptype : (FParsec.CharStream<unit> -> FParsec.Reply<LowType>)
      val array :
        'a ->
          typeParser:FParsec.Primitives.Parser<LowType,unit> ->
            FParsec.Primitives.Parser<LowType,unit>
      val structTuple :
        self:FParsec.Primitives.Parser<LowType,'a> ->
          typeParser:FParsec.Primitives.Parser<LowType,'a> ->
            FParsec.Primitives.Parser<LowType,'a>
      val byref :
        'a ->
          typeParser:FParsec.Primitives.Parser<LowType,'b> ->
            FParsec.Primitives.Parser<LowType,'b>
      val arrow :
        'a ->
          typeParser:FParsec.Primitives.Parser<LowType,'b> ->
            FParsec.Primitives.Parser<LowType,'b>
      val replaceWithVariable :
        variableNames:Set<string> -> _arg1:LowType -> LowType
      val signatureWildcard : FParsec.Primitives.Parser<SignatureQuery,unit>
      val genericPart : FParsec.Primitives.Parser<string list,unit>
      val memberNamePartial : FParsec.Primitives.Parser<ByName,unit>
      val anyOrSignature : FParsec.Primitives.Parser<SignatureQuery,unit>
      val nameQuery : FParsec.Primitives.Parser<QueryMethod,unit>
      val genericQuery : FParsec.Primitives.Parser<QueryMethod,unit>
      val signatureQuery : FParsec.Primitives.Parser<QueryMethod,unit>
      val query : (FParsec.CharStream<unit> -> FParsec.Reply<QueryMethod>)
      val parse : queryStr:string -> Query
    end
  end

namespace FSharpApiSearch
  module internal MatcherTypes = begin
    type Equations =
      {Equalities: (LowType * LowType) list;
       Inequalities: (LowType * LowType) list;}
    module Equations = begin
      val debugEquality : left:LowType * right:LowType -> string
      val debugInequality : left:LowType * right:LowType -> string
      val debug : x:Equations -> string
      val empty : Equations
    end
    type SubtypeResult =
      | Subtype of LowType
      | Contextual of LowType option
      | NonSubtype
    type SubtypeCache =
      System.Collections.Concurrent.ConcurrentDictionary<(LowType * LowType),
                                                         SubtypeResult>
    module SubtypeCache = begin
      val create :
        unit ->
          System.Collections.Concurrent.ConcurrentDictionary<(LowType * LowType),
                                                             SubtypeResult>
    end
    type Context =
      {Distance: int;
       Equations: Equations;
       QueryTypes: Map<PartialIdentity,FullTypeDefinition []>;
       ApiDictionaries: Map<string,ApiDictionary>;
       SubtypeCache: SubtypeCache;}
    module Context = begin
      val addDistance : reason:string -> x:int -> ctx:Context -> Context
      val newEquations :
        oldCtx:Context -> newCtx:Context -> (LowType * LowType) list
    end
    type MatchingResult =
      | Matched of Context
      | Continue of Context
      | Failure
    module MatchingResult = begin
      val inline bindContinue :
        f:(Context -> MatchingResult) -> x:MatchingResult -> MatchingResult
      val inline bindMatched :
        f:(Context -> MatchingResult) -> x:MatchingResult -> MatchingResult
      val inline mapMatched :
        f:(Context -> Context) -> x:MatchingResult -> MatchingResult
      val toBool : _arg1:MatchingResult -> bool
    end
    type ILowTypeMatcher =
      interface
        abstract member Test : LowType -> LowType -> Context -> MatchingResult
        abstract member
          TestAll : seq<LowType> -> seq<LowType> -> Context -> MatchingResult
        abstract member
          TestAllExactly : seq<LowType> ->
                             seq<LowType> -> Context -> MatchingResult
      end
    module Extensions = begin
      val private paramsAndRet : xs:seq<'a> -> 'a [] * 'a
      type ILowTypeMatcher with
        member
          TestArrow : leftTypes:seq<LowType> ->
                        rightTypes:seq<LowType> -> ctx:Context -> MatchingResult
      type ILowTypeMatcher with
        member
          TestReceiver : left:LowType ->
                           right:LowType -> ctx:Context -> MatchingResult
    end
    type IApiMatcher =
      interface
        abstract member
          Test : ILowTypeMatcher ->
                   QueryMethod -> Api -> Context -> MatchingResult
        abstract member Name : string
      end
    type Rule<'Left,'Right> =
      ILowTypeMatcher -> 'Left -> 'Right -> Context -> MatchingResult
    module Rule = begin
      val run :
        rule:Rule<'a,'b> ->
          matcher:ILowTypeMatcher ->
            left:'a -> right:'b -> ctx:Context -> MatchingResult
      val terminator : 'a -> 'b -> 'c -> 'd -> MatchingResult
      val continueFailure :
        rule:Rule<'a,'b> ->
          matcher:ILowTypeMatcher ->
            left:'a -> right:'b -> ctx:Context -> MatchingResult
      val compose :
        xs:Rule<'a,'b> [] ->
          test:ILowTypeMatcher ->
            left:'a -> right:'b -> ctx:Context -> MatchingResult
    end
  end

namespace FSharpApiSearch
  module internal TypeHierarchy = begin
    val transferVariableArgument :
      inheritArgs:Map<TypeVariable,LowType> -> baseType:LowType -> LowType list
    val instantiate : t:FullTypeDefinition -> args:LowType list -> LowType
    val getSuperTypes :
      ctx:MatcherTypes.Context ->
        t:FullTypeDefinition -> args:LowType list -> seq<LowType>
    val fullTypeDef :
      ctx:MatcherTypes.Context -> _arg1:Identity -> FullTypeDefinition []
  end

namespace FSharpApiSearch
  module internal LowTypeMatcher = begin
    module Context = begin
      val setEquations :
        eqs:MatcherTypes.Equations ->
          ctx:MatcherTypes.Context -> MatcherTypes.Context
    end
    module Equations = begin
      val sortTerm : x:'a -> y:'a -> 'a * 'a when 'a : comparison
      val containsEquality :
        left:LowType -> right:LowType -> eqs:MatcherTypes.Equations -> bool
      val containsInequality :
        left:LowType -> right:LowType -> eqs:MatcherTypes.Equations -> bool
      val findEqualities :
        left:LowType -> eqs:MatcherTypes.Equations -> (LowType * LowType) list
      val testInequality :
        left:LowType -> right:LowType -> eqs:MatcherTypes.Equations -> bool
      val isRecirsive : left:LowType -> right:LowType -> bool
      val isCircular :
        left:LowType -> right:LowType -> eqs:MatcherTypes.Equations -> bool
      val tryAddEquality :
        left:LowType ->
          right:LowType ->
            ctx:MatcherTypes.Context -> MatcherTypes.MatchingResult
    end
    module Rules = begin
      val terminator :
        MatcherTypes.ILowTypeMatcher ->
          LowType ->
            LowType -> MatcherTypes.Context -> MatcherTypes.MatchingResult
      val testLeftEqualities :
        lowTypeMatcher:MatcherTypes.ILowTypeMatcher ->
          leftEqualities:('a * LowType) list ->
            right:LowType ->
              ctx:MatcherTypes.Context -> MatcherTypes.MatchingResult
      val testVariableEquality :
        lowTypeMatcher:MatcherTypes.ILowTypeMatcher ->
          left:LowType ->
            right:LowType ->
              ctx:MatcherTypes.Context -> MatcherTypes.MatchingResult
      type Swapped = bool
      val swappedToInt : swapped:Swapped -> int
      val contains :
        lowTypeMatcher:MatcherTypes.ILowTypeMatcher ->
          testee:LowType ->
            back:LowType [] ->
              forward:LowType [] ->
                ctx:MatcherTypes.Context ->
                  (MatcherTypes.Context * LowType [] * LowType [] * Swapped) option
      val containsWildcard : xs:LowType [] -> bool
      val testAllWithComplementAndSwap :
        lowTypeMatcher:MatcherTypes.ILowTypeMatcher ->
          complementNumberLimit:int ->
            swapNumberLimit:int ->
              leftTypes:seq<LowType> ->
                rightTypes:seq<LowType> ->
                  ctx:MatcherTypes.Context -> MatcherTypes.MatchingResult
      val testAllExactly :
        lowTypeMatcher:MatcherTypes.ILowTypeMatcher ->
          leftTypes:seq<LowType> ->
            rightTypes:seq<LowType> ->
              ctx:MatcherTypes.Context -> MatcherTypes.MatchingResult
      val choiceRule :
        lowTypeMatcher:MatcherTypes.ILowTypeMatcher ->
          left:LowType ->
            right:LowType ->
              ctx:MatcherTypes.Context -> MatcherTypes.MatchingResult
      val typeAbbreviationRule :
        lowTypeMatcher:MatcherTypes.ILowTypeMatcher ->
          left:LowType ->
            right:LowType ->
              ctx:MatcherTypes.Context -> MatcherTypes.MatchingResult
      val testIdentity :
        nameEquality:Identity.Equality ->
          leftIdentity:Identity ->
            rightIdentity:Identity ->
              ctx:MatcherTypes.Context -> MatcherTypes.MatchingResult
      val identityRule :
        nameEquality:Identity.Equality ->
          'a ->
            left:LowType ->
              right:LowType ->
                ctx:MatcherTypes.Context -> MatcherTypes.MatchingResult
      val variableRule :
        lowTypeMatcher:MatcherTypes.ILowTypeMatcher ->
          left:LowType ->
            right:LowType ->
              ctx:MatcherTypes.Context -> MatcherTypes.MatchingResult
      val distanceFromVariable : _arg1:LowType -> int
      val seqDistance : xs:LowType list -> int
      val greedyVariableRule :
        lowTypeMatcher:MatcherTypes.ILowTypeMatcher ->
          left:LowType ->
            right:LowType ->
              ctx:MatcherTypes.Context -> MatcherTypes.MatchingResult
      val tupleRule :
        lowTypeMatcher:MatcherTypes.ILowTypeMatcher ->
          left:LowType ->
            right:LowType ->
              ctx:MatcherTypes.Context -> MatcherTypes.MatchingResult
      val testArrow :
        lowTypeMatcher:MatcherTypes.ILowTypeMatcher ->
          leftElems:seq<LowType> ->
            rightElems:seq<LowType> ->
              ctx:MatcherTypes.Context -> MatcherTypes.MatchingResult
      val arrowRule :
        lowTypeMatcher:MatcherTypes.ILowTypeMatcher ->
          left:LowType ->
            right:LowType ->
              ctx:MatcherTypes.Context -> MatcherTypes.MatchingResult
      val testArrow_IgnoreParameterStyle :
        lowTypeMatcher:MatcherTypes.ILowTypeMatcher ->
          leftElems:LowType list ->
            rightElems:LowType list ->
              ctx:MatcherTypes.Context -> MatcherTypes.MatchingResult
      val arrowRule_IgnoreParameterStyle :
        lowTypeMatcher:MatcherTypes.ILowTypeMatcher ->
          left:LowType ->
            right:LowType ->
              ctx:MatcherTypes.Context -> MatcherTypes.MatchingResult
      val genericRule :
        lowTypeMatcher:MatcherTypes.ILowTypeMatcher ->
          left:LowType ->
            right:LowType ->
              ctx:MatcherTypes.Context -> MatcherTypes.MatchingResult
      val wildcardRule :
        'a ->
          left:LowType ->
            right:LowType ->
              ctx:MatcherTypes.Context -> MatcherTypes.MatchingResult
      val wildcardGroupRule :
        lowTypeMatcher:MatcherTypes.ILowTypeMatcher ->
          left:LowType ->
            right:LowType ->
              ctx:MatcherTypes.Context -> MatcherTypes.MatchingResult
      val delegateRule :
        nameEquality:Identity.Equality ->
          lowTypeMatcher:MatcherTypes.ILowTypeMatcher ->
            left:LowType ->
              right:LowType ->
                ctx:MatcherTypes.Context -> MatcherTypes.MatchingResult
      val delegateAndArrowRule :
        lowTypeMatcher:MatcherTypes.ILowTypeMatcher ->
          left:LowType ->
            right:LowType ->
              ctx:MatcherTypes.Context -> MatcherTypes.MatchingResult
      val delegateAndArrowRule_IgnoreParameterStyle :
        lowTypeMatcher:MatcherTypes.ILowTypeMatcher ->
          left:LowType ->
            right:LowType ->
              ctx:MatcherTypes.Context -> MatcherTypes.MatchingResult
      val byrefRule :
        lowTypeMatcher:MatcherTypes.ILowTypeMatcher ->
          left:LowType ->
            right:LowType ->
              ctx:MatcherTypes.Context -> MatcherTypes.MatchingResult
      val flexibleTarget :
        ctx:'a -> _arg1:LowType -> (Identity * LowType list) option
      val ( |FlexibleTarget|_| ) :
        ctx:'a -> (LowType -> (Identity * LowType list) option)
      val testFlexible :
        lowTypeMatcher:MatcherTypes.ILowTypeMatcher ->
          flexible:LowType ->
            targetId:Identity ->
              targetArgs:LowType list ->
                ctx:MatcherTypes.Context ->
                  (MatcherTypes.Context * LowType) option
      val flexibleCacheValue :
        contextualType:(unit -> bool) ->
          lowTypeMatcher:MatcherTypes.ILowTypeMatcher ->
            flexible:LowType ->
              targetId:Identity ->
                targetArgs:LowType list ->
                  ctx:MatcherTypes.Context -> MatcherTypes.SubtypeResult
      val flexibleRule :
        isContextual:(LowType -> bool) ->
          lowTypeMatcher:MatcherTypes.ILowTypeMatcher ->
            left:LowType ->
              right:LowType ->
                ctx:MatcherTypes.Context -> MatcherTypes.MatchingResult
    end
    val instance : options:SearchOptions -> MatcherTypes.ILowTypeMatcher
  end

namespace FSharpApiSearch
  module internal NameMatcher = begin
    type StringOptions =
      {StringComparer: System.StringComparer;
       StringComparison: System.StringComparison;
       RegexOptions: System.Text.RegularExpressions.RegexOptions;}
    val stringOptions : ignoreCase:OptionStatus -> StringOptions
    val test' :
      strOpt:StringOptions ->
        expected:ByName list -> actualNames:DisplayNameItem list -> bool
    val test :
      strOpt:StringOptions ->
        query:QueryMethod ->
          api:Api -> ctx:MatcherTypes.Context -> MatcherTypes.MatchingResult
    val instance : options:SearchOptions -> MatcherTypes.IApiMatcher
  end

namespace FSharpApiSearch
  module internal SignatureMatcher = begin
    module Rules = begin
      val choiceRule :
        runRules:(MatcherTypes.ILowTypeMatcher -> SignatureQuery ->
                    ApiSignature -> MatcherTypes.Context ->
                    MatcherTypes.MatchingResult) ->
          lowTypeMatcher:MatcherTypes.ILowTypeMatcher ->
            left:SignatureQuery ->
              right:ApiSignature ->
                ctx:MatcherTypes.Context -> MatcherTypes.MatchingResult
      val moduleValueRule :
        lowTypeMatcher:MatcherTypes.ILowTypeMatcher ->
          left:SignatureQuery ->
            right:ApiSignature ->
              ctx:MatcherTypes.Context -> MatcherTypes.MatchingResult
      val ( |WildcardOrVariable|_| ) : _arg1:LowType -> unit option
      val trimOptionalParameters :
        leftElems:LowType list ->
          rightElems:Parameter list list -> Parameter list list
      val testArrow :
        lowTypeMatcher:MatcherTypes.ILowTypeMatcher ->
          leftElems:LowType list ->
            rightElems:Parameter list list ->
              ctx:MatcherTypes.Context -> MatcherTypes.MatchingResult
      val ( |Right_CurriedFunction|_| ) :
        xs:Parameter list list -> LowType list option
      val ( |Right_NonCurriedFunction|_| ) :
        xs:Parameter list list -> LowType list option
      val ( |Right_TupleFunction|_| ) :
        xs:Parameter list list -> LowType list option
      val ( |Left_CurriedFunction|_| ) : xs:LowType list -> LowType list option
      val ( |Left_NonCurriedFunction|_| ) :
        xs:LowType list -> LowType list option
      val testArrow_IgnoreParamStyle :
        lowTypeMatcher:MatcherTypes.ILowTypeMatcher ->
          leftElems:LowType list ->
            rightElems:Parameter list list ->
              ctx:MatcherTypes.Context -> MatcherTypes.MatchingResult
      val moduleFunctionRule :
        testArrow:(MatcherTypes.ILowTypeMatcher -> LowType list -> Function ->
                     MatcherTypes.Context -> MatcherTypes.MatchingResult) ->
          lowTypeMatcher:MatcherTypes.ILowTypeMatcher ->
            left:SignatureQuery ->
              right:ApiSignature ->
                ctx:MatcherTypes.Context -> MatcherTypes.MatchingResult
      val arrowQueryAndDelegateRule :
        lowTypeMatcher:MatcherTypes.ILowTypeMatcher ->
          left:SignatureQuery ->
            right:ApiSignature ->
              ctx:MatcherTypes.Context -> MatcherTypes.MatchingResult
      val activePatternRule :
        testArrow:(MatcherTypes.ILowTypeMatcher -> LowType list -> Function ->
                     MatcherTypes.Context -> MatcherTypes.MatchingResult) ->
          lowTypeMatcher:MatcherTypes.ILowTypeMatcher ->
            left:SignatureQuery ->
              right:ApiSignature ->
                ctx:MatcherTypes.Context -> MatcherTypes.MatchingResult
      val breakArrow : _arg1:LowType -> LowType list
      val ( |StaticMember|_| ) : _arg1:ApiSignature -> Member option
      val ( |NoArgsMember|_| ) : _arg1:Member -> Member option
      val extensionMemberRule :
        testArrow:(MatcherTypes.ILowTypeMatcher -> LowType list ->
                     Parameter list list -> MatcherTypes.Context ->
                     MatcherTypes.MatchingResult) ->
          lowTypeMatcher:MatcherTypes.ILowTypeMatcher ->
            left:SignatureQuery ->
              right:ApiSignature ->
                ctx:MatcherTypes.Context -> MatcherTypes.MatchingResult
      val staticMemberRule :
        testArrow:(MatcherTypes.ILowTypeMatcher -> LowType list ->
                     Parameter list list -> MatcherTypes.Context ->
                     MatcherTypes.MatchingResult) ->
          lowTypeMatcher:MatcherTypes.ILowTypeMatcher ->
            left:SignatureQuery ->
              right:ApiSignature ->
                ctx:MatcherTypes.Context -> MatcherTypes.MatchingResult
      val constructorRule :
        testArrow:(MatcherTypes.ILowTypeMatcher -> LowType list ->
                     Parameter list list -> MatcherTypes.Context ->
                     MatcherTypes.MatchingResult) ->
          lowTypeMatcher:MatcherTypes.ILowTypeMatcher ->
            left:SignatureQuery ->
              right:ApiSignature ->
                ctx:MatcherTypes.Context -> MatcherTypes.MatchingResult
      val methodPart : queryParams:'a list -> queryReturnType:'a -> 'a list
      val ( |InstanceMember|_| ) :
        _arg1:ApiSignature -> (LowType * Member) option
      val arrowAndInstanceMemberRule :
        testArrow:(MatcherTypes.ILowTypeMatcher -> LowType list ->
                     Parameter list list -> MatcherTypes.Context ->
                     MatcherTypes.MatchingResult) ->
          lowTypeMatcher:MatcherTypes.ILowTypeMatcher ->
            left:SignatureQuery ->
              right:ApiSignature ->
                ctx:MatcherTypes.Context -> MatcherTypes.MatchingResult
      val unionCaseRule :
        testArrow:(MatcherTypes.ILowTypeMatcher -> LowType list ->
                     Parameter list list -> MatcherTypes.Context ->
                     MatcherTypes.MatchingResult) ->
          lowTypeMatcher:MatcherTypes.ILowTypeMatcher ->
            left:SignatureQuery ->
              right:ApiSignature ->
                ctx:MatcherTypes.Context -> MatcherTypes.MatchingResult
      val typeDefRule :
        lowTypeMatcher:MatcherTypes.ILowTypeMatcher ->
          left:SignatureQuery ->
            right:ApiSignature ->
              ctx:MatcherTypes.Context -> MatcherTypes.MatchingResult
      val moduleDefRule :
        lowTypeMatcher:MatcherTypes.ILowTypeMatcher ->
          left:SignatureQuery ->
            right:ApiSignature ->
              ctx:MatcherTypes.Context -> MatcherTypes.MatchingResult
      val typeAbbreviationRule :
        lowTypeMatcher:MatcherTypes.ILowTypeMatcher ->
          left:SignatureQuery ->
            right:ApiSignature ->
              ctx:MatcherTypes.Context -> MatcherTypes.MatchingResult
    end
    val tryGetSignatureQuery : _arg1:QueryMethod -> SignatureQuery option
    val instance : options:SearchOptions -> MatcherTypes.IApiMatcher
  end

namespace FSharpApiSearch
  module internal ActivePatternMatcher = begin
    val testAllParameter :
      lowTypeMatcher:MatcherTypes.ILowTypeMatcher ->
        activePatternType:LowType ->
          returnType:LowType ->
            right:Function ->
              ctx:MatcherTypes.Context -> MatcherTypes.MatchingResult
    val test :
      lowTypeMatcher:MatcherTypes.ILowTypeMatcher ->
        query:ActivePatternQuery ->
          api:Api -> ctx:MatcherTypes.Context -> MatcherTypes.MatchingResult
    val instance : SearchOptions -> MatcherTypes.IApiMatcher
  end

namespace FSharpApiSearch
  module internal ConstraintSolver = begin
    val ( |ConstraintTestee|_| ) :
      _arg1:LowType -> (Identity * LowType list) option
    val createConstraintSolver :
      title:string ->
        testConstraint:(FullTypeDefinition -> LowType list ->
                          MatcherTypes.Context -> #seq<MatcherTypes.Context>) ->
          testeeType:LowType ->
            ctx:MatcherTypes.Context -> seq<MatcherTypes.Context>
    val firstMatched :
      f:('a -> MatcherTypes.MatchingResult) ->
        xs:seq<'a> -> seq<MatcherTypes.Context>
    val testSubtypeConstraint :
      lowTypeMatcher:MatcherTypes.ILowTypeMatcher ->
        parentType:LowType ->
          (LowType -> MatcherTypes.Context -> seq<MatcherTypes.Context>)
    val addGenericMemberReplacements :
      m:Member ->
        replacements:Map<TypeVariable,LowType> -> Map<TypeVariable,LowType>
    val normalizeGetterMethod : m:Member -> LowType
    val normalizeSetterMethod : m:Member -> LowType
    val normalizeMethod : m:Member -> LowType
    val testMemberConstraint :
      lowTypeMatcher:MatcherTypes.ILowTypeMatcher ->
        modifier:MemberModifier ->
          expectedMember:Member ->
            (LowType -> MatcherTypes.Context -> seq<MatcherTypes.Context>)
    val createConstraintStatusSolver :
      name:string ->
        get:(FullTypeDefinition -> ConstraintStatus) ->
          (LowType -> MatcherTypes.Context -> seq<MatcherTypes.Context>)
    val testNullnessConstraint :
      (LowType -> MatcherTypes.Context -> seq<MatcherTypes.Context>)
    val testDefaultConstructorConstriant :
      (LowType -> MatcherTypes.Context -> seq<MatcherTypes.Context>)
    val testValueTypeConstraint :
      (LowType -> MatcherTypes.Context -> seq<MatcherTypes.Context>)
    val testReferenceTypeConstraint :
      (LowType -> MatcherTypes.Context -> seq<MatcherTypes.Context>)
    val testEqualityConstraint :
      (LowType -> MatcherTypes.Context -> seq<MatcherTypes.Context>)
    val testComparisonConstraint :
      (LowType -> MatcherTypes.Context -> seq<MatcherTypes.Context>)
    val solve' :
      lowTypeMatcher:MatcherTypes.ILowTypeMatcher ->
        constraints:TypeConstraint list ->
          initialCtx:MatcherTypes.Context ->
            testEqualities:(LowType * LowType) list ->
              MatcherTypes.MatchingResult
    val solve :
      lowTypeMatcher:MatcherTypes.ILowTypeMatcher ->
        constraints:TypeConstraint list ->
          ctx:MatcherTypes.Context -> MatcherTypes.MatchingResult
    val instance : SearchOptions -> MatcherTypes.IApiMatcher
  end

namespace FSharpApiSearch
  module internal NonPublicFilter = begin
    val testAccessibility :
      ctx:MatcherTypes.Context ->
        _arg1:Accessibility -> MatcherTypes.MatchingResult
    val test :
      api:Api -> ctx:MatcherTypes.Context -> MatcherTypes.MatchingResult
    val instance : SearchOptions -> MatcherTypes.IApiMatcher
  end

namespace FSharpApiSearch
  module internal CSharpFilter = begin
    val test :
      api:Api -> ctx:MatcherTypes.Context -> MatcherTypes.MatchingResult
    val instance : SearchOptions -> MatcherTypes.IApiMatcher
  end

namespace FSharpApiSearch
  module internal ComputationExpressionMatcher = begin
    module Filter = begin
      val instance : SearchOptions -> MatcherTypes.IApiMatcher
    end
    val private collect :
      options:SearchOptions -> f:('a -> #seq<'c>) -> xs:#seq<'a> -> seq<'c>
    val private choose :
      options:SearchOptions -> f:('a -> 'b option) -> xs:seq<'a> -> seq<'b>
    val private append :
      options:SearchOptions -> xs:seq<'a> -> ys:seq<'a> -> seq<'a>
    val test :
      lowTypeMatcher:MatcherTypes.ILowTypeMatcher ->
        builderTypes:LowType ->
          ctx:MatcherTypes.Context -> api:Api -> MatcherTypes.MatchingResult
    val testComputationExpressionTypes :
      lowTypeMatcher:MatcherTypes.ILowTypeMatcher ->
        ctx:MatcherTypes.Context ->
          queryCeType:LowType -> ceTypes:seq<LowType> -> bool
    val search :
      options:SearchOptions ->
        targets:seq<ApiDictionary> ->
          lowTypeMatcher:MatcherTypes.ILowTypeMatcher ->
            query:ComputationExpressionQuery ->
              initialContext:MatcherTypes.Context -> seq<Result>
  end

namespace FSharpApiSearch
  module internal MatcherInitializer = begin
    val buildMatchers :
      options:SearchOptions ->
        apiMatchers:(SearchOptions -> 'a) list ->
          MatcherTypes.ILowTypeMatcher * 'a list
    val collectFromSignatureQuery :
      getTarget:(LowType -> 'a option) -> query:Query -> 'a list
        when 'a : equality
    val collectVariables : (Query -> LowType list)
    val collectWildcardGroups : (Query -> LowType list)
    val collectPartialIdentities : (Query -> PartialIdentity list)
    val initialEquations :
      options:SearchOptions ->
        query:Query -> eqs:MatcherTypes.Equations -> MatcherTypes.Equations
    val queryTypes :
      query:Query ->
        dictionaries:ApiDictionary [] ->
          Map<PartialIdentity,FullTypeDefinition []>
    val initializeContext :
      dictionaries:ApiDictionary [] ->
        options:SearchOptions -> query:Query -> MatcherTypes.Context
    val private replaceTypeAbbreviation' :
      nameEquality:(Identity -> Identity -> bool) ->
        table:TypeAbbreviation list -> query:Query -> Query
    val replaceTypeAbbreviation :
      table:TypeAbbreviation list ->
        options:SearchOptions -> query:Query -> Query
    val typeAbbreviationTableFromApiDictionary :
      dictionaries:seq<ApiDictionary> -> TypeAbbreviation list
    type IInitializeStorategy =
      interface
        abstract member
          InitialContext : Query * ApiDictionary [] * SearchOptions ->
                             MatcherTypes.Context
        abstract member
          InitializeQuery : Query * ApiDictionary [] * SearchOptions -> Query
        abstract member
          Matchers : SearchOptions ->
                       MatcherTypes.ILowTypeMatcher *
                       MatcherTypes.IApiMatcher list
        abstract member ParseQuery : string -> Query
      end
    type FSharpInitializeStorategy =
      class
        interface IInitializeStorategy
        new : unit -> FSharpInitializeStorategy
      end
    val csharpAliases : TypeAbbreviation list
    type CSharpInitializeStorategy =
      class
        interface IInitializeStorategy
        new : unit -> CSharpInitializeStorategy
      end
  end

namespace FSharpApiSearch
  module Matcher = begin
    val internal test :
      lowTypeMatcher:MatcherTypes.ILowTypeMatcher ->
        apiMatchers:MatcherTypes.IApiMatcher list ->
          query:Query ->
            ctx:MatcherTypes.Context -> api:Api -> MatcherTypes.MatchingResult
    val private choose :
      options:SearchOptions -> f:('a -> 'b option) -> xs:seq<'a> -> seq<'b>
    val internal search' :
      targets:seq<ApiDictionary> ->
        options:SearchOptions ->
          lowTypeMatcher:MatcherTypes.ILowTypeMatcher ->
            apiMatchers:MatcherTypes.IApiMatcher list ->
              query:Query -> initialContext:MatcherTypes.Context -> seq<Result>
    val internal storategy :
      options:SearchOptions -> MatcherInitializer.IInitializeStorategy
    val search :
      dictionaries:ApiDictionary [] ->
        options:SearchOptions ->
          targets:seq<ApiDictionary> -> queryStr:string -> seq<Result>
  end

namespace FSharpApiSearch
  module AssemblyLoader = begin
    type AssemblyResolver =
      {FSharpCore: string;
       Framework: string list;
       Directories: string list;}
      with
        member Resolve : assemblyName:string -> string option
      end
    val internal ignoreFSharpCompilerServiceError : unit -> unit
    val load :
      assemblyResolver:AssemblyResolver ->
        references:seq<string> -> Compiler.SourceCodeServices.FSharpAssembly []
  end

namespace FSharpApiSearch
  module internal CompilerOptimization = begin
    type ImplicitMember =
      {InstanceMembers: Member list;
       StaticMembers: Member list;}
    module FullIdentity = begin
      val Boolean : FullIdentity
      val Byte : FullIdentity
      val Char : FullIdentity
      val Decimal : FullIdentity
      val Double : FullIdentity
      val Single : FullIdentity
      val Int32 : FullIdentity
      val Int16 : FullIdentity
      val Int64 : FullIdentity
      val IntPtr : FullIdentity
      val SByte : FullIdentity
      val String : FullIdentity
      val UInt16 : FullIdentity
      val UInt32 : FullIdentity
      val UInt64 : FullIdentity
      val UIntPtr : FullIdentity
    end
    module LowType = begin
      val Boolean : LowType
      val Byte : LowType
      val Char : LowType
      val Decimal : LowType
      val Double : LowType
      val Single : LowType
      val Int32 : LowType
      val Int16 : LowType
      val Int64 : LowType
      val IntPtr : LowType
      val SByte : LowType
      val String : LowType
      val UInt16 : LowType
      val UInt32 : LowType
      val UInt64 : LowType
      val UIntPtr : LowType
    end
    module Parameter = begin
      val Boolean : Parameter
      val Byte : Parameter
      val Char : Parameter
      val Decimal : Parameter
      val Double : Parameter
      val Single : Parameter
      val Int32 : Parameter
      val Int16 : Parameter
      val Int64 : Parameter
      val IntPtr : Parameter
      val SByte : Parameter
      val String : Parameter
      val UInt16 : Parameter
      val UInt32 : Parameter
      val UInt64 : Parameter
      val UIntPtr : Parameter
    end
    val table : Map<FullIdentity,ImplicitMember>
    val implicitMembers : id:FullIdentity -> Member list * Member list
  end

namespace FSharpApiSearch
  module internal ComputationExpressionLoader = begin
    val ( |P| ) : p:Parameter -> LowType
    module Extract = begin
      val bind : m:Member -> LowType list
      val return' : m:Member -> LowType list
      val returnFrom : m:Member -> LowType list
      val run : m:Member -> LowType list
      val zero : m:Member -> LowType list
      val source : m:Member -> LowType list
      val customOperation : m:Member -> LowType
    end
    module BuilderMethod = begin
      val bind : _arg1:Member -> bool
      val delay : _arg1:Member -> bool
      val return' : _arg1:Member -> bool
      val returnFrom : _arg1:Member -> bool
      val combine : _arg1:Member -> bool
      val for' : _arg1:Member -> bool
      val tryFinally : _arg1:Member -> bool
      val tryWith : _arg1:Member -> bool
      val using : _arg1:Member -> bool
      val while' : _arg1:Member -> bool
      val yield' : _arg1:Member -> bool
      val yieldFrom : _arg1:Member -> bool
      val zero : _arg1:Member -> bool
    end
    val extractTypes :
      typeDef:FullTypeDefinition ->
        customOperations:seq<string * Member> -> seq<LowType>
    val hasMethod :
      builderTypeDef:FullTypeDefinition -> f:(Member -> bool) -> bool
    val syntaxMethods : (string * (Member -> bool) list) list
    val hasSyntax :
      builderTypeDef:FullTypeDefinition ->
        expectedMethods:(Member -> bool) list -> bool
    val extractSyntaxes : builderTypeDef:FullTypeDefinition -> Set<string>
  end

namespace FSharpApiSearch
  module ApiLoader = begin
    type TypeForward =
      {Type: string;
       From: string;
       To: string;}
    module internal Impl = begin
      type XmlDocCache = System.Collections.Generic.IDictionary<string,string>
      val createXmlDocCache :
        xml:System.Xml.Linq.XElement ->
          System.Collections.Generic.IDictionary<string,string>
      val VariableSource : VariableSource
      val inline tryGetXmlDoc :
        cache:XmlDocCache option -> symbol: ^a -> string option
          when  ^a : (member get_XmlDocSig :  ^a -> string)
      val genericSuffix : System.Text.RegularExpressions.Regex
      val inline compiledName :
        symbol: ^a -> string
          when  ^a : (member get_CompiledName :  ^a -> string)
      type FSharpGenericParameter with
        member IsAutoGeneric : bool
      type FSharpGenericParameter with
        member TypeVariable : TypeVariable
      val genericParameters :
        e:Compiler.SourceCodeServices.FSharpEntity -> TypeVariable list
      type FSharpEntity with
        member TypeAbbreviationFullName : string
      type FSharpEntity with
        member LoadingFullIdentity : FullIdentity
      type FSharpEntity with
        member Identity : LowType
      type FSharpEntity with
        member IsTuple : bool
      type FSharpEntity with
        member IsCompilerInternalModule : bool
      type FSharpEntity with
        member GetDisplayName : unit -> DisplayNameItem
      type FSharpType with
        member TryIdentity : LowType option
      type FSharpType with
        member TryFullIdentity : FullIdentity option
      val compiledNameOfProperty :
        x:Compiler.SourceCodeServices.FSharpMemberOrFunctionOrValue -> string
      type FSharpMemberOrFunctionOrValue with
        member IsStaticMember : bool
      type FSharpMemberOrFunctionOrValue with
        member IsMethod : bool
      type FSharpMemberOrFunctionOrValue with
        member IsCSharpExtensionMember : bool
      type FSharpMemberOrFunctionOrValue with
        member MemberModifier : MemberModifier
      type FSharpMemberOrFunctionOrValue with
        member PropertyKind : PropertyKind
      type FSharpMemberOrFunctionOrValue with
        member TargetSignatureConstructor : (LowType -> Member -> ApiSignature)
      type FSharpMemberOrFunctionOrValue with
        member GenericParametersAsTypeVariable : TypeVariable list
      type FSharpMemberOrFunctionOrValue with
        member GetDisplayName : DisplayNameItem
      type FSharpField with
        member TargetSignatureConstructor : (LowType -> Member -> ApiSignature)
      val accessibility :
        e:Compiler.SourceCodeServices.FSharpEntity -> Accessibility
      val autoGenericVariableLen : int
      val isByRef : t:Compiler.SourceCodeServices.FSharpType -> bool
      val fsharpTypeToLowType :
        t:Compiler.SourceCodeServices.FSharpType -> LowType option
      val delegateArrow :
        t:Compiler.SourceCodeServices.FSharpType -> LowType list option
      val abbreviationRoot :
        t:Compiler.SourceCodeServices.FSharpType -> LowType option
      val toFlatArrow :
        t:Compiler.SourceCodeServices.FSharpType -> LowType list option
      val listLowType :
        ts:seq<Compiler.SourceCodeServices.FSharpType> -> LowType list option
      val fsharpEntityToLowType :
        x:Compiler.SourceCodeServices.FSharpEntity -> LowType
      val collectTypeConstraints :
        genericParamters:seq<Compiler.SourceCodeServices.FSharpGenericParameter> ->
          TypeConstraint list
      val private ( |Fs_option|_| ) : _arg1:LowType -> LowType option
      val private ( |Fs_Option|_| ) : _arg1:LowType -> LowType option
      val private ( |IsOption|_| ) : _arg1:LowType -> LowType option
      val unwrapFsOptionalParameter : _arg1:LowType -> LowType
      val loadByRef :
        p:Compiler.SourceCodeServices.FSharpParameter -> t:LowType -> LowType
      val curriedParameterGroups :
        isFSharp:bool ->
          t:Compiler.SourceCodeServices.FSharpMemberOrFunctionOrValue ->
            Parameter list list option
      val complementUnitParameter :
        x:Compiler.SourceCodeServices.FSharpMemberOrFunctionOrValue ->
          ps:Parameter list list -> Parameter list list
      val toMemberName : name:DisplayNameItem -> string
      val methodMember :
        isFSharp:bool ->
          x:Compiler.SourceCodeServices.FSharpMemberOrFunctionOrValue ->
            (DisplayNameItem * Member) option
      val propertyMember :
        isFSharp:bool ->
          x:Compiler.SourceCodeServices.FSharpMemberOrFunctionOrValue ->
            (DisplayNameItem * Member) option
      val toModuleValue :
        isFSharp:bool ->
          xml:XmlDocCache option ->
            declaringModuleName:DisplayName ->
              x:Compiler.SourceCodeServices.FSharpMemberOrFunctionOrValue ->
                Api option
      val toTypeExtension :
        isFSharp:bool ->
          xml:XmlDocCache option ->
            declaringModuleName:DisplayName ->
              x:Compiler.SourceCodeServices.FSharpMemberOrFunctionOrValue ->
                Api option
      val toFSharpApi :
        isFSharp:bool ->
          xml:XmlDocCache option ->
            declaringModuleName:DisplayName ->
              x:Compiler.SourceCodeServices.FSharpMemberOrFunctionOrValue ->
                Api option
      val constructorSignature :
        isFSharp:bool ->
          xml:XmlDocCache option ->
            declaringSignatureName:DisplayName ->
              declaringSignature:LowType ->
                x:Compiler.SourceCodeServices.FSharpMemberOrFunctionOrValue ->
                  Api option
      val memberSignature :
        xml:XmlDocCache option ->
          loadMember:(Compiler.SourceCodeServices.FSharpMemberOrFunctionOrValue ->
                        (DisplayNameItem * Member) option) ->
            declaringSignatureName:DisplayName ->
              declaringEntity:Compiler.SourceCodeServices.FSharpEntity ->
                declaringSignature:LowType ->
                  x:Compiler.SourceCodeServices.FSharpMemberOrFunctionOrValue ->
                    Api option
      val isConstructor :
        x:Compiler.SourceCodeServices.FSharpMemberOrFunctionOrValue -> bool
      val toTypeMemberApi :
        xml:XmlDocCache option ->
          declaringSignatureName:DisplayName ->
            declaringEntity:Compiler.SourceCodeServices.FSharpEntity ->
              declaringSignature:LowType ->
                x:Compiler.SourceCodeServices.FSharpMemberOrFunctionOrValue ->
                  Api option
      val toFieldApi :
        xml:XmlDocCache option ->
          accessPath:DisplayName ->
            declaringEntity:Compiler.SourceCodeServices.FSharpEntity ->
              declaringSignature:LowType ->
                field:Compiler.SourceCodeServices.FSharpField -> Api option
      val toUnionCaseField :
        length:int ->
          n:int * field:Compiler.SourceCodeServices.FSharpField ->
            UnionCaseField option
      val toUnionCaseApi :
        xml:XmlDocCache option ->
          accessPath:DisplayName ->
            declaringEntity:Compiler.SourceCodeServices.FSharpEntity ->
              declaringSignature:LowType ->
                unionCase:Compiler.SourceCodeServices.FSharpUnionCase ->
                  Api option
      val resolveConflictGenericArgumnet :
        replacementVariables:LowType list ->
          m:Compiler.SourceCodeServices.FSharpMemberOrFunctionOrValue ->
            (TypeVariable * LowType) list
      val genericParametersAndArguments :
        t:Compiler.SourceCodeServices.FSharpType ->
          (TypeVariable * LowType) list
      val updateInterfaceDeclaringType :
        declaringSignatureName:DisplayName ->
          declaringSignature:LowType -> api:Api -> Api
      val collectTypeAbbreviationDefinition :
        xml:XmlDocCache option ->
          accessPath:DisplayName ->
            e:Compiler.SourceCodeServices.FSharpEntity -> seq<Api>
      val boolToConstraintStatus : _arg1:bool -> ConstraintStatus
      val supportNull : e:Compiler.SourceCodeServices.FSharpEntity -> bool
      val isStruct : e:Compiler.SourceCodeServices.FSharpEntity -> bool
      val hasDefaultConstructor : xs:seq<Member> -> bool
      type EqualityAndComparisonLoaderBuilder =
        {ConditionalAttrName: string;
         CustomAttrName: string;
         NoAttrName: string;
         SatisfyTypes: string list;
         ExpectedInterfaces: string list;}
      val loadEqualityAndComparison :
        builder:EqualityAndComparisonLoaderBuilder ->
          e:Compiler.SourceCodeServices.FSharpEntity ->
            Map<FullIdentity,ConstraintStatus> * ConstraintStatus
      val equality :
        e:Compiler.SourceCodeServices.FSharpEntity ->
          Map<FullIdentity,ConstraintStatus> * ConstraintStatus
      val comparison :
        e:Compiler.SourceCodeServices.FSharpEntity ->
          Map<FullIdentity,ConstraintStatus> * ConstraintStatus
      val typeDefKind :
        e:Compiler.SourceCodeServices.FSharpEntity -> TypeDefinitionKind
      val fullTypeDef :
        xml:XmlDocCache option ->
          name:DisplayName ->
            e:Compiler.SourceCodeServices.FSharpEntity ->
              members:seq<Api> -> (Api * FullTypeDefinition) option
      val moduleDef :
        xml:XmlDocCache option ->
          name:DisplayName -> e:Compiler.SourceCodeServices.FSharpEntity -> Api
      val tryExtractSyntaxes :
        typeDef:FullTypeDefinition ->
          customOperations:seq<string * 'a> -> Set<string> option
      val computationExpression :
        xml:string option ->
          typeDef:FullTypeDefinition ->
            customOperations:seq<string * Member> -> Api option
      val isCustomOperation :
        x:Compiler.SourceCodeServices.FSharpMemberOrFunctionOrValue ->
          string option
      val collectApi :
        xml:XmlDocCache option ->
          accessPath:DisplayName ->
            e:Compiler.SourceCodeServices.FSharpEntity -> seq<Api>
      val collectFromNestedEntities :
        xml:XmlDocCache option ->
          accessPath:DisplayName ->
            e:Compiler.SourceCodeServices.FSharpEntity -> seq<Api>
      val collectFromModule :
        xml:XmlDocCache option ->
          accessPath:DisplayName ->
            e:Compiler.SourceCodeServices.FSharpEntity -> seq<Api>
      val collectFromType :
        xml:XmlDocCache option ->
          accessPath:DisplayName ->
            e:Compiler.SourceCodeServices.FSharpEntity -> seq<Api>
      val tryGetXml :
        assembly:Compiler.SourceCodeServices.FSharpAssembly ->
          System.Xml.Linq.XElement option
      val typeDefsDict :
        xs:seq<FullTypeDefinition> ->
          System.Collections.Generic.Dictionary<FullIdentity,FullTypeDefinition>
      val makeDefAndAbb : api:ApiDictionary -> ApiDictionary
      val load :
        assembly:Compiler.SourceCodeServices.FSharpAssembly -> ApiDictionary
      module NameResolve = begin
        type AssemblyCache =
          System.Collections.Generic.IDictionary<string,DisplayName>
        type NameCache = (string * AssemblyCache) []
        module NameCache = begin
          val tryGetValue :
            key:string -> cache:NameCache -> AssemblyCache option
          val getValue : key:string -> cache:NameCache -> AssemblyCache
        end
        type Context =
          {Cache: NameCache;
           ForwardingLogs:
             System.Collections.Generic.IDictionary<string,TypeForward>;}
        val tryGetValue :
          key:'a ->
            dict:System.Collections.Generic.IDictionary<'a,'b> -> 'b option
        val tryResolve_Name :
          name:Name -> assemblyCache:AssemblyCache -> Name option
        val typeForwarding :
          context:Context -> fromAssemblyName:string -> name:Name -> Name option
        val resolve_Name : context:Context -> name:Name -> Name
        val resolve_LowType : context:Context -> _arg1:LowType -> LowType
        val resolve_Identity : cache:Context -> _arg2:Identity -> Identity
        val resolve_Signature :
          context:Context -> apiSig:ApiSignature -> ApiSignature
        val resolve_TypeConstraint :
          context:Context -> constraint':TypeConstraint -> TypeConstraint
        val resolve_Api : context:Context -> api:Api -> Api
        val resolve_ApiDictionary :
          cache:NameCache ->
            apiDic:ApiDictionary -> ApiDictionary * seq<TypeForward>
        val resolveLoadingName :
          dictionaries:ApiDictionary [] -> (ApiDictionary * seq<TypeForward>) []
      end
      module AutoGenericResolve = begin
        val variables : name:Name -> TypeVariable list
        val replaceVariables :
          table:Map<TypeVariable,TypeVariable> ->
            variables:TypeVariable list -> TypeVariable list
        val replaceName :
          table:Map<TypeVariable,TypeVariable> -> name:Name -> Name
        val resolve_TypeConstraint :
          variableTable:Map<TypeVariable,TypeVariable> ->
            lowTypeTable:Map<TypeVariable,LowType> ->
              constraint':TypeConstraint -> TypeConstraint
        val resolve_ApiSignature :
          table:Map<TypeVariable,LowType> -> apiSig:ApiSignature -> ApiSignature
        val resolve_Api : api:Api -> Api
        val resolveAutoGeneric : apiDict:ApiDictionary -> ApiDictionary
      end
    end
    val loadWithLogs :
      assemblies:Compiler.SourceCodeServices.FSharpAssembly [] ->
        (ApiDictionary * seq<TypeForward>) []
    val load :
      assemblies:Compiler.SourceCodeServices.FSharpAssembly [] ->
        ApiDictionary []
    val databaseName : string
    module internal Serialization = begin
      type T = (string * Api []) []
      val toDumpObj : xs:ApiDictionary [] -> T
      val fromDumpObj : xs:T -> ApiDictionary []
    end
    val internal initMessagePack : Lazy<unit>
    val internal saveStream :
      stream:System.IO.Stream -> dictionaries:ApiDictionary [] -> unit
    val save : path:string -> dictionaries:ApiDictionary [] -> unit
    val internal loadFromStream : stream:System.IO.Stream -> ApiDictionary []
    val loadFromFile : path:string -> ApiDictionary []
  end

namespace FSharpApiSearch
  type TargetSummary =
    {AssemblyName: string;
     PublicApiNumber: int;}
  type FSharpApiSearchClient =
    class
      new : targets:seq<string> * dictionaries:seq<ApiDictionary> ->
              FSharpApiSearchClient
      member Search : query:string * options:SearchOptions -> seq<Result>
      member Sort : results:seq<Result> -> seq<Result>
      member TargetAssemblies : string list
      member Targets : TargetSummary list
      static member DefaultReferences : string list
      static member DefaultTargets : string list
    end

namespace FSharpApiSearch
  type LinkGenerator = Api -> string option
  module LinkGenerator = begin
    val internal genericParameters : api:Api -> TypeVariable list
    val internal toLower : str:string -> string
    val internal urlEncode : str:string -> string
    val urlName : n:DisplayNameItem -> string
    module internal FSharp = begin
      val fullOpReplaceTable :
        System.Collections.Generic.IDictionary<string,string>
      val opReplaceTable : System.Collections.Generic.IDictionary<char,char>
      val isActivePattern : api:Api -> bool
      val replaceOp : name:string -> string
      val isArray : n:DisplayNameItem -> bool
      val generate : api:Api -> string option
    end
    module internal Msdn = begin
      val isGeneric : api:Api -> bool
      val canGenerate : api:Api -> bool
      val generate : api:Api -> string option
    end
    module internal DotNetApiBrowser = begin
      type VariableMemory = System.Collections.Generic.Dictionary<string,string>
      val variableId :
        kind:ApiKind ->
          name:DisplayName ->
            System.Collections.Generic.Dictionary<string,string>
      val nameElementsAndVariableId :
        api:Api ->
          string [] * System.Collections.Generic.Dictionary<string,string>
      val urlPart :
        elems:seq<string> ->
          sb:System.Text.StringBuilder -> System.Text.StringBuilder
      val parameterElement :
        api:Api ->
          variableMemory:LinkGenerator.DotNetApiBrowser.VariableMemory ->
            t:LowType ->
              sb:System.Text.StringBuilder -> System.Text.StringBuilder
      val hasParameter : member':Member -> bool
      val hashPart :
        nameElems:string [] ->
          variableMemory:LinkGenerator.DotNetApiBrowser.VariableMemory ->
            member':Member ->
              api:Api ->
                sb:System.Text.StringBuilder -> System.Text.StringBuilder
      val generate : view:string -> api:Api -> string option
    end
    val fsharp : baseUrl:string -> api:Api -> string option
    val msdn : baseUrl:string -> api:Api -> string option
    val dotNetApiBrowser :
      baseUrl:string -> view:string -> api:Api -> string option
  end

