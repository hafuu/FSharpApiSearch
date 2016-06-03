module ApiLoaderTest

open System.IO
open System.Reflection
open Persimmon
open Persimmon.Syntax.UseTestNameByReflection
open Persimmon.MuscleAssert
open FSharpApiSearch
open TestHelper.DSL
open TestAssemblies

let emptyDef: FullTypeDefinition = {
  Name = []
  FullName = ""
  AssemblyName = ""
  BaseType = None
  AllInterfaces = []
  GenericParameters = []
  TypeConstraints = []
  InstanceMembers = []
  StaticMembers = []

  ImplicitInstanceMembers = []
  ImplicitStaticMembers = []

  SupportNull = NotSatisfy
  ReferenceType = Satisfy
  ValueType = NotSatisfy
  DefaultConstructor = NotSatisfy
  Equality = Satisfy
  Comparison = NotSatisfy
}

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

let unit =
  let Unit = createType "Microsoft.FSharp.Core.Unit" [] |> updateAssembly fscore
  let unit = createType "Microsoft.FSharp.Core.unit" [] |> updateAssembly fscore
  typeAbbreviation Unit unit

let ienumerable t = createType "System.Collections.Generic.IEnumerable<'T>" [ t ] |> updateAssembly mscorlib
let seq t =
  let seq = createType "Microsoft.FSharp.Collections.seq<'T>" [ t ] |> updateAssembly fscore
  typeAbbreviation (ienumerable t) seq

let fsharpList t = createType "Microsoft.FSharp.Collections.List<'T>" [ t] |> updateAssembly fscore
let list t =
  let list = createType "Microsoft.FSharp.Collections.list<'T>" [ t ] |> updateAssembly fscore
  typeAbbreviation (fsharpList t) list

let fsharpOption t = createType "Microsoft.FSharp.Core.Option<'T>" [ t ] |> updateAssembly fscore
let option t =
  let opt = createType "Microsoft.FSharp.Core.option<'T>" [ t ] |> updateAssembly fscore
  typeAbbreviation (fsharpOption t) opt

let string =
  let String = createType "System.String" [] |> updateAssembly mscorlib
  let string = createType "Microsoft.FSharp.Core.string" [] |> updateAssembly fscore
  typeAbbreviation String string

let map k v = createType "Microsoft.FSharp.Collections.Map<'Key, 'Value>" [ k; v ] |> updateAssembly fscore

let array = array >> updateAssembly fscore
let array2D = array2D >> updateAssembly fscore

let istructualEquatable = createType "System.Collections.IStructuralEquatable" [] |> updateAssembly mscorlib
let iequatable x = createType "System.IEquatable<'t>" [ x ] |> updateAssembly mscorlib
let genericIComparable x = createType "System.IComparable<'t>" [ x ] |> updateAssembly mscorlib
let icomparable = createType "System.IComparable" [] |> updateAssembly mscorlib
let istructuralComparable = createType "System.IStructuralComparable" [] |> updateAssembly mscorlib

let valuetype = createType "System.ValueType" [] |> updateAssembly mscorlib

let testApi (assembly: TestCase<ApiDictionary>) (name, expected) = test {
  let! apiDict = assembly
  let name = Name.friendlyNameOfString name
  let actual =
    Seq.filter (fun x -> x.Name = name) apiDict.Api 
    |> Seq.map (fun x -> x.Signature)
    |> Seq.filter (function (ApiSignature.FullTypeDefinition _ | ApiSignature.TypeAbbreviation _) -> false | _ -> true)
    |> Seq.toList
    |> List.sort
  let expected = expected |> List.sort
  do! actual |> assertEquals expected
}

let testFullTypeDef' (assembly: TestCase<ApiDictionary>) filter (name, expected) = test {
  let! apiDict = assembly
  let actual =
    Seq.filter (fun x -> x.Name = name) apiDict.Api
    |> Seq.map (fun x -> x.Signature)
    |> Seq.choose (function ApiSignature.FullTypeDefinition x -> Some x | _ -> None)
    |> Seq.head
  do! (filter actual) |> assertEquals expected
}

let testFullTypeDef (assembly: TestCase<ApiDictionary>) (expected: FullTypeDefinition) = testFullTypeDef' assembly id (FriendlyName expected.Name, expected)

let testConstraints (assembly: TestCase<ApiDictionary>) (name, expectedTarget, expectedConstraints) = test {
  let! apiDict = assembly
  let name = Name.friendlyNameOfString name
  let actual = Seq.find (fun x -> x.Name = name) apiDict.Api
  do! actual.Signature |> assertEquals expectedTarget
  do! (List.sort actual.TypeConstraints) |> assertEquals (List.sort expectedConstraints)
}

module FSharp =
  let testApi = testApi fsharpAssemblyApi
  let testConstraints = testConstraints fsharpAssemblyApi

  let loadModuleMemberTest = parameterize {
    source [
      "PublicModule.nonGenericFunction", [ moduleFunction [ int; int; int ] ]
      "PublicModule.genericFunction", [ moduleFunction [ variable "a"; variable "b"; variable "b" ] ]
      "PublicModule.tupleFunction", [ moduleFunction [ tuple [ variable "a"; variable "b"; variable "c" ]; variable "a" ] ]
      "PublicModule.value", [ moduleValue int ]
      "PublicModule.NestedModule.publicFunction", [ moduleFunction [ int; int ] ]
      "PublicModule.listmap", [ moduleFunction [ arrow [ variable "a"; variable "b" ]; list (variable "a"); list (variable "b") ] ]
      "PublicModule.partialGenericMap", [ moduleFunction [ map int (variable "a"); variable "a" ] ]
      "PublicModule.floatReturnType", [ moduleFunction [ int; float ] ]
      "PublicModule.array", [ moduleValue (array int) ]
      "PublicModule.array2d", [ moduleValue (array2D int) ]
      "PublicModule.nestedArray", [ moduleValue (array (array2D int)) ]
      "PublicModule.( |ActivePattern| )", [ activePattern [ int; string ] ]
      "PublicModule.( |PartialActivePattern|_| )", [ partialActivePattern [ variable "a"; variable "a"; option (variable "a") ] ]
    ]
    run testApi
  }

  let loadStaticMemberTest =
    let t = createType "TopLevelNamespace.StaticMemberClass" [] |> updateAssembly fsharpAssemblyName
    parameterize {
      source [
        "TopLevelNamespace.StaticMemberClass.NoArgumentMethod", [ staticMember t (method' "NoArgumentMethod" [ unit ] int) ]
        "TopLevelNamespace.StaticMemberClass.OneArgumentMethod", [ staticMember t (method' "OneArgumentMethod" [ int ] int) ]
        "TopLevelNamespace.StaticMemberClass.NonCurriedMethod", [ staticMember t (method' "NonCurriedMethod" [ int; string ] int) ]
        "TopLevelNamespace.StaticMemberClass.CurriedMethod", [ staticMember t (curriedMethod "CurriedMethod" [ int; string ] int) ]
        "TopLevelNamespace.StaticMemberClass.TupleMethod", [ staticMember t (method' "TupleMethod" [ tuple [ int; string ] ] int) ]
        "TopLevelNamespace.StaticMemberClass.InferredFloat", [ staticMember t (method' "InferredFloat" [ float ] float) ]
        "TopLevelNamespace.StaticMemberClass.AnnotatedFloat", [ staticMember t (method' "AnnotatedFloat" [ float ] float) ]
        "TopLevelNamespace.StaticMemberClass", [ constructor' t (method' "StaticMemberClass" [ unit ] t); constructor' t (method' "StaticMemberClass" [ int ] t) ]
        "TopLevelNamespace.StaticMemberClass.OverloadMethod", [ staticMember t (method' "OverloadMethod" [ int ] int); staticMember t (method' "OverloadMethod" [ string; int ] string) ]
        "TopLevelNamespace.StaticMemberClass.Getter", [ staticMember t (property' "Getter" PropertyKind.Get [] string) ]
        "TopLevelNamespace.StaticMemberClass.Setter", [ staticMember t (property' "Setter" PropertyKind.Set [] int) ]
        "TopLevelNamespace.StaticMemberClass.GetterSetter", [ staticMember t (property' "GetterSetter" PropertyKind.GetSet [] float) ]
        "TopLevelNamespace.StaticMemberClass.IndexedGetter", [ staticMember t (property' "IndexedGetter" PropertyKind.Get [ int ] string) ]
        "TopLevelNamespace.StaticMemberClass.IndexedSetter", [ staticMember t (property' "IndexedSetter" PropertyKind.Set [ int ] string) ]
        "TopLevelNamespace.StaticMemberClass.IndexedGetterSetter", [ staticMember t (property' "IndexedGetterSetter" PropertyKind.GetSet [ string ] int) ]
      ]
      run testApi
    }

  let loadInstanceMemberTest =
    let t = createType "TopLevelNamespace.InstanceMemberClass" [] |> updateAssembly fsharpAssemblyName
    parameterize {
      source [
        "TopLevelNamespace.InstanceMemberClass.NoArgumentMethod", [ instanceMember t (method' "NoArgumentMethod" [ unit ] int) ]
        "TopLevelNamespace.InstanceMemberClass.OneArgumentMethod", [ instanceMember t (method' "OneArgumentMethod" [ int ] int) ]
        "TopLevelNamespace.InstanceMemberClass.NonCurriedMethod", [ instanceMember t (method' "NonCurriedMethod" [ int; string ] int) ]
        "TopLevelNamespace.InstanceMemberClass.CurriedMethod", [ instanceMember t (curriedMethod "CurriedMethod" [ int; string ] int) ]
        "TopLevelNamespace.InstanceMemberClass.TupleMethod", [ instanceMember t (method' "TupleMethod" [ tuple [ int; string ] ] int) ]
        "TopLevelNamespace.InstanceMemberClass.OverloadMethod", [ instanceMember t (method' "OverloadMethod" [ int ] int); instanceMember t (method' "OverloadMethod" [ string; int ] string) ]
        "TopLevelNamespace.InstanceMemberClass.Getter", [ instanceMember t (property' "Getter" PropertyKind.Get [] string) ]
        "TopLevelNamespace.InstanceMemberClass.Setter", [ instanceMember t (property' "Setter" PropertyKind.Set [] int) ]
        "TopLevelNamespace.InstanceMemberClass.GetterSetter", [ instanceMember t (property' "GetterSetter" PropertyKind.GetSet [] float) ]
        "TopLevelNamespace.InstanceMemberClass.IndexedGetter", [ instanceMember t (property' "IndexedGetter" PropertyKind.Get [ int ] string) ]
        "TopLevelNamespace.InstanceMemberClass.IndexedSetter", [ instanceMember t (property' "IndexedSetter" PropertyKind.Set [ int ] string) ]
        "TopLevelNamespace.InstanceMemberClass.IndexedGetterSetter", [ instanceMember t (property' "IndexedGetterSetter" PropertyKind.GetSet [ string ] int) ]
      ]
      run testApi
    }

  let loadGenericClassTest =
    let t = createType "TopLevelNamespace.GenericClass<'a>" [ variable "a" ] |> updateAssembly fsharpAssemblyName
    parameterize {
      source [
        "TopLevelNamespace.GenericClass<'a>.Method", [ instanceMember t (method' "Method" [ variable "a" ] int) ]
        "TopLevelNamespace.GenericClass<'a>", [ constructor' t (method' "GenericClass" [ unit ] t) ]
      ]
      run testApi
    }

  let loadRecordTest =
    let t = createType "OtherTypes.Record" [] |> updateAssembly fsharpAssemblyName
    parameterize {
      source [
        "OtherTypes.Record.FieldA", [ instanceMember t (field "FieldA" int) ]
        "OtherTypes.Record.FieldB", [ instanceMember t (field "FieldB" string) ]
        "OtherTypes.Record.InstanceMethod", [ instanceMember t (method' "InstanceMethod" [ unit ] int) ]
        "OtherTypes.Record.InstanceProperty", [ instanceMember t (property' "InstanceProperty" PropertyKind.GetSet [] int) ]
        "OtherTypes.Record.StaticMethod", [ staticMember t (method' "StaticMethod" [ unit ] string) ]
      ]
      run testApi  
    }

  let loadGenericRecordTest =
    let t = createType "OtherTypes.GenericRecord<'a>" [ variable "a" ] |> updateAssembly fsharpAssemblyName
    parameterize {
      source [
        "OtherTypes.GenericRecord<'a>.Field", [ instanceMember t (field "Field" (variable "a")) ]
      ]
      run testApi  
    }

  let loadUnionTest =
    let t = createType "OtherTypes.Union" [] |> updateAssembly fsharpAssemblyName
    parameterize {
      source [
        "OtherTypes.Union.InstanceMethod", [ instanceMember t (method' "InstanceMethod" [ unit ] int) ]
      ]
      run testApi
    }

  let laodStructTest =
    let t = createType "OtherTypes.Struct" [] |> updateAssembly fsharpAssemblyName
    parameterize {
      source [
        "OtherTypes.Struct.A", [ instanceMember t (field "A" int) ]
        "OtherTypes.Struct.B", [ instanceMember t (field "B" string) ]
        "OtherTypes.Struct.InstanceMethod", [ instanceMember t (method' "InstanceMethod" [ unit ] int) ]
      ]
      run testApi
    }

  let laodEnumTest =
    let t = createType "OtherTypes.Enum" [] |> updateAssembly fsharpAssemblyName
    parameterize {
      source [
        "OtherTypes.Enum.A", [ staticMember t (field "A" t) ]
        "OtherTypes.Enum.B", [ staticMember t (field "B" t) ]
      ]
      run testApi
    }

  let loadInterfaceTest =
    let t = createType "TopLevelNamespace.Interface" [] |> updateAssembly fsharpAssemblyName
    parameterize {
      source [
        "TopLevelNamespace.Interface.Method", [ instanceMember t (method' "Method" [ int; string] int ) ]
        "TopLevelNamespace.Interface.Property", [ instanceMember t (property' "Property" PropertyKind.GetSet [] string ) ]
      ]
      run testApi
    }

  let interfaceInheritanceTest =
    let child = createType "InterfaceInheritance.ChildInterface" [] |> updateAssembly fsharpAssemblyName
    let genericChild = createType "InterfaceInheritance.GenericChildInterface<'a>" [ variable "a" ] |> updateAssembly fsharpAssemblyName
    let intChild = createType "InterfaceInheritance.IntChildInterface" [] |> updateAssembly fsharpAssemblyName
    let confrict = createType "InterfaceInheritance.ConflictParameterInterface<'b>" [ variable "b" ] |> updateAssembly fsharpAssemblyName
    parameterize {
      source [
        "InterfaceInheritance.ChildInterface.ChildMethod", [ instanceMember child (method' "ChildMethod" [ unit ] float) ]
        "InterfaceInheritance.ChildInterface.ParentMethod", [ instanceMember child (method' "ParentMethod" [ unit] string) ]
        "InterfaceInheritance.ChildInterface.GrandParentMethod", [ instanceMember child (method' "GrandParentMethod" [ unit ] int) ]
        "InterfaceInheritance.GenericChildInterface<'a>.ParentMethod", [ instanceMember genericChild (method' "ParentMethod" [ variable "a" ] (variable "b")) ]
        "InterfaceInheritance.GenericChildInterface<'a>.GrandParentMethod", [ instanceMember genericChild (method' "GrandParentMethod" [ variable "a" ] (variable "u")) ]
        "InterfaceInheritance.ConflictParameterInterface<'b>.ParentMethod", [ instanceMember confrict (method' "ParentMethod" [ variable "b" ] (variable "b1")) ]
        "InterfaceInheritance.IntChildInterface.ParentMethod", [ instanceMember intChild (method' "ParentMethod" [ int ] (variable "b")) ]
      ]
      run testApi
    }

  // bug #60
  let internalInterfaceTest = test {
    let! mscorDict = mscorlibApi
    let tuple = mscorDict.TypeDefinitions |> Array.find (fun x -> x.Name = FriendlyName.ofString "System.Tuple<'T1, 'T2>" && x.GenericParameters.Length = 2)
    let existsITuple = tuple.AllInterfaces |> Seq.exists (function Identity (FullIdentity i) -> i.Name = Name.friendlyNameOfString "System.ITuple" | _ -> false)
    do! existsITuple |> assertEquals false
  }

  let nonloadedTest =
    parameterize {
      source[
        "PublicModule.internalFunction"
        "PublicModule.privateFunction"
        "InternalModule.publicFunction"
        "PrivateModule.publicFunction"
        "OtherTypes.Enum.value__"
        "TopLevelNamespace.StaticMemberClass.PrivateMethod"
        "TopLevelNamespace.StaticMemberClass.InternalMethod"
        "TopLevelNamespace.PrivateClass.PublicMethod"
        "TopLevelNamespace.InternalClass.PublicMethod"
      ]
      run (fun x -> testApi (x, []))
    }
  let typeConstraintsTest =
    let subtypeClass = createType "TypeConstraints.SubTypeClass<'a>" [ variable "a" ] |> updateAssembly fsharpAssemblyName
    let subtypeRecord = createType "TypeConstraints.SubTypeRecord<'a>" [ variable "a" ] |> updateAssembly fsharpAssemblyName
    parameterize {
      source [
        // subtype
        ("TypeConstraints.subtypeConFunction",
          (moduleFunction [ variable "Tseq"; unit ]),
          [ constraint' [ "Tseq"] (SubtypeConstraints (seq int)) ])
        ("TypeConstraints.SubTypeClass<'a>.Method",
          (staticMember subtypeClass (method' "Method" [ variable "a"; variable "b" ] unit)),
          [ constraint' [ "a" ] (SubtypeConstraints (seq int)); constraint' [ "b" ] (SubtypeConstraints (seq string)) ])
        ("TypeConstraints.SubTypeRecord<'a>.Field",
          (instanceMember subtypeRecord (field "Field" (variable "a"))),
          [ constraint' [ "a" ] (SubtypeConstraints (seq int)) ])

        // nullness
        ("TypeConstraints.nullnessFunction",
          (moduleFunction [ variable "a"; unit ]),
          [ constraint' [ "a"] NullnessConstraints ])

        // member
        ("TypeConstraints.memberConstraint_instanceMethod1",
          (moduleFunction [ variable "a"; unit ]),
          [ constraint' [ "a"] (MemberConstraints (MemberModifier.Instance, member' "Method" MemberKind.Method [ int; int ] int)) ])
        ("TypeConstraints.memberConstraint_instanceMethod2",
          (moduleFunction [ variable "a"; unit ]),
          [ constraint' [ "a"] (MemberConstraints (MemberModifier.Instance, member' "Method" MemberKind.Method [ int; int ] int)) ])
        ("TypeConstraints.memberConstraint_tupleMethod",
          (moduleFunction [ variable "a"; unit ]),
          [ constraint' [ "a"] (MemberConstraints (MemberModifier.Instance, member' "Method" MemberKind.Method [ tuple [ int; int ] ] int)) ])
        ("TypeConstraints.memberConstraint_staticMember",
          (moduleFunction [ variable "a"; unit ]),
          [ constraint' [ "a"] (MemberConstraints (MemberModifier.Static, member' "Method" MemberKind.Method [ int ] int)) ])
        ("TypeConstraints.memberConstraint_or",
          (moduleFunction [ variable "a"; variable "b"; unit ]),
          [ constraint' [ "a"; "b" ] (MemberConstraints (MemberModifier.Static, member' "Method" MemberKind.Method [ int ] int)) ])
        ("TypeConstraints.memberConstraint_noArgumentMember", // no argument means get property
          (moduleFunction [ variable "a"; unit ]),
          [ constraint' [ "a"] (MemberConstraints (MemberModifier.Instance, member' "get_Method" MemberKind.Method [ unit ] int)) ])
        ("TypeConstraints.memberConstraint_unitMethod",
          (moduleFunction [ variable "a"; unit ]),
          [ constraint' [ "a"] (MemberConstraints (MemberModifier.Instance, member' "Method" MemberKind.Method [ unit ] int)) ])
        ("TypeConstraints.memberConstraint_unitIntMethod",
          (moduleFunction [ variable "a"; unit ]),
          [ constraint' [ "a"] (MemberConstraints (MemberModifier.Instance, member' "Method" MemberKind.Method [ unit; int ] int)) ])
        ("TypeConstraints.memberConstraint_getterMethod",
          (moduleFunction [ variable "a"; unit ]),
          [ constraint' [ "a"] (MemberConstraints (MemberModifier.Instance, member' "get_Property" MemberKind.Method [ unit ] int)) ])
        ("TypeConstraints.memberConstraint_setterMethod",
          (moduleFunction [ variable "a"; unit ]),
          [ constraint' [ "a"] (MemberConstraints (MemberModifier.Instance, member' "set_Property" MemberKind.Method [ int ] unit)) ])
        ("TypeConstraints.memberConstraint_getProperty",
          (moduleFunction [ variable "a"; unit ]),
          [ constraint' [ "a"] (MemberConstraints (MemberModifier.Instance, member' "get_Property" MemberKind.Method [ unit ] int)) ])
        ("TypeConstraints.memberConstraint_setProperty",
          (moduleFunction [ variable "a"; unit ]),
          [ constraint' [ "a"] (MemberConstraints (MemberModifier.Instance, member' "set_Property" MemberKind.Method [ int ] unit)) ])
        ("TypeConstraints.memberConstraint_indexedGetProperty",
          (moduleFunction [ variable "a"; unit ]),
          [ constraint' [ "a"] (MemberConstraints (MemberModifier.Instance, member' "get_Property" MemberKind.Method [ int ] int)) ])
        ("TypeConstraints.memberConstraint_indexedSetProperty",
          (moduleFunction [ variable "a"; unit ]),
          [ constraint' [ "a"] (MemberConstraints (MemberModifier.Instance, member' "set_Property" MemberKind.Method [ int; int ] unit)) ])
        ("TypeConstraints.memberConstraint_staticNoArgumentMember", // no argument means get property
          (moduleFunction [ variable "a"; unit ]),
          [ constraint' [ "a"] (MemberConstraints (MemberModifier.Static, member' "get_Method" MemberKind.Method [ unit ] int)) ])
        ("TypeConstraints.memberConstraint_staticUnitMethod",
          (moduleFunction [ variable "a"; unit ]),
          [ constraint' [ "a"] (MemberConstraints (MemberModifier.Static, member' "Method" MemberKind.Method [ unit ] int)) ])
        ("TypeConstraints.memberConstraint_staticGetterMethod",
          (moduleFunction [ variable "a"; unit ]),
          [ constraint' [ "a"] (MemberConstraints (MemberModifier.Static, member' "get_Property" MemberKind.Method [ unit ] int)) ])
        ("TypeConstraints.memberConstraint_staticSetterMethod",
          (moduleFunction [ variable "a"; unit ]),
          [ constraint' [ "a"] (MemberConstraints (MemberModifier.Static, member' "set_Property" MemberKind.Method [ int ] unit)) ])
        ("TypeConstraints.memberConstraint_staticGetProperty",
          (moduleFunction [ variable "a"; unit ]),
          [ constraint' [ "a"] (MemberConstraints (MemberModifier.Static, member' "get_Property" MemberKind.Method [ unit ] int)) ])
        ("TypeConstraints.memberConstraint_staticSetProperty",
          (moduleFunction [ variable "a"; unit ]),
          [ constraint' [ "a"] (MemberConstraints (MemberModifier.Static, member' "set_Property" MemberKind.Method [ int ] unit)) ])
        ("TypeConstraints.memberConstraint_generic",
          (moduleFunction [ variable "a"; unit ]),
          [ constraint' [ "a"] (MemberConstraints (MemberModifier.Instance, member' "Method" MemberKind.Method [ variable "b" ] unit)) ])
        ("TypeConstraints.memberConstraint_operator",
          (moduleFunction [ variable "a"; variable "b"; unit ]),
          [ constraint' [ "a"; "b"; ] (MemberConstraints (MemberModifier.Static, member' "op_Addition" MemberKind.Method [ variable "a"; variable "b" ] (variable "c"))) ])

        // value, reference
        ("TypeConstraints.valueTypeConstraint",
          (moduleFunction [ variable "a"; unit ]),
          [ constraint' [ "a"; ] ValueTypeConstraints ])
        ("TypeConstraints.referenceTypeConstraint",
          (moduleFunction [ variable "a"; unit ]),
          [ constraint' [ "a"; ] ReferenceTypeConstraints ])

        // default constructor
        ("TypeConstraints.defaultConstructorConstraint",
          (moduleFunction [ variable "a"; unit ]),
          [ constraint' [ "a"; ] DefaultConstructorConstraints ])

        // equality
        ("TypeConstraints.equalityConstraint",
          (moduleFunction [ variable "a"; unit ]),
          [ constraint' [ "a"; ] EqualityConstraints ])

        // comparison
        ("TypeConstraints.comparisonConstraint",
          (moduleFunction [ variable "a"; unit ]),
          [ constraint' [ "a"; ] ComparisonConstraints ])
      ]
      run testConstraints
    }

  let fullTypeDefinitionTest =
    let plainClass = {
      emptyDef with
        Name = FriendlyName.ofString "FullTypeDefinition.PlainClass"
        FullName = "FullTypeDefinition.PlainClass"
        AssemblyName = fsharpAssemblyName
        BaseType = Some obj
        DefaultConstructor = Satisfy
    }

    let plainInterface = {
      emptyDef with
        Name = FriendlyName.ofString "FullTypeDefinition.PlainInterface"
        FullName = "FullTypeDefinition.PlainInterface"
        AssemblyName = fsharpAssemblyName
    }

    let interfaceImplClass = {
      emptyDef with
        Name = FriendlyName.ofString "FullTypeDefinition.InterfaceImplClass"
        FullName = "FullTypeDefinition.InterfaceImplClass"
        AssemblyName = fsharpAssemblyName
        BaseType = Some obj
        AllInterfaces = [ Identity (FullIdentity plainInterface.FullIdentity) ]
        DefaultConstructor = Satisfy
    }

    let interfaceInherit = {
      emptyDef with
        Name = FriendlyName.ofString "FullTypeDefinition.InterfaceInherit"
        FullName = "FullTypeDefinition.InterfaceInherit"
        AssemblyName = fsharpAssemblyName
        AllInterfaces = [ Identity (FullIdentity plainInterface.FullIdentity) ]
    }

    let supportNullClass = {
      emptyDef with
        Name = FriendlyName.ofString "FullTypeDefinition.SupportNullClass"
        FullName = "FullTypeDefinition.SupportNullClass"
        AssemblyName = fsharpAssemblyName
        BaseType = Some obj
        SupportNull = Satisfy
        DefaultConstructor = Satisfy
    }

    let nonSupportNullSubClass = {
      emptyDef with
        Name = FriendlyName.ofString "FullTypeDefinition.SupportNullSubClass"
        FullName = "FullTypeDefinition.SupportNullSubClass"
        AssemblyName = fsharpAssemblyName
        BaseType = Some (Identity (FullIdentity supportNullClass.FullIdentity))
        SupportNull = NotSatisfy
        DefaultConstructor = Satisfy
    }

    let supportNullInterface = {
      emptyDef with
        Name = FriendlyName.ofString "FullTypeDefinition.SupportNullInterface"
        FullName = "FullTypeDefinition.SupportNullInterface"
        AssemblyName = fsharpAssemblyName
        SupportNull = Satisfy
    }

    let supportNullSubInterface = {
      emptyDef with
        Name = FriendlyName.ofString "FullTypeDefinition.SupportNullSubInterface"
        FullName = "FullTypeDefinition.SupportNullSubInterface"
        AssemblyName = fsharpAssemblyName
        AllInterfaces = [ Identity (FullIdentity supportNullInterface.FullIdentity) ]
        SupportNull = Satisfy
    }

    let nonSupportNullSubInterface = {
      emptyDef with
        Name = FriendlyName.ofString "FullTypeDefinition.NonSupportNullSubInterface"
        FullName = "FullTypeDefinition.NonSupportNullSubInterface"
        AssemblyName = fsharpAssemblyName
        AllInterfaces = [ Identity (FullIdentity supportNullInterface.FullIdentity) ]
        SupportNull = NotSatisfy
    }

    let withoutDefaultConstructor = {
      emptyDef with
        Name = FriendlyName.ofString "FullTypeDefinition.WithoutDefaultConstructor"
        FullName = "FullTypeDefinition.WithoutDefaultConstructor"
        AssemblyName = fsharpAssemblyName
        BaseType = Some obj
        DefaultConstructor = NotSatisfy
    }

    let memberClassId = createType "FullTypeDefinition.MemberClass" [] |> updateAssembly fsharpAssemblyName

    let memberClass = {
      emptyDef with
        Name = FriendlyName.ofString "FullTypeDefinition.MemberClass"
        FullName = "FullTypeDefinition.MemberClass"
        AssemblyName = fsharpAssemblyName
        BaseType = Some obj
        StaticMembers =
          [
            method' "StaticMethod" [ unit ] int
            method' "op_Addition" [ memberClassId; int ] memberClassId
          ]
        InstanceMembers =
          [
            method' "InstanceMethod" [ int ] int
            property' "Property" PropertyKind.Get [] int
          ]
        DefaultConstructor = Satisfy
    }

    parameterize {
      source [
        plainClass
        plainInterface
        interfaceImplClass
        interfaceInherit

        supportNullClass
        nonSupportNullSubClass
        supportNullInterface
        supportNullSubInterface
        nonSupportNullSubInterface

        memberClass

        withoutDefaultConstructor
      ]
      run (testFullTypeDef fsharpAssemblyApi)
    }

  let testEquality = parameterize {
    source [
      "EqualityType", Satisfy
      "NoEqualityType", NotSatisfy
      "InferredEqualityRecord", Satisfy
      "InferredNoEqualityRecord", NotSatisfy
      "InferredEqualityUnion", Satisfy
      "InferredNoEqualityUnion", NotSatisfy
      "CustomEqualityRecord", Satisfy
      "GenericClass<'a, 'b>", Satisfy
      "EqualityConditionalClass<'a, 'b>", Dependence [ "a" ]
      "CustomEqualityAndConditionalRecord<'a, 'b>", Dependence [ "a" ]
      "EqualityGenericRecord<'a, 'b>", Dependence [ "a"; "b" ]
      "NoEqualityGenericRecord<'a, 'b>", NotSatisfy
      "EqualityWithGenericType", Satisfy
      "NoEqualityWithGenericType", NotSatisfy
      "RecursiveType<'a>", Dependence [ "a" ]
      "TupleAbbreviationFieldRecord", Satisfy
      "FunctionAbbreviationFieldRecord", NotSatisfy
    ]
    run (fun (name, expected) ->
      let testName = FriendlyName.ofString name @ FriendlyName.ofString "FullTypeDefinition.EqualityConstraints"
      testFullTypeDef' fsharpAssemblyApi (fun x -> x.Equality) (FriendlyName testName, expected))
  }

  let testComparison = parameterize {
    source [
      "ComparisonType", Satisfy
      "NotComparisonType", NotSatisfy
      "StructualComparisonType", Satisfy
      "InferredComparisonRecord", Satisfy
      "InferredNoComparisonRecord", NotSatisfy
      "NoComparisonRecord", NotSatisfy
      "InferredComparisonUnion", Satisfy
      "InferredNoComparisonUnion", NotSatisfy
      "CustomComparisonRecord", Satisfy
      "GenericNoComparisonClass<'a, 'b>", NotSatisfy
      "GenericComparisonClass<'a, 'b>", Satisfy
      "ComparisonConditionalClass<'a, 'b>", Dependence [ "a" ]
      "CustomComparisonAndConditionalRecord<'a, 'b>", Dependence [ "a" ]
      "ComparisonGenericRecord<'a, 'b>", Dependence [ "a"; "b" ]
      "NoComparisonGenericRecord<'a, 'b>", NotSatisfy
      "ComparisonWithGenericType", Satisfy
      "NoComparisonWithGenericType", NotSatisfy
      "RecursiveType<'a>", Dependence [ "a" ]
      "TupleAbbreviationFieldRecord", Satisfy
      "FunctionAbbreviationFieldRecord", NotSatisfy
    ]
    run (fun (name, expected) ->
      testFullTypeDef' fsharpAssemblyApi (fun x -> x.Comparison) (Name.friendlyNameOfString ("FullTypeDefinition.ComparisonConstraints." + name), expected))
  }

  let compilerInternalTest = test {
    let! fscoreDict = fscoreApi
    let actual =
      fscoreDict.Api
      |> Seq.filter (fun x ->
        let name = x.Name.Print()
        name.StartsWith("Microsoft.FSharp.Core.LanguagePrimitives.") || name.StartsWith("Microsoft.FSharp.Core.Operators.OperatorIntrinsics.")
      )
      |> Seq.length
    do! actual |> assertEquals 0
  }

module SpecialType =
  let tupleName = Name.friendlyNameOfString "System.Tuple<'T1, 'T2>"
  let tupleNullnessTest =
    testFullTypeDef' mscorlibApi (fun x -> x.SupportNull) (tupleName, NotSatisfy)
  let tupleEqualityTest =
    testFullTypeDef' mscorlibApi (fun x -> x.Equality) (tupleName, Dependence [ "T1"; "T2" ])
  let tupleComparisonTest =
    testFullTypeDef' mscorlibApi (fun x -> x.Comparison) (tupleName, Dependence [ "T1"; "T2" ])

  let arrayName = Name.friendlyNameOfString "Microsoft.FSharp.Core.[]<'T>"

  let arrayNullnessTest =
    testFullTypeDef' fscoreApi (fun x -> x.SupportNull) (arrayName, Satisfy)
  let arrayEquality =
    testFullTypeDef' fscoreApi (fun x -> x.Equality) (arrayName, Dependence [ "T" ])
  let arrayComparison =
    testFullTypeDef' fscoreApi (fun x -> x.Comparison) (arrayName, Dependence [ "T" ])

  let intptrComparison =
    testFullTypeDef' mscorlibApi (fun x -> x.Comparison) (Name.friendlyNameOfString "System.IntPtr", Satisfy)
  let uintptrComparison =
    testFullTypeDef' mscorlibApi (fun x -> x.Comparison) (Name.friendlyNameOfString "System.UIntPtr", Satisfy)

  let int32ImplicitStaticMembers =
    testFullTypeDef' mscorlibApi (fun x -> x.ImplicitStaticMembers |> List.exists (fun x -> x.Name = "op_Addition")) (Name.friendlyNameOfString "System.Int32", true)

  let Unit =
    testFullTypeDef' fscoreApi (fun x -> x.AssemblyName) (Name.friendlyNameOfString "Microsoft.FSharp.Core.Unit", "FSharp.Core")

  let UnionCaseInfo =
    testFullTypeDef' fscoreApi (fun x -> x.AssemblyName) (Name.friendlyNameOfString "Microsoft.FSharp.Reflection.UnionCaseInfo", "FSharp.Core")

  let Delegate =
    testFullTypeDef' csharpAssemblyApi (fun x -> x.AssemblyName) (Name.friendlyNameOfString "CSharpLoadTestAssembly.TestDelegate", csharpAssemblyName)

module TypeAbbreviation =
  let A = createType "TypeAbbreviations.A" [] |> updateAssembly fsharpAssemblyName
  let typeAbbreviationTest = parameterize {
    source [
      typeAbbreviationDef "TypeAbbreviations.GenericTypeAbbreviation<'b>" (createType "TypeAbbreviations.Original<'a>" [ variable "b" ] |> updateAssembly fsharpAssemblyName)
      typeAbbreviationDef "TypeAbbreviations.SpecializedTypeAbbreviation" (createType "TypeAbbreviations.Original<'a>" [ A ] |> updateAssembly fsharpAssemblyName)
      
      { typeAbbreviationDef "TypeAbbreviations.NestedTypeAbbreviation" (createType "TypeAbbreviations.Original<'a>"[ A ]  |> updateAssembly fsharpAssemblyName) with
          Abbreviated = createType "TypeAbbreviations.SpecializedTypeAbbreviation" [] |> updateAssembly fsharpAssemblyName
      }
      typeAbbreviationDef "TypeAbbreviations.NestedModule.TypeAbbreviationInModule<'a>" (createType "TypeAbbreviations.Original<'a>"[ variable "a" ]  |> updateAssembly fsharpAssemblyName)
      typeAbbreviationDef "TypeAbbreviations.FunctionAbbreviation" (arrow [ int; int ])
    ]
    run (fun entry -> test {
      let! api = fsharpAssemblyApi
      let expected = { entry with AssemblyName = fsharpAssemblyName }
      let actual = api.TypeAbbreviations |> Seq.tryFind (fun x -> x.FullName = expected.FullName)
      do! actual |> assertEquals (Some expected)
    })
  }

  let functionWithFunctionAbbreviationTest =
    let t = { Abbreviation = createType "TypeAbbreviations.FunctionAbbreviation" [] |> updateAssembly fsharpAssemblyName
              Original = arrow [ int; int ] }
    testApi fsharpAssemblyApi ("TypeAbbreviations.functionWithFunctionAbbreviation", [ moduleFunction [ TypeAbbreviation t; TypeAbbreviation t ] ])

module TypeExtension =
  let testApi = testApi fsharpAssemblyApi
  let testModule = FriendlyName.ofString "TypeExtensions"
  let fsharpList_t = fsharpList (variable "T")

  let typeExtensionTest = parameterize {
    source [
      "System.Int32.Method", [ typeExtension int32 testModule MemberModifier.Instance (method' "Method" [ int ] unit) ]
      "System.Int32.CurriedMethod", [ typeExtension int32 testModule MemberModifier.Instance (curriedMethod "CurriedMethod" [ int; string ] int) ]
      "System.Int32.NoncurriedMethod", [ typeExtension int32 testModule MemberModifier.Instance (method' "NoncurriedMethod" [ int; string ] string) ]

      "System.Int32.GetterProperty", [ typeExtension int32 testModule MemberModifier.Instance (property' "GetterProperty" PropertyKind.Get [] int) ]
      "System.Int32.SetterProperty", [ typeExtension int32 testModule MemberModifier.Instance (property' "SetterProperty" PropertyKind.Set [] string) ]
      "System.Int32.GetterSetterProperty", [
          typeExtension int32 testModule MemberModifier.Instance (property' "GetterSetterProperty" PropertyKind.Get [] string)
          typeExtension int32 testModule MemberModifier.Instance (property' "GetterSetterProperty" PropertyKind.Set [] string) 
        ]

      "System.Int32.GetterIndexedProperty", [ typeExtension int32 testModule MemberModifier.Instance (property' "GetterIndexedProperty" PropertyKind.Get [ int ] string) ]
      "System.Int32.SetterIndexedProperty", [ typeExtension int32 testModule MemberModifier.Instance (property' "SetterIndexedProperty" PropertyKind.Set [ int ] string) ]
      "System.Int32.GetterSetterIndexedProperty", [
          typeExtension int32 testModule MemberModifier.Instance (property' "GetterSetterIndexedProperty" PropertyKind.Get [ string ] int)
          typeExtension int32 testModule MemberModifier.Instance (property' "GetterSetterIndexedProperty" PropertyKind.Set [ string ] int) 
        ]

      "Microsoft.FSharp.Collections.List<'T>.Method", [ typeExtension fsharpList_t testModule MemberModifier.Static (method' "Method" [ variable "T" ] unit) ]
      "Microsoft.FSharp.Collections.List<'T>.CurriedMethod", [ typeExtension fsharpList_t testModule MemberModifier.Static { curriedMethod "CurriedMethod" [ int; variable "b" ] (variable "b") with GenericParameters = [ "b" ] } ]
      "Microsoft.FSharp.Collections.List<'T>.NoncurriedMethod", [ typeExtension fsharpList_t testModule MemberModifier.Static { method' "NoncurriedMethod" [ int; variable "b" ] int with GenericParameters = [ "b" ] } ]

      "Microsoft.FSharp.Collections.List<'T>.GetterProperty", [ typeExtension fsharpList_t testModule MemberModifier.Static (property' "GetterProperty" PropertyKind.Get [] int) ]
      "Microsoft.FSharp.Collections.List<'T>.SetterProperty", [ typeExtension fsharpList_t testModule MemberModifier.Static (property' "SetterProperty" PropertyKind.Set [] string) ]
      "Microsoft.FSharp.Collections.List<'T>.GetterSetterProperty", [
          typeExtension fsharpList_t testModule MemberModifier.Static (property' "GetterSetterProperty" PropertyKind.Get [] string)
          typeExtension fsharpList_t testModule MemberModifier.Static (property' "GetterSetterProperty" PropertyKind.Set [] string)
        ]

      "Microsoft.FSharp.Collections.List<'T>.GetterIndexedProperty", [ typeExtension fsharpList_t testModule MemberModifier.Static (property' "GetterIndexedProperty" PropertyKind.Get [ int ] string) ]
      "Microsoft.FSharp.Collections.List<'T>.SetterIndexedProperty", [ typeExtension fsharpList_t testModule MemberModifier.Static (property' "SetterIndexedProperty" PropertyKind.Set [ int ] string) ]
      "Microsoft.FSharp.Collections.List<'T>.GetterSetterIndexedProperty", [
          typeExtension fsharpList_t testModule MemberModifier.Static (property' "GetterSetterIndexedProperty" PropertyKind.Get [ string ] int)
          typeExtension fsharpList_t testModule MemberModifier.Static (property' "GetterSetterIndexedProperty" PropertyKind.Set [ string ] int) 
        ]
    ]
    run testApi
  }

  let extensionMemberTest = parameterize {
    source [
      "TypeExtensions.TestExtensions.ExtensionMethod", [ extensionMember (method' "ExtensionMethod" [ int ] int) ]
      "TypeExtensions.TestExtensions.ExtensionMethod2", [ extensionMember (method' "ExtensionMethod2" [ int; int; string ] unit) ]
    ]
    run testApi
  }

module CSharp =
  let testApi = testApi csharpAssemblyApi
  let testConstraints = testConstraints csharpAssemblyApi

  let loadStaticMemberTest =
    let t = createType "CSharpLoadTestAssembly.StaticMemberClass" [] |> updateAssembly csharpAssemblyName
    parameterize {
      source [
        "CSharpLoadTestAssembly.StaticMemberClass.NoArgumentMethod", [ staticMember t (method' "NoArgumentMethod" [ unit ] int) ]
        "CSharpLoadTestAssembly.StaticMemberClass.NonCurriedMethod", [ staticMember t (method' "NonCurriedMethod" [ int; string ] unit) ]
        "CSharpLoadTestAssembly.StaticMemberClass.TupleMethod", [ staticMember t (method' "TupleMethod" [ tuple [ int; string ] ] unit) ]
        "CSharpLoadTestAssembly.StaticMemberClass", [ constructor' t (method' "StaticMemberClass" [ unit ] t); constructor' t (method' "StaticMemberClass" [ string; string ] t) ]
        "CSharpLoadTestAssembly.StaticMemberClass.OverloadMethod", [ staticMember t (method' "OverloadMethod" [ int ] int); staticMember t (method' "OverloadMethod" [ string ] string) ]
        "CSharpLoadTestAssembly.StaticMemberClass.Getter", [ staticMember t (property' "Getter" PropertyKind.Get [] string) ]
        "CSharpLoadTestAssembly.StaticMemberClass.Setter", [ staticMember t (property' "Setter" PropertyKind.Set [] string) ]
        "CSharpLoadTestAssembly.StaticMemberClass.GetterSetter", [ staticMember t (property' "GetterSetter" PropertyKind.GetSet [] string) ]
      ]
      run testApi
    }

  let loadArrayTest =
    let t = createType "CSharpLoadTestAssembly.StaticMemberClass" [] |> updateAssembly csharpAssemblyName
    parameterize {
      source [
        "CSharpLoadTestAssembly.StaticMemberClass.ArrayMethod", [ staticMember t (method' "ArrayMethod" [ unit ] (array int)) ]
        "CSharpLoadTestAssembly.StaticMemberClass.Array2dMethod", [ staticMember t (method' "Array2dMethod" [ unit ] (array2D int)) ]
        "CSharpLoadTestAssembly.StaticMemberClass.NestedArrayMethod", [ staticMember t (method' "NestedArrayMethod" [ unit ] (array2D (array int))) ] // defined as int[,][] in C#
      ]
      run testApi
    }

  let loadInstanceMemberTest =
    let t = createType "CSharpLoadTestAssembly.InstanceMemberClass" [] |> updateAssembly csharpAssemblyName
    parameterize {
      source [
        "CSharpLoadTestAssembly.InstanceMemberClass.NoArgumentMethod", [ instanceMember t (method' "NoArgumentMethod" [ unit ] int) ]
        "CSharpLoadTestAssembly.InstanceMemberClass.NonCurriedMethod", [ instanceMember t (method' "NonCurriedMethod" [ int; string ] unit) ]
        "CSharpLoadTestAssembly.InstanceMemberClass.TupleMethod", [ instanceMember t (method' "TupleMethod" [ tuple [ int; string ] ] unit) ]
        "CSharpLoadTestAssembly.InstanceMemberClass.OverloadMethod", [ instanceMember t (method' "OverloadMethod" [ int ] int); instanceMember t (method' "OverloadMethod" [ string ] string) ]
        "CSharpLoadTestAssembly.InstanceMemberClass.Getter", [ instanceMember t (property' "Getter" PropertyKind.Get [] string) ]
        "CSharpLoadTestAssembly.InstanceMemberClass.Setter", [ instanceMember t (property' "Setter" PropertyKind.Set [] string) ]
        "CSharpLoadTestAssembly.InstanceMemberClass.GetterSetter", [ instanceMember t (property' "GetterSetter" PropertyKind.GetSet [] string) ]
      ]
      run testApi
    }

  let loadIndexerTest =
    let getter = createType "CSharpLoadTestAssembly.IndexedGetter" [] |> updateAssembly csharpAssemblyName
    let setter = createType "CSharpLoadTestAssembly.IndexedSetter" [] |> updateAssembly csharpAssemblyName
    let gettersetter = createType "CSharpLoadTestAssembly.IndexedGetterSetter" [] |> updateAssembly csharpAssemblyName
    parameterize {
      source [
        "CSharpLoadTestAssembly.IndexedGetter.Item", [ instanceMember getter (property' "Item" PropertyKind.Get [ int ] int) ]
        "CSharpLoadTestAssembly.IndexedSetter.Item", [ instanceMember setter (property' "Item" PropertyKind.Set [ int ] int) ]
        "CSharpLoadTestAssembly.IndexedGetterSetter.Item", [ instanceMember gettersetter (property' "Item" PropertyKind.GetSet [ int ] int) ]
      ]
      run testApi
    }

  let loadNestedClassTest =
    let outer = createType "CSharpLoadTestAssembly.OuterClass" [] |> updateAssembly csharpAssemblyName
    let inner = createType "CSharpLoadTestAssembly.OuterClass.InnerClass" [] |> updateAssembly csharpAssemblyName

    let genericOuter = createType "CSharpLoadTestAssembly.GenericOuterClass<'T>" [ variable "T" ] |> updateAssembly csharpAssemblyName
    let genericInner = createType "CSharpLoadTestAssembly.GenericOuterClass<'T>.GenericInnerClass<'T, 'U>" [ variable "T"; variable "U" ] |> updateAssembly csharpAssemblyName

    parameterize {
      source [
        "CSharpLoadTestAssembly.OuterClass", [ constructor' outer (method' "OuterClass" [ unit ] outer) ]
        "CSharpLoadTestAssembly.OuterClass.InnerClass", [ constructor' inner (method' "InnerClass" [ unit ] inner) ]
        "CSharpLoadTestAssembly.OuterClass.InnerClass.StaticMethod", [ staticMember inner (method' "StaticMethod" [ unit ] int) ]

        "CSharpLoadTestAssembly.GenericOuterClass<'T>", [ constructor' genericOuter (method' "GenericOuterClass" [ unit ] genericOuter) ]
        "CSharpLoadTestAssembly.GenericOuterClass<'T>.GenericInnerClass<'T, 'U>", [ constructor' genericInner (method' "GenericInnerClass" [ unit ] genericInner) ]
        "CSharpLoadTestAssembly.GenericOuterClass<'T>.GenericInnerClass<'T, 'U>.Method", [ staticMember genericInner (method' "Method" [ variable "T"; variable "U" ] unit) ]
      ]
      run testApi
    }

  let loadInterfaceTest =
    let i = createType "CSharpLoadTestAssembly.Interface" [] |> updateAssembly csharpAssemblyName
    let gi = createType "CSharpLoadTestAssembly.GenericInterface<'T>" [ variable "T" ] |> updateAssembly csharpAssemblyName
    parameterize {
      source [
        "CSharpLoadTestAssembly.Interface.Method", [ instanceMember i (method' "Method" [ int; string ] int) ]
        "CSharpLoadTestAssembly.Interface.Property", [ instanceMember i (property' "Property" PropertyKind.GetSet [] int) ]
        "CSharpLoadTestAssembly.GenericInterface<'T>.Method", [ instanceMember gi (method' "Method" [ variable "T" ] int) ]
        "CSharpLoadTestAssembly.GenericInterface<'T>.Property", [ instanceMember gi (property' "Property" PropertyKind.Set [] (variable "T")) ]
      ]
      run testApi
    }

  let nonloadedTest =
    parameterize {
      source[
        "CSharpLoadTestAssembly.StaticMemberClass.Field"
        "CSharpLoadTestAssembly.InstanceMemberClass.Field"
        "CSharpLoadTestAssembly.InstanceMemberClass.ProtectedMethod"
        "CSharpLoadTestAssembly.Struct.Field"
      ]
      run (fun x -> testApi (x, []))
    }

  let constraintsTest =
    let t = createType "CSharpLoadTestAssembly.TypeConstraints" [] |> updateAssembly csharpAssemblyName
    parameterize {
      source[
        ("CSharpLoadTestAssembly.TypeConstraints.Struct",
          (staticMember t (method' "Struct" [ variable "T" ] unit)),
          [ constraint' [ "T" ] (SubtypeConstraints valuetype); constraint' [ "T" ] DefaultConstructorConstraints; constraint' [ "T" ] ValueTypeConstraints ])
        ("CSharpLoadTestAssembly.TypeConstraints.Class",
          (staticMember t (method' "Class" [ variable "T" ] unit)),
          [ constraint' [ "T" ] ReferenceTypeConstraints ])
        ("CSharpLoadTestAssembly.TypeConstraints.New",
          (staticMember t (method' "New" [ variable "T" ] unit)),
          [ constraint' [ "T" ] DefaultConstructorConstraints ])
        ("CSharpLoadTestAssembly.TypeConstraints.Subtype",
          (staticMember t (method' "Subtype" [ variable "T" ] unit)),
          [ constraint' [ "T" ] (SubtypeConstraints icomparable) ])
        ("CSharpLoadTestAssembly.TypeConstraints.VariableSubtype",
          (staticMember t (method' "VariableSubtype" [ variable "T"; variable "U" ] unit)),
          [ constraint' [ "T" ] (SubtypeConstraints (variable "U")) ])
      ]
      run testConstraints
    }