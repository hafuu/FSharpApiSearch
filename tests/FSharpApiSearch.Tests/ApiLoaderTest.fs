module ApiLoaderTest

open System.IO
open System.Reflection
open Persimmon
open Persimmon.Syntax.UseTestNameByReflection
open Persimmon.MuscleAssert
open FSharpApiSearch
open TestHelper
open TestHelper.DSL
open TestAssemblies

let emptyDef: FullTypeDefinition = {
  Name = []
  FullName = ""
  AssemblyName = ""
  Accessibility = Public
  Kind = TypeDefinitionKind.Type
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

let bool =
  let Boolean = createType "System.Boolean" [] |> updateAssembly mscorlib
  let bool = createType "Microsoft.FSharp.Core.bool" [] |> updateAssembly fscore
  typeAbbreviation Boolean bool

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

let testApiWithoutParameterName (assembly: TestCase<ApiDictionary>) nameConverter (name, expected) = test {
  let! apiDict = assembly
  let name = nameConverter name
  let actual =
    Seq.filter (fun x -> x.Name = name) apiDict.Api 
    |> Seq.map (fun x -> x.Signature)
    |> Seq.filter (function (ApiSignature.FullTypeDefinition _ | ApiSignature.TypeAbbreviation _ | ApiSignature.ComputationExpressionBuilder _) -> false | _ -> true)
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

let testFullTypeDef (assembly: TestCase<ApiDictionary>) (expected: FullTypeDefinition) = testFullTypeDef' assembly id (DisplayName expected.Name, expected)

let testConstraints (assembly: TestCase<ApiDictionary>) (name, expectedTarget, expectedConstraints) = test {
  let! apiDict = assembly
  let name = Name.displayNameOfString name
  let actual = Seq.find (fun x -> x.Name = name) apiDict.Api
  do! actual.Signature |> assertEquals expectedTarget
  do! (List.sort actual.TypeConstraints) |> assertEquals (List.sort expectedConstraints)
}

module FSharp =
  let testApi = testApiWithoutParameterName fsharpAssemblyApi Name.displayNameOfString
  let testConstraints = testConstraints fsharpAssemblyApi

  let loadModuleMemberTest = parameterize {
    source [
      "PublicModule.nonGenericFunction", [ moduleFunction' [ [ pname "x" >> ptype int ]; [ pname "y" >> ptype int ]; [ ptype int ] ] ]
      "PublicModule.genericFunction<'a, 'b>", [ moduleFunction' [ [ pname "x" >> ptype (variable "'a") ]; [ pname "y" >> ptype (variable "'b") ]; [ ptype (variable "'b") ] ] ]
      "PublicModule.multiParamFunction<'a, 'b, 'c>", [ moduleFunction' [ [ pname "x" >> ptype (variable "'a"); pname "y" >> ptype (variable "'b"); pname "z" >> ptype (variable "'c") ]; [ ptype (variable "'a") ] ] ]
      "PublicModule.unitParamFunction", [ moduleFunction' [ [ ptype unit ]; [ ptype int ] ] ]
      "PublicModule.value", [ moduleValue int ]
      "PublicModule.NestedModule.publicFunction", [ moduleFunction' [ [ pname "x" >> ptype int ]; [ ptype int] ] ]
      "PublicModule.listmap<'a, 'b>", [ moduleFunction' [ [ pname "f" >> ptype (arrow [ variable "'a"; variable "'b" ]) ]; [ pname "xs" >> ptype (list (variable "'a")) ]; [ ptype (list (variable "'b")) ] ] ]
      "PublicModule.partialGenericMap<'a>", [ moduleFunction' [ [ pname "x" >> ptype (map int (variable "'a")) ]; [ ptype (variable "'a") ] ] ]
      "PublicModule.floatReturnType", [ moduleFunction' [ [ pname "x" >> ptype int ]; [ ptype float ] ] ]
      "PublicModule.array", [ moduleValue (array int) ]
      "PublicModule.array2d", [ moduleValue (array2D int) ]
      "PublicModule.nestedArray", [ moduleValue (array (array2D int)) ]
      "PublicModule.( |ActivePattern| )", [ activePattern [ [ pname "x" >> ptype int ]; [ ptype string ] ] ]
      "PublicModule.( |PartialActivePattern|_| )<'a>", [ partialActivePattern [ [ pname "y" >> ptype (variable "'a") ]; [ pname "x" >> ptype (variable "'a") ]; [ ptype (option (variable "'a")) ] ] ]
    ]
    run testApi
  }

  let loadStaticMemberTest =
    let t = createType "TopLevelNamespace.StaticMemberClass" [] |> updateAssembly fsharpAssemblyName
    parameterize {
      source [
        "TopLevelNamespace.StaticMemberClass.NoParameterMethod", [ staticMember t (method' "NoParameterMethod" [ [ ptype unit ] ] int) ]
        "TopLevelNamespace.StaticMemberClass.OneParameterMethod", [ staticMember t (method' "OneParameterMethod" [ [ pname "x" >> ptype int ] ] int) ]
        "TopLevelNamespace.StaticMemberClass.NonCurriedMethod", [ staticMember t (method' "NonCurriedMethod" [ [ pname "x" >> ptype int; pname "y" >> ptype string ] ] int) ]
        "TopLevelNamespace.StaticMemberClass.CurriedMethod", [ staticMember t (method' "CurriedMethod" [ [ pname "x" >> ptype int ]; [ pname "y" >> ptype string ] ] int) ]
        "TopLevelNamespace.StaticMemberClass.TupleMethod", [ staticMember t (method' "TupleMethod" [ [ pname "x" >> ptype (tuple [ int; string ]) ] ] int) ]
        "TopLevelNamespace.StaticMemberClass.InferredFloat", [ staticMember t (method' "InferredFloat" [ [ pname "x" >> ptype float ] ] float) ]
        "TopLevelNamespace.StaticMemberClass.AnnotatedFloat", [ staticMember t (method' "AnnotatedFloat" [ [ pname "x" >> ptype float ] ] float) ]
        "TopLevelNamespace.StaticMemberClass.new", [ constructor' t (method' "new" [ [ ptype unit ] ] t); constructor' t (method' "new" [ [ pname "x" >> ptype int ] ] t) ]
        "TopLevelNamespace.StaticMemberClass.OverloadMethod", [ staticMember t (method' "OverloadMethod" [ [ pname "x" >> ptype int ] ] int); staticMember t (method' "OverloadMethod" [ [ pname "x" >> ptype string; pname "y" >> ptype int ] ] string) ]
        "TopLevelNamespace.StaticMemberClass.Getter", [ staticMember t (property' "Getter" PropertyKind.Get [] string) ]
        "TopLevelNamespace.StaticMemberClass.Setter", [ staticMember t (property' "Setter" PropertyKind.Set [] int) ]
        "TopLevelNamespace.StaticMemberClass.GetterSetter", [ staticMember t (property' "GetterSetter" PropertyKind.GetSet [] float) ]
        "TopLevelNamespace.StaticMemberClass.IndexedGetter", [ staticMember t (property' "IndexedGetter" PropertyKind.Get [ [ ptype int ] ] string) ]
        "TopLevelNamespace.StaticMemberClass.IndexedSetter", [ staticMember t (property' "IndexedSetter" PropertyKind.Set [ [ ptype int ] ] string) ]
        "TopLevelNamespace.StaticMemberClass.IndexedGetterSetter", [ staticMember t (property' "IndexedGetterSetter" PropertyKind.GetSet [ [ ptype string ] ] int) ]
        "TopLevelNamespace.StaticMemberClass.GenericMethod<'a>", [ staticMember t (method' "GenericMethod" [ [ pname "x" >> ptype (variable "'a") ] ] (variable "'a")) ]
      ]
      run testApi
    }

  let loadInstanceMemberTest =
    let t = createType "TopLevelNamespace.InstanceMemberClass" [] |> updateAssembly fsharpAssemblyName
    parameterize {
      source [
        "TopLevelNamespace.InstanceMemberClass.NoParameterMethod", [ instanceMember t (method' "NoParameterMethod" [ [ ptype unit ] ] int) ]
        "TopLevelNamespace.InstanceMemberClass.OneParameterMethod", [ instanceMember t (method' "OneParameterMethod" [ [ pname "x" >> ptype int ] ] int) ]
        "TopLevelNamespace.InstanceMemberClass.NonCurriedMethod", [ instanceMember t (method' "NonCurriedMethod" [ [ pname "x" >> ptype int; pname "y" >> ptype string ] ] int) ]
        "TopLevelNamespace.InstanceMemberClass.CurriedMethod", [ instanceMember t (method' "CurriedMethod" [ [ pname "x" >> ptype int ]; [ pname "y" >> ptype string ] ] int) ]
        "TopLevelNamespace.InstanceMemberClass.TupleMethod", [ instanceMember t (method' "TupleMethod" [ [ pname "x" >> ptype (tuple [ int; string ]) ] ] int) ]
        "TopLevelNamespace.InstanceMemberClass.OverloadMethod", [ instanceMember t (method' "OverloadMethod" [ [ pname "x" >> ptype int ] ] int); instanceMember t (method' "OverloadMethod" [ [ pname "x" >> ptype string; pname "y" >> ptype int ] ] string) ]
        "TopLevelNamespace.InstanceMemberClass.Getter", [ instanceMember t (property' "Getter" PropertyKind.Get [] string) ]
        "TopLevelNamespace.InstanceMemberClass.Setter", [ instanceMember t (property' "Setter" PropertyKind.Set [] int) ]
        "TopLevelNamespace.InstanceMemberClass.GetterSetter", [ instanceMember t (property' "GetterSetter" PropertyKind.GetSet [] float) ]
        "TopLevelNamespace.InstanceMemberClass.IndexedGetter", [ instanceMember t (property' "IndexedGetter" PropertyKind.Get [ [ ptype int ] ] string) ]
        "TopLevelNamespace.InstanceMemberClass.IndexedSetter", [ instanceMember t (property' "IndexedSetter" PropertyKind.Set [ [ ptype int ] ] string) ]
        "TopLevelNamespace.InstanceMemberClass.IndexedGetterSetter", [ instanceMember t (property' "IndexedGetterSetter" PropertyKind.GetSet [ [ ptype string ] ] int) ]
      ]
      run testApi
    }

  let loadGenericClassTest =
    let t = createType "TopLevelNamespace.GenericClass<'a>" [ variable "'a" ] |> updateAssembly fsharpAssemblyName
    parameterize {
      source [
        "TopLevelNamespace.GenericClass<'a>.Method", [ instanceMember t (method' "Method" [ [ pname "x" >> ptype (variable "'a") ] ] int) ]
        "TopLevelNamespace.GenericClass<'a>.GenericMethod<'b>", [ instanceMember t (method' "GenericMethod" [ [ pname "x" >> ptype (variable "'b") ] ] (variable "'b")) ]
        "TopLevelNamespace.GenericClass<'a>.new", [ constructor' t (method' "new" [ [ ptype unit ] ] t) ]
      ]
      run testApi
    }

  let loadRecordTest =
    let t = createType "OtherTypes.Record" [] |> updateAssembly fsharpAssemblyName
    parameterize {
      source [
        "OtherTypes.Record.FieldA", [ instanceMember t (field "FieldA" int) ]
        "OtherTypes.Record.FieldB", [ instanceMember t (field "FieldB" string) ]
        "OtherTypes.Record.InstanceMethod", [ instanceMember t (method' "InstanceMethod" [ [ ptype unit ] ] int) ]
        "OtherTypes.Record.InstanceProperty", [ instanceMember t (property' "InstanceProperty" PropertyKind.GetSet [] int) ]
        "OtherTypes.Record.StaticMethod", [ staticMember t (method' "StaticMethod" [ [ ptype unit ] ] string) ]
      ]
      run testApi  
    }

  let loadGenericRecordTest =
    let t = createType "OtherTypes.GenericRecord<'a>" [ variable "'a" ] |> updateAssembly fsharpAssemblyName
    parameterize {
      source [
        "OtherTypes.GenericRecord<'a>.Field", [ instanceMember t (field "Field" (variable "'a")) ]
      ]
      run testApi  
    }

  let loadUnionTest =
    let t = createType "OtherTypes.Union" [] |> updateAssembly fsharpAssemblyName
    parameterize {
      source [
        "OtherTypes.Union.InstanceMethod", [ instanceMember t (method' "InstanceMethod" [ [ ptype unit ] ] int) ]
      ]
      run testApi
    }

  let laodStructTest =
    let t = createType "OtherTypes.Struct" [] |> updateAssembly fsharpAssemblyName
    parameterize {
      source [
        "OtherTypes.Struct.A", [ instanceMember t (field "A" int) ]
        "OtherTypes.Struct.B", [ instanceMember t (field "B" string) ]
        "OtherTypes.Struct.InstanceMethod", [ instanceMember t (method' "InstanceMethod" [ [ ptype unit ] ] int) ]
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
        "TopLevelNamespace.Interface.Method", [ instanceMember t (method' "Method" [ [ ptype int; ptype string ] ] int ) ]
        "TopLevelNamespace.Interface.GenericMethod<'a>", [ instanceMember t (method' "GenericMethod" [ [ ptype (variable "'a") ] ] (variable "'a" )) ]
        "TopLevelNamespace.Interface.Property", [ instanceMember t (property' "Property" PropertyKind.GetSet [] string ) ]
      ]
      run testApi
    }

  // bug #60
  let internalInterfaceTest = test {
    let! mscorDict = mscorlibApi
    let tuple = mscorDict.TypeDefinitions |> Array.find (fun x -> x.Name = DisplayName.ofString "System.Tuple<'T1, 'T2>" && x.GenericParameters.Length = 2)
    let existsITuple = tuple.AllInterfaces |> Seq.exists (function Identity (FullIdentity i) -> i.Name = Name.displayNameOfString "System.ITuple" | _ -> false)
    do! existsITuple |> assertEquals false
  }

  let loadUnionCaseTest =
    let t = createType "OtherTypes.Union" [] |> updateAssembly fsharpAssemblyName
    parameterize {
      source [
        "OtherTypes.Union.A", [ unionCase t "A" [] ]
        "OtherTypes.Union.B", [ unionCase t "B" [ (None, int); (Some "field2", string) ] ]
        "OtherTypes.Union.C", [ unionCase t "C" [ (None, int) ] ]
        "OtherTypes.Union.D", [ unionCase t "D" [ (Some "field", int) ] ]
      ]
      run testApi
    }

  let loadModuleTest =
    parameterize {
      source [
        "PublicModule", [ module' (DisplayName.ofString "PublicModule") Public ]
        "PublicModule.NestedModule", [ module' (DisplayName.ofString "PublicModule.NestedModule") Public ]
        "InternalModule", [ module' (DisplayName.ofString "InternalModule") Private ]
      ]
      run testApi
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
    let subtypeClass = createType "TypeConstraints.SubTypeClass<'a>" [ variable "'a" ] |> updateAssembly fsharpAssemblyName
    let subtypeRecord = createType "TypeConstraints.SubTypeRecord<'a>" [ variable "'a" ] |> updateAssembly fsharpAssemblyName
    parameterize {
      source [
        // subtype
        ("TypeConstraints.subtypeConFunction<'Tseq>",
          (moduleFunction' [ [ pname "x" >> ptype (variable "'Tseq") ]; [ ptype unit ] ]),
          [ constraint' [ "'Tseq"] (SubtypeConstraints (seq int)) ])
        ("TypeConstraints.SubTypeClass<'a>.Method<'b>",
          (staticMember subtypeClass (method' "Method" [ [ pname "x" >> ptype (variable "'a"); pname "y" >> ptype (variable "'b") ] ] unit)),
          [ constraint' [ "'a" ] (SubtypeConstraints (seq int)); constraint' [ "'b" ] (SubtypeConstraints (seq string)) ])
        ("TypeConstraints.SubTypeRecord<'a>.Field",
          (instanceMember subtypeRecord (field "Field" (variable "'a"))),
          [ constraint' [ "'a" ] (SubtypeConstraints (seq int)) ])

        // nullness
        ("TypeConstraints.nullnessFunction<'a>",
          (moduleFunction' [ [ pname "x" >> ptype (variable "'a") ]; [ ptype unit ] ]),
          [ constraint' [ "'a"] NullnessConstraints ])

        // member
        ("TypeConstraints.memberConstraint_instanceMethod1<^a>",
          (moduleFunction' [ [ pname "x" >> ptype (variable "^a") ]; [ ptype unit ] ]),
          [ constraint' [ "^a"] (MemberConstraints (MemberModifier.Instance, member' "Method" MemberKind.Method [ [ ptype int; ptype int ] ] int)) ])
        ("TypeConstraints.memberConstraint_instanceMethod2<^a>",
          (moduleFunction' [ [ pname "x" >> ptype (variable "^a") ]; [ ptype unit ] ]),
          [ constraint' [ "^a"] (MemberConstraints (MemberModifier.Instance, member' "Method" MemberKind.Method [ [ ptype int; ptype int ] ] int)) ])
        ("TypeConstraints.memberConstraint_tupleMethod<^a>",
          (moduleFunction' [ [ pname "x" >> ptype (variable "^a") ]; [ ptype unit ] ]),
          [ constraint' [ "^a"] (MemberConstraints (MemberModifier.Instance, member' "Method" MemberKind.Method [ [ ptype (tuple [ int; int ]) ] ] int)) ])
        ("TypeConstraints.memberConstraint_staticMember<^a>",
          (moduleFunction' [ [ pname "x" >> ptype (variable "^a") ]; [ ptype unit ] ]),
          [ constraint' [ "^a"] (MemberConstraints (MemberModifier.Static, member' "Method" MemberKind.Method [ [ ptype int ] ] int)) ])
        ("TypeConstraints.memberConstraint_or<^a, ^b>",
          (moduleFunction' [ [ pname "x" >> ptype (variable "^a") ]; [ pname "y" >> ptype (variable "^b") ]; [ ptype unit ] ]),
          [ constraint' [ "^a"; "^b" ] (MemberConstraints (MemberModifier.Static, member' "Method" MemberKind.Method [ [ ptype int ] ] int)) ])
        ("TypeConstraints.memberConstraint_noArgumentMember<^a>", // no argument means get property
          (moduleFunction' [ [ pname "x" >> ptype (variable "^a") ]; [ ptype unit ] ]),
          [ constraint' [ "^a"] (MemberConstraints (MemberModifier.Instance, member' "get_Method" MemberKind.Method [ [ ptype unit ] ] int)) ])
        ("TypeConstraints.memberConstraint_unitMethod<^a>",
          (moduleFunction' [ [ pname "x" >> ptype (variable "^a") ]; [ ptype unit ] ]),
          [ constraint' [ "^a"] (MemberConstraints (MemberModifier.Instance, member' "Method" MemberKind.Method [ [ ptype unit ] ] int)) ])
        ("TypeConstraints.memberConstraint_unitIntMethod<^a>",
          (moduleFunction' [ [ pname "x" >> ptype (variable "^a") ]; [ ptype unit ] ]),
          [ constraint' [ "^a"] (MemberConstraints (MemberModifier.Instance, member' "Method" MemberKind.Method [ [ ptype unit; ptype int ] ] int)) ])
        ("TypeConstraints.memberConstraint_getterMethod<^a>",
          (moduleFunction' [ [ pname "x" >> ptype (variable "^a") ]; [ ptype unit ] ]),
          [ constraint' [ "^a"] (MemberConstraints (MemberModifier.Instance, member' "get_Property" MemberKind.Method [ [ ptype unit ] ] int)) ])
        ("TypeConstraints.memberConstraint_setterMethod<^a>",
          (moduleFunction' [ [ pname "x" >> ptype (variable "^a") ]; [ ptype unit ] ]),
          [ constraint' [ "^a"] (MemberConstraints (MemberModifier.Instance, member' "set_Property" MemberKind.Method [ [ ptype int ] ] unit)) ])
        ("TypeConstraints.memberConstraint_getProperty<^a>",
          (moduleFunction' [ [ pname "x" >> ptype (variable "^a") ]; [ ptype unit ] ]),
          [ constraint' [ "^a"] (MemberConstraints (MemberModifier.Instance, member' "get_Property" MemberKind.Method [ [ ptype unit ] ] int)) ])
        ("TypeConstraints.memberConstraint_setProperty<^a>",
          (moduleFunction' [ [ pname "x" >> ptype (variable "^a") ]; [ ptype unit ] ]),
          [ constraint' [ "^a"] (MemberConstraints (MemberModifier.Instance, member' "set_Property" MemberKind.Method [ [ ptype int ] ] unit)) ])
        ("TypeConstraints.memberConstraint_indexedGetProperty<^a>",
          (moduleFunction' [ [ pname "x" >> ptype (variable "^a") ]; [ ptype unit ] ]),
          [ constraint' [ "^a"] (MemberConstraints (MemberModifier.Instance, member' "get_Property" MemberKind.Method [ [ ptype int ] ] int)) ])
        ("TypeConstraints.memberConstraint_indexedSetProperty<^a>",
          (moduleFunction' [ [ pname "x" >> ptype (variable "^a") ]; [ ptype unit ] ]),
          [ constraint' [ "^a"] (MemberConstraints (MemberModifier.Instance, member' "set_Property" MemberKind.Method [ [ ptype int; ptype int ] ] unit)) ])
        ("TypeConstraints.memberConstraint_staticNoArgumentMember<^a>", // no argument means get property
          (moduleFunction' [ [ pname "x" >> ptype (variable "^a") ]; [ ptype unit ] ]),
          [ constraint' [ "^a"] (MemberConstraints (MemberModifier.Static, member' "get_Method" MemberKind.Method [ [ ptype unit ] ] int)) ])
        ("TypeConstraints.memberConstraint_staticUnitMethod<^a>",
          (moduleFunction' [ [ pname "x" >> ptype (variable "^a") ]; [ ptype unit ] ]),
          [ constraint' [ "^a"] (MemberConstraints (MemberModifier.Static, member' "Method" MemberKind.Method [ [ ptype unit ] ] int)) ])
        ("TypeConstraints.memberConstraint_staticGetterMethod<^a>",
          (moduleFunction' [ [ pname "x" >> ptype (variable "^a") ]; [ ptype unit ] ]),
          [ constraint' [ "^a"] (MemberConstraints (MemberModifier.Static, member' "get_Property" MemberKind.Method [ [ ptype unit ] ] int)) ])
        ("TypeConstraints.memberConstraint_staticSetterMethod<^a>",
          (moduleFunction' [ [ pname "x" >> ptype (variable "^a") ]; [ ptype unit ] ]),
          [ constraint' [ "^a"] (MemberConstraints (MemberModifier.Static, member' "set_Property" MemberKind.Method [ [ ptype int ] ] unit)) ])
        ("TypeConstraints.memberConstraint_staticGetProperty<^a>",
          (moduleFunction' [ [ pname "x" >> ptype (variable "^a") ]; [ ptype unit ] ]),
          [ constraint' [ "^a"] (MemberConstraints (MemberModifier.Static, member' "get_Property" MemberKind.Method [ [ ptype unit ] ] int)) ])
        ("TypeConstraints.memberConstraint_staticSetProperty<^a>",
          (moduleFunction' [ [ pname "x" >> ptype (variable "^a") ]; [ ptype unit ] ]),
          [ constraint' [ "^a"] (MemberConstraints (MemberModifier.Static, member' "set_Property" MemberKind.Method [ [ ptype int ] ] unit)) ])
        ("TypeConstraints.memberConstraint_generic<^a, 'b>",
          (moduleFunction' [ [ pname "x" >> ptype (variable "^a") ]; [ ptype unit ] ]),
          [ constraint' [ "^a"] (MemberConstraints (MemberModifier.Instance, member' "Method" MemberKind.Method [ [ ptype (variable "'b") ] ] unit)) ])
        ("TypeConstraints.memberConstraint_operator<^a, ^b, 'c>",
          (moduleFunction' [ [ pname "x" >> ptype (variable "^a") ]; [ pname "y" >> ptype (variable "^b") ]; [ ptype unit ] ]),
          [ constraint' [ "^a"; "^b"; ] (MemberConstraints (MemberModifier.Static, member' "op_Addition" MemberKind.Method [ [ ptype (variable "^a"); ptype (variable "^b") ] ] (variable "'c"))) ])

        // value, reference
        ("TypeConstraints.valueTypeConstraint<'a>",
          (moduleFunction' [ [ pname "x" >> ptype (variable "'a") ]; [ ptype unit ] ]),
          [ constraint' [ "'a"; ] ValueTypeConstraints ])
        ("TypeConstraints.referenceTypeConstraint<'a>",
          (moduleFunction' [ [ pname "x" >> ptype (variable "'a") ]; [ ptype unit ] ]),
          [ constraint' [ "'a"; ] ReferenceTypeConstraints ])

        // default constructor
        ("TypeConstraints.defaultConstructorConstraint<'a>",
          (moduleFunction' [ [ pname "x" >> ptype (variable "'a") ]; [ ptype unit ] ]),
          [ constraint' [ "'a"; ] DefaultConstructorConstraints ])

        // equality
        ("TypeConstraints.equalityConstraint<'a>",
          (moduleFunction' [ [ pname "x" >> ptype (variable "'a") ]; [ ptype unit ] ]),
          [ constraint' [ "'a"; ] EqualityConstraints ])

        // comparison
        ("TypeConstraints.comparisonConstraint<'a>",
          (moduleFunction' [ [ pname "x" >> ptype (variable "'a") ]; [ ptype unit ] ]),
          [ constraint' [ "'a"; ] ComparisonConstraints ])
      ]
      run testConstraints
    }

  let fullTypeDefinitionTest =
    let plainClass = {
      emptyDef with
        Name = DisplayName.ofString "FullTypeDefinition.PlainClass"
        FullName = "FullTypeDefinition.PlainClass"
        AssemblyName = fsharpAssemblyName
        Kind = TypeDefinitionKind.Class
        BaseType = Some obj
        DefaultConstructor = Satisfy
    }

    let plainInterface = {
      emptyDef with
        Name = DisplayName.ofString "FullTypeDefinition.PlainInterface"
        FullName = "FullTypeDefinition.PlainInterface"
        AssemblyName = fsharpAssemblyName
        Kind = TypeDefinitionKind.Interface
    }

    let interfaceImplClass = {
      emptyDef with
        Name = DisplayName.ofString "FullTypeDefinition.InterfaceImplClass"
        FullName = "FullTypeDefinition.InterfaceImplClass"
        AssemblyName = fsharpAssemblyName
        Kind = TypeDefinitionKind.Class
        BaseType = Some obj
        AllInterfaces = [ Identity (FullIdentity plainInterface.FullIdentity) ]
        DefaultConstructor = Satisfy
    }

    let interfaceInherit = {
      emptyDef with
        Name = DisplayName.ofString "FullTypeDefinition.InterfaceInherit"
        FullName = "FullTypeDefinition.InterfaceInherit"
        Kind = TypeDefinitionKind.Interface
        AssemblyName = fsharpAssemblyName
        AllInterfaces = [ Identity (FullIdentity plainInterface.FullIdentity) ]
    }

    let supportNullClass = {
      emptyDef with
        Name = DisplayName.ofString "FullTypeDefinition.SupportNullClass"
        FullName = "FullTypeDefinition.SupportNullClass"
        AssemblyName = fsharpAssemblyName
        Kind = TypeDefinitionKind.Class
        BaseType = Some obj
        SupportNull = Satisfy
        DefaultConstructor = Satisfy
    }

    let nonSupportNullSubClass = {
      emptyDef with
        Name = DisplayName.ofString "FullTypeDefinition.SupportNullSubClass"
        FullName = "FullTypeDefinition.SupportNullSubClass"
        AssemblyName = fsharpAssemblyName
        Kind = TypeDefinitionKind.Class
        BaseType = Some (Identity (FullIdentity supportNullClass.FullIdentity))
        SupportNull = NotSatisfy
        DefaultConstructor = Satisfy
    }

    let supportNullInterface = {
      emptyDef with
        Name = DisplayName.ofString "FullTypeDefinition.SupportNullInterface"
        FullName = "FullTypeDefinition.SupportNullInterface"
        Kind = TypeDefinitionKind.Interface
        AssemblyName = fsharpAssemblyName
        SupportNull = Satisfy
    }

    let supportNullSubInterface = {
      emptyDef with
        Name = DisplayName.ofString "FullTypeDefinition.SupportNullSubInterface"
        FullName = "FullTypeDefinition.SupportNullSubInterface"
        AssemblyName = fsharpAssemblyName
        Kind = TypeDefinitionKind.Interface
        AllInterfaces = [ Identity (FullIdentity supportNullInterface.FullIdentity) ]
        SupportNull = Satisfy
    }

    let nonSupportNullSubInterface = {
      emptyDef with
        Name = DisplayName.ofString "FullTypeDefinition.NonSupportNullSubInterface"
        FullName = "FullTypeDefinition.NonSupportNullSubInterface"
        AssemblyName = fsharpAssemblyName
        Kind = TypeDefinitionKind.Interface
        AllInterfaces = [ Identity (FullIdentity supportNullInterface.FullIdentity) ]
        SupportNull = NotSatisfy
    }

    let withoutDefaultConstructor = {
      emptyDef with
        Name = DisplayName.ofString "FullTypeDefinition.WithoutDefaultConstructor"
        FullName = "FullTypeDefinition.WithoutDefaultConstructor"
        AssemblyName = fsharpAssemblyName
        Kind = TypeDefinitionKind.Class
        BaseType = Some obj
        DefaultConstructor = NotSatisfy
    }

    let memberClassId = createType "FullTypeDefinition.MemberClass" [] |> updateAssembly fsharpAssemblyName

    let memberClass = {
      emptyDef with
        Name = DisplayName.ofString "FullTypeDefinition.MemberClass"
        FullName = "FullTypeDefinition.MemberClass"
        AssemblyName = fsharpAssemblyName
        Kind = TypeDefinitionKind.Class
        BaseType = Some obj
        StaticMembers =
          [
            method' "StaticMethod" [ [ ptype unit ] ] int
            method' "op_Addition" [ [ pname "x" >> ptype memberClassId; pname "y" >> ptype int ] ] memberClassId
          ]
        InstanceMembers =
          [
            method' "InstanceMethod" [ [ ptype int ] ] int
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
      "EqualityConditionalClass<'a, 'b>", Dependence [ tv "'a" ]
      "CustomEqualityAndConditionalRecord<'a, 'b>", Dependence [ tv "'a" ]
      "EqualityGenericRecord<'a, 'b>", Dependence [ tv "'a"; tv "'b" ]
      "NoEqualityGenericRecord<'a, 'b>", NotSatisfy
      "EqualityWithGenericType", Satisfy
      "NoEqualityWithGenericType", NotSatisfy
      "RecursiveType<'a>", Dependence [ tv "'a" ]
      "TupleAbbreviationFieldRecord", Satisfy
      "FunctionAbbreviationFieldRecord", NotSatisfy
      "AbbreviatedGenericParameterField<'a>", Dependence [ tv "'a" ]
      "AbbreviatedGenericParameterInt", Satisfy
    ]
    run (fun (name, expected) ->
      let testName = DisplayName.ofString name @ DisplayName.ofString "FullTypeDefinition.EqualityConstraints"
      testFullTypeDef' fsharpAssemblyApi (fun x -> x.Equality) (DisplayName testName, expected))
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
      "ComparisonConditionalClass<'a, 'b>", Dependence [ tv "'a" ]
      "CustomComparisonAndConditionalRecord<'a, 'b>", Dependence [ tv "'a" ]
      "ComparisonGenericRecord<'a, 'b>", Dependence [ tv "'a"; tv "'b" ]
      "NoComparisonGenericRecord<'a, 'b>", NotSatisfy
      "ComparisonWithGenericType", Satisfy
      "NoComparisonWithGenericType", NotSatisfy
      "RecursiveType<'a>", Dependence [ tv "'a" ]
      "TupleAbbreviationFieldRecord", Satisfy
      "FunctionAbbreviationFieldRecord", NotSatisfy
      "AbbreviatedGenericParameterField<'a>", Dependence [ tv "'a" ]
      "AbbreviatedGenericParameterInt", Satisfy
    ]
    run (fun (name, expected) ->
      testFullTypeDef' fsharpAssemblyApi (fun x -> x.Comparison) (Name.displayNameOfString ("FullTypeDefinition.ComparisonConstraints." + name), expected))
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

  let accessibilityTest = parameterize {
    source[
      "FullTypeDefinition.PublicType", Public
      "FullTypeDefinition.PrivateType", Private
      "FullTypeDefinition.InternalType", Private

      "InternalSignature.InternalType", Private
    ]
    run (fun (name, expected) ->
      testFullTypeDef' fsharpAssemblyApi (fun x -> x.Accessibility) (Name.displayNameOfString name, expected))
  }

  let typeDefKindTest = parameterize {
    source [
      fsharpAssemblyApi, "TopLevelNamespace.StaticMemberClass", TypeDefinitionKind.Class
      fsharpAssemblyApi, "TopLevelNamespace.Interface", TypeDefinitionKind.Interface
      fsharpAssemblyApi, "OtherTypes.Record", TypeDefinitionKind.Record
      fsharpAssemblyApi, "OtherTypes.Union", TypeDefinitionKind.Union
      fsharpAssemblyApi, "OtherTypes.Enum", TypeDefinitionKind.Enumeration
      fsharpAssemblyApi, "OtherTypes.Struct", TypeDefinitionKind.Type

      fscoreApi, "Microsoft.FSharp.Core.Unit", TypeDefinitionKind.Type
    ]
    run (fun (api, name, expected) -> testFullTypeDef' api (fun x -> x.Kind) (Name.displayNameOfString name, expected))
  }

  let operatorTest =
    let testApi = testApiWithoutParameterName fsharpAssemblyApi DisplayName
    let t = createType "Operators.A" [] |> updateAssembly fsharpAssemblyName
    parameterize {
      source [
        (DisplayName.ofOperatorString "Operators.( + )"), [ moduleFunction' [ [ pname "x" >> ptype int ]; [ pname "y" >> ptype int ]; [ ptype int ] ] ]
        (DisplayName.ofOperatorString "Operators.A.( - )"), [ staticMember t (method' "op_Subtraction" [ [ pname "x" >> ptype t; pname "y" >> ptype t ] ] t) ]
      ]
      run testApi  
    }

  let delegateTest =
    let testDelegate = delegate' (createType "Delegate.TestDelegate" [] |> updateAssembly fsharpAssemblyName) [ int; int; bool ] |> updateAssembly fsharpAssemblyName
    let genericDelegate a b = delegate' (createType "Delegate.GenericDelegate<'a, 'b>" [ a; b ] |> updateAssembly fsharpAssemblyName) [ a; b; bool ] |> updateAssembly fsharpAssemblyName
    let func t tresult = delegate' (createType "System.Func<'T, 'TResult>" [ t; tresult ] |> updateAssembly mscorlib) [ t; tresult ] |> updateAssembly mscorlib
    parameterize {
      source [
        "Delegate.f1", [ moduleValue testDelegate ]
        "Delegate.f2<'a>", [ moduleFunction' [ [ pname "x" >> ptype (variable "'a") ]; [ pname "f" >> ptype testDelegate ]; [ ptype int ] ] ]

        "Delegate.f3<'a, 'b>", [ moduleFunction' [ [ pname "f" >> ptype (genericDelegate (variable "'a") (variable "'b")) ]; [ ptype int ] ] ]
        "Delegate.f4", [ moduleFunction' [ [ pname "f" >> ptype (genericDelegate int string) ]; [ ptype int ] ] ]

        "Delegate.f5<'a>", [ moduleFunction' [ [ pname "f" >> ptype (func int (variable "'a")) ]; [ ptype bool ] ] ]
      ]
      run testApi
    }

  let optionalParameterTest =
    let t = createType "OptionalParameters.X" [] |> updateAssembly fsharpAssemblyName
    parameterize {
      source [
        "OptionalParameters.X.F", [ instanceMember t (method' "F" [ [ pname "x" >> ptype int; popt >> pname "y" >> ptype string ] ] int) ]
        "OptionalParameters.X.G", [ instanceMember t (method' "G" [ [ pname "x" >> ptype (option string); popt >> pname "y" >> ptype (option int) ] ] (option string))]
      ]
      run testApi
    }

  let autoGenericTest = parameterize {
    source [
      "PublicModule.autoGenericFunction<'a>", [ moduleFunction' [ [ pname "x" >> ptype (variable "'a") ]; [ ptype (variable "'a") ] ] ]
      "PublicModule.autoGenericFunction2<'a, 'a0, 'b>", [ moduleFunction' [ [ pname "x" >> ptype (variable "'a") ]; [ pname "y" >> ptype (variable "'a0") ]; [ pname "z" >> ptype (variable "'b") ]; [ ptype (variable "'a") ] ] ]
      "PublicModule.autoGenericFunction3<'a, 'b, 'b0>", [ moduleFunction' [ [ pname "x" >> ptype (variable "'a") ]; [ pname "y" >> ptype (variable "'b") ]; [ pname "z" >> ptype (variable "'b0") ]; [ ptype (variable "'a") ] ] ]
      "PublicModule.autoGenericFunction4<'a, 'b, 'c>", [ moduleFunction' [ [ pname "x" >> ptype (variable "'a") ]; [ pname "y" >> ptype (variable "'b") ]; [ pname "z" >> ptype (variable "'c") ]; [ ptype (variable "'a") ] ] ]
      "PublicModule.autoGenericFunction5<'a0, 'a, 'b>", [ moduleFunction' [ [ pname "x" >> ptype (variable "'a0") ]; [ pname "y" >> ptype (variable "'a") ]; [ pname "z" >> ptype (variable "'b") ]; [ ptype (variable "'a0") ] ] ]
      "PublicModule.autoGenericFunction6<'a, 'a0, 'a1>", [ moduleFunction' [ [ pname "x" >> ptype (variable "'a") ]; [ pname "y" >> ptype (variable "'a0") ]; [ pname "z" >> ptype (variable "'a1") ]; [ ptype (variable "'a") ] ] ]
    ]

    run testApi
  }

  let flexibleTest = test {
    let! apiDict = fsharpAssemblyApi
    let name = Name.displayNameOfString "PublicModule.flexible<'a, 'b>"
    let actual = apiDict.Api |> Array.find (fun x -> x.Name = name)
    do! actual.Signature |> assertEquals (moduleFunction' [ [ pname "x" >> ptype (variable "'a") ]; [ ptype unit ] ])
    
    let expectedConstraints = [ constraint' [ "'a"] (SubtypeConstraints (seq (variable "'b"))) ]
    do! actual.TypeConstraints |> assertEquals expectedConstraints
  }

module SpecialType =
  let tupleName = Name.displayNameOfString "System.Tuple<'T1, 'T2>"
  let tupleNullnessTest =
    testFullTypeDef' mscorlibApi (fun x -> x.SupportNull) (tupleName, NotSatisfy)
  let tupleEqualityTest =
    testFullTypeDef' mscorlibApi (fun x -> x.Equality) (tupleName, Dependence [ tv "'T1"; tv "'T2" ])
  let tupleComparisonTest =
    testFullTypeDef' mscorlibApi (fun x -> x.Comparison) (tupleName, Dependence [ tv "'T1"; tv "'T2" ])

  let arrayName = Name.displayNameOfString "Microsoft.FSharp.Core.[]<'T>"

  let arrayNullnessTest =
    testFullTypeDef' fscoreApi (fun x -> x.SupportNull) (arrayName, Satisfy)
  let arrayEquality =
    testFullTypeDef' fscoreApi (fun x -> x.Equality) (arrayName, Dependence [ tv "'T" ])
  let arrayComparison =
    testFullTypeDef' fscoreApi (fun x -> x.Comparison) (arrayName, Dependence [ tv "'T" ])

  let intptrComparison =
    testFullTypeDef' mscorlibApi (fun x -> x.Comparison) (Name.displayNameOfString "System.IntPtr", Satisfy)
  let uintptrComparison =
    testFullTypeDef' mscorlibApi (fun x -> x.Comparison) (Name.displayNameOfString "System.UIntPtr", Satisfy)

  let int32ImplicitStaticMembers =
    testFullTypeDef' mscorlibApi (fun x -> x.ImplicitStaticMembers |> List.exists (fun x -> x.Name = "op_Addition")) (Name.displayNameOfString "System.Int32", true)

  let Unit =
    testFullTypeDef' fscoreApi (fun x -> x.AssemblyName) (Name.displayNameOfString "Microsoft.FSharp.Core.Unit", "FSharp.Core")

  let UnionCaseInfo =
    testFullTypeDef' fscoreApi (fun x -> x.AssemblyName) (Name.displayNameOfString "Microsoft.FSharp.Reflection.UnionCaseInfo", "FSharp.Core")

  let Delegate =
    testFullTypeDef' csharpAssemblyApi (fun x -> x.AssemblyName) (Name.displayNameOfString "CSharpLoadTestAssembly.TestDelegate", csharpAssemblyName)

module TypeAbbreviation =
  let A = createType "TypeAbbreviations.A" [] |> updateAssembly fsharpAssemblyName
  let typeAbbreviationTest = parameterize {
    source [
      typeAbbreviationDef "TypeAbbreviations.GenericTypeAbbreviation<'b>" (createType "TypeAbbreviations.Original<'a>" [ variable "'b" ] |> updateAssembly fsharpAssemblyName)
      typeAbbreviationDef "TypeAbbreviations.SpecializedTypeAbbreviation" (createType "TypeAbbreviations.Original<'a>" [ A ] |> updateAssembly fsharpAssemblyName)
      
      { typeAbbreviationDef "TypeAbbreviations.NestedTypeAbbreviation" (createType "TypeAbbreviations.Original<'a>"[ A ]  |> updateAssembly fsharpAssemblyName) with
          Abbreviated = createType "TypeAbbreviations.SpecializedTypeAbbreviation" [] |> updateAssembly fsharpAssemblyName
      }
      typeAbbreviationDef "TypeAbbreviations.NestedModule.TypeAbbreviationInModule<'a>" (createType "TypeAbbreviations.Original<'a>"[ variable "'a" ]  |> updateAssembly fsharpAssemblyName)
      typeAbbreviationDef "TypeAbbreviations.FunctionAbbreviation" (arrow [ int; int ])

      (typeAbbreviationDef "TypeAbbreviations.InternalTypeAbbreviation" (A)).AsPrivate
      (typeAbbreviationDef "TypeAbbreviations.PrivateTypeAbbreviation" (A)).AsPrivate
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
    testApiWithoutParameterName fsharpAssemblyApi Name.displayNameOfString ("TypeAbbreviations.functionWithFunctionAbbreviation", [ moduleFunction' [ [ pname "x" >> ptype (TypeAbbreviation t) ]; [ ptype (TypeAbbreviation t) ] ] ])

module TypeExtension =
  let testApi_net40 = testApiWithoutParameterName net40AssemblyApi Name.displayNameOfString
  let testApi = testApiWithoutParameterName fsharpAssemblyApi Name.displayNameOfString
  
  let testModule = DisplayName.ofString "TypeExtensions"
  let fsharpList_t = fsharpList (variable "'T")

  let typeExtensionTest = parameterize {
    source [
      "System.Int32.Method", [ typeExtension int32 testModule MemberModifier.Instance (method' "Method" [ [ pname "x" >> ptype int ] ] unit) ]
      "System.Int32.CurriedMethod", [ typeExtension int32 testModule MemberModifier.Instance (method' "CurriedMethod" [ [ pname "x" >> ptype int ]; [ pname "y" >> ptype string ] ] int) ]
      "System.Int32.NoncurriedMethod", [ typeExtension int32 testModule MemberModifier.Instance (method' "NoncurriedMethod" [ [ pname "x" >> ptype int; pname "y" >> ptype string ] ] string) ]

      "System.Int32.GetterProperty", [ typeExtension int32 testModule MemberModifier.Instance (property' "GetterProperty" PropertyKind.Get [] int) ]
      "System.Int32.SetterProperty", [ typeExtension int32 testModule MemberModifier.Instance (property' "SetterProperty" PropertyKind.Set [] string) ]
      "System.Int32.GetterSetterProperty", [
          typeExtension int32 testModule MemberModifier.Instance (property' "GetterSetterProperty" PropertyKind.Get [] string)
          typeExtension int32 testModule MemberModifier.Instance (property' "GetterSetterProperty" PropertyKind.Set [] string) 
        ]

      "System.Int32.GetterIndexedProperty", [ typeExtension int32 testModule MemberModifier.Instance (property' "GetterIndexedProperty" PropertyKind.Get [ [ ptype int ] ] string) ]
      "System.Int32.SetterIndexedProperty", [ typeExtension int32 testModule MemberModifier.Instance (property' "SetterIndexedProperty" PropertyKind.Set [ [ ptype int ] ] string) ]
      "System.Int32.GetterSetterIndexedProperty", [
          typeExtension int32 testModule MemberModifier.Instance (property' "GetterSetterIndexedProperty" PropertyKind.Get [ [ ptype string ] ] int)
          typeExtension int32 testModule MemberModifier.Instance (property' "GetterSetterIndexedProperty" PropertyKind.Set [ [ ptype string ] ] int) 
        ]

      "Microsoft.FSharp.Collections.List<'T>.Method", [ typeExtension fsharpList_t testModule MemberModifier.Static (method' "Method" [ [ ptype (variable "'T") ] ] unit) ]
      "Microsoft.FSharp.Collections.List<'T>.CurriedMethod", [ typeExtension fsharpList_t testModule MemberModifier.Static { method' "CurriedMethod" [ [ pname "x" >> ptype int ]; [ pname "y" >> ptype (variable "'b") ] ] (variable "'b") with GenericParameters = [ tv "'b" ] } ]
      "Microsoft.FSharp.Collections.List<'T>.NoncurriedMethod", [ typeExtension fsharpList_t testModule MemberModifier.Static { method' "NoncurriedMethod" [ [ pname "x" >> ptype int; pname "y" >> ptype (variable "'b") ] ] int with GenericParameters = [ tv "'b" ] } ]

      "Microsoft.FSharp.Collections.List<'T>.GetterProperty", [ typeExtension fsharpList_t testModule MemberModifier.Static (property' "GetterProperty" PropertyKind.Get [] int) ]
      "Microsoft.FSharp.Collections.List<'T>.SetterProperty", [ typeExtension fsharpList_t testModule MemberModifier.Static (property' "SetterProperty" PropertyKind.Set [] string) ]
      "Microsoft.FSharp.Collections.List<'T>.GetterSetterProperty", [
          typeExtension fsharpList_t testModule MemberModifier.Static (property' "GetterSetterProperty" PropertyKind.Get [] string)
          typeExtension fsharpList_t testModule MemberModifier.Static (property' "GetterSetterProperty" PropertyKind.Set [] string)
        ]

      "Microsoft.FSharp.Collections.List<'T>.GetterIndexedProperty", [ typeExtension fsharpList_t testModule MemberModifier.Static (property' "GetterIndexedProperty" PropertyKind.Get [ [ ptype int ] ] string) ]
      "Microsoft.FSharp.Collections.List<'T>.SetterIndexedProperty", [ typeExtension fsharpList_t testModule MemberModifier.Static (property' "SetterIndexedProperty" PropertyKind.Set [ [ ptype int ] ] string) ]
      "Microsoft.FSharp.Collections.List<'T>.GetterSetterIndexedProperty", [
          typeExtension fsharpList_t testModule MemberModifier.Static (property' "GetterSetterIndexedProperty" PropertyKind.Get [ [ ptype string ] ] int)
          typeExtension fsharpList_t testModule MemberModifier.Static (property' "GetterSetterIndexedProperty" PropertyKind.Set [ [ ptype string ] ] int) 
        ]
    ]
    run testApi
  }

  let extensionMemberTest = parameterize {
    source [
      "TypeExtensions.TestExtensions.ExtensionMethod", [ extensionMember (method' "ExtensionMethod" [ [ pname "x" >> ptype int ] ] int) ]
      "TypeExtensions.TestExtensions.ExtensionMethod2", [ extensionMember (method' "ExtensionMethod2" [ [ pname "x" >> ptype int; pname "y" >> ptype int; pname "z" >> ptype string ] ] unit) ]
    ]
    run testApi
  }

  let net40ExtensionMemberTest = parameterize {
    source[
      "Net40Assembly.TestExtensions.ExtensionMethod", [ extensionMember (method' "ExtensionMethod" [ [ pname "x" >> ptype int ] ] int) ]
    ]
    run testApi_net40
  }

module ComputationExpression =
  let optBuilder = createType "ComputationExpression.OptionBuilder" [] |> updateAssembly fsharpAssemblyName

  let tryFinallyTest = createType "ComputationExpression.TryFinallyTest" [] |> updateAssembly fsharpAssemblyName
  let genericDelayBuilder = createType "ComputationExpression.GenericDelayBuilder" [] |> updateAssembly fsharpAssemblyName
  let delayBuilder = createType "ComputationExpression.DelayBuilder" [] |> updateAssembly fsharpAssemblyName
  
  let customOperation = createType "ComputationExpression.CustomOperation" [] |> updateAssembly fsharpAssemblyName
  let customOperationBuilder = createType "ComputationExpression.CustomOperationBuilder" [] |> updateAssembly fsharpAssemblyName
  
  let computationExpressionTest = parameterize {
    source [
      "ComputationExpression.OptionBuilder", { BuilderType = optBuilder; ComputationExpressionTypes = [ fsharpOption (variable "'a"); fsharpOption (variable "'b") ]; Syntaxes = [ "let!"; "return"; "return!" ] }
      "ComputationExpression.GenericDelayBuilder", { BuilderType = genericDelayBuilder; ComputationExpressionTypes = [ tryFinallyTest ]; Syntaxes = [ "if/then"; "try/finally" ] }
      "ComputationExpression.DelayBuilder", { BuilderType = delayBuilder; ComputationExpressionTypes = [ tryFinallyTest ]; Syntaxes = [ "if/then"; "try/finally" ] }
      "ComputationExpression.CustomOperationBuilder", { BuilderType = customOperationBuilder; ComputationExpressionTypes = [ variable "'a"; customOperation ]; Syntaxes = [ "test"; "yield" ] }
    ]

    run (fun (name, expected) -> test {
      let name = DisplayName (DisplayName.ofString name)
      let! apiDict = fsharpAssemblyApi
      let actual =
        apiDict.Api
        |> Seq.filter (fun api -> api.Name = name)
        |> Seq.pick (fun api -> match api.Signature with ApiSignature.ComputationExpressionBuilder b -> Some b | _ -> None)
      do! actual |> assertEquals expected
    })
  }

  let nonloadedTest = parameterize {
    source [
      "ComputationExpression.NotBuilder"
    ]

    run (fun name -> test {
      let name = DisplayName (DisplayName.ofString name)
      let! apiDict = fsharpAssemblyApi
      let actual =
        apiDict.Api
        |> Seq.filter (fun api -> api.Name = name && api.Kind = ApiKind.ComputationExpressionBuilder)
      do! Seq.isEmpty actual |> assertEquals true
    })
  }

module CSharp =
  let testApi = testApiWithoutParameterName csharpAssemblyApi Name.displayNameOfString
  let testConstraints = testConstraints csharpAssemblyApi

  let loadStaticMemberTest =
    let t = createType "CSharpLoadTestAssembly.StaticMemberClass" [] |> updateAssembly csharpAssemblyName
    parameterize {
      source [
        "CSharpLoadTestAssembly.StaticMemberClass.NoParameterMethod", [ staticMember t (method' "NoParameterMethod" [ [ ptype unit ] ] int) ]
        "CSharpLoadTestAssembly.StaticMemberClass.NonCurriedMethod", [ staticMember t (method' "NonCurriedMethod" [ [ pname "x" >> ptype int; pname "y" >> ptype string ] ] unit) ]
        "CSharpLoadTestAssembly.StaticMemberClass.TupleMethod", [ staticMember t (method' "TupleMethod" [ [ pname "x" >> ptype (tuple [ int; string ]) ] ] unit) ]
        "CSharpLoadTestAssembly.StaticMemberClass.new", [ constructor' t (method' "new" [ [ ptype unit ] ] t); constructor' t (method' "new" [ [ pname "x" >> ptype string; pname "y" >> ptype string ] ] t) ]
        "CSharpLoadTestAssembly.StaticMemberClass.OverloadMethod", [ staticMember t (method' "OverloadMethod" [ [ pname "x" >> ptype int ] ] int); staticMember t (method' "OverloadMethod" [ [ pname "x" >> ptype string ] ] string) ]
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
        "CSharpLoadTestAssembly.StaticMemberClass.ArrayMethod", [ staticMember t (method' "ArrayMethod" [ [ ptype unit ] ] (array int)) ]
        "CSharpLoadTestAssembly.StaticMemberClass.Array2dMethod", [ staticMember t (method' "Array2dMethod" [ [ ptype unit ] ] (array2D int)) ]
        "CSharpLoadTestAssembly.StaticMemberClass.NestedArrayMethod", [ staticMember t (method' "NestedArrayMethod" [ [ ptype unit ] ] (array2D (array int))) ] // defined as int[,][] in C#
      ]
      run testApi
    }

  let loadInstanceMemberTest =
    let t = createType "CSharpLoadTestAssembly.InstanceMemberClass" [] |> updateAssembly csharpAssemblyName
    parameterize {
      source [
        "CSharpLoadTestAssembly.InstanceMemberClass.NoParameterMethod", [ instanceMember t (method' "NoParameterMethod" [ [ ptype unit ] ] int) ]
        "CSharpLoadTestAssembly.InstanceMemberClass.NonCurriedMethod", [ instanceMember t (method' "NonCurriedMethod" [ [ pname "x" >> ptype int; pname "y" >> ptype string ] ] unit) ]
        "CSharpLoadTestAssembly.InstanceMemberClass.TupleMethod", [ instanceMember t (method' "TupleMethod" [ [ pname "x" >> ptype (tuple [ int; string ]) ] ] unit) ]
        "CSharpLoadTestAssembly.InstanceMemberClass.OverloadMethod", [ instanceMember t (method' "OverloadMethod" [ [ pname "x" >> ptype int ] ] int); instanceMember t (method' "OverloadMethod" [ [ pname "x" >> ptype string ] ] string) ]
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
        "CSharpLoadTestAssembly.IndexedGetter.Item", [ instanceMember getter (property' "Item" PropertyKind.Get [ [ ptype int ] ] int) ]
        "CSharpLoadTestAssembly.IndexedSetter.Item", [ instanceMember setter (property' "Item" PropertyKind.Set [ [ ptype int ] ] int) ]
        "CSharpLoadTestAssembly.IndexedGetterSetter.Item", [ instanceMember gettersetter (property' "Item" PropertyKind.GetSet [ [ ptype int ] ] int) ]
      ]
      run testApi
    }

  let loadNestedClassTest =
    let outer = createType "CSharpLoadTestAssembly.OuterClass" [] |> updateAssembly csharpAssemblyName
    let inner = createType "CSharpLoadTestAssembly.OuterClass.InnerClass" [] |> updateAssembly csharpAssemblyName

    let genericOuter = createType "CSharpLoadTestAssembly.GenericOuterClass<'T>" [ variable "'T" ] |> updateAssembly csharpAssemblyName
    let genericInner = createType "CSharpLoadTestAssembly.GenericOuterClass<'T>.GenericInnerClass<'T, 'U>" [ variable "'T"; variable "'U" ] |> updateAssembly csharpAssemblyName

    parameterize {
      source [
        "CSharpLoadTestAssembly.OuterClass.new", [ constructor' outer (method' "new" [ [ ptype unit ] ] outer) ]
        "CSharpLoadTestAssembly.OuterClass.InnerClass.new", [ constructor' inner (method' "new" [ [ ptype unit ] ] inner) ]
        "CSharpLoadTestAssembly.OuterClass.InnerClass.StaticMethod", [ staticMember inner (method' "StaticMethod" [ [ ptype unit ] ] int) ]

        "CSharpLoadTestAssembly.GenericOuterClass<'T>.new", [ constructor' genericOuter (method' "new" [ [ ptype unit ] ] genericOuter) ]
        "CSharpLoadTestAssembly.GenericOuterClass<'T>.GenericInnerClass<'T, 'U>.new", [ constructor' genericInner (method' "new" [ [ ptype unit ] ] genericInner) ]
        "CSharpLoadTestAssembly.GenericOuterClass<'T>.GenericInnerClass<'T, 'U>.Method", [ staticMember genericInner (method' "Method" [ [ pname "x" >> ptype (variable "'T"); pname "y" >> ptype (variable "'U") ] ] unit) ]
      ]
      run testApi
    }

  let loadInterfaceTest =
    let i = createType "CSharpLoadTestAssembly.Interface" [] |> updateAssembly csharpAssemblyName
    let gi = createType "CSharpLoadTestAssembly.GenericInterface<'T>" [ variable "'T" ] |> updateAssembly csharpAssemblyName
    parameterize {
      source [
        "CSharpLoadTestAssembly.Interface.Method", [ instanceMember i (method' "Method" [ [ pname "x" >> ptype int; pname "y" >> ptype string ] ] int) ]
        "CSharpLoadTestAssembly.Interface.Property", [ instanceMember i (property' "Property" PropertyKind.GetSet [] int) ]
        "CSharpLoadTestAssembly.GenericInterface<'T>.Method", [ instanceMember gi (method' "Method" [ [ pname "x" >> ptype (variable "'T") ] ] int) ]
        "CSharpLoadTestAssembly.GenericInterface<'T>.Property", [ instanceMember gi (property' "Property" PropertyKind.Set [] (variable "'T")) ]
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
        ("CSharpLoadTestAssembly.TypeConstraints.Struct<'T>",
          (staticMember t (method' "Struct" [ [ pname "x" >> ptype (variable "'T") ] ] unit)),
          [ constraint' [ "'T" ] (SubtypeConstraints valuetype); constraint' [ "'T" ] DefaultConstructorConstraints; constraint' [ "'T" ] ValueTypeConstraints ])
        ("CSharpLoadTestAssembly.TypeConstraints.Class<'T>",
          (staticMember t (method' "Class" [ [ pname "x" >> ptype (variable "'T") ] ] unit)),
          [ constraint' [ "'T" ] ReferenceTypeConstraints ])
        ("CSharpLoadTestAssembly.TypeConstraints.New<'T>",
          (staticMember t (method' "New" [ [ pname "x" >> ptype (variable "'T") ] ] unit)),
          [ constraint' [ "'T" ] DefaultConstructorConstraints ])
        ("CSharpLoadTestAssembly.TypeConstraints.Subtype<'T>",
          (staticMember t (method' "Subtype" [ [ pname "x" >> ptype (variable "'T") ] ] unit)),
          [ constraint' [ "'T" ] (SubtypeConstraints icomparable) ])
        ("CSharpLoadTestAssembly.TypeConstraints.VariableSubtype<'T, 'U>",
          (staticMember t (method' "VariableSubtype" [ [ pname "x" >> ptype (variable "'T"); pname "y" >> ptype (variable "'U") ] ] unit)),
          [ constraint' [ "'T" ] (SubtypeConstraints (variable "'U")) ])
      ]
      run testConstraints
    }

  let operatorTest =
    let testApi = testApiWithoutParameterName csharpAssemblyApi DisplayName
    let t = createType "CSharpLoadTestAssembly.Operators" [] |> updateAssembly csharpAssemblyName
    parameterize {
      source [
        (DisplayName.ofString "CSharpLoadTestAssembly.Operators.op_Addition"), [ staticMember t (method' "op_Addition" [ [ pname "x" >> ptype t; pname "y" >> ptype t ] ] t) ]
        (DisplayName.ofString "CSharpLoadTestAssembly.Operators.op_Implicit"), [ staticMember t (method' "op_Implicit" [ [ pname "x" >> ptype string ] ] t) ]
      ]
      run testApi  
    }

  let optionalParameterTest =
    let t = createType "CSharpLoadTestAssembly.OptinalParameters" [] |> updateAssembly csharpAssemblyName
    parameterize {
      source [
        "CSharpLoadTestAssembly.OptinalParameters.F", [ staticMember t (method' "F" [ [ popt >> pname "x" >> ptype int ] ] unit) ]
        "CSharpLoadTestAssembly.OptinalParameters.G", [ staticMember t (method' "G" [ [ popt >> pname "x" >> ptype (fsharpOption int) ] ] unit) ]
      ]
      run testApi
    }

module XmlDocTest =
  let xmlDocTest = parameterize {
    source[
      "XmlDoc.Type", Some "this is comment" 
      "XmlDoc.f", Some "this is function comment"
      "XmlDoc.NoComment", None
    ]
    run (fun (name, expected) -> test {
      let! apiDic = fsharpAssemblyApi
      let name = Name.displayNameOfString name
      let api = apiDic.Api |> Seq.find (fun x -> x.Name = name)
      let actual = api.Document
      do! actual |> assertEquals expected
    })
  }