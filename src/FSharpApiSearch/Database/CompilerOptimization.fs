module internal FSharpApiSearch.CompilerOptimization

open FSharp.Compiler

type ImplicitMember = {
  InstanceMembers: Member list
  StaticMembers: Member list
}

module TypeInfo =
  open System
  open SpecialTypes.Identifier

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

module LowType =
  open System
  open SpecialTypes.LowType

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

module Parameter =
  open LowType

  let Boolean = Parameter.ofLowType Boolean
  let Byte = Parameter.ofLowType Byte
  let Char = Parameter.ofLowType Char
  let Decimal = Parameter.ofLowType Decimal
  let Double = Parameter.ofLowType Double
  let Single = Parameter.ofLowType Single
  let Int32 = Parameter.ofLowType Int32
  let Int16 = Parameter.ofLowType Int16
  let Int64 = Parameter.ofLowType Int64
  let IntPtr = Parameter.ofLowType IntPtr
  let SByte = Parameter.ofLowType SByte
  let String = Parameter.ofLowType String
  let UInt16 = Parameter.ofLowType UInt16
  let UInt32 = Parameter.ofLowType UInt32
  let UInt64 = Parameter.ofLowType UInt64
  let UIntPtr = Parameter.ofLowType UIntPtr


let table: Map<Identifier, ImplicitMember> =
  Map.ofList [
    (TypeInfo.Boolean, 
      {
        InstanceMembers = []
        StaticMembers =
          [
            { Name = PrettyNaming.CompileOpName("<"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Boolean; Parameter.Boolean ] ]; ReturnParameter = Parameter.Boolean }
            { Name = PrettyNaming.CompileOpName("<>"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Boolean; Parameter.Boolean ] ]; ReturnParameter = Parameter.Boolean }
            { Name = PrettyNaming.CompileOpName("="); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Boolean; Parameter.Boolean ] ]; ReturnParameter = Parameter.Boolean }
          ]
      })
    (TypeInfo.Byte, 
      {
        InstanceMembers = []
        StaticMembers =
          [
            { Name = PrettyNaming.CompileOpName("%"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Byte; Parameter.Byte ] ]; ReturnParameter = Parameter.Byte }
            { Name = PrettyNaming.CompileOpName("&&&"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Byte; Parameter.Byte ] ]; ReturnParameter = Parameter.Byte }
            { Name = PrettyNaming.CompileOpName("*"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Byte; Parameter.Byte ] ]; ReturnParameter = Parameter.Byte }
            { Name = PrettyNaming.CompileOpName("+"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Byte; Parameter.Byte ] ]; ReturnParameter = Parameter.Byte }
            { Name = PrettyNaming.CompileOpName("-"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Byte; Parameter.Byte ] ]; ReturnParameter = Parameter.Byte }
            { Name = PrettyNaming.CompileOpName("/"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Byte; Parameter.Byte ] ]; ReturnParameter = Parameter.Byte }
            { Name = PrettyNaming.CompileOpName("<"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Byte; Parameter.Byte ] ]; ReturnParameter = Parameter.Boolean }
            { Name = PrettyNaming.CompileOpName("<<<"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Byte; Parameter.Int32 ] ]; ReturnParameter = Parameter.Byte }
            { Name = PrettyNaming.CompileOpName("<>"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Byte; Parameter.Byte ] ]; ReturnParameter = Parameter.Boolean }
            { Name = PrettyNaming.CompileOpName("="); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Byte; Parameter.Byte ] ]; ReturnParameter = Parameter.Boolean }
            { Name = PrettyNaming.CompileOpName(">>>"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Byte; Parameter.Int32 ] ]; ReturnParameter = Parameter.Byte }
            { Name = PrettyNaming.CompileOpName("^^^"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Byte; Parameter.Byte ] ]; ReturnParameter = Parameter.Byte }
            { Name = PrettyNaming.CompileOpName("|||"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Byte; Parameter.Byte ] ]; ReturnParameter = Parameter.Byte }
            { Name = PrettyNaming.CompileOpName("~+"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Byte ] ]; ReturnParameter = Parameter.Byte }
            { Name = PrettyNaming.CompileOpName("~~~"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Byte ] ]; ReturnParameter = Parameter.Byte }
            { Name = "One"; Kind = MemberKind.Property PropertyKind.Get; GenericParameters = []; Parameters = [  ]; ReturnParameter = Parameter.Byte }
            { Name = "Zero"; Kind = MemberKind.Property PropertyKind.Get; GenericParameters = []; Parameters = [  ]; ReturnParameter = Parameter.Byte }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Byte ] ]; ReturnParameter = Parameter.Byte }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Byte ] ]; ReturnParameter = Parameter.Char }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Byte ] ]; ReturnParameter = Parameter.Decimal }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Byte ] ]; ReturnParameter = Parameter.Double }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Byte ] ]; ReturnParameter = Parameter.Single }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Byte ] ]; ReturnParameter = Parameter.Int16 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Byte ] ]; ReturnParameter = Parameter.Int32 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Byte ] ]; ReturnParameter = Parameter.Int64 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Byte ] ]; ReturnParameter = Parameter.IntPtr }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Byte ] ]; ReturnParameter = Parameter.SByte }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Byte ] ]; ReturnParameter = Parameter.UInt16 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Byte ] ]; ReturnParameter = Parameter.UInt32 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Byte ] ]; ReturnParameter = Parameter.UInt64 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Byte ] ]; ReturnParameter = Parameter.UIntPtr }
          ]
      })
    (TypeInfo.Char, 
      {
        InstanceMembers = []
        StaticMembers =
          [
            { Name = PrettyNaming.CompileOpName("+"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Char; Parameter.Char ] ]; ReturnParameter = Parameter.Char }
            { Name = PrettyNaming.CompileOpName("<"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Char; Parameter.Char ] ]; ReturnParameter = Parameter.Boolean }
            { Name = PrettyNaming.CompileOpName("<>"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Char; Parameter.Char ] ]; ReturnParameter = Parameter.Boolean }
            { Name = PrettyNaming.CompileOpName("="); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Char; Parameter.Char ] ]; ReturnParameter = Parameter.Boolean }
            { Name = "One"; Kind = MemberKind.Property PropertyKind.Get; GenericParameters = []; Parameters = [  ]; ReturnParameter = Parameter.Char }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Char ] ]; ReturnParameter = Parameter.Byte }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Char ] ]; ReturnParameter = Parameter.Char }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Char ] ]; ReturnParameter = Parameter.Double }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Char ] ]; ReturnParameter = Parameter.Single }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Char ] ]; ReturnParameter = Parameter.Int16 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Char ] ]; ReturnParameter = Parameter.Int32 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Char ] ]; ReturnParameter = Parameter.Int64 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Char ] ]; ReturnParameter = Parameter.IntPtr }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Char ] ]; ReturnParameter = Parameter.SByte }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Char ] ]; ReturnParameter = Parameter.UInt16 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Char ] ]; ReturnParameter = Parameter.UInt32 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Char ] ]; ReturnParameter = Parameter.UInt64 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Char ] ]; ReturnParameter = Parameter.UIntPtr }
          ]
      })
    (TypeInfo.Decimal, 
      {
        InstanceMembers =
          [
            { Name = "Sign"; Kind = MemberKind.Property PropertyKind.Get; GenericParameters = []; Parameters = [  ]; ReturnParameter = Parameter.Int32 }
          ]
        StaticMembers =
          [
            { Name = PrettyNaming.CompileOpName("%"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Decimal; Parameter.Decimal ] ]; ReturnParameter = Parameter.Decimal }
            { Name = PrettyNaming.CompileOpName("*"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Decimal; Parameter.Decimal ] ]; ReturnParameter = Parameter.Decimal }
            { Name = PrettyNaming.CompileOpName("+"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Decimal; Parameter.Decimal ] ]; ReturnParameter = Parameter.Decimal }
            { Name = PrettyNaming.CompileOpName("-"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Decimal; Parameter.Decimal ] ]; ReturnParameter = Parameter.Decimal }
            { Name = PrettyNaming.CompileOpName("/"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Decimal; Parameter.Decimal ] ]; ReturnParameter = Parameter.Decimal }
            { Name = PrettyNaming.CompileOpName("<"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Decimal; Parameter.Decimal ] ]; ReturnParameter = Parameter.Boolean }
            { Name = PrettyNaming.CompileOpName("<>"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Decimal; Parameter.Decimal ] ]; ReturnParameter = Parameter.Boolean }
            { Name = PrettyNaming.CompileOpName("="); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Decimal; Parameter.Decimal ] ]; ReturnParameter = Parameter.Boolean }
            { Name = PrettyNaming.CompileOpName("~+"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Decimal ] ]; ReturnParameter = Parameter.Decimal }
            { Name = PrettyNaming.CompileOpName("~-"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Decimal ] ]; ReturnParameter = Parameter.Decimal }
            { Name = "DivideByInt"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Decimal; Parameter.Int32 ] ]; ReturnParameter = Parameter.Decimal }
            { Name = "One"; Kind = MemberKind.Property PropertyKind.Get; GenericParameters = []; Parameters = [  ]; ReturnParameter = Parameter.Decimal }
            { Name = "Zero"; Kind = MemberKind.Property PropertyKind.Get; GenericParameters = []; Parameters = [  ]; ReturnParameter = Parameter.Decimal }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Decimal ] ]; ReturnParameter = Parameter.Decimal }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Decimal ] ]; ReturnParameter = Parameter.Double }
          ]
      })
    (TypeInfo.Double, 
      {
        InstanceMembers =
          [
            { Name = "Sign"; Kind = MemberKind.Property PropertyKind.Get; GenericParameters = []; Parameters = [  ]; ReturnParameter = Parameter.Int32 }
          ]
        StaticMembers =
          [
            { Name = PrettyNaming.CompileOpName("%"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Double; Parameter.Double ] ]; ReturnParameter = Parameter.Double }
            { Name = PrettyNaming.CompileOpName("*"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Double; Parameter.Double ] ]; ReturnParameter = Parameter.Double }
            { Name = PrettyNaming.CompileOpName("+"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Double; Parameter.Double ] ]; ReturnParameter = Parameter.Double }
            { Name = PrettyNaming.CompileOpName("-"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Double; Parameter.Double ] ]; ReturnParameter = Parameter.Double }
            { Name = PrettyNaming.CompileOpName("/"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Double; Parameter.Double ] ]; ReturnParameter = Parameter.Double }
            { Name = PrettyNaming.CompileOpName("<"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Double; Parameter.Double ] ]; ReturnParameter = Parameter.Boolean }
            { Name = PrettyNaming.CompileOpName("<>"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Double; Parameter.Double ] ]; ReturnParameter = Parameter.Boolean }
            { Name = PrettyNaming.CompileOpName("="); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Double; Parameter.Double ] ]; ReturnParameter = Parameter.Boolean }
            { Name = PrettyNaming.CompileOpName("~+"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Double ] ]; ReturnParameter = Parameter.Double }
            { Name = PrettyNaming.CompileOpName("~-"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Double ] ]; ReturnParameter = Parameter.Double }
            { Name = "Acos"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Double ] ]; ReturnParameter = Parameter.Double }
            { Name = "Asin"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Double ] ]; ReturnParameter = Parameter.Double }
            { Name = "Atan"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Double ] ]; ReturnParameter = Parameter.Double }
            { Name = "Atan2"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Double; Parameter.Double ] ]; ReturnParameter = Parameter.Double }
            { Name = "Ceiling"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Double ] ]; ReturnParameter = Parameter.Double }
            { Name = "Cos"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Double ] ]; ReturnParameter = Parameter.Double }
            { Name = "Cosh"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Double ] ]; ReturnParameter = Parameter.Double }
            { Name = "DivideByInt"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Double; Parameter.Int32 ] ]; ReturnParameter = Parameter.Double }
            { Name = "Exp"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Double ] ]; ReturnParameter = Parameter.Double }
            { Name = "Floor"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Double ] ]; ReturnParameter = Parameter.Double }
            { Name = "Log"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Double ] ]; ReturnParameter = Parameter.Double }
            { Name = "Log10"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Double ] ]; ReturnParameter = Parameter.Double }
            { Name = "One"; Kind = MemberKind.Property PropertyKind.Get; GenericParameters = []; Parameters = [  ]; ReturnParameter = Parameter.Double }
            { Name = "Pow"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Double; Parameter.Double ] ]; ReturnParameter = Parameter.Double }
            { Name = "Round"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Double ] ]; ReturnParameter = Parameter.Double }
            { Name = "Sin"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Double ] ]; ReturnParameter = Parameter.Double }
            { Name = "Sinh"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Double ] ]; ReturnParameter = Parameter.Double }
            { Name = "Sqrt"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Double ] ]; ReturnParameter = Parameter.Double }
            { Name = "Tan"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Double ] ]; ReturnParameter = Parameter.Double }
            { Name = "Tanh"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Double ] ]; ReturnParameter = Parameter.Double }
            { Name = "Truncate"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Double ] ]; ReturnParameter = Parameter.Double }
            { Name = "Zero"; Kind = MemberKind.Property PropertyKind.Get; GenericParameters = []; Parameters = [  ]; ReturnParameter = Parameter.Double }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Double ] ]; ReturnParameter = Parameter.Byte }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Double ] ]; ReturnParameter = Parameter.Char }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Double ] ]; ReturnParameter = Parameter.Decimal }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Double ] ]; ReturnParameter = Parameter.Double }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Double ] ]; ReturnParameter = Parameter.Single }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Double ] ]; ReturnParameter = Parameter.Int16 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Double ] ]; ReturnParameter = Parameter.Int32 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Double ] ]; ReturnParameter = Parameter.Int64 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Double ] ]; ReturnParameter = Parameter.IntPtr }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Double ] ]; ReturnParameter = Parameter.SByte }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Double ] ]; ReturnParameter = Parameter.UInt16 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Double ] ]; ReturnParameter = Parameter.UInt32 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Double ] ]; ReturnParameter = Parameter.UInt64 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Double ] ]; ReturnParameter = Parameter.UIntPtr }
          ]
      })
    (TypeInfo.Single, 
      {
        InstanceMembers =
          [
            { Name = "Sign"; Kind = MemberKind.Property PropertyKind.Get; GenericParameters = []; Parameters = [  ]; ReturnParameter = Parameter.Int32 }
          ]
        StaticMembers =
          [
            { Name = PrettyNaming.CompileOpName("%"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Single; Parameter.Single ] ]; ReturnParameter = Parameter.Single }
            { Name = PrettyNaming.CompileOpName("*"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Single; Parameter.Single ] ]; ReturnParameter = Parameter.Single }
            { Name = PrettyNaming.CompileOpName("+"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Single; Parameter.Single ] ]; ReturnParameter = Parameter.Single }
            { Name = PrettyNaming.CompileOpName("-"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Single; Parameter.Single ] ]; ReturnParameter = Parameter.Single }
            { Name = PrettyNaming.CompileOpName("/"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Single; Parameter.Single ] ]; ReturnParameter = Parameter.Single }
            { Name = PrettyNaming.CompileOpName("<>"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Single; Parameter.Single ] ]; ReturnParameter = Parameter.Boolean }
            { Name = PrettyNaming.CompileOpName("="); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Single; Parameter.Single ] ]; ReturnParameter = Parameter.Boolean }
            { Name = PrettyNaming.CompileOpName("~+"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Single ] ]; ReturnParameter = Parameter.Single }
            { Name = PrettyNaming.CompileOpName("~-"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Single ] ]; ReturnParameter = Parameter.Single }
            { Name = "Acos"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Single ] ]; ReturnParameter = Parameter.Single }
            { Name = "Asin"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Single ] ]; ReturnParameter = Parameter.Single }
            { Name = "Atan"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Single ] ]; ReturnParameter = Parameter.Single }
            { Name = "Atan2"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Single; Parameter.Single ] ]; ReturnParameter = Parameter.Single }
            { Name = "Ceiling"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Single ] ]; ReturnParameter = Parameter.Single }
            { Name = "Cos"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Single ] ]; ReturnParameter = Parameter.Single }
            { Name = "Cosh"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Single ] ]; ReturnParameter = Parameter.Single }
            { Name = "DivideByInt"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Single; Parameter.Int32 ] ]; ReturnParameter = Parameter.Single }
            { Name = "Exp"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Single ] ]; ReturnParameter = Parameter.Single }
            { Name = "Floor"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Single ] ]; ReturnParameter = Parameter.Single }
            { Name = "Log"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Single ] ]; ReturnParameter = Parameter.Single }
            { Name = "Log10"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Single ] ]; ReturnParameter = Parameter.Single }
            { Name = "One"; Kind = MemberKind.Property PropertyKind.Get; GenericParameters = []; Parameters = [  ]; ReturnParameter = Parameter.Single }
            { Name = "Pow"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Single; Parameter.Single ] ]; ReturnParameter = Parameter.Single }
            { Name = "Round"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Single ] ]; ReturnParameter = Parameter.Single }
            { Name = "Sin"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Single ] ]; ReturnParameter = Parameter.Single }
            { Name = "Sinh"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Single ] ]; ReturnParameter = Parameter.Single }
            { Name = "Sqrt"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Single ] ]; ReturnParameter = Parameter.Single }
            { Name = "Tan"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Single ] ]; ReturnParameter = Parameter.Single }
            { Name = "Tanh"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Single ] ]; ReturnParameter = Parameter.Single }
            { Name = "Truncate"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Single ] ]; ReturnParameter = Parameter.Single }
            { Name = "Zero"; Kind = MemberKind.Property PropertyKind.Get; GenericParameters = []; Parameters = [  ]; ReturnParameter = Parameter.Single }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Single ] ]; ReturnParameter = Parameter.Byte }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Single ] ]; ReturnParameter = Parameter.Char }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Single ] ]; ReturnParameter = Parameter.Decimal }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Single ] ]; ReturnParameter = Parameter.Double }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Single ] ]; ReturnParameter = Parameter.Single }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Single ] ]; ReturnParameter = Parameter.Int16 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Single ] ]; ReturnParameter = Parameter.Int32 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Single ] ]; ReturnParameter = Parameter.Int64 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Single ] ]; ReturnParameter = Parameter.IntPtr }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Single ] ]; ReturnParameter = Parameter.SByte }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Single ] ]; ReturnParameter = Parameter.UInt16 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Single ] ]; ReturnParameter = Parameter.UInt32 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Single ] ]; ReturnParameter = Parameter.UInt64 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Single ] ]; ReturnParameter = Parameter.UIntPtr }
          ]
      })
    (TypeInfo.Int16, 
      {
        InstanceMembers =
          [
            { Name = "Sign"; Kind = MemberKind.Property PropertyKind.Get; GenericParameters = []; Parameters = [  ]; ReturnParameter = Parameter.Int32 }
          ]
        StaticMembers =
          [
            { Name = PrettyNaming.CompileOpName("%"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Int16; Parameter.Int16 ] ]; ReturnParameter = Parameter.Int16 }
            { Name = PrettyNaming.CompileOpName("&&&"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Int16; Parameter.Int16 ] ]; ReturnParameter = Parameter.Int16 }
            { Name = PrettyNaming.CompileOpName("*"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Int16; Parameter.Int16 ] ]; ReturnParameter = Parameter.Int16 }
            { Name = PrettyNaming.CompileOpName("+"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Int16; Parameter.Int16 ] ]; ReturnParameter = Parameter.Int16 }
            { Name = PrettyNaming.CompileOpName("-"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Int16; Parameter.Int16 ] ]; ReturnParameter = Parameter.Int16 }
            { Name = PrettyNaming.CompileOpName("/"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Int16; Parameter.Int16 ] ]; ReturnParameter = Parameter.Int16 }
            { Name = PrettyNaming.CompileOpName("<"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Int16; Parameter.Int16 ] ]; ReturnParameter = Parameter.Boolean }
            { Name = PrettyNaming.CompileOpName("<<<"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Int16; Parameter.Int32 ] ]; ReturnParameter = Parameter.Int16 }
            { Name = PrettyNaming.CompileOpName("<>"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Int16; Parameter.Int16 ] ]; ReturnParameter = Parameter.Boolean }
            { Name = PrettyNaming.CompileOpName("="); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Int16; Parameter.Int16 ] ]; ReturnParameter = Parameter.Boolean }
            { Name = PrettyNaming.CompileOpName(">>>"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Int16; Parameter.Int32 ] ]; ReturnParameter = Parameter.Int16 }
            { Name = PrettyNaming.CompileOpName("^^^"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Int16; Parameter.Int16 ] ]; ReturnParameter = Parameter.Int16 }
            { Name = PrettyNaming.CompileOpName("|||"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Int16; Parameter.Int16 ] ]; ReturnParameter = Parameter.Int16 }
            { Name = PrettyNaming.CompileOpName("~+"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Int16 ] ]; ReturnParameter = Parameter.Int16 }
            { Name = PrettyNaming.CompileOpName("~-"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Int16 ] ]; ReturnParameter = Parameter.Int16 }
            { Name = PrettyNaming.CompileOpName("~~~"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Int16 ] ]; ReturnParameter = Parameter.Int16 }
            { Name = "One"; Kind = MemberKind.Property PropertyKind.Get; GenericParameters = []; Parameters = [  ]; ReturnParameter = Parameter.Int16 }
            { Name = "Zero"; Kind = MemberKind.Property PropertyKind.Get; GenericParameters = []; Parameters = [  ]; ReturnParameter = Parameter.Int16 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Int16 ] ]; ReturnParameter = Parameter.Byte }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Int16 ] ]; ReturnParameter = Parameter.Char }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Int16 ] ]; ReturnParameter = Parameter.Decimal }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Int16 ] ]; ReturnParameter = Parameter.Double }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Int16 ] ]; ReturnParameter = Parameter.Single }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Int16 ] ]; ReturnParameter = Parameter.Int16 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Int16 ] ]; ReturnParameter = Parameter.Int32 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Int16 ] ]; ReturnParameter = Parameter.Int64 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Int16 ] ]; ReturnParameter = Parameter.IntPtr }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Int16 ] ]; ReturnParameter = Parameter.SByte }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Int16 ] ]; ReturnParameter = Parameter.UInt16 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Int16 ] ]; ReturnParameter = Parameter.UInt32 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Int16 ] ]; ReturnParameter = Parameter.UInt64 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Int16 ] ]; ReturnParameter = Parameter.UIntPtr }
          ]
      })
    (TypeInfo.Int32, 
      {
        InstanceMembers =
          [
            { Name = "Sign"; Kind = MemberKind.Property PropertyKind.Get; GenericParameters = []; Parameters = [  ]; ReturnParameter = Parameter.Int32 }
          ]
        StaticMembers =
          [
            { Name = PrettyNaming.CompileOpName("%"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Int32; Parameter.Int32 ] ]; ReturnParameter = Parameter.Int32 }
            { Name = PrettyNaming.CompileOpName("&&&"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Int32; Parameter.Int32 ] ]; ReturnParameter = Parameter.Int32 }
            { Name = PrettyNaming.CompileOpName("*"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Int32; Parameter.Int32 ] ]; ReturnParameter = Parameter.Int32 }
            { Name = PrettyNaming.CompileOpName("+"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Int32; Parameter.Int32 ] ]; ReturnParameter = Parameter.Int32 }
            { Name = PrettyNaming.CompileOpName("-"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Int32; Parameter.Int32 ] ]; ReturnParameter = Parameter.Int32 }
            { Name = PrettyNaming.CompileOpName("/"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Int32; Parameter.Int32 ] ]; ReturnParameter = Parameter.Int32 }
            { Name = PrettyNaming.CompileOpName("<"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Int32; Parameter.Int32 ] ]; ReturnParameter = Parameter.Boolean }
            { Name = PrettyNaming.CompileOpName("<<<"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Int32; Parameter.Int32 ] ]; ReturnParameter = Parameter.Int32 }
            { Name = PrettyNaming.CompileOpName("<>"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Int32; Parameter.Int32 ] ]; ReturnParameter = Parameter.Boolean }
            { Name = PrettyNaming.CompileOpName("="); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Int32; Parameter.Int32 ] ]; ReturnParameter = Parameter.Boolean }
            { Name = PrettyNaming.CompileOpName(">>>"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Int32; Parameter.Int32 ] ]; ReturnParameter = Parameter.Int32 }
            { Name = PrettyNaming.CompileOpName("^^^"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Int32; Parameter.Int32 ] ]; ReturnParameter = Parameter.Int32 }
            { Name = PrettyNaming.CompileOpName("|||"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Int32; Parameter.Int32 ] ]; ReturnParameter = Parameter.Int32 }
            { Name = PrettyNaming.CompileOpName("~+"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Int32 ] ]; ReturnParameter = Parameter.Int32 }
            { Name = PrettyNaming.CompileOpName("~-"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Int32 ] ]; ReturnParameter = Parameter.Int32 }
            { Name = PrettyNaming.CompileOpName("~~~"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Int32 ] ]; ReturnParameter = Parameter.Int32 }
            { Name = "One"; Kind = MemberKind.Property PropertyKind.Get; GenericParameters = []; Parameters = [  ]; ReturnParameter = Parameter.Int32 }
            { Name = "Zero"; Kind = MemberKind.Property PropertyKind.Get; GenericParameters = []; Parameters = [  ]; ReturnParameter = Parameter.Int32 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Int32 ] ]; ReturnParameter = Parameter.Byte }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Int32 ] ]; ReturnParameter = Parameter.Char }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Int32 ] ]; ReturnParameter = Parameter.Decimal }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Int32 ] ]; ReturnParameter = Parameter.Double }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Int32 ] ]; ReturnParameter = Parameter.Single }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Int32 ] ]; ReturnParameter = Parameter.Int16 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Int32 ] ]; ReturnParameter = Parameter.Int32 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Int32 ] ]; ReturnParameter = Parameter.Int64 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Int32 ] ]; ReturnParameter = Parameter.IntPtr }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Int32 ] ]; ReturnParameter = Parameter.SByte }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Int32 ] ]; ReturnParameter = Parameter.UInt16 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Int32 ] ]; ReturnParameter = Parameter.UInt32 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Int32 ] ]; ReturnParameter = Parameter.UInt64 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Int32 ] ]; ReturnParameter = Parameter.UIntPtr }
          ]
      })
    (TypeInfo.Int64, 
      {
        InstanceMembers =
          [
            { Name = "Sign"; Kind = MemberKind.Property PropertyKind.Get; GenericParameters = []; Parameters = [  ]; ReturnParameter = Parameter.Int32 }
          ]
        StaticMembers =
          [
            { Name = PrettyNaming.CompileOpName("%"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Int64; Parameter.Int64 ] ]; ReturnParameter = Parameter.Int64 }
            { Name = PrettyNaming.CompileOpName("&&&"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Int64; Parameter.Int64 ] ]; ReturnParameter = Parameter.Int64 }
            { Name = PrettyNaming.CompileOpName("*"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Int64; Parameter.Int64 ] ]; ReturnParameter = Parameter.Int64 }
            { Name = PrettyNaming.CompileOpName("+"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Int64; Parameter.Int64 ] ]; ReturnParameter = Parameter.Int64 }
            { Name = PrettyNaming.CompileOpName("-"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Int64; Parameter.Int64 ] ]; ReturnParameter = Parameter.Int64 }
            { Name = PrettyNaming.CompileOpName("/"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Int64; Parameter.Int64 ] ]; ReturnParameter = Parameter.Int64 }
            { Name = PrettyNaming.CompileOpName("<"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Int64; Parameter.Int64 ] ]; ReturnParameter = Parameter.Boolean }
            { Name = PrettyNaming.CompileOpName("<<<"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Int64; Parameter.Int32 ] ]; ReturnParameter = Parameter.Int64 }
            { Name = PrettyNaming.CompileOpName("<>"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Int64; Parameter.Int64 ] ]; ReturnParameter = Parameter.Boolean }
            { Name = PrettyNaming.CompileOpName("="); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Int64; Parameter.Int64 ] ]; ReturnParameter = Parameter.Boolean }
            { Name = PrettyNaming.CompileOpName(">>>"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Int64; Parameter.Int32 ] ]; ReturnParameter = Parameter.Int64 }
            { Name = PrettyNaming.CompileOpName("^^^"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Int64; Parameter.Int64 ] ]; ReturnParameter = Parameter.Int64 }
            { Name = PrettyNaming.CompileOpName("|||"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Int64; Parameter.Int64 ] ]; ReturnParameter = Parameter.Int64 }
            { Name = PrettyNaming.CompileOpName("~+"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Int64 ] ]; ReturnParameter = Parameter.Int64 }
            { Name = PrettyNaming.CompileOpName("~-"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Int64 ] ]; ReturnParameter = Parameter.Int64 }
            { Name = PrettyNaming.CompileOpName("~~~"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Int64 ] ]; ReturnParameter = Parameter.Int64 }
            { Name = "One"; Kind = MemberKind.Property PropertyKind.Get; GenericParameters = []; Parameters = [  ]; ReturnParameter = Parameter.Int64 }
            { Name = "Zero"; Kind = MemberKind.Property PropertyKind.Get; GenericParameters = []; Parameters = [  ]; ReturnParameter = Parameter.Int64 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Int64 ] ]; ReturnParameter = Parameter.Byte }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Int64 ] ]; ReturnParameter = Parameter.Char }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Int64 ] ]; ReturnParameter = Parameter.Decimal }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Int64 ] ]; ReturnParameter = Parameter.Double }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Int64 ] ]; ReturnParameter = Parameter.Single }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Int64 ] ]; ReturnParameter = Parameter.Int16 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Int64 ] ]; ReturnParameter = Parameter.Int32 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Int64 ] ]; ReturnParameter = Parameter.Int64 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Int64 ] ]; ReturnParameter = Parameter.IntPtr }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Int64 ] ]; ReturnParameter = Parameter.SByte }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Int64 ] ]; ReturnParameter = Parameter.UInt16 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Int64 ] ]; ReturnParameter = Parameter.UInt32 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Int64 ] ]; ReturnParameter = Parameter.UInt64 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.Int64 ] ]; ReturnParameter = Parameter.UIntPtr }
          ]
      })
    (TypeInfo.IntPtr, 
      {
        InstanceMembers =
          [
            { Name = "Sign"; Kind = MemberKind.Property PropertyKind.Get; GenericParameters = []; Parameters = [  ]; ReturnParameter = Parameter.Int32 }
          ]
        StaticMembers =
          [
            { Name = PrettyNaming.CompileOpName("%"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.IntPtr; Parameter.IntPtr ] ]; ReturnParameter = Parameter.IntPtr }
            { Name = PrettyNaming.CompileOpName("&&&"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.IntPtr; Parameter.IntPtr ] ]; ReturnParameter = Parameter.IntPtr }
            { Name = PrettyNaming.CompileOpName("*"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.IntPtr; Parameter.IntPtr ] ]; ReturnParameter = Parameter.IntPtr }
            { Name = PrettyNaming.CompileOpName("+"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.IntPtr; Parameter.IntPtr ] ]; ReturnParameter = Parameter.IntPtr }
            { Name = PrettyNaming.CompileOpName("-"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.IntPtr; Parameter.IntPtr ] ]; ReturnParameter = Parameter.IntPtr }
            { Name = PrettyNaming.CompileOpName("/"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.IntPtr; Parameter.IntPtr ] ]; ReturnParameter = Parameter.IntPtr }
            { Name = PrettyNaming.CompileOpName("<"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.IntPtr; Parameter.IntPtr ] ]; ReturnParameter = Parameter.Boolean }
            { Name = PrettyNaming.CompileOpName("<<<"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.IntPtr; Parameter.Int32 ] ]; ReturnParameter = Parameter.IntPtr }
            { Name = PrettyNaming.CompileOpName("<>"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.IntPtr; Parameter.IntPtr ] ]; ReturnParameter = Parameter.Boolean }
            { Name = PrettyNaming.CompileOpName("="); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.IntPtr; Parameter.IntPtr ] ]; ReturnParameter = Parameter.Boolean }
            { Name = PrettyNaming.CompileOpName(">>>"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.IntPtr; Parameter.Int32 ] ]; ReturnParameter = Parameter.IntPtr }
            { Name = PrettyNaming.CompileOpName("^^^"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.IntPtr; Parameter.IntPtr ] ]; ReturnParameter = Parameter.IntPtr }
            { Name = PrettyNaming.CompileOpName("|||"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.IntPtr; Parameter.IntPtr ] ]; ReturnParameter = Parameter.IntPtr }
            { Name = PrettyNaming.CompileOpName("~+"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.IntPtr ] ]; ReturnParameter = Parameter.IntPtr }
            { Name = PrettyNaming.CompileOpName("~-"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.IntPtr ] ]; ReturnParameter = Parameter.IntPtr }
            { Name = PrettyNaming.CompileOpName("~~~"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.IntPtr ] ]; ReturnParameter = Parameter.IntPtr }
            { Name = "One"; Kind = MemberKind.Property PropertyKind.Get; GenericParameters = []; Parameters = [  ]; ReturnParameter = Parameter.IntPtr }
            { Name = "Zero"; Kind = MemberKind.Property PropertyKind.Get; GenericParameters = []; Parameters = [  ]; ReturnParameter = Parameter.IntPtr }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.IntPtr ] ]; ReturnParameter = Parameter.Byte }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.IntPtr ] ]; ReturnParameter = Parameter.Char }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.IntPtr ] ]; ReturnParameter = Parameter.Decimal }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.IntPtr ] ]; ReturnParameter = Parameter.Double }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.IntPtr ] ]; ReturnParameter = Parameter.Single }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.IntPtr ] ]; ReturnParameter = Parameter.Int16 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.IntPtr ] ]; ReturnParameter = Parameter.Int32 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.IntPtr ] ]; ReturnParameter = Parameter.Int64 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.IntPtr ] ]; ReturnParameter = Parameter.IntPtr }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.IntPtr ] ]; ReturnParameter = Parameter.SByte }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.IntPtr ] ]; ReturnParameter = Parameter.UInt16 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.IntPtr ] ]; ReturnParameter = Parameter.UInt32 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.IntPtr ] ]; ReturnParameter = Parameter.UInt64 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.IntPtr ] ]; ReturnParameter = Parameter.UIntPtr }
          ]
      })
    (TypeInfo.SByte, 
      {
        InstanceMembers =
          [
            { Name = "Sign"; Kind = MemberKind.Property PropertyKind.Get; GenericParameters = []; Parameters = [  ]; ReturnParameter = Parameter.Int32 }
          ]
        StaticMembers =
          [
            { Name = PrettyNaming.CompileOpName("%"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.SByte; Parameter.SByte ] ]; ReturnParameter = Parameter.SByte }
            { Name = PrettyNaming.CompileOpName("&&&"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.SByte; Parameter.SByte ] ]; ReturnParameter = Parameter.SByte }
            { Name = PrettyNaming.CompileOpName("*"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.SByte; Parameter.SByte ] ]; ReturnParameter = Parameter.SByte }
            { Name = PrettyNaming.CompileOpName("+"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.SByte; Parameter.SByte ] ]; ReturnParameter = Parameter.SByte }
            { Name = PrettyNaming.CompileOpName("-"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.SByte; Parameter.SByte ] ]; ReturnParameter = Parameter.SByte }
            { Name = PrettyNaming.CompileOpName("/"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.SByte; Parameter.SByte ] ]; ReturnParameter = Parameter.SByte }
            { Name = PrettyNaming.CompileOpName("<"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.SByte; Parameter.SByte ] ]; ReturnParameter = Parameter.Boolean }
            { Name = PrettyNaming.CompileOpName("<<<"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.SByte; Parameter.Int32 ] ]; ReturnParameter = Parameter.SByte }
            { Name = PrettyNaming.CompileOpName("<>"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.SByte; Parameter.SByte ] ]; ReturnParameter = Parameter.Boolean }
            { Name = PrettyNaming.CompileOpName("="); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.SByte; Parameter.SByte ] ]; ReturnParameter = Parameter.Boolean }
            { Name = PrettyNaming.CompileOpName(">>>"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.SByte; Parameter.Int32 ] ]; ReturnParameter = Parameter.SByte }
            { Name = PrettyNaming.CompileOpName("^^^"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.SByte; Parameter.SByte ] ]; ReturnParameter = Parameter.SByte }
            { Name = PrettyNaming.CompileOpName("|||"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.SByte; Parameter.SByte ] ]; ReturnParameter = Parameter.SByte }
            { Name = PrettyNaming.CompileOpName("~+"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.SByte ] ]; ReturnParameter = Parameter.SByte }
            { Name = PrettyNaming.CompileOpName("~-"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.SByte ] ]; ReturnParameter = Parameter.SByte }
            { Name = PrettyNaming.CompileOpName("~~~"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.SByte ] ]; ReturnParameter = Parameter.SByte }
            { Name = "One"; Kind = MemberKind.Property PropertyKind.Get; GenericParameters = []; Parameters = [  ]; ReturnParameter = Parameter.SByte }
            { Name = "Zero"; Kind = MemberKind.Property PropertyKind.Get; GenericParameters = []; Parameters = [  ]; ReturnParameter = Parameter.SByte }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.SByte ] ]; ReturnParameter = Parameter.Byte }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.SByte ] ]; ReturnParameter = Parameter.Char }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.SByte ] ]; ReturnParameter = Parameter.Decimal }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.SByte ] ]; ReturnParameter = Parameter.Double }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.SByte ] ]; ReturnParameter = Parameter.Single }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.SByte ] ]; ReturnParameter = Parameter.Int16 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.SByte ] ]; ReturnParameter = Parameter.Int32 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.SByte ] ]; ReturnParameter = Parameter.Int64 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.SByte ] ]; ReturnParameter = Parameter.IntPtr }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.SByte ] ]; ReturnParameter = Parameter.SByte }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.SByte ] ]; ReturnParameter = Parameter.UInt16 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.SByte ] ]; ReturnParameter = Parameter.UInt32 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.SByte ] ]; ReturnParameter = Parameter.UInt64 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.SByte ] ]; ReturnParameter = Parameter.UIntPtr }
          ]
      })
    (TypeInfo.String, 
      {
        InstanceMembers = []
        StaticMembers =
          [
            { Name = PrettyNaming.CompileOpName("+"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.String; Parameter.String ] ]; ReturnParameter = Parameter.String }
            { Name = PrettyNaming.CompileOpName("<"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.String; Parameter.String ] ]; ReturnParameter = Parameter.Boolean }
            { Name = PrettyNaming.CompileOpName("<="); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.String; Parameter.String ] ]; ReturnParameter = Parameter.Boolean }
            { Name = PrettyNaming.CompileOpName("<>"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.String; Parameter.String ] ]; ReturnParameter = Parameter.Boolean }
            { Name = PrettyNaming.CompileOpName("="); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.String; Parameter.String ] ]; ReturnParameter = Parameter.Boolean }
            { Name = PrettyNaming.CompileOpName(">"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.String; Parameter.String ] ]; ReturnParameter = Parameter.Boolean }
            { Name = PrettyNaming.CompileOpName(">="); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.String; Parameter.String ] ]; ReturnParameter = Parameter.Boolean }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.String ] ]; ReturnParameter = Parameter.Byte }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.String ] ]; ReturnParameter = Parameter.Char }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.String ] ]; ReturnParameter = Parameter.Decimal }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.String ] ]; ReturnParameter = Parameter.Double }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.String ] ]; ReturnParameter = Parameter.Single }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.String ] ]; ReturnParameter = Parameter.Int16 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.String ] ]; ReturnParameter = Parameter.Int32 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.String ] ]; ReturnParameter = Parameter.Int64 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.String ] ]; ReturnParameter = Parameter.IntPtr }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.String ] ]; ReturnParameter = Parameter.SByte }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.String ] ]; ReturnParameter = Parameter.UInt16 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.String ] ]; ReturnParameter = Parameter.UInt32 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.String ] ]; ReturnParameter = Parameter.UInt64 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.String ] ]; ReturnParameter = Parameter.UIntPtr }
          ]
      })
    (TypeInfo.UInt16, 
      {
        InstanceMembers = []
        StaticMembers =
          [
            { Name = PrettyNaming.CompileOpName("%"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UInt16; Parameter.UInt16 ] ]; ReturnParameter = Parameter.UInt16 }
            { Name = PrettyNaming.CompileOpName("&&&"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UInt16; Parameter.UInt16 ] ]; ReturnParameter = Parameter.UInt16 }
            { Name = PrettyNaming.CompileOpName("*"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UInt16; Parameter.UInt16 ] ]; ReturnParameter = Parameter.UInt16 }
            { Name = PrettyNaming.CompileOpName("+"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UInt16; Parameter.UInt16 ] ]; ReturnParameter = Parameter.UInt16 }
            { Name = PrettyNaming.CompileOpName("-"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UInt16; Parameter.UInt16 ] ]; ReturnParameter = Parameter.UInt16 }
            { Name = PrettyNaming.CompileOpName("/"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UInt16; Parameter.UInt16 ] ]; ReturnParameter = Parameter.UInt16 }
            { Name = PrettyNaming.CompileOpName("<"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UInt16; Parameter.UInt16 ] ]; ReturnParameter = Parameter.Boolean }
            { Name = PrettyNaming.CompileOpName("<<<"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UInt16; Parameter.Int32 ] ]; ReturnParameter = Parameter.UInt16 }
            { Name = PrettyNaming.CompileOpName("<>"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UInt16; Parameter.UInt16 ] ]; ReturnParameter = Parameter.Boolean }
            { Name = PrettyNaming.CompileOpName("="); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UInt16; Parameter.UInt16 ] ]; ReturnParameter = Parameter.Boolean }
            { Name = PrettyNaming.CompileOpName(">>>"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UInt16; Parameter.Int32 ] ]; ReturnParameter = Parameter.UInt16 }
            { Name = PrettyNaming.CompileOpName("^^^"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UInt16; Parameter.UInt16 ] ]; ReturnParameter = Parameter.UInt16 }
            { Name = PrettyNaming.CompileOpName("|||"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UInt16; Parameter.UInt16 ] ]; ReturnParameter = Parameter.UInt16 }
            { Name = PrettyNaming.CompileOpName("~+"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UInt16 ] ]; ReturnParameter = Parameter.UInt16 }
            { Name = PrettyNaming.CompileOpName("~~~"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UInt16 ] ]; ReturnParameter = Parameter.UInt16 }
            { Name = "One"; Kind = MemberKind.Property PropertyKind.Get; GenericParameters = []; Parameters = [  ]; ReturnParameter = Parameter.UInt16 }
            { Name = "Zero"; Kind = MemberKind.Property PropertyKind.Get; GenericParameters = []; Parameters = [  ]; ReturnParameter = Parameter.UInt16 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UInt16 ] ]; ReturnParameter = Parameter.Byte }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UInt16 ] ]; ReturnParameter = Parameter.Char }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UInt16 ] ]; ReturnParameter = Parameter.Decimal }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UInt16 ] ]; ReturnParameter = Parameter.Double }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UInt16 ] ]; ReturnParameter = Parameter.Single }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UInt16 ] ]; ReturnParameter = Parameter.Int16 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UInt16 ] ]; ReturnParameter = Parameter.Int32 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UInt16 ] ]; ReturnParameter = Parameter.Int64 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UInt16 ] ]; ReturnParameter = Parameter.IntPtr }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UInt16 ] ]; ReturnParameter = Parameter.SByte }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UInt16 ] ]; ReturnParameter = Parameter.UInt16 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UInt16 ] ]; ReturnParameter = Parameter.UInt32 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UInt16 ] ]; ReturnParameter = Parameter.UInt64 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UInt16 ] ]; ReturnParameter = Parameter.UIntPtr }
          ]
      })
    (TypeInfo.UInt32, 
      {
        InstanceMembers = []
        StaticMembers =
          [
            { Name = PrettyNaming.CompileOpName("%"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UInt32; Parameter.UInt32 ] ]; ReturnParameter = Parameter.UInt32 }
            { Name = PrettyNaming.CompileOpName("&&&"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UInt32; Parameter.UInt32 ] ]; ReturnParameter = Parameter.UInt32 }
            { Name = PrettyNaming.CompileOpName("*"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UInt32; Parameter.UInt32 ] ]; ReturnParameter = Parameter.UInt32 }
            { Name = PrettyNaming.CompileOpName("+"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UInt32; Parameter.UInt32 ] ]; ReturnParameter = Parameter.UInt32 }
            { Name = PrettyNaming.CompileOpName("-"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UInt32; Parameter.UInt32 ] ]; ReturnParameter = Parameter.UInt32 }
            { Name = PrettyNaming.CompileOpName("/"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UInt32; Parameter.UInt32 ] ]; ReturnParameter = Parameter.UInt32 }
            { Name = PrettyNaming.CompileOpName("<"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UInt32; Parameter.UInt32 ] ]; ReturnParameter = Parameter.Boolean }
            { Name = PrettyNaming.CompileOpName("<<<"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UInt32; Parameter.Int32 ] ]; ReturnParameter = Parameter.UInt32 }
            { Name = PrettyNaming.CompileOpName("<>"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UInt32; Parameter.UInt32 ] ]; ReturnParameter = Parameter.Boolean }
            { Name = PrettyNaming.CompileOpName("="); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UInt32; Parameter.UInt32 ] ]; ReturnParameter = Parameter.Boolean }
            { Name = PrettyNaming.CompileOpName(">>>"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UInt32; Parameter.Int32 ] ]; ReturnParameter = Parameter.UInt32 }
            { Name = PrettyNaming.CompileOpName("^^^"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UInt32; Parameter.UInt32 ] ]; ReturnParameter = Parameter.UInt32 }
            { Name = PrettyNaming.CompileOpName("|||"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UInt32; Parameter.UInt32 ] ]; ReturnParameter = Parameter.UInt32 }
            { Name = PrettyNaming.CompileOpName("~+"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UInt32 ] ]; ReturnParameter = Parameter.UInt32 }
            { Name = PrettyNaming.CompileOpName("~~~"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UInt32 ] ]; ReturnParameter = Parameter.UInt32 }
            { Name = "One"; Kind = MemberKind.Property PropertyKind.Get; GenericParameters = []; Parameters = [  ]; ReturnParameter = Parameter.UInt32 }
            { Name = "Zero"; Kind = MemberKind.Property PropertyKind.Get; GenericParameters = []; Parameters = [  ]; ReturnParameter = Parameter.UInt32 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UInt32 ] ]; ReturnParameter = Parameter.Byte }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UInt32 ] ]; ReturnParameter = Parameter.Char }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UInt32 ] ]; ReturnParameter = Parameter.Decimal }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UInt32 ] ]; ReturnParameter = Parameter.Double }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UInt32 ] ]; ReturnParameter = Parameter.Single }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UInt32 ] ]; ReturnParameter = Parameter.Int16 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UInt32 ] ]; ReturnParameter = Parameter.Int32 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UInt32 ] ]; ReturnParameter = Parameter.Int64 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UInt32 ] ]; ReturnParameter = Parameter.IntPtr }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UInt32 ] ]; ReturnParameter = Parameter.SByte }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UInt32 ] ]; ReturnParameter = Parameter.UInt16 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UInt32 ] ]; ReturnParameter = Parameter.UInt32 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UInt32 ] ]; ReturnParameter = Parameter.UInt64 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UInt32 ] ]; ReturnParameter = Parameter.UIntPtr }
          ]
      })
    (TypeInfo.UInt64, 
      {
        InstanceMembers = []
        StaticMembers =
          [
            { Name = PrettyNaming.CompileOpName("%"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UInt64; Parameter.UInt64 ] ]; ReturnParameter = Parameter.UInt64 }
            { Name = PrettyNaming.CompileOpName("&&&"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UInt64; Parameter.UInt64 ] ]; ReturnParameter = Parameter.UInt64 }
            { Name = PrettyNaming.CompileOpName("*"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UInt64; Parameter.UInt64 ] ]; ReturnParameter = Parameter.UInt64 }
            { Name = PrettyNaming.CompileOpName("+"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UInt64; Parameter.UInt64 ] ]; ReturnParameter = Parameter.UInt64 }
            { Name = PrettyNaming.CompileOpName("-"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UInt64; Parameter.UInt64 ] ]; ReturnParameter = Parameter.UInt64 }
            { Name = PrettyNaming.CompileOpName("/"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UInt64; Parameter.UInt64 ] ]; ReturnParameter = Parameter.UInt64 }
            { Name = PrettyNaming.CompileOpName("<"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UInt64; Parameter.UInt64 ] ]; ReturnParameter = Parameter.Boolean }
            { Name = PrettyNaming.CompileOpName("<<<"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UInt64; Parameter.Int32 ] ]; ReturnParameter = Parameter.UInt64 }
            { Name = PrettyNaming.CompileOpName("<>"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UInt64; Parameter.UInt64 ] ]; ReturnParameter = Parameter.Boolean }
            { Name = PrettyNaming.CompileOpName("="); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UInt64; Parameter.UInt64 ] ]; ReturnParameter = Parameter.Boolean }
            { Name = PrettyNaming.CompileOpName(">>>"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UInt64; Parameter.Int32 ] ]; ReturnParameter = Parameter.UInt64 }
            { Name = PrettyNaming.CompileOpName("^^^"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UInt64; Parameter.UInt64 ] ]; ReturnParameter = Parameter.UInt64 }
            { Name = PrettyNaming.CompileOpName("|||"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UInt64; Parameter.UInt64 ] ]; ReturnParameter = Parameter.UInt64 }
            { Name = PrettyNaming.CompileOpName("~+"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UInt64 ] ]; ReturnParameter = Parameter.UInt64 }
            { Name = PrettyNaming.CompileOpName("~~~"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UInt64 ] ]; ReturnParameter = Parameter.UInt64 }
            { Name = "One"; Kind = MemberKind.Property PropertyKind.Get; GenericParameters = []; Parameters = [  ]; ReturnParameter = Parameter.UInt64 }
            { Name = "Zero"; Kind = MemberKind.Property PropertyKind.Get; GenericParameters = []; Parameters = [  ]; ReturnParameter = Parameter.UInt64 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UInt64 ] ]; ReturnParameter = Parameter.Byte }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UInt64 ] ]; ReturnParameter = Parameter.Char }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UInt64 ] ]; ReturnParameter = Parameter.Decimal }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UInt64 ] ]; ReturnParameter = Parameter.Double }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UInt64 ] ]; ReturnParameter = Parameter.Single }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UInt64 ] ]; ReturnParameter = Parameter.Int16 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UInt64 ] ]; ReturnParameter = Parameter.Int32 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UInt64 ] ]; ReturnParameter = Parameter.Int64 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UInt64 ] ]; ReturnParameter = Parameter.IntPtr }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UInt64 ] ]; ReturnParameter = Parameter.SByte }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UInt64 ] ]; ReturnParameter = Parameter.UInt16 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UInt64 ] ]; ReturnParameter = Parameter.UInt32 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UInt64 ] ]; ReturnParameter = Parameter.UInt64 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UInt64 ] ]; ReturnParameter = Parameter.UIntPtr }
          ]
      })
    (TypeInfo.UIntPtr, 
      {
        InstanceMembers = []
        StaticMembers =
          [
            { Name = PrettyNaming.CompileOpName("%"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UIntPtr; Parameter.UIntPtr ] ]; ReturnParameter = Parameter.UIntPtr }
            { Name = PrettyNaming.CompileOpName("&&&"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UIntPtr; Parameter.UIntPtr ] ]; ReturnParameter = Parameter.UIntPtr }
            { Name = PrettyNaming.CompileOpName("*"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UIntPtr; Parameter.UIntPtr ] ]; ReturnParameter = Parameter.UIntPtr }
            { Name = PrettyNaming.CompileOpName("+"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UIntPtr; Parameter.UIntPtr ] ]; ReturnParameter = Parameter.UIntPtr }
            { Name = PrettyNaming.CompileOpName("-"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UIntPtr; Parameter.UIntPtr ] ]; ReturnParameter = Parameter.UIntPtr }
            { Name = PrettyNaming.CompileOpName("/"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UIntPtr; Parameter.UIntPtr ] ]; ReturnParameter = Parameter.UIntPtr }
            { Name = PrettyNaming.CompileOpName("<"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UIntPtr; Parameter.UIntPtr ] ]; ReturnParameter = Parameter.Boolean }
            { Name = PrettyNaming.CompileOpName("<<<"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UIntPtr; Parameter.Int32 ] ]; ReturnParameter = Parameter.UIntPtr }
            { Name = PrettyNaming.CompileOpName("<>"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UIntPtr; Parameter.UIntPtr ] ]; ReturnParameter = Parameter.Boolean }
            { Name = PrettyNaming.CompileOpName("="); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UIntPtr; Parameter.UIntPtr ] ]; ReturnParameter = Parameter.Boolean }
            { Name = PrettyNaming.CompileOpName(">>>"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UIntPtr; Parameter.Int32 ] ]; ReturnParameter = Parameter.UIntPtr }
            { Name = PrettyNaming.CompileOpName("^^^"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UIntPtr; Parameter.UIntPtr ] ]; ReturnParameter = Parameter.UIntPtr }
            { Name = PrettyNaming.CompileOpName("|||"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UIntPtr; Parameter.UIntPtr ] ]; ReturnParameter = Parameter.UIntPtr }
            { Name = PrettyNaming.CompileOpName("~+"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UIntPtr ] ]; ReturnParameter = Parameter.UIntPtr }
            { Name = PrettyNaming.CompileOpName("~~~"); Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UIntPtr ] ]; ReturnParameter = Parameter.UIntPtr }
            { Name = "One"; Kind = MemberKind.Property PropertyKind.Get; GenericParameters = []; Parameters = [  ]; ReturnParameter = Parameter.UIntPtr }
            { Name = "Zero"; Kind = MemberKind.Property PropertyKind.Get; GenericParameters = []; Parameters = [  ]; ReturnParameter = Parameter.UIntPtr }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UIntPtr ] ]; ReturnParameter = Parameter.Byte }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UIntPtr ] ]; ReturnParameter = Parameter.Char }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UIntPtr ] ]; ReturnParameter = Parameter.Decimal }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UIntPtr ] ]; ReturnParameter = Parameter.Double }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UIntPtr ] ]; ReturnParameter = Parameter.Single }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UIntPtr ] ]; ReturnParameter = Parameter.Int16 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UIntPtr ] ]; ReturnParameter = Parameter.Int32 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UIntPtr ] ]; ReturnParameter = Parameter.Int64 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UIntPtr ] ]; ReturnParameter = Parameter.IntPtr }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UIntPtr ] ]; ReturnParameter = Parameter.SByte }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UIntPtr ] ]; ReturnParameter = Parameter.UInt16 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UIntPtr ] ]; ReturnParameter = Parameter.UInt32 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UIntPtr ] ]; ReturnParameter = Parameter.UInt64 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Parameters = [ [ Parameter.UIntPtr ] ]; ReturnParameter = Parameter.UIntPtr }
          ]
      })
  ]

let implicitMembers id =
  match Map.tryFind id table with
  | Some im -> (im.InstanceMembers, im.StaticMembers)
  | None -> [], []