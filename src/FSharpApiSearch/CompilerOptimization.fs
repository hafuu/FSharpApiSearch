module internal FSharpApiSearch.CompilerOptimization

open Microsoft.FSharp.Compiler

type ImplicitMember = {
  InstanceMembers: Member list
  StaticMembers: Member list
}

module FullIdentity =
  open System
  open SpecialTypes.FullIdentity

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

let table: Map<FullIdentity, ImplicitMember> =
  Map.ofList [
    (FullIdentity.Boolean, 
      {
        InstanceMembers = []
        StaticMembers =
          [
            { Name = PrettyNaming.CompileOpName("<"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Boolean; LowType.Boolean ]; IsCurried = false; ReturnType = LowType.Boolean }
            { Name = PrettyNaming.CompileOpName("<>"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Boolean; LowType.Boolean ]; IsCurried = false; ReturnType = LowType.Boolean }
            { Name = PrettyNaming.CompileOpName("="); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Boolean; LowType.Boolean ]; IsCurried = false; ReturnType = LowType.Boolean }
          ]
      })
    (FullIdentity.Byte, 
      {
        InstanceMembers = []
        StaticMembers =
          [
            { Name = PrettyNaming.CompileOpName("%"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Byte; LowType.Byte ]; IsCurried = false; ReturnType = LowType.Byte }
            { Name = PrettyNaming.CompileOpName("&&&"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Byte; LowType.Byte ]; IsCurried = false; ReturnType = LowType.Byte }
            { Name = PrettyNaming.CompileOpName("*"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Byte; LowType.Byte ]; IsCurried = false; ReturnType = LowType.Byte }
            { Name = PrettyNaming.CompileOpName("+"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Byte; LowType.Byte ]; IsCurried = false; ReturnType = LowType.Byte }
            { Name = PrettyNaming.CompileOpName("-"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Byte; LowType.Byte ]; IsCurried = false; ReturnType = LowType.Byte }
            { Name = PrettyNaming.CompileOpName("/"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Byte; LowType.Byte ]; IsCurried = false; ReturnType = LowType.Byte }
            { Name = PrettyNaming.CompileOpName("<"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Byte; LowType.Byte ]; IsCurried = false; ReturnType = LowType.Boolean }
            { Name = PrettyNaming.CompileOpName("<<<"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Byte; LowType.Int32 ]; IsCurried = false; ReturnType = LowType.Byte }
            { Name = PrettyNaming.CompileOpName("<>"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Byte; LowType.Byte ]; IsCurried = false; ReturnType = LowType.Boolean }
            { Name = PrettyNaming.CompileOpName("="); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Byte; LowType.Byte ]; IsCurried = false; ReturnType = LowType.Boolean }
            { Name = PrettyNaming.CompileOpName(">>>"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Byte; LowType.Int32 ]; IsCurried = false; ReturnType = LowType.Byte }
            { Name = PrettyNaming.CompileOpName("^^^"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Byte; LowType.Byte ]; IsCurried = false; ReturnType = LowType.Byte }
            { Name = PrettyNaming.CompileOpName("|||"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Byte; LowType.Byte ]; IsCurried = false; ReturnType = LowType.Byte }
            { Name = PrettyNaming.CompileOpName("~+"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Byte ]; IsCurried = false; ReturnType = LowType.Byte }
            { Name = PrettyNaming.CompileOpName("~~~"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Byte ]; IsCurried = false; ReturnType = LowType.Byte }
            { Name = "One"; Kind = MemberKind.Property PropertyKind.Get; GenericParameters = []; Arguments = [  ]; IsCurried = false; ReturnType = LowType.Byte }
            { Name = "Zero"; Kind = MemberKind.Property PropertyKind.Get; GenericParameters = []; Arguments = [  ]; IsCurried = false; ReturnType = LowType.Byte }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Byte ]; IsCurried = false; ReturnType = LowType.Byte }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Byte ]; IsCurried = false; ReturnType = LowType.Char }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Byte ]; IsCurried = false; ReturnType = LowType.Decimal }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Byte ]; IsCurried = false; ReturnType = LowType.Double }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Byte ]; IsCurried = false; ReturnType = LowType.Single }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Byte ]; IsCurried = false; ReturnType = LowType.Int16 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Byte ]; IsCurried = false; ReturnType = LowType.Int32 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Byte ]; IsCurried = false; ReturnType = LowType.Int64 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Byte ]; IsCurried = false; ReturnType = LowType.IntPtr }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Byte ]; IsCurried = false; ReturnType = LowType.SByte }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Byte ]; IsCurried = false; ReturnType = LowType.UInt16 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Byte ]; IsCurried = false; ReturnType = LowType.UInt32 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Byte ]; IsCurried = false; ReturnType = LowType.UInt64 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Byte ]; IsCurried = false; ReturnType = LowType.UIntPtr }
          ]
      })
    (FullIdentity.Char, 
      {
        InstanceMembers = []
        StaticMembers =
          [
            { Name = PrettyNaming.CompileOpName("+"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Char; LowType.Char ]; IsCurried = false; ReturnType = LowType.Char }
            { Name = PrettyNaming.CompileOpName("<"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Char; LowType.Char ]; IsCurried = false; ReturnType = LowType.Boolean }
            { Name = PrettyNaming.CompileOpName("<>"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Char; LowType.Char ]; IsCurried = false; ReturnType = LowType.Boolean }
            { Name = PrettyNaming.CompileOpName("="); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Char; LowType.Char ]; IsCurried = false; ReturnType = LowType.Boolean }
            { Name = "One"; Kind = MemberKind.Property PropertyKind.Get; GenericParameters = []; Arguments = [  ]; IsCurried = false; ReturnType = LowType.Char }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Char ]; IsCurried = false; ReturnType = LowType.Byte }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Char ]; IsCurried = false; ReturnType = LowType.Char }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Char ]; IsCurried = false; ReturnType = LowType.Double }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Char ]; IsCurried = false; ReturnType = LowType.Single }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Char ]; IsCurried = false; ReturnType = LowType.Int16 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Char ]; IsCurried = false; ReturnType = LowType.Int32 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Char ]; IsCurried = false; ReturnType = LowType.Int64 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Char ]; IsCurried = false; ReturnType = LowType.IntPtr }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Char ]; IsCurried = false; ReturnType = LowType.SByte }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Char ]; IsCurried = false; ReturnType = LowType.UInt16 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Char ]; IsCurried = false; ReturnType = LowType.UInt32 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Char ]; IsCurried = false; ReturnType = LowType.UInt64 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Char ]; IsCurried = false; ReturnType = LowType.UIntPtr }
          ]
      })
    (FullIdentity.Decimal, 
      {
        InstanceMembers =
          [
            { Name = "Sign"; Kind = MemberKind.Property PropertyKind.Get; GenericParameters = []; Arguments = [  ]; IsCurried = false; ReturnType = LowType.Int32 }
          ]
        StaticMembers =
          [
            { Name = PrettyNaming.CompileOpName("%"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Decimal; LowType.Decimal ]; IsCurried = false; ReturnType = LowType.Decimal }
            { Name = PrettyNaming.CompileOpName("*"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Decimal; LowType.Decimal ]; IsCurried = false; ReturnType = LowType.Decimal }
            { Name = PrettyNaming.CompileOpName("+"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Decimal; LowType.Decimal ]; IsCurried = false; ReturnType = LowType.Decimal }
            { Name = PrettyNaming.CompileOpName("-"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Decimal; LowType.Decimal ]; IsCurried = false; ReturnType = LowType.Decimal }
            { Name = PrettyNaming.CompileOpName("/"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Decimal; LowType.Decimal ]; IsCurried = false; ReturnType = LowType.Decimal }
            { Name = PrettyNaming.CompileOpName("<"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Decimal; LowType.Decimal ]; IsCurried = false; ReturnType = LowType.Boolean }
            { Name = PrettyNaming.CompileOpName("<>"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Decimal; LowType.Decimal ]; IsCurried = false; ReturnType = LowType.Boolean }
            { Name = PrettyNaming.CompileOpName("="); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Decimal; LowType.Decimal ]; IsCurried = false; ReturnType = LowType.Boolean }
            { Name = PrettyNaming.CompileOpName("~+"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Decimal ]; IsCurried = false; ReturnType = LowType.Decimal }
            { Name = PrettyNaming.CompileOpName("~-"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Decimal ]; IsCurried = false; ReturnType = LowType.Decimal }
            { Name = "DivideByInt"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Decimal; LowType.Int32 ]; IsCurried = false; ReturnType = LowType.Decimal }
            { Name = "One"; Kind = MemberKind.Property PropertyKind.Get; GenericParameters = []; Arguments = [  ]; IsCurried = false; ReturnType = LowType.Decimal }
            { Name = "Zero"; Kind = MemberKind.Property PropertyKind.Get; GenericParameters = []; Arguments = [  ]; IsCurried = false; ReturnType = LowType.Decimal }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Decimal ]; IsCurried = false; ReturnType = LowType.Decimal }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Decimal ]; IsCurried = false; ReturnType = LowType.Double }
          ]
      })
    (FullIdentity.Double, 
      {
        InstanceMembers =
          [
            { Name = "Sign"; Kind = MemberKind.Property PropertyKind.Get; GenericParameters = []; Arguments = [  ]; IsCurried = false; ReturnType = LowType.Int32 }
          ]
        StaticMembers =
          [
            { Name = PrettyNaming.CompileOpName("%"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Double; LowType.Double ]; IsCurried = false; ReturnType = LowType.Double }
            { Name = PrettyNaming.CompileOpName("*"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Double; LowType.Double ]; IsCurried = false; ReturnType = LowType.Double }
            { Name = PrettyNaming.CompileOpName("+"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Double; LowType.Double ]; IsCurried = false; ReturnType = LowType.Double }
            { Name = PrettyNaming.CompileOpName("-"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Double; LowType.Double ]; IsCurried = false; ReturnType = LowType.Double }
            { Name = PrettyNaming.CompileOpName("/"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Double; LowType.Double ]; IsCurried = false; ReturnType = LowType.Double }
            { Name = PrettyNaming.CompileOpName("<"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Double; LowType.Double ]; IsCurried = false; ReturnType = LowType.Boolean }
            { Name = PrettyNaming.CompileOpName("<>"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Double; LowType.Double ]; IsCurried = false; ReturnType = LowType.Boolean }
            { Name = PrettyNaming.CompileOpName("="); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Double; LowType.Double ]; IsCurried = false; ReturnType = LowType.Boolean }
            { Name = PrettyNaming.CompileOpName("~+"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Double ]; IsCurried = false; ReturnType = LowType.Double }
            { Name = PrettyNaming.CompileOpName("~-"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Double ]; IsCurried = false; ReturnType = LowType.Double }
            { Name = "Acos"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Double ]; IsCurried = false; ReturnType = LowType.Double }
            { Name = "Asin"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Double ]; IsCurried = false; ReturnType = LowType.Double }
            { Name = "Atan"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Double ]; IsCurried = false; ReturnType = LowType.Double }
            { Name = "Atan2"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Double; LowType.Double ]; IsCurried = false; ReturnType = LowType.Double }
            { Name = "Ceiling"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Double ]; IsCurried = false; ReturnType = LowType.Double }
            { Name = "Cos"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Double ]; IsCurried = false; ReturnType = LowType.Double }
            { Name = "Cosh"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Double ]; IsCurried = false; ReturnType = LowType.Double }
            { Name = "DivideByInt"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Double; LowType.Int32 ]; IsCurried = false; ReturnType = LowType.Double }
            { Name = "Exp"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Double ]; IsCurried = false; ReturnType = LowType.Double }
            { Name = "Floor"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Double ]; IsCurried = false; ReturnType = LowType.Double }
            { Name = "Log"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Double ]; IsCurried = false; ReturnType = LowType.Double }
            { Name = "Log10"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Double ]; IsCurried = false; ReturnType = LowType.Double }
            { Name = "One"; Kind = MemberKind.Property PropertyKind.Get; GenericParameters = []; Arguments = [  ]; IsCurried = false; ReturnType = LowType.Double }
            { Name = "Pow"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Double; LowType.Double ]; IsCurried = false; ReturnType = LowType.Double }
            { Name = "Round"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Double ]; IsCurried = false; ReturnType = LowType.Double }
            { Name = "Sin"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Double ]; IsCurried = false; ReturnType = LowType.Double }
            { Name = "Sinh"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Double ]; IsCurried = false; ReturnType = LowType.Double }
            { Name = "Sqrt"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Double ]; IsCurried = false; ReturnType = LowType.Double }
            { Name = "Tan"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Double ]; IsCurried = false; ReturnType = LowType.Double }
            { Name = "Tanh"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Double ]; IsCurried = false; ReturnType = LowType.Double }
            { Name = "Truncate"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Double ]; IsCurried = false; ReturnType = LowType.Double }
            { Name = "Zero"; Kind = MemberKind.Property PropertyKind.Get; GenericParameters = []; Arguments = [  ]; IsCurried = false; ReturnType = LowType.Double }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Double ]; IsCurried = false; ReturnType = LowType.Byte }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Double ]; IsCurried = false; ReturnType = LowType.Char }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Double ]; IsCurried = false; ReturnType = LowType.Decimal }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Double ]; IsCurried = false; ReturnType = LowType.Double }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Double ]; IsCurried = false; ReturnType = LowType.Single }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Double ]; IsCurried = false; ReturnType = LowType.Int16 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Double ]; IsCurried = false; ReturnType = LowType.Int32 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Double ]; IsCurried = false; ReturnType = LowType.Int64 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Double ]; IsCurried = false; ReturnType = LowType.IntPtr }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Double ]; IsCurried = false; ReturnType = LowType.SByte }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Double ]; IsCurried = false; ReturnType = LowType.UInt16 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Double ]; IsCurried = false; ReturnType = LowType.UInt32 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Double ]; IsCurried = false; ReturnType = LowType.UInt64 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Double ]; IsCurried = false; ReturnType = LowType.UIntPtr }
          ]
      })
    (FullIdentity.Single, 
      {
        InstanceMembers =
          [
            { Name = "Sign"; Kind = MemberKind.Property PropertyKind.Get; GenericParameters = []; Arguments = [  ]; IsCurried = false; ReturnType = LowType.Int32 }
          ]
        StaticMembers =
          [
            { Name = PrettyNaming.CompileOpName("%"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Single; LowType.Single ]; IsCurried = false; ReturnType = LowType.Single }
            { Name = PrettyNaming.CompileOpName("*"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Single; LowType.Single ]; IsCurried = false; ReturnType = LowType.Single }
            { Name = PrettyNaming.CompileOpName("+"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Single; LowType.Single ]; IsCurried = false; ReturnType = LowType.Single }
            { Name = PrettyNaming.CompileOpName("-"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Single; LowType.Single ]; IsCurried = false; ReturnType = LowType.Single }
            { Name = PrettyNaming.CompileOpName("/"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Single; LowType.Single ]; IsCurried = false; ReturnType = LowType.Single }
            { Name = PrettyNaming.CompileOpName("<>"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Single; LowType.Single ]; IsCurried = false; ReturnType = LowType.Boolean }
            { Name = PrettyNaming.CompileOpName("="); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Single; LowType.Single ]; IsCurried = false; ReturnType = LowType.Boolean }
            { Name = PrettyNaming.CompileOpName("~+"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Single ]; IsCurried = false; ReturnType = LowType.Single }
            { Name = PrettyNaming.CompileOpName("~-"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Single ]; IsCurried = false; ReturnType = LowType.Single }
            { Name = "Acos"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Single ]; IsCurried = false; ReturnType = LowType.Single }
            { Name = "Asin"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Single ]; IsCurried = false; ReturnType = LowType.Single }
            { Name = "Atan"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Single ]; IsCurried = false; ReturnType = LowType.Single }
            { Name = "Atan2"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Single; LowType.Single ]; IsCurried = false; ReturnType = LowType.Single }
            { Name = "Ceiling"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Single ]; IsCurried = false; ReturnType = LowType.Single }
            { Name = "Cos"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Single ]; IsCurried = false; ReturnType = LowType.Single }
            { Name = "Cosh"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Single ]; IsCurried = false; ReturnType = LowType.Single }
            { Name = "DivideByInt"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Single; LowType.Int32 ]; IsCurried = false; ReturnType = LowType.Single }
            { Name = "Exp"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Single ]; IsCurried = false; ReturnType = LowType.Single }
            { Name = "Floor"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Single ]; IsCurried = false; ReturnType = LowType.Single }
            { Name = "Log"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Single ]; IsCurried = false; ReturnType = LowType.Single }
            { Name = "Log10"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Single ]; IsCurried = false; ReturnType = LowType.Single }
            { Name = "One"; Kind = MemberKind.Property PropertyKind.Get; GenericParameters = []; Arguments = [  ]; IsCurried = false; ReturnType = LowType.Single }
            { Name = "Pow"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Single; LowType.Single ]; IsCurried = false; ReturnType = LowType.Single }
            { Name = "Round"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Single ]; IsCurried = false; ReturnType = LowType.Single }
            { Name = "Sin"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Single ]; IsCurried = false; ReturnType = LowType.Single }
            { Name = "Sinh"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Single ]; IsCurried = false; ReturnType = LowType.Single }
            { Name = "Sqrt"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Single ]; IsCurried = false; ReturnType = LowType.Single }
            { Name = "Tan"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Single ]; IsCurried = false; ReturnType = LowType.Single }
            { Name = "Tanh"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Single ]; IsCurried = false; ReturnType = LowType.Single }
            { Name = "Truncate"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Single ]; IsCurried = false; ReturnType = LowType.Single }
            { Name = "Zero"; Kind = MemberKind.Property PropertyKind.Get; GenericParameters = []; Arguments = [  ]; IsCurried = false; ReturnType = LowType.Single }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Single ]; IsCurried = false; ReturnType = LowType.Byte }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Single ]; IsCurried = false; ReturnType = LowType.Char }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Single ]; IsCurried = false; ReturnType = LowType.Decimal }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Single ]; IsCurried = false; ReturnType = LowType.Double }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Single ]; IsCurried = false; ReturnType = LowType.Single }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Single ]; IsCurried = false; ReturnType = LowType.Int16 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Single ]; IsCurried = false; ReturnType = LowType.Int32 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Single ]; IsCurried = false; ReturnType = LowType.Int64 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Single ]; IsCurried = false; ReturnType = LowType.IntPtr }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Single ]; IsCurried = false; ReturnType = LowType.SByte }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Single ]; IsCurried = false; ReturnType = LowType.UInt16 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Single ]; IsCurried = false; ReturnType = LowType.UInt32 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Single ]; IsCurried = false; ReturnType = LowType.UInt64 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Single ]; IsCurried = false; ReturnType = LowType.UIntPtr }
          ]
      })
    (FullIdentity.Int16, 
      {
        InstanceMembers =
          [
            { Name = "Sign"; Kind = MemberKind.Property PropertyKind.Get; GenericParameters = []; Arguments = [  ]; IsCurried = false; ReturnType = LowType.Int32 }
          ]
        StaticMembers =
          [
            { Name = PrettyNaming.CompileOpName("%"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Int16; LowType.Int16 ]; IsCurried = false; ReturnType = LowType.Int16 }
            { Name = PrettyNaming.CompileOpName("&&&"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Int16; LowType.Int16 ]; IsCurried = false; ReturnType = LowType.Int16 }
            { Name = PrettyNaming.CompileOpName("*"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Int16; LowType.Int16 ]; IsCurried = false; ReturnType = LowType.Int16 }
            { Name = PrettyNaming.CompileOpName("+"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Int16; LowType.Int16 ]; IsCurried = false; ReturnType = LowType.Int16 }
            { Name = PrettyNaming.CompileOpName("-"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Int16; LowType.Int16 ]; IsCurried = false; ReturnType = LowType.Int16 }
            { Name = PrettyNaming.CompileOpName("/"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Int16; LowType.Int16 ]; IsCurried = false; ReturnType = LowType.Int16 }
            { Name = PrettyNaming.CompileOpName("<"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Int16; LowType.Int16 ]; IsCurried = false; ReturnType = LowType.Boolean }
            { Name = PrettyNaming.CompileOpName("<<<"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Int16; LowType.Int32 ]; IsCurried = false; ReturnType = LowType.Int16 }
            { Name = PrettyNaming.CompileOpName("<>"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Int16; LowType.Int16 ]; IsCurried = false; ReturnType = LowType.Boolean }
            { Name = PrettyNaming.CompileOpName("="); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Int16; LowType.Int16 ]; IsCurried = false; ReturnType = LowType.Boolean }
            { Name = PrettyNaming.CompileOpName(">>>"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Int16; LowType.Int32 ]; IsCurried = false; ReturnType = LowType.Int16 }
            { Name = PrettyNaming.CompileOpName("^^^"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Int16; LowType.Int16 ]; IsCurried = false; ReturnType = LowType.Int16 }
            { Name = PrettyNaming.CompileOpName("|||"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Int16; LowType.Int16 ]; IsCurried = false; ReturnType = LowType.Int16 }
            { Name = PrettyNaming.CompileOpName("~+"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Int16 ]; IsCurried = false; ReturnType = LowType.Int16 }
            { Name = PrettyNaming.CompileOpName("~-"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Int16 ]; IsCurried = false; ReturnType = LowType.Int16 }
            { Name = PrettyNaming.CompileOpName("~~~"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Int16 ]; IsCurried = false; ReturnType = LowType.Int16 }
            { Name = "One"; Kind = MemberKind.Property PropertyKind.Get; GenericParameters = []; Arguments = [  ]; IsCurried = false; ReturnType = LowType.Int16 }
            { Name = "Zero"; Kind = MemberKind.Property PropertyKind.Get; GenericParameters = []; Arguments = [  ]; IsCurried = false; ReturnType = LowType.Int16 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Int16 ]; IsCurried = false; ReturnType = LowType.Byte }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Int16 ]; IsCurried = false; ReturnType = LowType.Char }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Int16 ]; IsCurried = false; ReturnType = LowType.Decimal }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Int16 ]; IsCurried = false; ReturnType = LowType.Double }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Int16 ]; IsCurried = false; ReturnType = LowType.Single }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Int16 ]; IsCurried = false; ReturnType = LowType.Int16 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Int16 ]; IsCurried = false; ReturnType = LowType.Int32 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Int16 ]; IsCurried = false; ReturnType = LowType.Int64 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Int16 ]; IsCurried = false; ReturnType = LowType.IntPtr }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Int16 ]; IsCurried = false; ReturnType = LowType.SByte }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Int16 ]; IsCurried = false; ReturnType = LowType.UInt16 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Int16 ]; IsCurried = false; ReturnType = LowType.UInt32 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Int16 ]; IsCurried = false; ReturnType = LowType.UInt64 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Int16 ]; IsCurried = false; ReturnType = LowType.UIntPtr }
          ]
      })
    (FullIdentity.Int32, 
      {
        InstanceMembers =
          [
            { Name = "Sign"; Kind = MemberKind.Property PropertyKind.Get; GenericParameters = []; Arguments = [  ]; IsCurried = false; ReturnType = LowType.Int32 }
          ]
        StaticMembers =
          [
            { Name = PrettyNaming.CompileOpName("%"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Int32; LowType.Int32 ]; IsCurried = false; ReturnType = LowType.Int32 }
            { Name = PrettyNaming.CompileOpName("&&&"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Int32; LowType.Int32 ]; IsCurried = false; ReturnType = LowType.Int32 }
            { Name = PrettyNaming.CompileOpName("*"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Int32; LowType.Int32 ]; IsCurried = false; ReturnType = LowType.Int32 }
            { Name = PrettyNaming.CompileOpName("+"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Int32; LowType.Int32 ]; IsCurried = false; ReturnType = LowType.Int32 }
            { Name = PrettyNaming.CompileOpName("-"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Int32; LowType.Int32 ]; IsCurried = false; ReturnType = LowType.Int32 }
            { Name = PrettyNaming.CompileOpName("/"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Int32; LowType.Int32 ]; IsCurried = false; ReturnType = LowType.Int32 }
            { Name = PrettyNaming.CompileOpName("<"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Int32; LowType.Int32 ]; IsCurried = false; ReturnType = LowType.Boolean }
            { Name = PrettyNaming.CompileOpName("<<<"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Int32; LowType.Int32 ]; IsCurried = false; ReturnType = LowType.Int32 }
            { Name = PrettyNaming.CompileOpName("<>"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Int32; LowType.Int32 ]; IsCurried = false; ReturnType = LowType.Boolean }
            { Name = PrettyNaming.CompileOpName("="); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Int32; LowType.Int32 ]; IsCurried = false; ReturnType = LowType.Boolean }
            { Name = PrettyNaming.CompileOpName(">>>"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Int32; LowType.Int32 ]; IsCurried = false; ReturnType = LowType.Int32 }
            { Name = PrettyNaming.CompileOpName("^^^"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Int32; LowType.Int32 ]; IsCurried = false; ReturnType = LowType.Int32 }
            { Name = PrettyNaming.CompileOpName("|||"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Int32; LowType.Int32 ]; IsCurried = false; ReturnType = LowType.Int32 }
            { Name = PrettyNaming.CompileOpName("~+"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Int32 ]; IsCurried = false; ReturnType = LowType.Int32 }
            { Name = PrettyNaming.CompileOpName("~-"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Int32 ]; IsCurried = false; ReturnType = LowType.Int32 }
            { Name = PrettyNaming.CompileOpName("~~~"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Int32 ]; IsCurried = false; ReturnType = LowType.Int32 }
            { Name = "One"; Kind = MemberKind.Property PropertyKind.Get; GenericParameters = []; Arguments = [  ]; IsCurried = false; ReturnType = LowType.Int32 }
            { Name = "Zero"; Kind = MemberKind.Property PropertyKind.Get; GenericParameters = []; Arguments = [  ]; IsCurried = false; ReturnType = LowType.Int32 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Int32 ]; IsCurried = false; ReturnType = LowType.Byte }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Int32 ]; IsCurried = false; ReturnType = LowType.Char }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Int32 ]; IsCurried = false; ReturnType = LowType.Decimal }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Int32 ]; IsCurried = false; ReturnType = LowType.Double }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Int32 ]; IsCurried = false; ReturnType = LowType.Single }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Int32 ]; IsCurried = false; ReturnType = LowType.Int16 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Int32 ]; IsCurried = false; ReturnType = LowType.Int32 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Int32 ]; IsCurried = false; ReturnType = LowType.Int64 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Int32 ]; IsCurried = false; ReturnType = LowType.IntPtr }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Int32 ]; IsCurried = false; ReturnType = LowType.SByte }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Int32 ]; IsCurried = false; ReturnType = LowType.UInt16 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Int32 ]; IsCurried = false; ReturnType = LowType.UInt32 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Int32 ]; IsCurried = false; ReturnType = LowType.UInt64 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Int32 ]; IsCurried = false; ReturnType = LowType.UIntPtr }
          ]
      })
    (FullIdentity.Int64, 
      {
        InstanceMembers =
          [
            { Name = "Sign"; Kind = MemberKind.Property PropertyKind.Get; GenericParameters = []; Arguments = [  ]; IsCurried = false; ReturnType = LowType.Int32 }
          ]
        StaticMembers =
          [
            { Name = PrettyNaming.CompileOpName("%"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Int64; LowType.Int64 ]; IsCurried = false; ReturnType = LowType.Int64 }
            { Name = PrettyNaming.CompileOpName("&&&"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Int64; LowType.Int64 ]; IsCurried = false; ReturnType = LowType.Int64 }
            { Name = PrettyNaming.CompileOpName("*"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Int64; LowType.Int64 ]; IsCurried = false; ReturnType = LowType.Int64 }
            { Name = PrettyNaming.CompileOpName("+"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Int64; LowType.Int64 ]; IsCurried = false; ReturnType = LowType.Int64 }
            { Name = PrettyNaming.CompileOpName("-"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Int64; LowType.Int64 ]; IsCurried = false; ReturnType = LowType.Int64 }
            { Name = PrettyNaming.CompileOpName("/"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Int64; LowType.Int64 ]; IsCurried = false; ReturnType = LowType.Int64 }
            { Name = PrettyNaming.CompileOpName("<"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Int64; LowType.Int64 ]; IsCurried = false; ReturnType = LowType.Boolean }
            { Name = PrettyNaming.CompileOpName("<<<"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Int64; LowType.Int32 ]; IsCurried = false; ReturnType = LowType.Int64 }
            { Name = PrettyNaming.CompileOpName("<>"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Int64; LowType.Int64 ]; IsCurried = false; ReturnType = LowType.Boolean }
            { Name = PrettyNaming.CompileOpName("="); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Int64; LowType.Int64 ]; IsCurried = false; ReturnType = LowType.Boolean }
            { Name = PrettyNaming.CompileOpName(">>>"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Int64; LowType.Int32 ]; IsCurried = false; ReturnType = LowType.Int64 }
            { Name = PrettyNaming.CompileOpName("^^^"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Int64; LowType.Int64 ]; IsCurried = false; ReturnType = LowType.Int64 }
            { Name = PrettyNaming.CompileOpName("|||"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Int64; LowType.Int64 ]; IsCurried = false; ReturnType = LowType.Int64 }
            { Name = PrettyNaming.CompileOpName("~+"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Int64 ]; IsCurried = false; ReturnType = LowType.Int64 }
            { Name = PrettyNaming.CompileOpName("~-"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Int64 ]; IsCurried = false; ReturnType = LowType.Int64 }
            { Name = PrettyNaming.CompileOpName("~~~"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Int64 ]; IsCurried = false; ReturnType = LowType.Int64 }
            { Name = "One"; Kind = MemberKind.Property PropertyKind.Get; GenericParameters = []; Arguments = [  ]; IsCurried = false; ReturnType = LowType.Int64 }
            { Name = "Zero"; Kind = MemberKind.Property PropertyKind.Get; GenericParameters = []; Arguments = [  ]; IsCurried = false; ReturnType = LowType.Int64 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Int64 ]; IsCurried = false; ReturnType = LowType.Byte }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Int64 ]; IsCurried = false; ReturnType = LowType.Char }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Int64 ]; IsCurried = false; ReturnType = LowType.Decimal }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Int64 ]; IsCurried = false; ReturnType = LowType.Double }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Int64 ]; IsCurried = false; ReturnType = LowType.Single }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Int64 ]; IsCurried = false; ReturnType = LowType.Int16 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Int64 ]; IsCurried = false; ReturnType = LowType.Int32 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Int64 ]; IsCurried = false; ReturnType = LowType.Int64 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Int64 ]; IsCurried = false; ReturnType = LowType.IntPtr }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Int64 ]; IsCurried = false; ReturnType = LowType.SByte }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Int64 ]; IsCurried = false; ReturnType = LowType.UInt16 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Int64 ]; IsCurried = false; ReturnType = LowType.UInt32 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Int64 ]; IsCurried = false; ReturnType = LowType.UInt64 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.Int64 ]; IsCurried = false; ReturnType = LowType.UIntPtr }
          ]
      })
    (FullIdentity.IntPtr, 
      {
        InstanceMembers =
          [
            { Name = "Sign"; Kind = MemberKind.Property PropertyKind.Get; GenericParameters = []; Arguments = [  ]; IsCurried = false; ReturnType = LowType.Int32 }
          ]
        StaticMembers =
          [
            { Name = PrettyNaming.CompileOpName("%"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.IntPtr; LowType.IntPtr ]; IsCurried = false; ReturnType = LowType.IntPtr }
            { Name = PrettyNaming.CompileOpName("&&&"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.IntPtr; LowType.IntPtr ]; IsCurried = false; ReturnType = LowType.IntPtr }
            { Name = PrettyNaming.CompileOpName("*"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.IntPtr; LowType.IntPtr ]; IsCurried = false; ReturnType = LowType.IntPtr }
            { Name = PrettyNaming.CompileOpName("+"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.IntPtr; LowType.IntPtr ]; IsCurried = false; ReturnType = LowType.IntPtr }
            { Name = PrettyNaming.CompileOpName("-"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.IntPtr; LowType.IntPtr ]; IsCurried = false; ReturnType = LowType.IntPtr }
            { Name = PrettyNaming.CompileOpName("/"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.IntPtr; LowType.IntPtr ]; IsCurried = false; ReturnType = LowType.IntPtr }
            { Name = PrettyNaming.CompileOpName("<"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.IntPtr; LowType.IntPtr ]; IsCurried = false; ReturnType = LowType.Boolean }
            { Name = PrettyNaming.CompileOpName("<<<"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.IntPtr; LowType.Int32 ]; IsCurried = false; ReturnType = LowType.IntPtr }
            { Name = PrettyNaming.CompileOpName("<>"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.IntPtr; LowType.IntPtr ]; IsCurried = false; ReturnType = LowType.Boolean }
            { Name = PrettyNaming.CompileOpName("="); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.IntPtr; LowType.IntPtr ]; IsCurried = false; ReturnType = LowType.Boolean }
            { Name = PrettyNaming.CompileOpName(">>>"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.IntPtr; LowType.Int32 ]; IsCurried = false; ReturnType = LowType.IntPtr }
            { Name = PrettyNaming.CompileOpName("^^^"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.IntPtr; LowType.IntPtr ]; IsCurried = false; ReturnType = LowType.IntPtr }
            { Name = PrettyNaming.CompileOpName("|||"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.IntPtr; LowType.IntPtr ]; IsCurried = false; ReturnType = LowType.IntPtr }
            { Name = PrettyNaming.CompileOpName("~+"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.IntPtr ]; IsCurried = false; ReturnType = LowType.IntPtr }
            { Name = PrettyNaming.CompileOpName("~-"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.IntPtr ]; IsCurried = false; ReturnType = LowType.IntPtr }
            { Name = PrettyNaming.CompileOpName("~~~"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.IntPtr ]; IsCurried = false; ReturnType = LowType.IntPtr }
            { Name = "One"; Kind = MemberKind.Property PropertyKind.Get; GenericParameters = []; Arguments = [  ]; IsCurried = false; ReturnType = LowType.IntPtr }
            { Name = "Zero"; Kind = MemberKind.Property PropertyKind.Get; GenericParameters = []; Arguments = [  ]; IsCurried = false; ReturnType = LowType.IntPtr }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.IntPtr ]; IsCurried = false; ReturnType = LowType.Byte }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.IntPtr ]; IsCurried = false; ReturnType = LowType.Char }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.IntPtr ]; IsCurried = false; ReturnType = LowType.Decimal }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.IntPtr ]; IsCurried = false; ReturnType = LowType.Double }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.IntPtr ]; IsCurried = false; ReturnType = LowType.Single }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.IntPtr ]; IsCurried = false; ReturnType = LowType.Int16 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.IntPtr ]; IsCurried = false; ReturnType = LowType.Int32 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.IntPtr ]; IsCurried = false; ReturnType = LowType.Int64 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.IntPtr ]; IsCurried = false; ReturnType = LowType.IntPtr }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.IntPtr ]; IsCurried = false; ReturnType = LowType.SByte }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.IntPtr ]; IsCurried = false; ReturnType = LowType.UInt16 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.IntPtr ]; IsCurried = false; ReturnType = LowType.UInt32 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.IntPtr ]; IsCurried = false; ReturnType = LowType.UInt64 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.IntPtr ]; IsCurried = false; ReturnType = LowType.UIntPtr }
          ]
      })
    (FullIdentity.SByte, 
      {
        InstanceMembers =
          [
            { Name = "Sign"; Kind = MemberKind.Property PropertyKind.Get; GenericParameters = []; Arguments = [  ]; IsCurried = false; ReturnType = LowType.Int32 }
          ]
        StaticMembers =
          [
            { Name = PrettyNaming.CompileOpName("%"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.SByte; LowType.SByte ]; IsCurried = false; ReturnType = LowType.SByte }
            { Name = PrettyNaming.CompileOpName("&&&"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.SByte; LowType.SByte ]; IsCurried = false; ReturnType = LowType.SByte }
            { Name = PrettyNaming.CompileOpName("*"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.SByte; LowType.SByte ]; IsCurried = false; ReturnType = LowType.SByte }
            { Name = PrettyNaming.CompileOpName("+"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.SByte; LowType.SByte ]; IsCurried = false; ReturnType = LowType.SByte }
            { Name = PrettyNaming.CompileOpName("-"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.SByte; LowType.SByte ]; IsCurried = false; ReturnType = LowType.SByte }
            { Name = PrettyNaming.CompileOpName("/"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.SByte; LowType.SByte ]; IsCurried = false; ReturnType = LowType.SByte }
            { Name = PrettyNaming.CompileOpName("<"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.SByte; LowType.SByte ]; IsCurried = false; ReturnType = LowType.Boolean }
            { Name = PrettyNaming.CompileOpName("<<<"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.SByte; LowType.Int32 ]; IsCurried = false; ReturnType = LowType.SByte }
            { Name = PrettyNaming.CompileOpName("<>"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.SByte; LowType.SByte ]; IsCurried = false; ReturnType = LowType.Boolean }
            { Name = PrettyNaming.CompileOpName("="); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.SByte; LowType.SByte ]; IsCurried = false; ReturnType = LowType.Boolean }
            { Name = PrettyNaming.CompileOpName(">>>"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.SByte; LowType.Int32 ]; IsCurried = false; ReturnType = LowType.SByte }
            { Name = PrettyNaming.CompileOpName("^^^"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.SByte; LowType.SByte ]; IsCurried = false; ReturnType = LowType.SByte }
            { Name = PrettyNaming.CompileOpName("|||"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.SByte; LowType.SByte ]; IsCurried = false; ReturnType = LowType.SByte }
            { Name = PrettyNaming.CompileOpName("~+"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.SByte ]; IsCurried = false; ReturnType = LowType.SByte }
            { Name = PrettyNaming.CompileOpName("~-"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.SByte ]; IsCurried = false; ReturnType = LowType.SByte }
            { Name = PrettyNaming.CompileOpName("~~~"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.SByte ]; IsCurried = false; ReturnType = LowType.SByte }
            { Name = "One"; Kind = MemberKind.Property PropertyKind.Get; GenericParameters = []; Arguments = [  ]; IsCurried = false; ReturnType = LowType.SByte }
            { Name = "Zero"; Kind = MemberKind.Property PropertyKind.Get; GenericParameters = []; Arguments = [  ]; IsCurried = false; ReturnType = LowType.SByte }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.SByte ]; IsCurried = false; ReturnType = LowType.Byte }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.SByte ]; IsCurried = false; ReturnType = LowType.Char }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.SByte ]; IsCurried = false; ReturnType = LowType.Decimal }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.SByte ]; IsCurried = false; ReturnType = LowType.Double }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.SByte ]; IsCurried = false; ReturnType = LowType.Single }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.SByte ]; IsCurried = false; ReturnType = LowType.Int16 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.SByte ]; IsCurried = false; ReturnType = LowType.Int32 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.SByte ]; IsCurried = false; ReturnType = LowType.Int64 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.SByte ]; IsCurried = false; ReturnType = LowType.IntPtr }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.SByte ]; IsCurried = false; ReturnType = LowType.SByte }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.SByte ]; IsCurried = false; ReturnType = LowType.UInt16 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.SByte ]; IsCurried = false; ReturnType = LowType.UInt32 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.SByte ]; IsCurried = false; ReturnType = LowType.UInt64 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.SByte ]; IsCurried = false; ReturnType = LowType.UIntPtr }
          ]
      })
    (FullIdentity.String, 
      {
        InstanceMembers = []
        StaticMembers =
          [
            { Name = PrettyNaming.CompileOpName("+"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.String; LowType.String ]; IsCurried = false; ReturnType = LowType.String }
            { Name = PrettyNaming.CompileOpName("<"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.String; LowType.String ]; IsCurried = false; ReturnType = LowType.Boolean }
            { Name = PrettyNaming.CompileOpName("<="); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.String; LowType.String ]; IsCurried = false; ReturnType = LowType.Boolean }
            { Name = PrettyNaming.CompileOpName("<>"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.String; LowType.String ]; IsCurried = false; ReturnType = LowType.Boolean }
            { Name = PrettyNaming.CompileOpName("="); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.String; LowType.String ]; IsCurried = false; ReturnType = LowType.Boolean }
            { Name = PrettyNaming.CompileOpName(">"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.String; LowType.String ]; IsCurried = false; ReturnType = LowType.Boolean }
            { Name = PrettyNaming.CompileOpName(">="); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.String; LowType.String ]; IsCurried = false; ReturnType = LowType.Boolean }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.String ]; IsCurried = false; ReturnType = LowType.Byte }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.String ]; IsCurried = false; ReturnType = LowType.Char }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.String ]; IsCurried = false; ReturnType = LowType.Decimal }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.String ]; IsCurried = false; ReturnType = LowType.Double }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.String ]; IsCurried = false; ReturnType = LowType.Single }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.String ]; IsCurried = false; ReturnType = LowType.Int16 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.String ]; IsCurried = false; ReturnType = LowType.Int32 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.String ]; IsCurried = false; ReturnType = LowType.Int64 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.String ]; IsCurried = false; ReturnType = LowType.IntPtr }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.String ]; IsCurried = false; ReturnType = LowType.SByte }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.String ]; IsCurried = false; ReturnType = LowType.UInt16 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.String ]; IsCurried = false; ReturnType = LowType.UInt32 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.String ]; IsCurried = false; ReturnType = LowType.UInt64 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.String ]; IsCurried = false; ReturnType = LowType.UIntPtr }
          ]
      })
    (FullIdentity.UInt16, 
      {
        InstanceMembers = []
        StaticMembers =
          [
            { Name = PrettyNaming.CompileOpName("%"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UInt16; LowType.UInt16 ]; IsCurried = false; ReturnType = LowType.UInt16 }
            { Name = PrettyNaming.CompileOpName("&&&"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UInt16; LowType.UInt16 ]; IsCurried = false; ReturnType = LowType.UInt16 }
            { Name = PrettyNaming.CompileOpName("*"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UInt16; LowType.UInt16 ]; IsCurried = false; ReturnType = LowType.UInt16 }
            { Name = PrettyNaming.CompileOpName("+"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UInt16; LowType.UInt16 ]; IsCurried = false; ReturnType = LowType.UInt16 }
            { Name = PrettyNaming.CompileOpName("-"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UInt16; LowType.UInt16 ]; IsCurried = false; ReturnType = LowType.UInt16 }
            { Name = PrettyNaming.CompileOpName("/"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UInt16; LowType.UInt16 ]; IsCurried = false; ReturnType = LowType.UInt16 }
            { Name = PrettyNaming.CompileOpName("<"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UInt16; LowType.UInt16 ]; IsCurried = false; ReturnType = LowType.Boolean }
            { Name = PrettyNaming.CompileOpName("<<<"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UInt16; LowType.Int32 ]; IsCurried = false; ReturnType = LowType.UInt16 }
            { Name = PrettyNaming.CompileOpName("<>"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UInt16; LowType.UInt16 ]; IsCurried = false; ReturnType = LowType.Boolean }
            { Name = PrettyNaming.CompileOpName("="); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UInt16; LowType.UInt16 ]; IsCurried = false; ReturnType = LowType.Boolean }
            { Name = PrettyNaming.CompileOpName(">>>"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UInt16; LowType.Int32 ]; IsCurried = false; ReturnType = LowType.UInt16 }
            { Name = PrettyNaming.CompileOpName("^^^"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UInt16; LowType.UInt16 ]; IsCurried = false; ReturnType = LowType.UInt16 }
            { Name = PrettyNaming.CompileOpName("|||"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UInt16; LowType.UInt16 ]; IsCurried = false; ReturnType = LowType.UInt16 }
            { Name = PrettyNaming.CompileOpName("~+"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UInt16 ]; IsCurried = false; ReturnType = LowType.UInt16 }
            { Name = PrettyNaming.CompileOpName("~~~"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UInt16 ]; IsCurried = false; ReturnType = LowType.UInt16 }
            { Name = "One"; Kind = MemberKind.Property PropertyKind.Get; GenericParameters = []; Arguments = [  ]; IsCurried = false; ReturnType = LowType.UInt16 }
            { Name = "Zero"; Kind = MemberKind.Property PropertyKind.Get; GenericParameters = []; Arguments = [  ]; IsCurried = false; ReturnType = LowType.UInt16 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UInt16 ]; IsCurried = false; ReturnType = LowType.Byte }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UInt16 ]; IsCurried = false; ReturnType = LowType.Char }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UInt16 ]; IsCurried = false; ReturnType = LowType.Decimal }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UInt16 ]; IsCurried = false; ReturnType = LowType.Double }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UInt16 ]; IsCurried = false; ReturnType = LowType.Single }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UInt16 ]; IsCurried = false; ReturnType = LowType.Int16 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UInt16 ]; IsCurried = false; ReturnType = LowType.Int32 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UInt16 ]; IsCurried = false; ReturnType = LowType.Int64 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UInt16 ]; IsCurried = false; ReturnType = LowType.IntPtr }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UInt16 ]; IsCurried = false; ReturnType = LowType.SByte }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UInt16 ]; IsCurried = false; ReturnType = LowType.UInt16 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UInt16 ]; IsCurried = false; ReturnType = LowType.UInt32 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UInt16 ]; IsCurried = false; ReturnType = LowType.UInt64 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UInt16 ]; IsCurried = false; ReturnType = LowType.UIntPtr }
          ]
      })
    (FullIdentity.UInt32, 
      {
        InstanceMembers = []
        StaticMembers =
          [
            { Name = PrettyNaming.CompileOpName("%"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UInt32; LowType.UInt32 ]; IsCurried = false; ReturnType = LowType.UInt32 }
            { Name = PrettyNaming.CompileOpName("&&&"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UInt32; LowType.UInt32 ]; IsCurried = false; ReturnType = LowType.UInt32 }
            { Name = PrettyNaming.CompileOpName("*"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UInt32; LowType.UInt32 ]; IsCurried = false; ReturnType = LowType.UInt32 }
            { Name = PrettyNaming.CompileOpName("+"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UInt32; LowType.UInt32 ]; IsCurried = false; ReturnType = LowType.UInt32 }
            { Name = PrettyNaming.CompileOpName("-"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UInt32; LowType.UInt32 ]; IsCurried = false; ReturnType = LowType.UInt32 }
            { Name = PrettyNaming.CompileOpName("/"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UInt32; LowType.UInt32 ]; IsCurried = false; ReturnType = LowType.UInt32 }
            { Name = PrettyNaming.CompileOpName("<"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UInt32; LowType.UInt32 ]; IsCurried = false; ReturnType = LowType.Boolean }
            { Name = PrettyNaming.CompileOpName("<<<"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UInt32; LowType.Int32 ]; IsCurried = false; ReturnType = LowType.UInt32 }
            { Name = PrettyNaming.CompileOpName("<>"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UInt32; LowType.UInt32 ]; IsCurried = false; ReturnType = LowType.Boolean }
            { Name = PrettyNaming.CompileOpName("="); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UInt32; LowType.UInt32 ]; IsCurried = false; ReturnType = LowType.Boolean }
            { Name = PrettyNaming.CompileOpName(">>>"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UInt32; LowType.Int32 ]; IsCurried = false; ReturnType = LowType.UInt32 }
            { Name = PrettyNaming.CompileOpName("^^^"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UInt32; LowType.UInt32 ]; IsCurried = false; ReturnType = LowType.UInt32 }
            { Name = PrettyNaming.CompileOpName("|||"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UInt32; LowType.UInt32 ]; IsCurried = false; ReturnType = LowType.UInt32 }
            { Name = PrettyNaming.CompileOpName("~+"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UInt32 ]; IsCurried = false; ReturnType = LowType.UInt32 }
            { Name = PrettyNaming.CompileOpName("~~~"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UInt32 ]; IsCurried = false; ReturnType = LowType.UInt32 }
            { Name = "One"; Kind = MemberKind.Property PropertyKind.Get; GenericParameters = []; Arguments = [  ]; IsCurried = false; ReturnType = LowType.UInt32 }
            { Name = "Zero"; Kind = MemberKind.Property PropertyKind.Get; GenericParameters = []; Arguments = [  ]; IsCurried = false; ReturnType = LowType.UInt32 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UInt32 ]; IsCurried = false; ReturnType = LowType.Byte }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UInt32 ]; IsCurried = false; ReturnType = LowType.Char }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UInt32 ]; IsCurried = false; ReturnType = LowType.Decimal }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UInt32 ]; IsCurried = false; ReturnType = LowType.Double }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UInt32 ]; IsCurried = false; ReturnType = LowType.Single }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UInt32 ]; IsCurried = false; ReturnType = LowType.Int16 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UInt32 ]; IsCurried = false; ReturnType = LowType.Int32 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UInt32 ]; IsCurried = false; ReturnType = LowType.Int64 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UInt32 ]; IsCurried = false; ReturnType = LowType.IntPtr }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UInt32 ]; IsCurried = false; ReturnType = LowType.SByte }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UInt32 ]; IsCurried = false; ReturnType = LowType.UInt16 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UInt32 ]; IsCurried = false; ReturnType = LowType.UInt32 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UInt32 ]; IsCurried = false; ReturnType = LowType.UInt64 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UInt32 ]; IsCurried = false; ReturnType = LowType.UIntPtr }
          ]
      })
    (FullIdentity.UInt64, 
      {
        InstanceMembers = []
        StaticMembers =
          [
            { Name = PrettyNaming.CompileOpName("%"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UInt64; LowType.UInt64 ]; IsCurried = false; ReturnType = LowType.UInt64 }
            { Name = PrettyNaming.CompileOpName("&&&"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UInt64; LowType.UInt64 ]; IsCurried = false; ReturnType = LowType.UInt64 }
            { Name = PrettyNaming.CompileOpName("*"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UInt64; LowType.UInt64 ]; IsCurried = false; ReturnType = LowType.UInt64 }
            { Name = PrettyNaming.CompileOpName("+"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UInt64; LowType.UInt64 ]; IsCurried = false; ReturnType = LowType.UInt64 }
            { Name = PrettyNaming.CompileOpName("-"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UInt64; LowType.UInt64 ]; IsCurried = false; ReturnType = LowType.UInt64 }
            { Name = PrettyNaming.CompileOpName("/"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UInt64; LowType.UInt64 ]; IsCurried = false; ReturnType = LowType.UInt64 }
            { Name = PrettyNaming.CompileOpName("<"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UInt64; LowType.UInt64 ]; IsCurried = false; ReturnType = LowType.Boolean }
            { Name = PrettyNaming.CompileOpName("<<<"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UInt64; LowType.Int32 ]; IsCurried = false; ReturnType = LowType.UInt64 }
            { Name = PrettyNaming.CompileOpName("<>"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UInt64; LowType.UInt64 ]; IsCurried = false; ReturnType = LowType.Boolean }
            { Name = PrettyNaming.CompileOpName("="); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UInt64; LowType.UInt64 ]; IsCurried = false; ReturnType = LowType.Boolean }
            { Name = PrettyNaming.CompileOpName(">>>"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UInt64; LowType.Int32 ]; IsCurried = false; ReturnType = LowType.UInt64 }
            { Name = PrettyNaming.CompileOpName("^^^"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UInt64; LowType.UInt64 ]; IsCurried = false; ReturnType = LowType.UInt64 }
            { Name = PrettyNaming.CompileOpName("|||"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UInt64; LowType.UInt64 ]; IsCurried = false; ReturnType = LowType.UInt64 }
            { Name = PrettyNaming.CompileOpName("~+"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UInt64 ]; IsCurried = false; ReturnType = LowType.UInt64 }
            { Name = PrettyNaming.CompileOpName("~~~"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UInt64 ]; IsCurried = false; ReturnType = LowType.UInt64 }
            { Name = "One"; Kind = MemberKind.Property PropertyKind.Get; GenericParameters = []; Arguments = [  ]; IsCurried = false; ReturnType = LowType.UInt64 }
            { Name = "Zero"; Kind = MemberKind.Property PropertyKind.Get; GenericParameters = []; Arguments = [  ]; IsCurried = false; ReturnType = LowType.UInt64 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UInt64 ]; IsCurried = false; ReturnType = LowType.Byte }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UInt64 ]; IsCurried = false; ReturnType = LowType.Char }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UInt64 ]; IsCurried = false; ReturnType = LowType.Decimal }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UInt64 ]; IsCurried = false; ReturnType = LowType.Double }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UInt64 ]; IsCurried = false; ReturnType = LowType.Single }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UInt64 ]; IsCurried = false; ReturnType = LowType.Int16 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UInt64 ]; IsCurried = false; ReturnType = LowType.Int32 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UInt64 ]; IsCurried = false; ReturnType = LowType.Int64 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UInt64 ]; IsCurried = false; ReturnType = LowType.IntPtr }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UInt64 ]; IsCurried = false; ReturnType = LowType.SByte }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UInt64 ]; IsCurried = false; ReturnType = LowType.UInt16 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UInt64 ]; IsCurried = false; ReturnType = LowType.UInt32 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UInt64 ]; IsCurried = false; ReturnType = LowType.UInt64 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UInt64 ]; IsCurried = false; ReturnType = LowType.UIntPtr }
          ]
      })
    (FullIdentity.UIntPtr, 
      {
        InstanceMembers = []
        StaticMembers =
          [
            { Name = PrettyNaming.CompileOpName("%"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UIntPtr; LowType.UIntPtr ]; IsCurried = false; ReturnType = LowType.UIntPtr }
            { Name = PrettyNaming.CompileOpName("&&&"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UIntPtr; LowType.UIntPtr ]; IsCurried = false; ReturnType = LowType.UIntPtr }
            { Name = PrettyNaming.CompileOpName("*"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UIntPtr; LowType.UIntPtr ]; IsCurried = false; ReturnType = LowType.UIntPtr }
            { Name = PrettyNaming.CompileOpName("+"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UIntPtr; LowType.UIntPtr ]; IsCurried = false; ReturnType = LowType.UIntPtr }
            { Name = PrettyNaming.CompileOpName("-"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UIntPtr; LowType.UIntPtr ]; IsCurried = false; ReturnType = LowType.UIntPtr }
            { Name = PrettyNaming.CompileOpName("/"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UIntPtr; LowType.UIntPtr ]; IsCurried = false; ReturnType = LowType.UIntPtr }
            { Name = PrettyNaming.CompileOpName("<"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UIntPtr; LowType.UIntPtr ]; IsCurried = false; ReturnType = LowType.Boolean }
            { Name = PrettyNaming.CompileOpName("<<<"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UIntPtr; LowType.Int32 ]; IsCurried = false; ReturnType = LowType.UIntPtr }
            { Name = PrettyNaming.CompileOpName("<>"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UIntPtr; LowType.UIntPtr ]; IsCurried = false; ReturnType = LowType.Boolean }
            { Name = PrettyNaming.CompileOpName("="); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UIntPtr; LowType.UIntPtr ]; IsCurried = false; ReturnType = LowType.Boolean }
            { Name = PrettyNaming.CompileOpName(">>>"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UIntPtr; LowType.Int32 ]; IsCurried = false; ReturnType = LowType.UIntPtr }
            { Name = PrettyNaming.CompileOpName("^^^"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UIntPtr; LowType.UIntPtr ]; IsCurried = false; ReturnType = LowType.UIntPtr }
            { Name = PrettyNaming.CompileOpName("|||"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UIntPtr; LowType.UIntPtr ]; IsCurried = false; ReturnType = LowType.UIntPtr }
            { Name = PrettyNaming.CompileOpName("~+"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UIntPtr ]; IsCurried = false; ReturnType = LowType.UIntPtr }
            { Name = PrettyNaming.CompileOpName("~~~"); Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UIntPtr ]; IsCurried = false; ReturnType = LowType.UIntPtr }
            { Name = "One"; Kind = MemberKind.Property PropertyKind.Get; GenericParameters = []; Arguments = [  ]; IsCurried = false; ReturnType = LowType.UIntPtr }
            { Name = "Zero"; Kind = MemberKind.Property PropertyKind.Get; GenericParameters = []; Arguments = [  ]; IsCurried = false; ReturnType = LowType.UIntPtr }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UIntPtr ]; IsCurried = false; ReturnType = LowType.Byte }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UIntPtr ]; IsCurried = false; ReturnType = LowType.Char }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UIntPtr ]; IsCurried = false; ReturnType = LowType.Decimal }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UIntPtr ]; IsCurried = false; ReturnType = LowType.Double }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UIntPtr ]; IsCurried = false; ReturnType = LowType.Single }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UIntPtr ]; IsCurried = false; ReturnType = LowType.Int16 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UIntPtr ]; IsCurried = false; ReturnType = LowType.Int32 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UIntPtr ]; IsCurried = false; ReturnType = LowType.Int64 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UIntPtr ]; IsCurried = false; ReturnType = LowType.IntPtr }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UIntPtr ]; IsCurried = false; ReturnType = LowType.SByte }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UIntPtr ]; IsCurried = false; ReturnType = LowType.UInt16 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UIntPtr ]; IsCurried = false; ReturnType = LowType.UInt32 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UIntPtr ]; IsCurried = false; ReturnType = LowType.UInt64 }
            { Name = "op_Explicit"; Kind = MemberKind.Method; GenericParameters = []; Arguments = [ LowType.UIntPtr ]; IsCurried = false; ReturnType = LowType.UIntPtr }
          ]
      })
  ]

let implicitMembers id =
  match Map.tryFind id table with
  | Some im -> (im.InstanceMembers, im.StaticMembers)
  | None -> [], []