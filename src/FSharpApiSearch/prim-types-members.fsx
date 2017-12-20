open System
open System.IO
open System.Text.RegularExpressions

let primitiveTypes =
  Map.ofList [
    "bool",       "Boolean"
    "byte",       "Byte"
    "char",       "Char"
    "decimal",    "Decimal"
    "float",      "Double"
    "float32",    "Single"
    "int16",      "Int16"
    "int",        "Int32"
    "int32",      "Int32"
    "int64",      "Int64"
    "nativeint",  "IntPtr"
    "sbyte",      "SByte"
    "string",     "String"
    "uint16",     "UInt16"
    "uint32",     "UInt32"
    "uint64",     "UInt64"
    "unativeint", "UIntPtr"
  ]

let file =
  File.ReadAllLines(fsi.CommandLineArgs.[1])

let nameAndSig (line: string) =
  let m = Regex.Match(line, @"\(static member ([^:]+):\s+([^)]+)\)")
  let name = m.Groups.[1].Value.Trim()
  let sig' = m.Groups.[2].Value.Trim()
  name, sig'

let readType (line: string) =
  let m = Regex.Match(line, @"when \^\w+ : (\w+) ")
  if m.Success then
    Some (m.Groups.[1].Value.Trim())
  else
    None

let rec back index =
  if file.[index].Contains("let") then
    index + 1
  else
    back (index - 1)

let rec forward index =
  if file.[index].Contains("let") then
    index - 1
  else
    forward (index + 1)

file
|> Array.indexed
|> Array.filter (fun (_, line) -> Regex.IsMatch(line, @"\(static member") && (Regex.IsMatch(line, @"^\s+let") = false))
|> Array.collect (fun (i, line) ->
  let types =
    file.[(back i)..(forward i)]
    |> Array.choose readType

  let name, sig' = nameAndSig line
  types |> Array.map (fun t -> t, name, sig')
)
|> Array.groupBy (fun (t, _, _) -> t)
|> Array.sort
|> Array.iter (fun (t, members) ->
  let getName key = Map.find key primitiveTypes
  let typeName = getName t
  let members =
    members
    |> Array.distinct
    |> Array.sort
    |> Array.map (fun (_, memberName, sig') ->
      let memberName =
        if memberName.StartsWith("(") then
          sprintf "PrettyNaming.CompileOpName(\"%s\")" (memberName.Replace("(", "").Replace(")", ""))
        else
          sprintf "\"%s\"" memberName
      let parameters, ret =
        let sig' = Regex.Replace(sig', @"['\^]\w+", t)
        let xs = sig'.Split([| "->" |], StringSplitOptions.None) |> Array.map (fun x -> x.Trim())
        let args = Array.truncate (Array.length xs - 1) xs |> Array.toList
        let ret = Array.last xs
        (args, getName ret)
      let kind =
        match parameters with
        | [] -> "MemberKind.Property PropertyKind.Get"
        | _ -> "MemberKind.Method"
      let parameters =
        parameters
        |> List.collect (fun x -> x.Split('*') |> Array.toList)
        |> List.map (fun x -> x.Trim())
        |> List.map (fun x -> sprintf "Parameter.%s" (getName x))
        |> function
          | [] -> ""
          | xs -> String.concat "; " xs |> sprintf "[ %s ]"
        |> sprintf "[ %s ]"
      sprintf """            { Name = %s; Kind = %s; GenericParameters = []; Parameters = %s; ReturnParameter = Parameter.%s }""" memberName kind parameters ret
    )
    |> String.concat "\r\n"
  printfn "    (TypeInfo.%s, " typeName
  printfn """      {
        InstanceMembers = []
        StaticMembers =
          [
%s
          ]
      })""" members
)

Console.WriteLine("-----------------")

primitiveTypes
|> Map.toList
|> List.map snd
|> List.distinct
|> List.iter (fun t -> printfn "let %s = ofDotNetType typeof<%s>" t t)