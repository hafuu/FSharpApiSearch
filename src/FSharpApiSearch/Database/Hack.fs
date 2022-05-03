module internal FSharpApiSearch.Hack

open FSharp.Compiler.Symbols

let genericArguments (t: FSharpType) =
  let args = t.GenericArguments |> Seq.toList
  args

let isAbbreviation (t: FSharpType) = t.IsAbbreviation

let isMeasure (t: FSharpType) = t.HasTypeDefinition && t.TypeDefinition.IsMeasure // HACK: The herz measure is infinit loop of type abbreviation. 

let isUnitOnlyParameter (x: FSharpMemberOrFunctionOrValue) = // CurriedParameterGroups of the parameter is unit only (`member _.X : unit -> int`) is [ [] ]
  Seq.length x.CurriedParameterGroups = 1 && Seq.length (Seq.item 0 x.CurriedParameterGroups) = 0

let isTupleType (t: FSharpType) =
  if t.HasTypeDefinition then
    let td = t.TypeDefinition
    td.TryFullName |> Option.exists (fun name -> name.StartsWith("System.Tuple`") || name.StartsWith("System.ValueTuple`"))
  else
    t.IsTupleType

let isStructTupleType (t: FSharpType) =
  if t.HasTypeDefinition then
    let td = t.TypeDefinition
    td.TryFullName |> Option.exists (fun name -> name.StartsWith("System.ValueTuple`"))
  else
    t.IsStructTupleType