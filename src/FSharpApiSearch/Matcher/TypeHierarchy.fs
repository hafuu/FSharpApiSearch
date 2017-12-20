module internal FSharpApiSearch.TypeHierarchy

open FSharpApiSearch.MatcherTypes

let transferVariableArgument (inheritArgs: Map<TypeVariable, LowType>) (baseType: LowType): LowType list =
  let rec genericArguments = function
    | Type _ -> []
    | Generic (_, args) -> args
    | TypeAbbreviation { Original = o } -> genericArguments o
    | _ -> failwith "invalid base type."
  genericArguments baseType
  |> List.map (function
    | Variable (VariableSource.Target, v) -> inheritArgs.[v]
    | a -> a)

let instantiate (t: FullTypeDefinition) (args: LowType list) =
  let id = Type (ActualType t.ActualType)
  match args with
  | [] -> id
  | _ -> Generic (id, args)

let rec getSuperTypes (ctx: Context) (t: FullTypeDefinition) (args: LowType list): LowType seq = seq {
  let argPair = List.zip t.GenericParameters args |> Map.ofList

  let thisType = instantiate t args
  yield thisType 

  let parents = seq {
    yield! t.AllInterfaces

    match t.BaseType with
    | Some baseType -> yield baseType
    | None -> ()
  }

  for p in parents do
    let baseTypeArgs = transferVariableArgument argPair p
    let baseTypeDef =
      let rec getActualType = function
        | Type (ActualType full) -> full
        | Generic (Type (ActualType full), _) -> full
        | TypeAbbreviation { Original = o } -> getActualType o
        | _ -> failwith "It is not actual type."
      let full = getActualType p
      ctx.ApiDictionaries.[full.AssemblyName].TypeDefinitions.[full]
    yield! getSuperTypes ctx baseTypeDef baseTypeArgs
}

open Printer

let fullTypeDef (ctx: Context) = function
  | ActualType i ->
    match ctx.ApiDictionaries.TryFind(i.AssemblyName) with
    | Some apiDict ->
      match apiDict.TypeDefinitions.TryGetValue(i) with
      | true, typeDef -> Array.singleton typeDef
      | false, _ -> failwithf """Type "%s" in "%s" is not found.""" (i.Name.Print()) i.AssemblyName
    | None -> failwithf """Assembly "%s" is not found.""" i.AssemblyName
  | UserInputType i -> ctx.QueryTypes.[i]