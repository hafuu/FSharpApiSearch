module internal FSharpApiSearch.TypeHierarchy

open FSharpApiSearch.MatcherTypes

let transferVariableArgument (inheritArgs: Map<TypeVariable, LowType>) (baseType: LowType): LowType list =
  let rec genericArguments = function
    | Identity _ -> []
    | Generic (_, args) -> args
    | TypeAbbreviation { Original = o } -> genericArguments o
    | _ -> failwith "invalid base type."
  genericArguments baseType
  |> List.map (function
    | Variable (VariableSource.Target, v) -> inheritArgs.[v]
    | a -> a)

let instantiate (t: FullTypeDefinition) (args: LowType list) =
  let id = Identity (FullIdentity t.FullIdentity)
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
      let rec getFullIdentity = function
        | Identity (FullIdentity full) -> full
        | Generic (Identity (FullIdentity full), _) -> full
        | TypeAbbreviation { Original = o } -> getFullIdentity o
        | _ -> failwith "It is not full identity."
      let full = getFullIdentity p
      ctx.ApiDictionaries.[full.AssemblyName].TypeDefinitions.[full]
    yield! getSuperTypes ctx baseTypeDef baseTypeArgs
}

open Printer

let fullTypeDef (ctx: Context) = function
  | FullIdentity i ->
    match ctx.ApiDictionaries.TryFind(i.AssemblyName) with
    | Some apiDict ->
      match apiDict.TypeDefinitions.TryGetValue(i) with
      | true, typeDef -> Array.singleton typeDef
      | false, _ -> failwithf """Type "%s" in "%s" is not found.""" (i.Name.Print()) i.AssemblyName
    | None -> failwithf """Assembly "%s" is not found.""" i.AssemblyName
  | PartialIdentity i -> ctx.QueryTypes.[i]