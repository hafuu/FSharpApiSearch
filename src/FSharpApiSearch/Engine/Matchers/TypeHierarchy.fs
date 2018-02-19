module internal FSharpApiSearch.TypeHierarchy

open FSharpApiSearch.EngineTypes

let transferVariableArgument (inheritArgs: Map<TypeVariable, LowType>) (baseType: LowType): LowType list =
  let rec genericArguments = function
    | Identifier _ -> []
    | Generic (_, args, _) -> args
    | TypeAbbreviation ({ Original = o }, _) -> genericArguments o
    | _ -> failwith "invalid base type."
  genericArguments baseType
  |> List.map (function
    | Variable (VariableSource.Target, v, _) -> inheritArgs.[v]
    | a -> a)

let instantiate (t: FullTypeDefinition) (args: LowType list) =
  let id = Identifier.create (ConcreteType t.ConcreteType)
  match args with
  | [] -> id
  | _ -> Generic.create (id, args)

let rec getBaseTypes (ctx: Context) (t: FullTypeDefinition) (args: LowType list): LowType seq = seq {
  let argPair = Map.ofList2 t.GenericParameters args

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
      let rec getConcreteType = function
        | Identifier (ConcreteType full, _) -> full
        | Generic (Identifier (ConcreteType full, _), _, _) -> full
        | TypeAbbreviation ({ Original = o }, _) -> getConcreteType o
        | _ -> failwith "It is not actual type."
      let full = getConcreteType p
      ctx.ApiDictionaries.[full.AssemblyName].TypeDefinitions.[full]
    yield! getBaseTypes ctx baseTypeDef baseTypeArgs
}

open StringPrinter

let fullTypeDef (ctx: Context) = function
  | ConcreteType i ->
    match ctx.ApiDictionaries.TryGetValue(i.AssemblyName) with
    | true, apiDict ->
      match apiDict.TypeDefinitions.TryGetValue(i) with
      | true, typeDef -> Array.singleton typeDef
      | false, _ -> failwithf """Type "%s" in "%s" is not found.""" (i.Name.Print()) i.AssemblyName
    | false, _ -> failwithf """Assembly "%s" is not found.""" i.AssemblyName
  | UserInputType i -> ctx.QueryTypes.[i]