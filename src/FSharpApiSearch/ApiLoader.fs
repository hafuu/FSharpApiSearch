module FSharpApiSearch.ApiLoader

open Microsoft.FSharp.Compiler.SourceCodeServices
open System.IO
open System.Reflection

let syslib name =
  Path.Combine(
    System.Environment.GetFolderPath(System.Environment.SpecialFolder.ProgramFilesX86)
    , @"Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5\"
    , name)
let fscore4400 =
  Path.Combine(
    System.Environment.GetFolderPath(System.Environment.SpecialFolder.ProgramFilesX86)
    , @"Reference Assemblies\Microsoft\FSharp\.NETFramework\v4.0\4.4.0.0\FSharp.Core.dll")

let resolvePath (assemblyName: string) =
  let assemblyName = if assemblyName.EndsWith(".dll") = false then assemblyName + ".dll" else assemblyName

  if File.Exists(syslib assemblyName) then
    Some (syslib assemblyName)
  elif assemblyName = "FSharp.Core.dll" then
    Some fscore4400
  elif File.Exists(assemblyName) then
    Some (assemblyName)
  else
    None

let ignoreFSharpCompilerServiceError() =
  typeof<FSharpChecker>.Assembly.GetType("Microsoft.FSharp.Compiler.AbstractIL.Diagnostics")
  |> Option.ofObj
  |> Option.bind (fun diagMod -> diagMod.GetMember("diagnosticsLog", BindingFlags.NonPublic ||| BindingFlags.Static) |> Array.tryHead)
  |> Option.bind (tryUnbox<PropertyInfo>)
  |> Option.bind (fun x -> x.GetValue(null) |> Option.ofObj)
  |> Option.bind (tryUnbox<ref<Option<System.IO.TextWriter>>>)
  |> Option.iter (fun x -> x := None)

let loadAssembly references =
  ignoreFSharpCompilerServiceError()

  let checker = FSharpChecker.Create()
  let base1 = Path.GetTempFileName()
  let fileName1 = Path.ChangeExtension(base1, ".fs")
  let projFileName = Path.ChangeExtension(base1, ".fsproj")
  let dllName = Path.ChangeExtension(base1, ".dll")
  let options =
    checker.GetProjectOptionsFromCommandLineArgs
      (projFileName,
        [|  yield "--simpleresolution" 
            yield "--noframework" 
            yield "--debug:full" 
            yield "--define:DEBUG" 
            yield "--optimize-" 
            yield "--out:" + dllName
            yield "--warn:3" 
            yield "--fullpaths" 
            yield "--flaterrors" 
            yield "--target:library" 
            yield fileName1
            for r in references |> Seq.choose resolvePath do
              yield "-r:" + r |]
      )
  let refAssemblies =
    let x = checker.ParseAndCheckProject(options) |> Async.RunSynchronously
    x.ProjectContext.GetReferencedAssemblies()
  refAssemblies

let rec toSignature (t: FSharpType) =
  if Hack.isMeasure t then
    None
  elif t.IsFunctionType then
    option {
      let! xs = toFlatArrow t
      return Arrow xs
    }
  elif t.IsTupleType then
    option {
      let! xs = listSignature t.GenericArguments
      return Signature.tuple xs
    }
  elif t.IsGenericParameter then
    Some (Variable (Target, t.GenericParameter.Name))
  elif t.HasTypeDefinition then
    let signature =
      match Hack.genericArguments t with
      | [] -> Some (identity t.TypeDefinition)
      | xs -> 
        option {
          let! xs = listSignature xs
          return Generic (identity t.TypeDefinition, xs)
        }
    option {
      let! signature = signature
      if Hack.isAbbreviation t then
        let! original = abbreviationRoot t
        return TypeAbbreviation { Abbreviation = signature; Original = original }
      else
        return signature
    }
  else
    None
and abbreviationRoot (t: FSharpType) =
  if t.IsAbbreviation then
    abbreviationRoot t.AbbreviatedType
  elif Hack.isFloat t then
    Some (Identity (Signature.fullName "System.Double"))
  elif t.IsFunctionType then
    None
  else
    toSignature t
and toFlatArrow (t: FSharpType): Signature list option =
  match Seq.toList t.GenericArguments with
  | [ x; y ] when y.IsFunctionType ->
    option {
      let! xSig = toSignature x
      let! ySigs = toFlatArrow y
      return xSig :: ySigs
    }
  | [ x; y ] ->
    option {
      let! xSig = toSignature x
      let! ySig = toSignature y
      return [ xSig; ySig ]
    }
  | _ -> None
and listSignature (ts: FSharpType seq) =
  let f (t: FSharpType) (acc: Signature list option) =
    option {
      let! acc = acc
      let! signature = toSignature t
      return signature :: acc
    }
  Seq.foldBack f ts (Some [])
and identity (e: FSharpEntity) =
  if e.IsArrayType then
    Identity (Signature.fullName e.DisplayName)
  else
    Identity (Signature.fullName (e.AccessPath + "." + e.DisplayName))
and fsharpEntityToSignature (x: FSharpEntity) =
  let identity = identity x
  let args = x.GenericParameters |> Seq.map (fun p -> Variable (Source.Target, p.DisplayName)) |> Seq.toList
  match args with
  | [] -> identity
  | xs -> Generic (identity, xs)

let isStaticMember (x: FSharpMemberOrFunctionOrValue) = not x.IsInstanceMember
let isMethod (x: FSharpMemberOrFunctionOrValue) = x.FullType.IsFunctionType && not x.IsPropertyGetterMethod && not x.IsPropertySetterMethod

let private methodSignature (t: FSharpType) =
  match List.ofSeq t.GenericArguments with
  | [ arguments; returnType ] ->
    option {
      let! arguments =
        if arguments.IsTupleType then
          listSignature arguments.GenericArguments
        else
          toSignature arguments |> Option.map List.singleton
      let! returnType = toSignature returnType
      return (arguments, returnType)
    }
  | _ -> None

let staticMethodSignature (t: FSharpType) =
  option {
    let! args, ret = methodSignature t
    return StaticMethod { Arguments = args; ReturnType = ret }
  }

let propertySignature (x: FSharpMemberOrFunctionOrValue) =
  option {
    let! args =
      if Seq.length x.CurriedParameterGroups = 1 then
        listSignature (x.CurriedParameterGroups |> Seq.head |> Seq.map (fun p -> p.Type))
      else
        None
    let! propertyType = toSignature x.ReturnParameter.Type
    match args with
    | [] -> return (None, propertyType)
    | [ x ] -> return (Some x, propertyType)
    | _ -> return! None
  }

let staticPropertySignature (x: FSharpMemberOrFunctionOrValue) =
  option {
    let! arg, propertyType = propertySignature x
    match arg with
    | Some arg -> return Arrow [ arg; propertyType ]
    | None -> return propertyType
  }

let instancePropertySignature (declaringType: FSharpEntity) (x: FSharpMemberOrFunctionOrValue) =
  option {
    let! arg, propertyType = propertySignature x
    let arg = arg |> Option.toList
    return InstanceMember { Source = Source.Target; Receiver = fsharpEntityToSignature declaringType; Arguments = arg; ReturnType = propertyType }
  }

let instanceMethodSignature (declaringType: FSharpEntity) (t: FSharpType) =
  option {
    let! args, ret = methodSignature t
    return InstanceMember { Source = Source.Target; Receiver = fsharpEntityToSignature declaringType; Arguments = args; ReturnType = ret }
  }
module CSharp =
  let constructorName = ".ctor"
  let isConstructor (x: FSharpMemberOrFunctionOrValue) = x.DisplayName = constructorName
  let constructorSignature (declaringType: FSharpEntity) (t: FSharpType) =
    option {
      let! args, _ = methodSignature t
      return StaticMethod { Arguments = args; ReturnType = fsharpEntityToSignature declaringType }
    }
  
let toFSharpApi (x: FSharpMemberOrFunctionOrValue) =
  option {
    let! signature = toSignature x.FullType
    return { Name = x.FullName; Signature = signature }
  }

let toTypeMemberApi (declaringType: FSharpEntity) (x: FSharpMemberOrFunctionOrValue) =
  if CSharp.isConstructor x then
    option {
      let! signature = CSharp.constructorSignature declaringType x.FullType
      return { Name = declaringType.FullName; Signature = signature }
    }
  elif isStaticMember x && isMethod x then
    option {
      let! signature = staticMethodSignature x.FullType
      return { Name = x.FullName; Signature = signature }
    }
  elif x.IsInstanceMember && isMethod x then
    option {
      let! signature = instanceMethodSignature declaringType x.FullType
      return { Name = x.FullName; Signature = signature}
    }
  elif isStaticMember x && x.IsProperty then
    option {
      let! signature = staticPropertySignature x
      return { Name = x.FullName; Signature = signature }
    }
  elif x.IsInstanceMember && x.IsProperty then
    option {
      let! signature = instancePropertySignature declaringType x
      return { Name = x.FullName; Signature = signature }
    }
  else
    None

let toFieldApi (declaringType: FSharpEntity) (field: FSharpField) =
  option {
    if field.Name = Hack.enumValue then
      return! None
    else
      let! fieldSignature = toSignature field.FieldType
      let signature = InstanceMember { Source = Source.Target; Receiver = fsharpEntityToSignature declaringType; Arguments = []; ReturnType = fieldSignature }
      let name = sprintf "%s.%s.%s" declaringType.AccessPath declaringType.DisplayName field.Name
      return { Name = name; Signature = signature }
  }

let resolveConflictGenericArgumnet replacementVariables (m: FSharpMemberOrFunctionOrValue) =
  m.GenericParameters
  |> Seq.choose (fun p ->
    let name = p.Name.TrimStart(''')
    let isConflict = replacementVariables |> List.exists (function Signature.Patterns.AnyVariable (_, n) -> n = name | _ -> false)
    if isConflict then Some name else None
  )
  |> Seq.map (fun confrictName -> (confrictName, Variable (Source.Target, confrictName + "1")))
  |> Seq.toList

let genericParametersAndArguments (t: FSharpType) =
  Seq.zip t.TypeDefinition.GenericParameters t.GenericArguments
  |> Seq.choose (fun (parameter, arg) -> option {
    let! s = toSignature arg
    return parameter.Name.TrimStart('''), s
  })
  |> Seq.toList

let updateReceiver receiverName receiver = function
  | { Signature = InstanceMember m } as api ->
    let newName =
      let xs = api.Name.Split('.')
      xs.[Array.length xs - 2] <- receiverName
      String.concat "." xs
    Some { api with Name = newName; Signature = InstanceMember { m with Receiver = receiver } }
  | _ -> None

let rec collectApi' (e: FSharpEntity): Api seq =
  seq {
    if e.IsNamespace then
      yield! collectFromNestedEntities e
    if e.IsFSharpModule then
      yield! collectFromModule e
    if e.IsClass || e.IsValueType || e.IsFSharpRecord || e.IsFSharpUnion then
      yield! collectFromType e
    if e.IsInterface then
      yield! collectFromInterface [] e
  }
and collectFromModule (e: FSharpEntity): Api seq =
  seq {
    yield! e.MembersFunctionsAndValues
            |> Seq.filter (fun x -> x.Accessibility.IsPublic)
            |> Seq.choose toFSharpApi
    yield! collectFromNestedEntities e
  }
and collectFromType (e: FSharpEntity): Api seq =
  seq {
    yield! e.MembersFunctionsAndValues
            |> Seq.filter (fun x -> x.Accessibility.IsPublic)
            |> Seq.choose (toTypeMemberApi e)
    yield! e.FSharpFields
            |> Seq.filter (fun x -> x.Accessibility.IsPublic)
            |> Seq.choose (toFieldApi e)
    yield! collectFromNestedEntities e
  }
and collectFromInterface inheritArgs (e: FSharpEntity): Api seq =
  seq {
    let replacementVariables = inheritArgs |> List.map snd |> List.collect Signature.collectVariables |> List.distinct
    yield! e.MembersFunctionsAndValues
            |> Seq.filter (fun x -> x.Accessibility.IsPublic)
            |> Seq.choose (fun m -> option {
              let! api = toTypeMemberApi e m
              let inheritArgs = List.append (resolveConflictGenericArgumnet replacementVariables m) inheritArgs
              return (api, inheritArgs)
            })
            |> Seq.map (fun (api, variableReplacements) ->
              List.fold (fun api (variableName, replacement) -> { api with Signature = Signature.replaceVariable variableName replacement api.Signature }) api variableReplacements
            )

    let receiver = fsharpEntityToSignature e
    for parentInterface in e.DeclaredInterfaces do
      let inheritArgs = genericParametersAndArguments parentInterface
      yield! collectFromInterface inheritArgs parentInterface.TypeDefinition
              |> Seq.choose (updateReceiver e.DisplayName receiver)
  }
and collectFromNestedEntities (e: FSharpEntity): Api seq = seq { for ne in e.NestedEntities do yield! collectApi' ne }
  
let rec collectAbbreviations' (e: FSharpEntity) = seq {
  if not e.IsMeasure && e.Accessibility.IsPublic && e.IsFSharpAbbreviation then
    let abbreviation = option {
      let a = fsharpEntityToSignature e
      let! o = abbreviationRoot e.AbbreviatedType
      return { Abbreviation = a; Original = o }
    }
    match abbreviation with
    | Some a -> yield a
    | None -> ()
  yield! e.NestedEntities |> Seq.collect collectAbbreviations'
}

let load (assembly: FSharpAssembly): ApiDictionary =
  let api =
    assembly.Contents.Entities
    |> Seq.collect collectApi'
    |> Seq.toList
  let typeAbbreviations =
    assembly.Contents.Entities
    |> Seq.collect collectAbbreviations'
    |> Seq.toList
  { AssemblyName = assembly.SimpleName; Api = api; TypeAbbreviations = typeAbbreviations }