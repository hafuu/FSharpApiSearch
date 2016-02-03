module FSharpApiSearch.ApiLoader

open Microsoft.FSharp.Compiler.SourceCodeServices
open System.IO

let toAssemblyName assemblyPath = Path.GetFileName(assemblyPath).Replace(".dll", "")

let loadAssembly references =
  let checker = FSharpChecker.Create()
  let base1 = Path.GetTempFileName()
  let fileName1 = Path.ChangeExtension(base1, ".fs")
  let projFileName = Path.ChangeExtension(base1, ".fsproj")
  let dllName = Path.ChangeExtension(base1, ".dll")
  let options =
    let syslib name =
      System.Environment.GetFolderPath(System.Environment.SpecialFolder.ProgramFilesX86)
      + @"\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5\"
      + (name + ".dll")
    let fscore4400 =
      System.Environment.GetFolderPath(System.Environment.SpecialFolder.ProgramFilesX86)
      + @"\Reference Assemblies\Microsoft\FSharp\.NETFramework\v4.0\4.4.0.0\FSharp.Core.dll"
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
            let defaultReferences =
              [ syslib "mscorlib" 
                syslib "System"
                syslib "System.Core"
                syslib "System.Xml"
                syslib "System.Configuration"
                fscore4400 ]
            for r in defaultReferences do 
              yield "-r:" + r
            for a in references do
              yield "-r:" + a |]
      )
  let targetAssemblies = [
    yield "mscorlib"
    yield "System"
    yield "System.Core"
    yield "FSharp.Core"
    yield! references |> Seq.map toAssemblyName
  ]

  let refAssemblies =
    let x = checker.ParseAndCheckProject(options) |> Async.RunSynchronously
    x.ProjectContext.GetReferencedAssemblies()
  refAssemblies
  |> List.filter (fun x -> List.exists ((=)x.SimpleName) targetAssemblies)

let genericArguments (t: FSharpType) =
  if t.HasTypeDefinition && t.TypeDefinition.DisplayName = "float" then
    t.GenericArguments |> Seq.filter (fun x -> not x.HasTypeDefinition || (x.HasTypeDefinition && x.TypeDefinition.DisplayName <> "MeasureOne"))
  else
    t.GenericArguments :> _ seq

let rec toSignature (t: FSharpType) =
  if t.IsFunctionType then
    Arrow (toFlatArrow t [])
  elif t.IsTupleType then
    Tuple (t.GenericArguments |> Seq.map toSignature |> Seq.toList)
  elif t.IsGenericParameter then
    Variable (Target, t.GenericParameter.Name)
  elif t.HasTypeDefinition then
    let args = genericArguments t
    match List.ofSeq args with
    | [] -> identity t.TypeDefinition
    | xs -> Generic (identity t.TypeDefinition, List.map toSignature xs)
  else
    Unknown
and toFlatArrow (t: FSharpType) xs =
  match Seq.toList t.GenericArguments with
  | [ x; y ] when y.IsFunctionType -> toSignature x :: toFlatArrow y xs
  | [ x; y ] -> toSignature x :: toSignature y :: xs
  | _ -> Unknown :: xs
and identity (e: FSharpEntity) = Identity e.DisplayName

let isStaticMember (x: FSharpMemberOrFunctionOrValue) = not x.IsInstanceMember
let isMethod (x: FSharpMemberOrFunctionOrValue) = x.FullType.IsFunctionType && not x.IsPropertyGetterMethod && not x.IsPropertySetterMethod

let private memberSignature (t: FSharpType) =
  match List.ofSeq t.GenericArguments with
  | [ arguments; returnType ] ->
    let arguments =
      if arguments.IsTupleType then
        arguments.GenericArguments |> Seq.map toSignature |> Seq.toList
      else
        [ toSignature arguments ]
    let returnType = toSignature returnType
    Some (arguments, returnType)
  | _ -> None

let staticMethodSignature (t: FSharpType) =
  memberSignature t |> Option.map (fun (args, ret) -> StaticMethod { Arguments = args; ReturnType = ret })

let instanceMemberSignature (declaringType: FSharpEntity) (t: FSharpType) =
  memberSignature t |> Option.map (fun (args, ret) -> InstanceMember { Source = Source.Target; Receiver = identity declaringType; Arguments = args; ReturnType = ret })

module CSharp =
  let constructorName = ".ctor"
  let isConstructor (x: FSharpMemberOrFunctionOrValue) = x.DisplayName = constructorName
  let constructorSignature (declaringType: FSharpEntity) (t: FSharpType) =
    memberSignature t |> Option.map (fun (args, _) -> StaticMethod { Arguments = args; ReturnType = identity declaringType })
  
let toFSharpApi (x: FSharpMemberOrFunctionOrValue) = { Name = x.FullName; Signature = toSignature x.FullType; }

let toTypeMemberApi (declaringType: FSharpEntity) (x: FSharpMemberOrFunctionOrValue) =
  if CSharp.isConstructor x then
    CSharp.constructorSignature declaringType x.FullType |> Option.map (fun signature -> { Name = declaringType.FullName; Signature = signature })
  elif isStaticMember x && isMethod x then
    staticMethodSignature x.FullType |> Option.map (fun signature -> { Name = x.FullName; Signature = signature })
  elif x.IsInstanceMember && isMethod x then
    instanceMemberSignature declaringType x.FullType |> Option.map (fun signature -> { Name = x.FullName; Signature = signature})
  else
    None

let rec collectApi' (e: FSharpEntity): Api seq =
  seq {
    if e.IsNamespace then
      yield! collectFromNestedEntities e
    if e.IsFSharpModule then
      yield! collectFromModule e
    if e.IsClass || e.IsValueType || e.IsFSharpRecord || e.IsFSharpUnion then
      yield! collectFromType e
  }
and collectFromModule (e: FSharpEntity): Api seq =
  seq {
    yield! e.MembersFunctionsAndValues
            |> Seq.filter (fun x -> x.Accessibility.IsPublic)
            |> Seq.map toFSharpApi
            |> Seq.filter (fun x -> x.Signature <> Unknown)
    yield! collectFromNestedEntities e
  }
and collectFromType (e: FSharpEntity): Api seq =
  seq {
    yield! e.MembersFunctionsAndValues
            |> Seq.filter (fun x -> x.Accessibility.IsPublic)
            |> Seq.choose (toTypeMemberApi e)
            |> Seq.filter (fun x -> x.Signature <> Unknown)
    yield! collectFromNestedEntities e
  }
and collectFromNestedEntities (e: FSharpEntity): Api seq = seq { for ne in e.NestedEntities do yield! collectApi' ne }
  
let collectApi (assembly: FSharpAssembly): Api seq =
  assembly.Contents.Entities
  |> Seq.collect collectApi'