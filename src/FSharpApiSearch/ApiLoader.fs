module FSharpApiSearch.ApiLoader

open Microsoft.FSharp.Compiler.SourceCodeServices
open System.IO
open FSharpApiSearch.Types

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
                fscore4400 ]
            for r in defaultReferences do 
              yield "-r:" + r
            for a in references do
              yield "-r:" + a |]
      )
  let refAssemblies =
    let x = checker.ParseAndCheckProject(options) |> Async.RunSynchronously
    x.ProjectContext.GetReferencedAssemblies()
  refAssemblies

let rec toSignature (t: FSharpType) =
  if t.IsFunctionType then
    Arrow (toFlatArrow t [])
  elif t.IsTupleType then
    Tuple (t.GenericArguments |> Seq.map toSignature |> Seq.toList)
  elif t.IsGenericParameter then
    Variable (Target, t.GenericParameter.Name)
  elif t.HasTypeDefinition then
    let name = t.TypeDefinition.DisplayName
    match List.ofSeq t.GenericArguments with
    | [] -> Identity name
    | xs -> Generic (Identity name, List.map toSignature xs)
  else
    Unknown
and toFlatArrow (t: FSharpType) xs =
  match Seq.toList t.GenericArguments with
  | [ x; y ] when y.IsFunctionType -> toSignature x :: toFlatArrow y xs
  | [ x; y ] -> toSignature x :: toSignature y :: xs
  | _ -> Unknown :: xs

    
let toApi (x: FSharpMemberOrFunctionOrValue) = { Name = x.FullName; Signature = toSignature x.FullType; }

let rec collectFromModule (e: FSharpEntity): Api seq = seq {
  if e.IsFSharpModule then
    yield! e.MembersFunctionsAndValues
           |> Seq.filter (fun x -> x.Accessibility.IsPublic)
           |> Seq.map toApi
           |> Seq.filter (fun x -> x.Signature <> Unknown)
    yield! e.NestedEntities |> Seq.collect collectFromModule
}
  
let collectApi (assembly: FSharpAssembly): Api seq =
  assembly.Contents.Entities
  |> Seq.collect collectFromModule
  |> Seq.cache