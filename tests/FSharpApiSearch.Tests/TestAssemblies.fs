module TestAssemblies

open System
open System.IO
open System.Reflection
open Persimmon
open Persimmon.Syntax.UseTestNameByReflection
open FSharpApiSearch

let assemblyResolver: AssemblyLoader.AssemblyResolver = {
  FSharpCore = Path.Combine(System.Environment.GetFolderPath(Environment.SpecialFolder.ProgramFilesX86), @"Reference Assemblies\Microsoft\FSharp\.NETFramework\v4.0\4.4.0.0\")
  Framework = Path.Combine(System.Environment.GetFolderPath(Environment.SpecialFolder.ProgramFilesX86), @"Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5\")
  Directories = []
}

let fsharpAssemblyName = @"LoadTestAssembly"
let fsharpAssemblyPath =
  Path.Combine(
    Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)
    , fsharpAssemblyName + ".dll")

let csharpAssemblyName = @"CSharpLoadTestAssembly";
let csharpAssemblyPath =
  Path.Combine(
    Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)
    , csharpAssemblyName + ".dll")

let assemblies = lazy FSharpApiSearch.AssemblyLoader.load assemblyResolver (Path.GetFullPath(fsharpAssemblyPath) :: Path.GetFullPath(csharpAssemblyPath) :: FSharpApiSearch.FSharpApiSearchClient.DefaultReferences)

let fsharpAssemblyApi = test {
  return assemblies.Value |> List.find (fun x -> x.FileName = Some fsharpAssemblyPath ) |> ApiLoader.load
}

let csharpAssemblyApi = test {
  return assemblies.Value |> List.find (fun x -> x.FileName = Some csharpAssemblyPath ) |> ApiLoader.load
}

let fscoreApi = test {
  return assemblies.Value |> List.find (fun x -> x.SimpleName = "FSharp.Core") |> ApiLoader.load
}

let mscorlibApi = test {
  return assemblies.Value |> List.find (fun x -> x.SimpleName = "mscorlib") |> ApiLoader.load
}