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

let assemblies = test {
  return FSharpApiSearch.AssemblyLoader.load assemblyResolver (Path.GetFullPath(fsharpAssemblyPath) :: Path.GetFullPath(csharpAssemblyPath) :: FSharpApiSearch.FSharpApiSearchClient.DefaultReferences)
}

let apiDictionary = test {
  let! assemblies = assemblies
  return ApiLoader.load assemblies
}

let fsharpAssemblyApi = test {
  let! apiDictionary = apiDictionary
  return apiDictionary |> Array.find (fun x -> x.AssemblyName = fsharpAssemblyName)
}

let csharpAssemblyApi = test {
  let! apiDictionary = apiDictionary
  return apiDictionary |> Array.find (fun x -> x.AssemblyName = csharpAssemblyName)
}

let fscoreApi = test {
  let! apiDictionary = apiDictionary
  return apiDictionary |> Array.find (fun x -> x.AssemblyName = "FSharp.Core")
}

let mscorlibApi = test {
  let! apiDictionary = apiDictionary
  return apiDictionary |> Array.find (fun x -> x.AssemblyName = "mscorlib")
}