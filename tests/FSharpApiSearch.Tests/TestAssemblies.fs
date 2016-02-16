module TestAssemblies

open System.IO
open System.Reflection
open Persimmon
open Persimmon.Syntax.UseTestNameByReflection
open FSharpApiSearch

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

let assemblies = lazy FSharpApiSearch.AssemblyLoader.load (Path.GetFullPath(fsharpAssemblyPath) :: Path.GetFullPath(csharpAssemblyPath) :: FSharpApiSearch.FSharpApiSearchClient.DefaultReferences)

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