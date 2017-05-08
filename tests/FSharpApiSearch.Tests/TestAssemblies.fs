module TestAssemblies

open System
open System.IO
open System.Reflection
open Persimmon
open Persimmon.Syntax.UseTestNameByReflection
open FSharpApiSearch

let assemblyResolver: AssemblyLoader.AssemblyResolver = {
  FSharpCore = Path.Combine(System.Environment.GetFolderPath(Environment.SpecialFolder.ProgramFilesX86), @"Reference Assemblies\Microsoft\FSharp\.NETFramework\v4.0\4.4.1.0\")
  Framework =
    [
      Path.Combine(System.Environment.GetFolderPath(Environment.SpecialFolder.ProgramFilesX86), @"Reference Assemblies\Microsoft\Framework\.NETFramework\v4.6.1\")
      @"C:\Windows\Microsoft.NET\Framework\v4.0.30319\"
    ]
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

let net40AssemblyName = @"Net40Assembly"
let net40AssemblyPath =
  Path.Combine(
    Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)
    , net40AssemblyName + ".dll")

let net20AssemblyName = @"Net20Assembly"
let net20AssemblyPath =
  Path.Combine(
    Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)
    , net20AssemblyName + ".dll")

let valueTupleAssemblyName = @"System.ValueTuple"
let valueTupleAssemblyPath =
  Path.Combine(
    Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)
    , valueTupleAssemblyName + ".dll")

let assemblies = test {
  let assemblies = [ 
    yield Path.GetFullPath(fsharpAssemblyPath)
    yield Path.GetFullPath(net40AssemblyPath)
    yield Path.GetFullPath(net20AssemblyPath)
    yield Path.GetFullPath(csharpAssemblyPath)
    yield Path.GetFullPath(valueTupleAssemblyPath)
    yield! FSharpApiSearch.FSharpApiSearchClient.DefaultReferences

    yield "System.Runtime"
  ]
  return FSharpApiSearch.AssemblyLoader.load assemblyResolver assemblies
}

let apiDictionary = test {
  let! assemblies = assemblies
  return ApiLoader.load assemblies
}

let fsharpAssemblyApi = test {
  let! apiDictionary = apiDictionary
  return apiDictionary |> Array.find (fun x -> x.AssemblyName = fsharpAssemblyName)
}

let net40AssemblyApi = test {
  let! apiDictionary = apiDictionary
  return apiDictionary |> Array.find (fun x -> x.AssemblyName = net40AssemblyName)
}

let net20AssemblyApi = test {
  let! apiDictionary = apiDictionary
  return apiDictionary |> Array.find (fun x -> x.AssemblyName = net20AssemblyName)
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

let systemCoreApi = test {
  let! apiDictionary = apiDictionary
  return apiDictionary |> Array.find (fun x -> x.AssemblyName = "System.Core")
}

let valueTupleApi = test {
  let! apiDictionary = apiDictionary
  return apiDictionary |> Array.find (fun x -> x.AssemblyName = valueTupleAssemblyName)
}