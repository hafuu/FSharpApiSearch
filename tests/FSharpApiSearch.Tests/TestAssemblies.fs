module TestAssemblies

open System
open System.IO
open System.Reflection
open Persimmon
open Persimmon.Syntax.UseTestNameByReflection
open FSharpApiSearch

let assemblyResolver: AssemblyLoader.AssemblyResolver = {
  FSharpCore = Path.Combine(System.Environment.GetFolderPath(Environment.SpecialFolder.ProgramFiles), @"dotnet\sdk\6.0.102\FSharp\")
  Framework =
    [
      Path.Combine(System.Environment.GetFolderPath(Environment.SpecialFolder.ProgramFiles), @"dotnet\shared\Microsoft.NETCore.App\6.0.2\")
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

let valueTupleAssemblyName = @"System.ValueTuple"
let valueTupleAssemblyPath =
  Path.Combine(
    Path.GetDirectoryName(Assembly.Load("System.ValueTuple").Location)
    , valueTupleAssemblyName + ".dll")

let fparsecAssemblyName = @"FParsec";
let fparsecAssenmblyPath = 
  Path.Combine(
    Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)
    , fparsecAssemblyName + ".dll")

let fparsecCSAssemblyName = @"FParsecCS";
let fparsecCSAssenmblyPath = 
  Path.Combine(
    Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)
    , fparsecCSAssemblyName + ".dll")

let assemblies = test {
  let assemblies = [ 
    yield Path.GetFullPath(fsharpAssemblyPath)
    yield Path.GetFullPath(csharpAssemblyPath)
    yield Path.GetFullPath(valueTupleAssemblyPath)
    yield Path.GetFullPath(fparsecAssenmblyPath)
    yield Path.GetFullPath(fparsecCSAssenmblyPath)
  ]
  return FSharpApiSearch.AssemblyLoader.load (assemblyResolver.ResolveAll(assemblies))
}

let database = test {
  let! assemblies = assemblies
  return ApiLoader.load assemblies
}

let fsharpAssemblyApi = test {
  let! apiDictionary = database
  return apiDictionary |> Array.find (fun x -> x.AssemblyName = fsharpAssemblyName)
}

let csharpAssemblyApi = test {
  let! apiDictionary = database
  return apiDictionary |> Array.find (fun x -> x.AssemblyName = csharpAssemblyName)
}

let fscoreApi = test {
  let! apiDictionary = database
  return apiDictionary |> Array.find (fun x -> x.AssemblyName = "FSharp.Core")
}

let mscorlibApi = test {
  let! apiDictionary = database
  return apiDictionary |> Array.find (fun x -> x.AssemblyName = "mscorlib")
}

let systemCoreApi = test {
  let! apiDictionary = database
  return apiDictionary |> Array.find (fun x -> x.AssemblyName = "System.Core")
}

let valueTupleApi = test {
  let! apiDictionary = database
  return apiDictionary |> Array.find (fun x -> x.AssemblyName = valueTupleAssemblyName)
}

let fparsecApi = test {
  let! apiDictionary = database
  return apiDictionary |> Array.find (fun x -> x.AssemblyName = fparsecAssemblyName)
}