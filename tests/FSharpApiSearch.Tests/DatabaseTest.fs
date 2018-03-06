module DatabaseTest

open Persimmon
open Persimmon.Syntax.UseTestNameByReflection
open FSharpApiSearch

[<Category("compact")>]
module CompactTest =
  open TestHelper.DSL

  let testRemoveUnusedAssembly =
    let t1 = createType "t1" [] |> updateAssembly "lib1"
    let t2 = createType "t2" [] |> updateAssembly "lib2"
    let t3 = createType "t3" [] |> updateAssembly "lib2_1"
    let t4 = createType "t4" [] |> updateAssembly "lib2_1_1"
    let lib1 = {
      AssemblyName = "lib1"
      Api =
        [|
          { Name = ApiName.ofString "test"; Signature = moduleValue t1; TypeConstraints = []; Document = None }
        |]
      TypeDefinitions = IDictionary.empty
      TypeAbbreviations = [||]
    }
    let lib2 = {
      AssemblyName = "lib2"
      Api =
        [|
          { Name = ApiName.ofString "test"; Signature = moduleFunction' [ [ ptype t2 ]; [ ptype t3 ] ]; TypeConstraints = []; Document = None }
        |]
      TypeDefinitions = IDictionary.empty
      TypeAbbreviations = [||]
    }
    let lib2_1 = {
      AssemblyName = "lib2_1"
      Api =
        [|
          { Name = ApiName.ofString "test"; Signature = moduleFunction' [ [ ptype t3 ]; [ ptype t4 ] ]; TypeConstraints = []; Document = None }
        |]
      TypeDefinitions = IDictionary.empty
      TypeAbbreviations = [||]
    }
    let lib2_1_1 = {
      AssemblyName = "lib2_1_1"
      Api = [||]
      TypeDefinitions = IDictionary.empty
      TypeAbbreviations = [||]
    }
    let lib3 = {
      AssemblyName = "lib3"
      Api = [||]
      TypeDefinitions = IDictionary.empty
      TypeAbbreviations = [||]
    }
    let database = [| lib1; lib2; lib2_1; lib2_1_1; lib3 |]
    parameterize {
      source [
        [ "lib1" ], [ "lib1" ]
        [ "lib2" ], [ "lib2"; "lib2_1"; "lib2_1_1" ]
        [ "lib2_1" ], [ "lib2_1"; "lib2_1_1" ]
        [ "lib2_1_1" ], [ "lib2_1_1" ]
        [ "lib1"; "lib2" ], [ "lib1"; "lib2"; "lib2_1"; "lib2_1_1" ]
      ]
      run (fun (mainAssemblies, expected) -> test {
        let actual = Database.CompactImpl.removeUnusedAssembly (Set.ofList mainAssemblies) database |> Array.map (fun x -> x.AssemblyName) |> Set
        let expected = Set.ofList expected
        do! actual |> assertEquals expected
      })
    }