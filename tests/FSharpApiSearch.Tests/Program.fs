open Persimmon.Console
open System.Reflection

[<EntryPoint>]
let main args =
  let args = Args.parse Args.empty (List.ofArray args)
  Runner.runTestsInAssembly args [ Assembly.GetExecutingAssembly() ]