open FSharpApiSearch
open System.Diagnostics

[<EntryPoint>]
let main argv = 
  let database = Database.loadFromFile Database.databaseName
  
  let sw = Stopwatch.StartNew()

  for i = 0 to 50 do
    for apiDict in database do
      for api in apiDict.Api do
        LinkGenerator.dotNetApiBrowser "base" "view" api |> ignore

  printfn "%d" sw.ElapsedMilliseconds

  0