module FSharpApiSearch.Desktop.Program

open System

[<STAThread; EntryPoint>]
let main args =
  let app = App()
  app.Run()