module FSharpApiSearch.Desktop.Program

open System
open System.Threading
open System.Windows.Threading

[<STAThread; EntryPoint>]
let main args =
  let app = App()
  app.Run()