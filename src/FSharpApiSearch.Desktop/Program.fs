module FSharpApiSearch.Desktop.Program

open System
open System.Windows

[<STAThread; EntryPoint>]
let main args =
  let app = Application.LoadComponent(Uri("App.xaml", UriKind.Relative)) :?> Application
  app.Run()