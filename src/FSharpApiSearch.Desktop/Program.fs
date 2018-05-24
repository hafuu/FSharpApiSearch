module FSharpApiSearch.Desktop.Program

open System
open System.Windows

[<STAThread; EntryPoint>]
let main args =
  let window = Application.LoadComponent(Uri("MainWindow.xaml", UriKind.Relative)) :?> Window
  Application().Run(window)