namespace FSharpApiSearch.Desktop

open FsXaml
open ViewModule
open ViewModule.FSharp
open FSharpApiSearch

type MainWindow = XAML<"MainWindow.xaml">

type MainWindowViewModel(session: FSharpApiSearchSession) as this =
  inherit ViewModelBase()

  let query = this.Factory.Backing(<@ this.Query @>, "")
  let results = this.Factory.Backing(<@ this.Results @>, [||])

  new() = MainWindowViewModel(FSharpApiSearchSession(lazy (Database.loadFromFile Database.databaseName)))

  member this.Query with get() = query.Value and set(value) = query.Value <- value
  member this.Results with get() = results.Value and set(value) = results.Value <- value

  member val Search = this.Factory.CommandAsync(fun _ -> async {
    let! results = session.SearchAsync(this.Query)
    do this.Results <- results
  })