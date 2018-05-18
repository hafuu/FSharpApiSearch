namespace FSharpApiSearch.Desktop

open FsXaml
open ViewModule
open ViewModule.FSharp
open FSharpApiSearch
open System.Windows.Data
open System.Windows
open System

type MainWindow = XAML<"MainWindow.xaml">

type BooleanVisibilityConverter() =
  interface IValueConverter with
    member this.Convert(value, targetType, parameter, culture) =
      match value with
      | :? bool as v -> if v then Visibility.Visible else Visibility.Collapsed
      | _ -> Visibility.Visible
      |> box
    member this.ConvertBack(value, targetType, parameter, culture) =
      raise (NotSupportedException())

type MainWindowViewModel(session: FSharpApiSearchSession) as this =
  inherit ViewModelBase()

  let query = this.Factory.Backing(<@ this.Query @>, "")
  
  let searched = this.Factory.Backing(<@ this.Searched @>, false)

  let resultCount = this.Factory.Backing(<@ this.ResultCount @>, 0)
  let results = this.Factory.Backing(<@ this.Results @>, [||])

  let hasError = this.Factory.Backing(<@ this.HasError @>, false)
  let errorMessage = this.Factory.Backing(<@ this.ErrorMessage @>, "")

  new() = MainWindowViewModel(FSharpApiSearchSession(lazy (Database.loadFromFile Database.databaseName)))

  member this.Query with get() = query.Value and set(value) = query.Value <- value

  member this.Searched with get() = searched.Value and set(value) = searched.Value <- value
  member this.ResultCount with get() = resultCount.Value and set(value) = resultCount.Value <- value
  member this.Results with get() = results.Value and set(value) = results.Value <- value

  member this.HasError with get() = hasError.Value and set(value) = hasError.Value <- value
  member this.ErrorMessage with get() = errorMessage.Value and set(value) = errorMessage.Value <- value

  member this.ClearError() =
    this.HasError <- false
    this.ErrorMessage <- ""

  member this.SetError(ex: exn) =
    this.HasError <- true
    this.ErrorMessage <- ex.Message

  member this.ClearResults() =
    this.Searched <- false
    this.ResultCount <- 0
    this.Results <- [||]

  member this.SetResults(results: SearchResult[]) =
    this.Searched <- true
    this.ResultCount <- results.Length
    this.Results <- results

  member val Search = this.Factory.CommandAsync(fun _ -> async {
    try
      do this.ClearResults()
      do this.ClearError()
      let! results = session.SearchAsync(this.Query)
      do this.SetResults(results)
    with
      ex -> this.SetError(ex)
  })