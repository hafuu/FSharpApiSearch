namespace FSharpApiSearch.Desktop

open FSharpApiSearch
open System.Windows.Data
open System.Windows
open System
open Reactive.Bindings
open Reactive.Bindings.Extensions
open System.Reactive.Disposables
open ReactivePropertyHelpers
open System.Windows.Media
open System.Windows.Controls
open WpfHelpers

module internal Impl =
  let boolToVisibility = function
    | true -> Visibility.Visible
    | false -> Visibility.Collapsed

open Impl

type SignatureItemViewModel(model: SignatureItem) =
  inherit NotificationObject()

  member val Text = model.Text
  member val Color =
    match model.Color with
    | Some c ->
      SolidColorBrush(c)
      |> freeze
      :> obj
    | None -> null
  member val HasColor = model.Color |> Option.isSome

type SearchResultViewModel(model: SearchResult) =
  inherit NotificationObject()

  member val DeclarationType = model.DeclarationType
  member val Name = model.Name
  member val Signature = model.Signature |> Array.map SignatureItemViewModel

  member val Constraints = model.Constraints |> Option.defaultValue ""
  member val HasConstraints = model.Constraints |> Option.isSome |> boolToVisibility
  member val AccessPath = model.AccessPath
  member val Kind = model.Kind
  member val Assembly = model.Assembly
  member val Document = model.Document |> Option.defaultValue ""
  member val HasDocument = model.Document |> Option.isSome |> boolToVisibility

type MainWindowViewModel(model: FSharpApiSearchSession) =
  inherit NotificationObject()

  let disposable = new CompositeDisposable()

  let query =
    model
    |> ReactiveProperty.observePropertyAsSynchronized <@ fun x -> x.Query @> disposable

  let searched =
    model
    |> Observable.fromProperty <@ fun x -> x.Results @>
    |> Observable.map (Option.isSome >> boolToVisibility)
    |> Observable.toReadOnlyReactiveProperty disposable

  let results =
    model
    |> Observable.fromProperty <@ fun x -> x.Results @>
    |> Observable.map (function
      | Some xs -> xs |> Array.map SearchResultViewModel
      | None -> [||])
    |> Observable.toReadOnlyReactiveProperty disposable

  let resultCount =
    model
    |> Observable.fromProperty <@ fun x -> x.Results @>
    |> Observable.map (function
      | Some xs -> Array.length xs
      | None -> 0)
    |> Observable.toReadOnlyReactiveProperty disposable

  let errorMessage =
    model
    |> Observable.fromProperty <@ fun x -> x.ErrorMessage @>
    |> Observable.map (function
      | Some msg -> msg
      | None -> "")
    |> Observable.toReadOnlyReactiveProperty disposable

  let hasError =
    errorMessage
    |> Observable.map (String.IsNullOrEmpty >> not >> boolToVisibility)
    |> Observable.toReadOnlyReactiveProperty disposable

  let searchCommand =
    new AsyncReactiveCommand()
    |> AsyncReactiveCommand.addCallback (fun ctx -> async {
      do! Async.SwitchToThreadPool()
      do model.Search()
      do! Async.SwitchToContext ctx
    }) disposable

  let initializeModelCommand =
    new AsyncReactiveCommand()
    |> AsyncReactiveCommand.addCallback (fun ctx -> async {
      do model.InitializeInBackground()
    }) disposable

  new() = new MainWindowViewModel(FSharpApiSearchSession(lazy (Database.loadFromFile Database.databaseName)))

  member val Query = query
  member val Searched = searched
  member val Results = results
  member val ResultCount = resultCount
  member val ErrorMessage = errorMessage
  member val HasError = hasError
  member val SearchCommand = searchCommand
  member val InitializeModelCommand = initializeModelCommand

  interface IDisposable with
    member this.Dispose() = disposable.Dispose()