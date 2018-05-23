namespace FSharpApiSearch.Desktop

open FSharpApiSearch
open System.Windows.Data
open System.Windows
open System
open Reactive.Bindings
open Reactive.Bindings.Extensions
open System.Reactive.Disposables
open ReactivePropertyHelpers

type BooleanVisibilityConverter() =
  interface IValueConverter with
    member this.Convert(value, targetType, parameter, culture) =
      match value with
      | :? bool as v -> if v then Visibility.Visible else Visibility.Collapsed
      | _ -> Visibility.Visible
      |> box
    member this.ConvertBack(value, targetType, parameter, culture) =
      raise (NotSupportedException())

type MainWindowViewModel(model: FSharpApiSearchSession) as this =
  inherit NotificationObject()

  let disposable = new CompositeDisposable()

  let query =
    model
    |> ReactiveProperty.fromPropertyAsSynchronized <@ fun x -> x.Query @> disposable

  let searched =
    model
    |> Observable.fromProperty <@ fun x -> x.Results @>
    |> Observable.map Option.isSome
    |> Observable.toReactiveProperty disposable

  let results =
    model
    |> Observable.fromProperty <@ fun x -> x.Results @>
    |> Observable.map (function
      | Some xs -> xs
      | None -> Array.empty)
    |> Observable.toReactiveProperty disposable

  let resultCount =
    results
    |> Observable.map Array.length
    |> Observable.toReadOnlyReactiveProperty disposable

  let errorMessage =
    model
    |> Observable.fromProperty <@ fun x -> x.ErrorMessage @>
    |> Observable.map (function
      | Some msg -> msg
      | None -> "")
    |> Observable.toReactiveProperty disposable

  let hasError =
    errorMessage
    |> Observable.map (String.IsNullOrEmpty >> not)
    |> Observable.toReadOnlyReactiveProperty disposable

  let searchCommand =
    new AsyncReactiveCommand()
    |> AsyncReactiveCommand.addCallback (fun ctx -> async {
      do! Async.SwitchToThreadPool()
      do model.Search()
      do! Async.SwitchToContext ctx
    })

  new() = new MainWindowViewModel(FSharpApiSearchSession(lazy (Database.loadFromFile Database.databaseName)))

  member val Query = query
  member val Searched = searched
  member val Results = results
  member val ResultCount = resultCount
  member val ErrorMessage = errorMessage
  member val HasError = hasError
  member val SearchCommand = searchCommand

  interface IDisposable with
    member this.Dispose() = disposable.Dispose()