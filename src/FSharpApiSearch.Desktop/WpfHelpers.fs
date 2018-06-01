module FSharpApiSearch.Desktop.WpfHelpers

open System.ComponentModel
open System.Runtime.CompilerServices
open System.Windows

type NotificationObject() =
  let propertyChanged = Event<_, _>()

  member this.RisePropertyChanged([<CallerMemberName>]?propName: string) =
    match propName with
    | Some propName -> propertyChanged.Trigger(this, PropertyChangedEventArgs(propName))
    | None -> ()

  interface INotifyPropertyChanged with
    [<CLIEvent>]
    member this.PropertyChanged = propertyChanged.Publish

let freeze<'a when 'a :> Freezable> (x: 'a) = x.Freeze(); x