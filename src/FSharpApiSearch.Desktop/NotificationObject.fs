namespace FSharpApiSearch.Desktop

open System.ComponentModel
open System.Runtime.CompilerServices

type NotificationObject() =
  let propertyChanged = Event<_, _>()

  member this.RisePropertyChanged([<CallerMemberName>]?propName: string) =
    match propName with
    | Some propName -> propertyChanged.Trigger(this, PropertyChangedEventArgs(propName))
    | None -> ()

  interface INotifyPropertyChanged with
    [<CLIEvent>]
    member this.PropertyChanged = propertyChanged.Publish