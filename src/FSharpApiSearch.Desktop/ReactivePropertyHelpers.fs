module rec ReactivePropertyHelpers

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Linq.RuntimeHelpers
open System
open System.Linq.Expressions
open System.ComponentModel
open Reactive.Bindings
open Reactive.Bindings.Extensions
open System.Reactive.Disposables
open System.Threading
open System.Threading.Tasks

module Linq =
  let fromFSharpFuncExpr (expr: Expr<'TSubject -> 'TProperty>) =
    let linqExpr = LeafExpressionConverter.QuotationToExpression expr :?> MethodCallExpression
    let lambda = linqExpr.Arguments.[0] :?> LambdaExpression
    Expression.Lambda<Func<'TSubject, 'TProperty>>(lambda.Body, lambda.Parameters)

module Observable =
  let fromProperty propertySelector subject =
    INotifyPropertyChangedExtensions.ObserveProperty(subject, Linq.fromFSharpFuncExpr propertySelector)

  let toReactiveProperty container (observable: IObservable<_>) =
    observable.ToReactiveProperty()
    |> ReactiveProperty.addTo container

  let toReadOnlyReactiveProperty container (observable: IObservable<_>) =
    observable.ToReadOnlyReactiveProperty()
    |> ReadOnlyReactiveProperty.addTo container

module ReactiveProperty =
  let fromProperty propertySelector container subject =
    subject
    |> Observable.fromProperty propertySelector
    |> Observable.toReactiveProperty container

  let fromPropertyAsSynchronized propertySelector container subject =
    INotifyPropertyChangedExtensions.ToReactivePropertyAsSynchronized(subject, Linq.fromFSharpFuncExpr propertySelector)
    |> addTo container

  let addTo (container: CompositeDisposable) (prop: ReactiveProperty<_>) = prop.AddTo(container)

module ReadOnlyReactiveProperty =
  let addTo (container: CompositeDisposable) (prop: ReadOnlyReactiveProperty<_>) = prop.AddTo(container)

module AsyncReactiveCommand =
  let addCallback (callback: SynchronizationContext -> Async<unit>) (command: AsyncReactiveCommand) =
    let callback = callback SynchronizationContext.Current
    command.Subscribe(Func<Task>(fun () -> Async.StartImmediateAsTask callback :> Task)) |> ignore
    command