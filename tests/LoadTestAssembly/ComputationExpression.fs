module ComputationExpression

type OptionBuilder() =
  member this.Bind(x: option<'a>, f: 'a -> 'b option) = Option.bind f x
  member this.Return(x: 'a) = Some x
  member this.ReturnFrom(x: option<'a>) = x

let option = OptionBuilder()
let option2 _ = OptionBuilder()

do
  let x = option {
    let! x = Some 3
    return x
  }
  ()

type NotBuilder() =
  member this.Run() = Option<int>.None

let notBuilder = NotBuilder()

type TryFinallyTest = TryFinallyTest
type GenericDelayBuilder() =
  member this.Zero() = TryFinallyTest
  member this.TryFinally(f, g) = try f finally g()
  member this.Delay(_) = TryFinallyTest

let genericDelay = GenericDelayBuilder()

do
  let x = genericDelay { try () finally () }
  ()

type DelayBuilder() =
  member this.Zero() = TryFinallyTest
  member this.TryFinally(f, g) = try f finally g()
  member this.Delay(f) = f()

let delay = DelayBuilder()

do
  let x = delay { try () finally () }
  ()

type CustomOperation = CustomOperation
type CustomOperationBuilder() =
  [<CustomOperation("test")>]
  member this.Test(value: 'a) = CustomOperation

  member this.Yield(x: 'a) = x

let customOperation = CustomOperationBuilder()

do
  let x = customOperation { test }
  ()