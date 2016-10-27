module ComputationExpression

type OptionBuilder() =
  member this.Bind(x, f) = Option.bind f x
  member this.Return(x) = Some x
  member this.ReturnFrom(x) = x

let option = OptionBuilder()
let option2 _ = OptionBuilder()

do
  option {
    let! x = Some 3
    return x
  }
  |> ignore

type NotBuilder() =
  member this.Run() = Option<int>.None

let notBuilder = NotBuilder()

type TryFinallyTest = TryFinallyTest
type GenericDelayBuilder() =
  member this.Zero() = TryFinallyTest
  member this.TryFinally(f, g) = try f finally g()
  member this.Delay(_) = TryFinallyTest

let genericDelay = GenericDelayBuilder()

let test1 = genericDelay { try () finally () }

type DelayBuilder() =
  member this.Zero() = TryFinallyTest
  member this.TryFinally(f, g) = try f finally g()
  member this.Delay(f) = f()

let delay = DelayBuilder()

let test2 = delay { try () finally () }