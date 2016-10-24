module ComputationExpression

type OptionBuilder() =
  member inline this.Bind(x, f) = Option.bind f x
  member inline this.Return(x) = Some x
  member inline this.ReturnFrom(x) = x

let option = OptionBuilder()
let option2 _ = OptionBuilder()

type NotBuilder() =
  member this.Run() = Option<int>.None

let notBuilder = NotBuilder()