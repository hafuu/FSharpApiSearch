module DequeTest

open Persimmon
open Persimmon.Syntax.UseTestNameByReflection
open Persimmon.MuscleAssert
open FSharpApiSearch
open FSharpApiSearch.Deque

let initialValue = Wildcard(Option.Some(""),Unknown)
let variable0 = Variable(VariableSource.Query,{Name = "test0" ; IsSolveAtCompileTime = false},Unknown)
let variable1 = Variable(VariableSource.Query,{Name = "test1" ; IsSolveAtCompileTime = false},Unknown)
let variable2 = Variable(VariableSource.Query,{Name = "test2" ; IsSolveAtCompileTime = false},Unknown)
let variable3 = Variable(VariableSource.Query,{Name = "test3" ; IsSolveAtCompileTime = false},Unknown)
let variable4 = Variable(VariableSource.Query,{Name = "test4" ; IsSolveAtCompileTime = false},Unknown)
let variable5 = Variable(VariableSource.Query,{Name = "test5" ; IsSolveAtCompileTime = false},Unknown)

let PushFrontTest = test {
  
  let dq = new Deque(3)
  dq.PushFront variable1
  dq.PushFront variable2
  dq.PushFront variable3
  let actual = [|variable3;variable2;variable1|]
  do! actual |> assertEquals(dq.GetDeque)
      
}

let PopFrontTest = test {
  let dq = new Deque(3)
  dq.PushFront variable1
  dq.PushFront variable2
  dq.PushFront variable3
  let actual = variable3
  do! actual |> assertEquals(dq.PopFront)
  let actual = [|initialValue;variable2;variable1|]
  do! actual |> assertEquals(dq.GetDeque)
}

let PushBackTest = test {
  let dq = new Deque(3)
  dq.PushBack variable1
  dq.PushBack variable2
  dq.PushBack variable3
  let actual = [|variable1;variable2;variable3|]
  do! actual |> assertEquals(dq.GetDeque)
}

let PopBackTest = test {
  let dq = new Deque(3)
  dq.PushBack variable1
  dq.PushBack variable2
  dq.PushBack variable3
  let actual = variable3
  do! actual |> assertEquals(dq.PopBack)
  let actual = [|variable1;variable2;initialValue|]
  do! actual |> assertEquals(dq.GetDeque)
}

let DequeIsEmptyTest = test {
  let dq = new Deque(3)
  dq.PushBack variable1
  dq.PushBack variable2
  dq.PushBack variable3
  let actual = false
  do! actual |> assertEquals(dq.IsEmpty)
  dq.PopBack |> ignore
  dq.PopBack |> ignore
  dq.PopBack |> ignore
  let actual = true
  do! actual |> assertEquals(dq.IsEmpty)
}

let DequeLengthTest = test {
  let dq = new Deque(3)
  dq.PushBack variable1
  dq.PushBack variable2
  dq.PushBack variable3
  let actual = 3
  do! actual |> assertEquals(dq.Length)
  dq.PopBack |> ignore
  dq.PopBack |> ignore
  dq.PopBack |> ignore
  let actual = 0
  do! actual |> assertEquals(dq.Length)
}

let MixPushPopTest = test {
  let dq = new Deque(5)
  dq.PushFront variable1
  dq.PushFront variable2
  dq.PushFront variable3
  dq.PushBack variable4
  dq.PushBack variable5

  let actual = [|variable3;variable2;variable1;variable4;variable5|]
  do! actual |> assertEquals(dq.GetDeque)

  do! variable3 |> assertEquals(dq.PopFront)
  do! variable2 |> assertEquals(dq.PopFront)
  do! variable5 |> assertEquals(dq.PopBack)
  do! variable4 |> assertEquals(dq.PopBack)
  do! variable1 |> assertEquals(dq.PopBack)

  let actual = [|initialValue;initialValue;initialValue;initialValue;initialValue|]
  do! actual |> assertEquals(dq.GetDeque)

  dq.PushBack variable1
  dq.PushFront variable2
  let actual = [|initialValue;variable2;variable1;initialValue;initialValue|]
  do! actual |> assertEquals(dq.GetDeque)
  do! variable2 |> assertEquals(dq.Front)
  do! variable1 |> assertEquals(dq.Back)
}