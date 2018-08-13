module FSharpApiSearch.Deque

exception DequeFullException of string
exception DequeEmptyException of string

type Deque(size : int) =
  let mutable MaxLength = size
  
  let initialValue = Wildcard(Option.Some(""),Unknown)

  let mutable deque : LowType[] = seq{
                                    for i = 0 to MaxLength - 1 do
                                      yield initialValue
                                  } |> Seq.toArray

  let mutable FrontIndex : int = -1
  let mutable BackIndex : int = 0
  
  member  this.GetDeque = deque
  
  member this.Length =
      BackIndex - FrontIndex - 1

  member this.IsEmpty =
    if this.Length = 0 then
      true
    else
      false

  member this.Front =
    if this.IsEmpty then
      DequeEmptyException("Deque is Empty") |> raise
    else
      deque.[FrontIndex + 1]

  member this.Back =
    if this.IsEmpty then
      DequeEmptyException("Deque is Empty") |> raise
    else
      deque.[BackIndex - 1]

  member this.PushFront (data : LowType) =
    if this.Length < MaxLength then
      if FrontIndex = -1 then
        for idx = BackIndex downto 1 do
          deque.[idx] <- deque.[idx - 1]
        deque.[0] <- data
        BackIndex <- BackIndex + 1

      else
        deque.[FrontIndex] <- data
        FrontIndex <- FrontIndex - 1
        
    else
      DequeFullException("Deque is Full") |> raise

  member this.PopFront : LowType =
    if this.IsEmpty then
      DequeEmptyException("Deque is Empty") |> raise
    else
      let front = this.Front
      deque.[FrontIndex + 1] <- initialValue
      FrontIndex <- FrontIndex + 1
      front

  member this.PushBack (data :LowType) =
    if this.Length < MaxLength then
      if BackIndex = MaxLength then
        for idx = FrontIndex to MaxLength - 2 do
          deque.[idx] <- deque.[idx + 1]
        deque.[MaxLength - 1] <- data
        FrontIndex <- FrontIndex - 1
      else
        deque.[BackIndex] <- data
        BackIndex <- BackIndex + 1

    else
      DequeFullException("Deque is Full") |> raise

  member this. PopBack : LowType =
    if this.IsEmpty then
      DequeEmptyException("Deque is Empty") |> raise
    else
      let back = this.Back
      deque.[BackIndex - 1] <- initialValue
      BackIndex <- BackIndex - 1
      back