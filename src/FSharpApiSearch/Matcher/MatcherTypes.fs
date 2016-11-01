module internal FSharpApiSearch.MatcherTypes

open System.Diagnostics
open FSharpApiSearch

type Equations = {
  Equalities: (LowType * LowType) list
  Inequalities: (LowType * LowType) list
}

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Equations =
  let debugEquality (left, right) = LowType.debug left + " = " + LowType.debug right
  let debugInequality (left, right) = LowType.debug left + " <> " + LowType.debug right
  let debug x = sprintf "Equalities = %A, Inequalities = %A" (List.map debugEquality x.Equalities) (List.map debugInequality x.Inequalities)

  let empty = { Equalities = []; Inequalities = [] }

type Context = {
  Distance: int
  Equations: Equations
  QueryTypes: Map<PartialIdentity, FullTypeDefinition[]>
  ApiDictionaries: Map<string, ApiDictionary>
}

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Context =
  let addDistance x (ctx: Context) =
    let newDistance = ctx.Distance + x
    Debug.WriteLine(sprintf "Update distance from %d to %d" ctx.Distance newDistance)
    { ctx with Distance = newDistance }

type MatchingResult =
  | Matched of Context
  | Continue of Context
  | Failure

type SwapState =
  | Fixed
  | Swap

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module MatchingResult =
  let inline bindContinue f = function Continue x -> f x | r -> r
  let inline bindMatched f = function Matched x -> f x | r -> r
  let inline mapMatched f = function Matched x -> Matched (f x) | Continue x -> Continue x | Failure -> Failure

  let toBool = function Matched _ -> true | _ -> false

  let bindFailureWithSwap f (ctx: Context) (result, swapState) =
    match result, swapState with
    | Matched _ as m, _ -> m
    | Failure, [||] -> Failure
    | Failure, _ -> f swapState ctx
    | _ -> Failure

type ILowTypeMatcher =
  abstract Test: LowType -> LowType -> Context -> MatchingResult
  abstract Test2: LowType -> LowType -> Context -> MatchingResult * SwapState
  abstract TestAll: LowType seq -> LowType seq -> Context -> MatchingResult
  abstract TestAll2: LowType seq -> LowType seq -> Context -> MatchingResult * SwapState[]
  abstract TestAllWithSwap: LowType seq -> LowType seq -> SwapState[] -> Context -> MatchingResult

[<AutoOpen>]
module Extensions =
  type ILowTypeMatcher with
    member this.TestArrowElementsWithSwap (leftTypes: LowType seq) (rightTypes: LowType seq) (swapState: SwapState[]) (ctx: Context) =
      if Array.last swapState = Swap then
        Debug.WriteLine("Return type is failed. It dose not swap.")
        Failure
      else
        this.TestAllWithSwap leftTypes rightTypes swapState ctx

type IApiMatcher =
  abstract Name: string
  abstract Test: ILowTypeMatcher -> QueryMethod -> Api -> Context -> MatchingResult

type Rule<'Left, 'Right> = ILowTypeMatcher -> 'Left -> 'Right -> Context -> MatchingResult

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Rule =
  let run (rule: Rule<_, _>) matcher left right ctx = rule matcher left right ctx
  let terminator _ _ _ _ =
    Debug.WriteLine("It reached the terminator.")
    Failure
  let inline compose (xs: Rule<_, _> seq): Rule<_, _> =
    fun test left right ctx ->
      xs
      |> Seq.fold (fun result rule -> result |> MatchingResult.bindContinue (run rule test left right)) (Continue ctx)