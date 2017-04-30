module internal FSharpApiSearch.MatcherTypes

open System.Diagnostics
open FSharpApiSearch
open FSharpApiSearch.Printer

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
  let addDistance reason x (ctx: Context) =
    let newDistance = ctx.Distance + x
    Debug.WriteLine(sprintf "Update distance from %d to %d by %s" ctx.Distance newDistance reason)
    { ctx with Distance = newDistance }

type MatchingResult =
  | Matched of Context
  | Continue of Context
  | Failure

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module MatchingResult =
  let inline bindContinue f = function Continue x -> f x | r -> r
  let inline bindMatched f = function Matched x -> f x | r -> r
  let inline mapMatched f = function Matched x -> Matched (f x) | Continue x -> Continue x | Failure -> Failure

  let toBool = function Matched _ -> true | _ -> false

type ILowTypeMatcher =
  abstract Test: LowType -> LowType -> Context -> MatchingResult
  abstract TestAll: LowType seq -> LowType seq -> Context -> MatchingResult
  abstract TestAllExactly: LowType seq -> LowType seq -> Context -> MatchingResult

[<AutoOpen>]
module Extensions =
  let private paramsAndRet (xs: 'a seq) =
    let xs = Array.ofSeq xs
    let ps = Array.take (xs.Length - 1) xs
    let ret = Array.last xs
    (ps, ret)
  type ILowTypeMatcher with
    member this.TestArrow (leftTypes: LowType seq) (rightTypes: LowType seq) (ctx: Context) =
      let leftParams, leftRet = paramsAndRet leftTypes
      let rightParams, rightRet = paramsAndRet rightTypes
      this.Test leftRet rightRet ctx
      |> MatchingResult.bindMatched (this.TestAll leftParams rightParams)

    member this.TestReceiver (left: LowType) (right: LowType) (ctx: Context) =
      match left, right with
      | Tuple _, Tuple _ -> this.Test left right ctx
      | Tuple _, _ | _, Tuple _ -> Failure
      | _ -> this.Test left right ctx 

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