module internal FSharpApiSearch.MatcherTypes

open System.Diagnostics
open FSharpApiSearch
open FSharpApiSearch.Printer
open System.Collections.Concurrent

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

type SubtypeResult =
  | Subtype of LowType
  | Contextual of LowType option
  | NonSubtype

type SubtypeCache = ConcurrentDictionary<LowType * LowType, SubtypeResult>

module SubtypeCache =
  open System.Collections.Generic

  let create() = SubtypeCache()

type Context = {
  Distance: int
  Equations: Equations
  QueryTypes: Map<PartialIdentity, FullTypeDefinition[]>
  ApiDictionaries: Map<string, ApiDictionary>
  SubtypeCache: SubtypeCache
}

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Context =
  let addDistance reason x (ctx: Context) =
    let newDistance = ctx.Distance + x
    Debug.WriteLine(sprintf "Update distance from %d to %d by %s" ctx.Distance newDistance reason)
    { ctx with Distance = newDistance }

  let newEquations (oldCtx: Context) (newCtx: Context) =
    newCtx.Equations.Equalities |> List.take (newCtx.Equations.Equalities.Length - oldCtx.Equations.Equalities.Length)

type MatchingResult =
  | Matched of Context
  | Continue of Context
  | Failure

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module MatchingResult =
  let inline bindContinue f x = match x with Continue x -> f x | r -> r
  let inline bindMatched f x = match x with Matched x -> f x | r -> r
  let inline mapMatched f x = match x with Matched x -> Matched (f x) | Continue x -> Continue x | Failure -> Failure

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
  let compose (xs: Rule<_, _> seq): Rule<_, _> =
    fun test left right ctx ->
      let mutable continue' = true
      let mutable state = ctx
      let mutable result = Continue ctx
      let ruleEnum = xs.GetEnumerator()
      while continue' && ruleEnum.MoveNext() do
        let rule = ruleEnum.Current
        let newResult = run rule test left right state
        result <- newResult
        match newResult with
        | Continue ctx -> state <- ctx
        | _ -> continue' <- false
      result