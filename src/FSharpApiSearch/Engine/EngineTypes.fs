module internal FSharpApiSearch.EngineTypes

open System.Diagnostics
open System.Collections.Generic
open FSharpApiSearch
open FSharpApiSearch.StringPrinter
open System.Collections.Concurrent

type Equations = {
  Equalities: (LowType * LowType) list
  Inequalities: (LowType * LowType) list
}

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
  let create() = SubtypeCache()

type Context = {
  Distance: int
  Equations: Equations
  MatchPositions: Map<SignatureId, QueryId>
  QueryTypes: Map<UserInputType, FullTypeDefinition[]>
  ApiDictionaries: IDictionary<string, ApiDictionary>
  SubtypeCache: SubtypeCache
}

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
  type ILowTypeMatcher with
    member this.TestArrow (left: Arrow) (right: Arrow) (ctx: Context) =
      this.Test (snd left) (snd right) ctx
      |> MatchingResult.bindMatched (this.TestAll (fst left) (fst right))

    member this.TestReceiver (left: LowType) (right: LowType) (ctx: Context) =
      match left, right with
      | Tuple _, Tuple _ -> this.Test left right ctx
      | Tuple _, _ | _, Tuple _ -> Failure
      | _ -> this.Test left right ctx 

type IApiMatcher =
  abstract Name: string
  abstract Test: ILowTypeMatcher -> Query -> Api -> Context -> MatchingResult

module ApiMatcher =
  let test (lowTypeMatcher: ILowTypeMatcher) (apiMatcher: IApiMatcher) (query: Query) (api: Api) (ctx: Context) =
    Debug.WriteLine(sprintf "Test \"%s\" and \"%s\" by %s. Equations: %s"
      query.OriginalString
      (ApiSignature.debug api.Signature)
      apiMatcher.Name
      (Equations.debug ctx.Equations))
    Debug.Indent()
    let result = apiMatcher.Test lowTypeMatcher query api ctx
    Debug.Unindent()
    result

type Rule<'Matcher, 'Left, 'Right> = 'Matcher -> 'Left -> 'Right -> Context -> MatchingResult

module Rule =
  let run (rule: Rule<_, _, _>) matcher left right ctx = rule matcher left right ctx
  let terminator _ _ _ _ =
    Debug.WriteLine("It reached the terminator.")
    Failure
  let continueFailure (rule: Rule<_, _, _>) matcher left right ctx =
    match run rule matcher left right ctx with
    | Failure -> Continue ctx
    | (Matched _ | Continue _) as result -> result
  let compose (xs: Rule<_, _, _>[]): Rule<_, _, _> =
    fun test left right ctx ->
      let mutable continue' = true
      let mutable state = ctx
      let mutable result = Continue ctx
      let mutable index = 0
      while continue' && index < xs.Length do
        let rule = xs.[index]
        index <- index + 1
        let newResult = run rule test left right state
        result <- newResult
        match newResult with
        | Continue ctx -> state <- ctx
        | _ -> continue' <- false
      result