module internal FSharpApiSearch.LowTypeMatcher

open System.Diagnostics
open FSharpApiSearch.MatcherTypes

module Context =
    let setEquations eqs ctx = { ctx with Equations = eqs }

module LowType =
  let rec collectVariableOrWildcardGroup = function
    | Wildcard (Some _) as w -> [ w ]
    | Wildcard None -> []
    | Variable _ as v -> [ v ]
    | Identity _ -> []
    | Arrow xs -> List.collect collectVariableOrWildcardGroup xs
    | Tuple xs -> List.collect collectVariableOrWildcardGroup xs
    | Generic (id, args) -> List.collect collectVariableOrWildcardGroup (id :: args)
    | TypeAbbreviation t -> collectVariableOrWildcardGroup t.Original
    | Delegate (t, _) -> collectVariableOrWildcardGroup t
    | Choice xs -> List.collect collectVariableOrWildcardGroup xs

module Equations =
  let sortTerm x y = if x < y then (x, y) else (y, x)

  let containsEquality left right eqs =
    let x = sortTerm left right
    eqs.Equalities |> List.contains x

  let containsInequality left right eqs =
    let x = sortTerm left right
    eqs.Inequalities |> List.contains x

  let findEqualities left eqs = eqs.Equalities |> List.filter (fst >> ((=)left))

  let testInequality left right eqs =
    Debug.WriteLine(sprintf "Test inequality between %s and %s." (LowType.debug left) (LowType.debug right))
    if containsInequality left right eqs then
      Debug.WriteLine("The inequality exists.")
      false
    else
      let result =
        eqs.Inequalities
        |> List.choose (fun ((x, y) as xy) -> if x = left then Some (y, xy) elif y = left then Some (x, xy) else None)
        |> List.forall (fun (inequalityTerm, inequality) ->
          let xy = sortTerm right inequalityTerm
          match eqs.Equalities |> List.tryFind ((=)xy) with
          | Some equality ->
            Debug.WriteLine(
              sprintf "The inequality between %s and %s exists. It was drived from the following equations. : [ %s, %s ]"
                (LowType.debug left)
                (LowType.debug right)
                (Equations.debugInequality inequality)
                (Equations.debugEquality equality))
            false
          | None -> true
        )
      Debug.WriteLine("It passed the inequality test.")
      result

  let isRecirsive left right =
    match left, right with
    | (Variable _ as variable), other
    | other, (Variable _ as variable) ->
      if LowType.collectVariableOrWildcardGroup other |> List.exists ((=)variable) then
        Debug.WriteLine(sprintf "It is the recursive type.")
        true
      else
        false
    | _ -> false
      
  let isCircular left right eqs =
    match left, right with
    | (Variable _ as variable), other
    | other, (Variable _ as variable) ->
      let oneSteps = seq {
        for (x, y) in eqs.Equalities |> List.filter (fun xy -> xy <> (variable, other) && xy <> (other, variable)) do
          for otherVariable in LowType.collectVariableOrWildcardGroup other |> List.distinct do
            if x = otherVariable then
              yield (y, (x, y))
            elif y = otherVariable then
              yield (x, (x, y))
      }
      oneSteps
      |> Seq.exists (fun (other, xy) ->
        let isCircular =  LowType.collectVariableOrWildcardGroup other |> Seq.exists ((=)variable)
        if isCircular then Debug.WriteLine(sprintf "It is the circular type. It was derived from the following equation. : %s" (Equations.debugEquality xy))
        isCircular)
    | _ -> false

  let tryAddEquality left right (ctx: Context) =
    let left, right = sortTerm left right
    if testInequality left right ctx.Equations then
      let eqs = ctx.Equations
      let newEqs = { eqs with Equalities = (left, right) :: eqs.Equalities }
      Matched (Context.setEquations newEqs ctx)
    else
      Failure

module Rules =
  let terminator (_: ILowTypeMatcher) (_: LowType) (_: LowType) (_: Context) =
    Debug.WriteLine("It reached the terminator.")
    Failure

  let testVariableEquality (lowTypeMatcher: ILowTypeMatcher) left right (ctx: Context) =
    let left, right = Equations.sortTerm left right
    Debug.WriteLine(sprintf "Test equaliity of \"%s\" and \"%s\"." (LowType.debug left) (LowType.debug right))
    if Equations.isRecirsive left right then
      Failure
    elif Equations.isCircular left right ctx.Equations then
      Failure
    else
      let leftEqualities = ctx.Equations |> Equations.findEqualities left
      Debug.WriteLine(
        match leftEqualities with
        | [] -> sprintf "It didn't find known equalities of \"%s\"." (LowType.debug left)
        | _ -> sprintf "It found known equalities of \"%s\". It begins the testing the \"%s\" and %A."
                (LowType.debug left)
                (LowType.debug right)
                (List.map Equations.debugEquality leftEqualities))
      let result =
        leftEqualities
        |> List.fold (fun result (_, x) -> MatchingResult.bindMatched (lowTypeMatcher.Test right x) result) (Matched ctx)
        |> MatchingResult.bindMatched (Equations.tryAddEquality left right)
      Debug.WriteLine(
        match result with
        | Matched _ -> sprintf "It passed the test. The equality has been added.: \"%s\" = \"%s\"" (LowType.debug left) (LowType.debug right)
        | Continue _ | Failure -> sprintf "It failed to add the equality.: \"%s\" = \"%s\"" (LowType.debug left) (LowType.debug right))
      result

  let testAll2 (lowTypeMatcher: ILowTypeMatcher) (leftTypes: LowType seq) (rightTypes: LowType seq) (ctx: Context) : MatchingResult * SwapState[] =
    Debug.WriteLine(sprintf "Test %A and %A." (Seq.map LowType.debug leftTypes |> Seq.toList) (Seq.map LowType.debug rightTypes |> Seq.toList))
    if Seq.length leftTypes <> Seq.length rightTypes then
      Debug.WriteLine("The numbers of the parameters are different.")
      Failure, [||]
    else
      let lastContext, revSwapStatuses, success =
        Seq.zip leftTypes rightTypes
        |> Seq.fold (fun (lastContext, revSwapStatuses, lastResult) (left, right) ->
          Debug.WriteLine(sprintf "Test %s and %s." (LowType.debug left) (LowType.debug right))
          Debug.Indent()
          let result, swapStatus = lowTypeMatcher.Test2 left right lastContext
          Debug.Unindent()

          let newResult = lastResult && MatchingResult.toBool result
          let newContext =
            match result with
            | Matched ctx -> ctx
            | _ -> lastContext
          
          (newContext, swapStatus :: revSwapStatuses, newResult)
        ) (ctx, [], true)
      if success then
        Matched lastContext, [||]
      else
        Failure, (revSwapStatuses |> List.rev |> List.toArray)

  let makeSwapTable depth (swapState: SwapState[]) : int[][] =
    let swapTable = Array.indexed swapState |> Array.choose (fun (i, state) -> match state with Swap -> Some i | Fixed -> None)
    let swap (xs: int[]) : int[][] =
      [|
        yield xs
        for i in swapTable do
          for k in swapTable do
            if i < k then
              let buff = Array.copy xs
              buff.[i] <- xs.[k]
              buff.[k] <- xs.[i]
              yield buff
      |]
    let swappedAll (xs: int[]): bool =
      swapTable
      |> Array.forall (fun i -> xs.[i] <> i)
    let init = Array.init (swapState.Length) id
    let rec loop n acc =
      if n <= 0 then
        acc
      else
        loop (n - 1) (acc |> Array.collect swap)
    loop depth [| init |]
    |> Seq.filter swappedAll
    |> Seq.distinct
    |> Seq.toArray

  let swap (swapTable: int[]) (xs: LowType seq) =
    Seq.permute (fun i -> swapTable.[i]) xs

  let testSwap (swapDepth: int) (lowTypeMatcher: ILowTypeMatcher) (leftTypes: LowType seq) (rightTypes: LowType seq) (swapState: SwapState[]) (ctx: Context): MatchingResult =
    Debug.WriteLine("Test swap.")
    Debug.Indent()
    let result =
      makeSwapTable swapDepth swapState
      |> Array.tryPick (fun swapTable ->
        Debug.WriteLine(sprintf "Swap table: %A" swapTable)
        Debug.Indent()
        let leftTypes = swap swapTable leftTypes
        let result = lowTypeMatcher.TestAll leftTypes rightTypes ctx
        Debug.Unindent()
        match result with
        | Matched _ as m -> Some m
        | _ -> None
      )
    Debug.Unindent()
    match result with
    | Some m -> m |> MatchingResult.mapMatched (Context.addDistance 1)
    | _ -> Failure

  let choiceRule (lowTypeMatcher: ILowTypeMatcher) left right ctx =
    match left, right with
    | Choice choices, other
    | other, Choice choices ->
      Debug.WriteLine("choice rule.")
      Debug.WriteLine(sprintf "test %A and %s" (choices |> List.map (fun x -> x.Debug())) (other.Debug()))
      choices
      |> Seq.tryPick (fun c -> match lowTypeMatcher.Test c other ctx with Matched _ as m -> Some m | _ -> None)
      |> function
        | Some matched -> matched
        | None -> Failure
    | _ -> Continue ctx

  let typeAbbreviationRule (lowTypeMatcher: ILowTypeMatcher) left right ctx =
    match left, right with
    | (TypeAbbreviation abbreviation), other
    | other, (TypeAbbreviation abbreviation) ->
      Debug.WriteLine("type abbreviation rule.")
      Debug.WriteLine(sprintf "(%s) -> (%s)" (LowType.debug abbreviation.Abbreviation) (LowType.debug abbreviation.Original))
      lowTypeMatcher.Test abbreviation.Original other ctx
    | _ -> Continue ctx

  let testIdentity nameEquality leftIdentity rightIdentity ctx =
    if nameEquality leftIdentity rightIdentity then
      Debug.WriteLine("There are same identities.")
      Matched ctx
    else
      Debug.WriteLine("There are deferent identities.")
      Failure

  let identityRule nameEquality _ left right ctx =
    match left, right with
    | Identity leftIdentity, Identity rightIdentity ->
      Debug.WriteLine("identity rule.")
      testIdentity nameEquality leftIdentity rightIdentity ctx
    | _ -> Continue ctx

  let variableRule lowTypeMatcher left right ctx =
    match left, right with
    | Variable _, Variable _ ->
      Debug.WriteLine("variable rule.")
      if Equations.containsEquality left right ctx.Equations then
        Debug.WriteLine("The equality already exists.")
        Matched ctx
      else
        testVariableEquality lowTypeMatcher left right ctx
    | _ -> Continue ctx

  let rec distanceFromVariable = function
    | Wildcard _ -> 0
    | Variable _ -> 0
    | Identity _ -> 1
    | Arrow xs -> seqDistance xs
    | Tuple _ -> 1
    | Generic _ -> 1
    | TypeAbbreviation x -> distanceFromVariable x.Original
    | Delegate _ -> 1
    | Choice _ -> 1
  and seqDistance xs = xs |> Seq.sumBy (distanceFromVariable >> max 1)

  let greedyVariableRule lowTypeMatcher left right ctx =
    match left, right with
    | (Variable _ as variable), other
    | other, (Variable _ as variable) ->
      Debug.WriteLine("greedy variable rule.")
      if Equations.containsEquality variable other ctx.Equations then
        Debug.WriteLine("The equality already exists.")
        Matched ctx
      else
        testVariableEquality lowTypeMatcher variable other ctx
        |> MatchingResult.mapMatched (Context.addDistance (distanceFromVariable other))
    | _ -> Continue ctx

  let tupleRule (lowTypeMatcher: ILowTypeMatcher) left right ctx =
    match left, right with
    | Tuple leftElems, Tuple rightElems ->
      Debug.WriteLine("tuple rule.")
      lowTypeMatcher.TestAll2 leftElems rightElems ctx
      |> MatchingResult.bindFailureWithSwap (lowTypeMatcher.TestAllWithSwap leftElems rightElems) ctx
    | _ -> Continue ctx

  let testArrow (lowTypeMatcher: ILowTypeMatcher) leftElems rightElems ctx =
    lowTypeMatcher.TestAll2 leftElems rightElems ctx
    |> MatchingResult.bindFailureWithSwap (lowTypeMatcher.TestArrowElementsWithSwap leftElems rightElems) ctx

  let arrowRule lowTypeMatcher left right ctx =
    match left, right with
    | Arrow leftElems, Arrow rightElems ->
      Debug.WriteLine("arrow rule.")
      testArrow lowTypeMatcher leftElems rightElems ctx
    | _ -> Continue ctx

  let testArrow_IgnoreParameterStyle (lowTypeMatcher: ILowTypeMatcher) leftElems rightElems ctx =
    match leftElems, rightElems with
    | [ _; _ ], [ _; _ ] ->
      lowTypeMatcher.TestAll2 leftElems rightElems ctx
      |> MatchingResult.bindFailureWithSwap (lowTypeMatcher.TestArrowElementsWithSwap leftElems rightElems) ctx
    | [ Tuple leftArgs; leftRet ], _ ->
      let leftElems = seq { yield! leftArgs; yield leftRet }
      lowTypeMatcher.TestAll2 leftElems rightElems ctx
      |> MatchingResult.bindFailureWithSwap (lowTypeMatcher.TestArrowElementsWithSwap leftElems rightElems) ctx
      |> MatchingResult.mapMatched (Context.addDistance 1)
    | _, [ Tuple rightArgs; rightRet ] ->
      let rightElems = seq { yield! rightArgs; yield rightRet }
      lowTypeMatcher.TestAll2 leftElems rightElems ctx
      |> MatchingResult.bindFailureWithSwap (lowTypeMatcher.TestArrowElementsWithSwap leftElems rightElems) ctx
      |> MatchingResult.mapMatched (Context.addDistance 1)
    | _, _ ->
      lowTypeMatcher.TestAll2 leftElems rightElems ctx 
      |> MatchingResult.bindFailureWithSwap (lowTypeMatcher.TestArrowElementsWithSwap leftElems rightElems) ctx

  let arrowRule_IgnoreParameterStyle lowTypeMatcher left right ctx =
    match left, right with
    | Arrow leftElems, Arrow rightElems ->
      Debug.WriteLine("arrow rule (ignore parameter style).")
      testArrow_IgnoreParameterStyle lowTypeMatcher leftElems rightElems ctx
    | _ -> Continue ctx

  let genericRule (lowTypeMatcher: ILowTypeMatcher) left right ctx =
    match left, right with
    | Generic (leftId, leftArgs), Generic (rightId, rightArgs) ->
      Debug.WriteLine("generic rule.")
      lowTypeMatcher.TestAll (leftId :: leftArgs) (rightId :: rightArgs) ctx
    | _ -> Continue ctx

  let wildcardRule _ left right ctx =
    match left, right with
    | Wildcard None, _
    | _, Wildcard None ->
      Debug.WriteLine("wildcard rule.")
      Matched ctx
    | _ -> Continue ctx

  let wildcardGroupRule lowTypeMatcher left right ctx =
    match left, right with
    | (Wildcard (Some _)), _
    | _, (Wildcard (Some _))->
      Debug.WriteLine("wildcard group rule.")
      if Equations.containsEquality left right ctx.Equations then
        Debug.WriteLine("The equality already exists.")
        Matched ctx
      else
        testVariableEquality lowTypeMatcher left right ctx
    | _ -> Continue ctx

  let delegateRule nameEquality (lowTypeMatcher: ILowTypeMatcher) left right ctx =
    match left, right with
    | Delegate (Identity leftId, _), Delegate (Identity rightId, _)
    | Delegate (Identity leftId, _), Identity rightId
    | Identity leftId, Delegate (Identity rightId, _) ->
      Debug.WriteLine("deligate rule.")
      testIdentity nameEquality leftId rightId ctx
    | Delegate (Generic (leftId, leftArgs), _), Delegate (Generic (rightId, rightArgs), _)
    | Delegate (Generic (leftId, leftArgs), _), Generic (rightId, rightArgs)
    | Generic (leftId, leftArgs), Delegate (Generic (rightId, rightArgs), _) ->
      Debug.WriteLine("generic delegate rule.")
      lowTypeMatcher.TestAll (leftId :: leftArgs) (rightId :: rightArgs) ctx
    | _ -> Continue ctx

  let delegateAndArrowRule lowTypeMatcher left right ctx =
    match left, right with
    | Delegate (_, leftElems), Arrow rightElems
    | Arrow leftElems, Delegate (_, rightElems) ->
      Debug.WriteLine("delegate and arrow rule.")
      testArrow lowTypeMatcher leftElems rightElems ctx
      |> MatchingResult.mapMatched (Context.addDistance 1)
    | _ -> Continue ctx

  let delegateAndArrowRule_IgnoreParameterStyle lowTypeMatcher left right ctx =
    match left, right with
    | Delegate (_, leftElems), Arrow rightElems
    | Arrow leftElems, Delegate (_, rightElems) ->
      Debug.WriteLine("delegate and arrow rule (ignore parameter style).")
      testArrow_IgnoreParameterStyle lowTypeMatcher leftElems rightElems ctx
      |> MatchingResult.mapMatched (Context.addDistance 1)
    | _ -> Continue ctx

let instance options =
  let nameEquality = Identity.equalityFromOptions options

  let rule =
    Rule.compose [
      yield Rules.choiceRule
      yield Rules.typeAbbreviationRule
      yield Rules.wildcardGroupRule
      yield Rules.wildcardRule
    
      match options.GreedyMatching with
      | Enabled -> yield Rules.greedyVariableRule
      | Disabled -> yield Rules.variableRule

      yield Rules.identityRule nameEquality
      yield Rules.tupleRule
      yield Rules.genericRule

      match options.IgnoreParameterStyle with
      | Enabled -> yield Rules.arrowRule_IgnoreParameterStyle
      | Disabled -> yield Rules.arrowRule
        
      yield Rules.delegateRule nameEquality
      match options.IgnoreParameterStyle with
      | Enabled -> yield Rules.delegateAndArrowRule_IgnoreParameterStyle
      | Disabled -> yield Rules.delegateAndArrowRule

      yield Rule.terminator
    ]

  let toSwapState = function
    | Matched _ -> SwapState.Fixed
    | _ -> SwapState.Swap

  let testAllWithSwap =
    match options.SwapOrderDepth with
    | 0 -> fun _ _ _ _ _ -> Failure
    | depth -> fun lowTypeMatcher left right swapState ctx -> Rules.testSwap depth lowTypeMatcher left right swapState ctx

  { new ILowTypeMatcher with
      member this.Test left right ctx =
        Debug.WriteLine(sprintf "Test \"%s\" and \"%s\". Equations: %s"
          (left.Debug())
          (right.Debug())
          (Equations.debug ctx.Equations))
        Debug.Indent()
        let result = Rule.run rule this left right ctx
        Debug.Unindent()
        result
      member this.Test2 left right ctx =
        let result = this.Test left right ctx
        let swap = result |> toSwapState
        (result, swap)
      member this.TestAll left right ctx = this.TestAll2 left right ctx |> fst
      member this.TestAll2 left right ctx = Rules.testAll2 this left right ctx
      member this.TestAllWithSwap left right swapState ctx = testAllWithSwap this left right swapState ctx }