module internal FSharpApiSearch.LowTypeMatcher

open System.Diagnostics
open FSharpApiSearch.MatcherTypes
open FSharpApiSearch.Printer
open FSharpApiSearch.SpecialTypes
open FSharpApiSearch.SpecialTypes.LowType.Patterns

module Context =
    let setEquations eqs ctx = { ctx with Equations = eqs }

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

  let testLeftEqualities (lowTypeMatcher: ILowTypeMatcher) (leftEqualities: _ list) right ctx =
    let mutable continue' = true
    let mutable state = ctx
    let mutable tail = leftEqualities
    while continue' && not tail.IsEmpty do
      let (_, x) = tail.Head
      let result = lowTypeMatcher.Test right x state
      match result with
      | Matched ctx ->
        state <- ctx
        tail <- tail.Tail
      | _ -> continue' <- false
    if continue' then
      Matched state
    else
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
        testLeftEqualities lowTypeMatcher leftEqualities right ctx
        |> MatchingResult.bindMatched (Equations.tryAddEquality left right)
      Debug.WriteLine(
        match result with
        | Matched _ -> sprintf "It passed the test. The equality has been added.: \"%s\" = \"%s\"" (LowType.debug left) (LowType.debug right)
        | Continue _ | Failure -> sprintf "It failed to add the equality.: \"%s\" = \"%s\"" (LowType.debug left) (LowType.debug right))
      result

  type Swapped = bool

  let swappedToInt (swapped: Swapped) = if swapped then 1 else 0

  let contains (lowTypeMatcher: ILowTypeMatcher) (testee: LowType) (back: LowType[]) (forward: LowType[]) (ctx: Context) : option<Context * LowType[] * LowType[] * Swapped> =
    let test x ys = Array.indexed ys |> Array.tryPick (fun (i, y) -> match lowTypeMatcher.Test x y ctx with Matched ctx -> Some (i, ctx) | _ -> None)
    let pick i xs =
      let back = Array.take i xs
      let forward = Array.skip (i + 1) xs
      let value = Array.item i xs
      (value, back, forward)
    Debug.WriteLine(sprintf "Try find from forward: %A" (forward |> Array.map (fun x -> x.Debug())))
    match test testee forward with
    | Some (i, ctx) ->
      let _, f_back, f_forward = pick i forward
      let back = Array.append back f_back
      let forward = f_forward
      Debug.WriteLine("Found from forward.")
      Some (ctx, back, forward, false)
    | None ->
      Debug.WriteLine("Not found from forward.")
      Debug.WriteLine(sprintf "Try find from back: %A" (back |> Array.map (fun x -> x.Debug())))
      match test testee back with
      | Some (i, ctx) ->
        let _, b_back, b_forward = pick i back
        let forward = Array.append b_forward forward
        let back = b_back
        Debug.WriteLine("Found from back.")
        Some (ctx, back, forward, true)
      | None ->
        Debug.WriteLine("Not found from back.")
        None

  let containsWildcard xs = xs |> Array.exists (function Wildcard _ -> true | _ -> false)

  let testAllWithComplementAndSwap (lowTypeMatcher: ILowTypeMatcher) (complementNumberLimit: int) (swapNumberLimit: int) (leftTypes: LowType seq) (rightTypes: LowType seq) (ctx: Context) : MatchingResult =
    Debug.WriteLine(sprintf "Test %A and %A." (Seq.map LowType.debug leftTypes |> Seq.toList) (Seq.map LowType.debug rightTypes |> Seq.toList))
    let short, long =
      let s, l =
        if Seq.length leftTypes <= Seq.length rightTypes then
          leftTypes, rightTypes
        else
          rightTypes, leftTypes
      (Array.ofSeq s, Array.ofSeq l)

    let complementNumber = long.Length - short.Length
    if complementNumber > complementNumberLimit then
      Failure
    elif short.Length <> long.Length && containsWildcard long then
      Debug.WriteLine("There is a wildcard.")
      Failure
    else
      let mutable continue' = true
      let mutable state = ctx, Array.empty, long, 0
      let mutable shortIndex = 0
      while continue' && shortIndex < short.Length do
        let testee = short.[shortIndex]
        shortIndex <- shortIndex + 1
        let ctx, back, forward, swapNumber = state
        Debug.WriteLine(sprintf "Test %s" (testee.Debug()))
        Debug.Indent()
        let result = contains lowTypeMatcher testee back forward ctx
        Debug.Unindent()
        match result with
        | Some (ctx, back, forward, swapped)  ->
          let swapNumber = swapNumber + swappedToInt swapped
          state <- ctx, back, forward, swapNumber
          continue' <- not (swapNumber > swapNumberLimit)
        | None ->
          continue' <- false
      if continue' then
        let ctx, _, _, swapNumber = state
        Matched (ctx |> Context.addDistance "swap and complement" (swapNumber + complementNumber))
      else
        Failure

  let testAllExactly (lowTypeMatcher: ILowTypeMatcher) (leftTypes: LowType seq) (rightTypes: LowType seq) (ctx: Context): MatchingResult =
    Debug.WriteLine(sprintf "Test %A and %A." (Seq.map LowType.debug leftTypes |> Seq.toList) (Seq.map LowType.debug rightTypes |> Seq.toList))
    if Seq.length leftTypes <> Seq.length rightTypes then
      Debug.WriteLine("The numbers of the parameters are different.")
      Failure
    else
      let mutable continue' = true
      let mutable state = ctx
      let leftEnum = leftTypes.GetEnumerator()
      let rightEnum = rightTypes.GetEnumerator()
      while continue' && leftEnum.MoveNext() && rightEnum.MoveNext() do
        let left = leftEnum.Current
        let right = rightEnum.Current
        Debug.WriteLine(sprintf "Test %s and %s." (LowType.debug left) (LowType.debug right))
        Debug.Indent()
        let result = lowTypeMatcher.Test left right state
        Debug.Unindent()
        match result with
        | Matched ctx -> state <- ctx
        | _ -> continue' <- false
      if continue' then
        Matched state
      else
        Failure

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

  let testIdentity (nameEquality: Identity.Equality) leftIdentity rightIdentity ctx =
    match nameEquality leftIdentity rightIdentity with
    | Identity.IdentityEqualityResult.Matched ->
      Debug.WriteLine("There are same identities.")
      Matched ctx
    | failed ->
      Debug.WriteLine(sprintf "There are deferent identities. The reason is %A" failed)
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
    | Arrow (ps, ret) -> seqDistance ps + distanceFromVariable ret
    | Tuple _ -> 1
    | Generic _ -> 1
    | TypeAbbreviation x -> distanceFromVariable x.Original
    | Delegate _ -> 1
    | ByRef _ -> 1
    | LowType.Subtype _ -> 0
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
        |> MatchingResult.mapMatched (Context.addDistance "greedy variable" (distanceFromVariable other))
    | _ -> Continue ctx

  let tupleRule (lowTypeMatcher: ILowTypeMatcher) left right ctx =
    match left, right with
    | Tuple left, Tuple right ->
      Debug.WriteLine("tuple rule.")
      lowTypeMatcher.TestAll left.Elements right.Elements ctx
      |> MatchingResult.mapMatched (Context.addDistance "tuple type difference" (if left.IsStruct <> right.IsStruct then 1 else 0))
    | Tuple tuple, other
    | other, Tuple tuple ->
      Debug.WriteLine("tuple rule.")
      let other = [ other ]
      lowTypeMatcher.TestAll tuple.Elements other ctx
    | _ -> Continue ctx

  let testArrow (lowTypeMatcher: ILowTypeMatcher) leftElems rightElems ctx =
    lowTypeMatcher.TestArrow leftElems rightElems ctx

  let arrowRule lowTypeMatcher left right ctx =
    match left, right with
    | Arrow leftElems, Arrow rightElems ->
      Debug.WriteLine("arrow rule.")
      testArrow lowTypeMatcher leftElems rightElems ctx
    | _ -> Continue ctx

  let testArrow_IgnoreParameterStyle (lowTypeMatcher: ILowTypeMatcher) (left: Arrow) (right: Arrow) ctx =
    match left, right with
    | ([ _ ], _), ([ _ ], _) ->
      lowTypeMatcher.TestArrow left right ctx
    | ([ Tuple { Elements = leftArgs } ], leftRet), _ ->
      let left = leftArgs, leftRet
      lowTypeMatcher.TestArrow left right ctx
      |> MatchingResult.mapMatched (Context.addDistance "parameter style" 1)
    | _, ([ Tuple { Elements = rightArgs } ], rightRet) ->
      let right = rightArgs, rightRet
      lowTypeMatcher.TestArrow left right ctx
      |> MatchingResult.mapMatched (Context.addDistance "parameter style" 1)
    | _, _ ->
      lowTypeMatcher.TestArrow left right ctx

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
      lowTypeMatcher.Test leftId rightId ctx
      |> MatchingResult.bindMatched (lowTypeMatcher.TestAllExactly leftArgs rightArgs)
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
      lowTypeMatcher.Test leftId rightId ctx
      |> MatchingResult.bindMatched (lowTypeMatcher.TestAllExactly leftArgs rightArgs)
    | _ -> Continue ctx

  let delegateAndArrowRule lowTypeMatcher left right ctx =
    match left, right with
    | Delegate (_, leftElems), Arrow rightElems
    | Arrow leftElems, Delegate (_, rightElems) ->
      Debug.WriteLine("delegate and arrow rule.")
      testArrow lowTypeMatcher leftElems rightElems ctx
      |> MatchingResult.mapMatched (Context.addDistance "delegate and arrow" 1)
    | _ -> Continue ctx

  let delegateAndArrowRule_IgnoreParameterStyle lowTypeMatcher left right ctx =
    match left, right with
    | Delegate (_, leftElems), Arrow rightElems
    | Arrow leftElems, Delegate (_, rightElems) ->
      Debug.WriteLine("delegate and arrow rule (ignore parameter style).")
      testArrow_IgnoreParameterStyle lowTypeMatcher leftElems rightElems ctx
      |> MatchingResult.mapMatched (Context.addDistance "delegate and arrow" 1)
    | _ -> Continue ctx

  let byrefRule (lowTypeMatcher: ILowTypeMatcher) left right ctx =
    match left, right with
    | ByRef (_, left), ByRef (_, right) ->
      Debug.WriteLine("byref rule.")
      lowTypeMatcher.Test left right ctx
    | ByRef (_, left), right
    | left, ByRef (_, right) ->
      Debug.WriteLine("byref rule (byref and type).")
      lowTypeMatcher.Test left right ctx
      |> MatchingResult.mapMatched (Context.addDistance "byref and type" 1)
    | _ -> Continue ctx

  let rec subtypeTarget ctx = function
    | Identity id -> Some (id, [])
    | Generic (Identity id, args) -> Some (id, args)
    | ByRef _ as t -> subtypeTarget ctx t
    | Delegate (t, _) -> subtypeTarget ctx t
    | LowType.Subtype t -> subtypeTarget ctx t
    | TypeAbbreviation _ as t -> (|AbbreviationRoot|_|) t |> Option.bind (subtypeTarget ctx)
    | Generic _ | Variable _ | Wildcard _ | Tuple _ | Arrow _ | Choice _ -> None

  let (|SubtypeTarget|_|) ctx = subtypeTarget ctx

  let testSubtype (lowTypeMatcher: ILowTypeMatcher) baseType targetId targetArgs ctx =
    let targetIdDef = TypeHierarchy.fullTypeDef ctx targetId
    targetIdDef
    |> Seq.collect (fun td -> TypeHierarchy.getSuperTypes ctx td targetArgs)
    |> Seq.tryPick (fun target ->
      match lowTypeMatcher.Test baseType target ctx with
      | Matched ctx -> Some (ctx, target)
      | _ -> None
    )

  let subtypeCacheValue contextualType (lowTypeMatcher: ILowTypeMatcher) baseType targetId targetArgs ctx =
    testSubtype lowTypeMatcher baseType targetId targetArgs ctx
    |> function
      | Some (_, target) ->
        if contextualType() then
          Contextual (Some target)
        else
          Subtype target
      | None ->
        if contextualType() then
          Contextual None
        else
          NonSubtype

  let subtypeRule isContextual (lowTypeMatcher: ILowTypeMatcher) left right ctx =
    match left, right with
    | LowType.Subtype baseType, target
    | target, LowType.Subtype baseType ->
      match target with
      | SubtypeTarget ctx (targetId, targetArgs) ->
        Debug.WriteLine("subtype rule.")

        let valueFactory _ =
          let contextualType() = isContextual baseType || isContextual target
          subtypeCacheValue contextualType lowTypeMatcher baseType targetId targetArgs ctx

        let key = (baseType, target)
        let result = ctx.SubtypeCache.GetOrAdd(key, valueFactory)
        match result with
        | Subtype target -> lowTypeMatcher.Test baseType target ctx
        | NonSubtype -> Failure
        | Contextual None ->
          match testSubtype lowTypeMatcher baseType targetId targetArgs ctx with
          | Some (ctx, target) ->
            ctx.SubtypeCache.TryUpdate(key, (Contextual (Some target)), result) |> ignore
            Matched ctx
          | None -> Failure
        | Contextual (Some target) -> lowTypeMatcher.Test baseType target ctx

      | _ -> Continue ctx

    | _ -> Continue ctx

let instance options =
  let nameEquality = Identity.equalityFromOptions options

  let isContextual =
    let f =
      match options.GreedyMatching with
      | Enabled -> LowType.collectVariableOrWildcardGroup
      | Disabled -> LowType.collectWildcardGroup
    f >> List.isEmpty >> not

  let rule =
    Rule.compose [|
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

      yield Rules.byrefRule
      yield Rules.subtypeRule isContextual

      yield Rule.terminator
    |]

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
      member this.TestAll left right ctx = Rules.testAllWithComplementAndSwap this options.ComplementDepth options.SwapOrderDepth left right ctx
      member this.TestAllExactly left right ctx = Rules.testAllExactly this left right ctx}