module internal FSharpApiSearch.LowTypeMatcher

open FSharpApiSearch.EngineTypes
open FSharpApiSearch.StringPrinter
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
    EngineDebug.WriteLine(sprintf "Test inequality between %s and %s." (LowType.debug left) (LowType.debug right))
    if containsInequality left right eqs then
      EngineDebug.WriteLine("The inequality exists.")
      false
    else
      let result =
        let target x y = if x = left then Some y elif y = left then Some x else None
        let rec loop = function
          | [] -> true
          | (x, y) as inequality :: rest ->
            match target x y with
            | None -> loop rest
            | Some inequalityTerm ->
              let xy = sortTerm right inequalityTerm
              match eqs.Equalities |> List.tryFind ((=)xy) with
              | Some equality ->
                EngineDebug.WriteLine(
                  sprintf "The inequality between %s and %s exists. It was drived from the following equations. : [ %s, %s ]"
                    (LowType.debug left)
                    (LowType.debug right)
                    (Equations.debugInequality inequality)
                    (Equations.debugEquality equality))
                false
              | None -> true
        loop eqs.Inequalities
      EngineDebug.WriteLine("It passed the inequality test.")
      result

  let isRecirsive left right =
    match left, right with
    | (Variable _ as variable), other
    | other, (Variable _ as variable) ->
      if LowType.collectVariableOrWildcardGroup other |> Array.exists ((=)variable) then
        EngineDebug.WriteLine(sprintf "It is the recursive type.")
        true
      else
        false
    | _ -> false
      
  let isCircular left right eqs =
    match left, right with
    | (Variable _ as variable), other
    | other, (Variable _ as variable) ->
      let oneSteps = seq {
        for ((x, y) as xy) in eqs.Equalities do
          if xy <> (variable, other) && xy <> (other, variable) then
            for otherVariable in LowType.collectVariableOrWildcardGroup other |> Array.distinct do
              if x = otherVariable then
                yield (y, (x, y))
              elif y = otherVariable then
                yield (x, (x, y))
      }
      oneSteps
      |> Seq.exists (fun (other, xy) ->
        let isCircular =  LowType.collectVariableOrWildcardGroup other |> Array.exists ((=)variable)
        if isCircular then EngineDebug.WriteLine(sprintf "It is the circular type. It was derived from the following equation. : %s" (Equations.debugEquality xy))
        isCircular)
    | _ -> false

  let tryAddEquality left right (ctx: Context) =
    let left, right = sortTerm left right
    if testInequality left right ctx.Equations then
      let eqs = ctx.Equations
      let newEqs = { eqs with Equalities = (left, right) :: eqs.Equalities }
      Matched (Context.setEquations newEqs ctx)
    else
      Failure FailureInfo.None

module MatchPositions =
  let update (left: LowType) (right: LowType) (ctx: Context) =
    match left, left.Position, right, right.Position with
    | queryType, AtQuery (Some queryPos, _), sigType, AtSignature sigPos
    | sigType, AtSignature sigPos, queryType, AtQuery (Some queryPos, _) ->
      EngineDebug.WriteLine(sprintf "QueryType:%s and SigType:%s are same positions. : (%d, %d)" (LowType.debug queryType) (LowType.debug sigType) queryPos.Id sigPos.Id)
      { ctx with MatchPositions = Map.add sigPos queryPos ctx.MatchPositions }
    | _ -> ctx

module Rules =
  let terminator (_: ILowTypeMatcher) (_: LowType) (_: LowType) (_: Context) =
    EngineDebug.WriteLine("It reached the terminator.")
    Failure FailureInfo.None

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
      Failure FailureInfo.None

  let testVariableEquality (lowTypeMatcher: ILowTypeMatcher) left right (ctx: Context) =
    let left, right = Equations.sortTerm left right
    EngineDebug.WriteLine(sprintf "Test equaliity of \"%s\" and \"%s\"." (LowType.debug left) (LowType.debug right))
    if Equations.isRecirsive left right then
      Failure FailureInfo.None
    elif Equations.isCircular left right ctx.Equations then
      Failure FailureInfo.None
    else
      let leftEqualities = ctx.Equations |> Equations.findEqualities left
      EngineDebug.WriteLine(
        match leftEqualities with
        | [] -> sprintf "It didn't find known equalities of \"%s\"." (LowType.debug left)
        | _ -> sprintf "It found known equalities of \"%s\". It begins the testing the \"%s\" and %A."
                (LowType.debug left)
                (LowType.debug right)
                (List.map Equations.debugEquality leftEqualities))
      let result =
        testLeftEqualities lowTypeMatcher leftEqualities right ctx
        |> MatchingResult.bindMatched (Equations.tryAddEquality left right)
      EngineDebug.WriteLine(
        match result with
        | Matched _ -> sprintf "It passed the test. The equality has been added.: \"%s\" = \"%s\"" (LowType.debug left) (LowType.debug right)
        | Continue | Failure _ -> sprintf "It failed to add the equality.: \"%s\" = \"%s\"" (LowType.debug left) (LowType.debug right))
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
    EngineDebug.WriteLine(sprintf "Try find from forward: %A" (forward |> Array.map (fun x -> x.Debug())))
    match test testee forward with
    | Some (i, ctx) ->
      let _, f_back, f_forward = pick i forward
      let back = Array.append back f_back
      let forward = f_forward
      EngineDebug.WriteLine("Found from forward.")
      Some (ctx, back, forward, false)
    | None ->
      EngineDebug.WriteLine("Not found from forward.")
      EngineDebug.WriteLine(sprintf "Try find from back: %A" (back |> Array.map (fun x -> x.Debug())))
      match test testee back with
      | Some (i, ctx) ->
        let _, b_back, b_forward = pick i back
        let forward = Array.append b_forward forward
        let back = b_back
        EngineDebug.WriteLine("Found from back.")
        Some (ctx, back, forward, true)
      | None ->
        EngineDebug.WriteLine("Not found from back.")
        None

  let containsWildcard xs = xs |> Array.exists (function Wildcard _ -> true | _ -> false)

  let testAllWithComplementAndSwap (lowTypeMatcher: ILowTypeMatcher) (complementNumberLimit: int) (swapNumberLimit: int) (leftTypes: LowType seq) (rightTypes: LowType seq) (ctx: Context) : MatchingResult =
    EngineDebug.WriteLine(sprintf "Test %A and %A." (Seq.map LowType.debug leftTypes |> Seq.toList) (Seq.map LowType.debug rightTypes |> Seq.toList))
    let short, long =
      let s, l =
        if Seq.length leftTypes <= Seq.length rightTypes then
          leftTypes, rightTypes
        else
          rightTypes, leftTypes
      (Array.ofSeq s, Array.ofSeq l)

    let complementNumber = long.Length - short.Length
    if complementNumber > complementNumberLimit then
      Failure FailureInfo.None
    elif short.Length <> long.Length && containsWildcard long then
      EngineDebug.WriteLine("There is a wildcard.")
      Failure FailureInfo.None
    else
      let mutable continue' = true
      let mutable state = ctx, Array.empty, long, 0
      let mutable shortIndex = 0
      while continue' && shortIndex < short.Length do
        let testee = short.[shortIndex]
        shortIndex <- shortIndex + 1
        let ctx, back, forward, swapNumber = state
        EngineDebug.WriteLine(sprintf "Test %s" (testee.Debug()))
        EngineDebug.Indent()
        let result = contains lowTypeMatcher testee back forward ctx
        EngineDebug.Unindent()
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
        Failure FailureInfo.None

  let testAllExactly (lowTypeMatcher: ILowTypeMatcher) (leftTypes: LowType seq) (rightTypes: LowType seq) (ctx: Context): MatchingResult =
    EngineDebug.WriteLine(sprintf "Test %A and %A." (Seq.map LowType.debug leftTypes |> Seq.toList) (Seq.map LowType.debug rightTypes |> Seq.toList))
    if Seq.length leftTypes <> Seq.length rightTypes then
      EngineDebug.WriteLine("The numbers of the parameters are different.")
      Failure FailureInfo.None
    else
      let mutable continue' = true
      let mutable state = ctx
      let leftEnum = leftTypes.GetEnumerator()
      let rightEnum = rightTypes.GetEnumerator()
      while continue' && leftEnum.MoveNext() && rightEnum.MoveNext() do
        let left = leftEnum.Current
        let right = rightEnum.Current
        EngineDebug.WriteLine(sprintf "Test %s and %s." (LowType.debug left) (LowType.debug right))
        EngineDebug.Indent()
        let result = lowTypeMatcher.Test left right state
        EngineDebug.Unindent()
        match result with
        | Matched ctx -> state <- ctx
        | _ -> continue' <- false
      if continue' then
        Matched state
      else
        Failure FailureInfo.None

  let testChoice (lowTypeMatcher: ILowTypeMatcher) (valueToFind: LowType) (choices: LowType list) ctx =
    let rec loop failureInfo = function
      | current :: rest ->
        match lowTypeMatcher.Test valueToFind current ctx with
        | Matched _ as matched -> matched
        | Failure info -> loop (info :: failureInfo) rest
        | Continue -> loop failureInfo rest
      | [] -> Failure (FailureInfo.Many failureInfo)
    loop [] choices

  let choiceRule (lowTypeMatcher: ILowTypeMatcher) left right ctx =
    match left, right with
    | Choice (_, choices, _), other
    | other, Choice (_, choices, _) ->
      EngineDebug.WriteLine("choice rule.")
      EngineDebug.WriteLine(sprintf "test %A and %s" (choices |> List.map (fun x -> x.Debug())) (other.Debug()))

      testChoice lowTypeMatcher other choices ctx
    | _ -> Continue

  let typeAbbreviationRule (lowTypeMatcher: ILowTypeMatcher) left right ctx =
    match left, right with
    | (TypeAbbreviation (abbreviation, _)), other
    | other, (TypeAbbreviation (abbreviation, _)) ->
      EngineDebug.WriteLine("type abbreviation rule.")
      EngineDebug.WriteLine(sprintf "(%s) -> (%s)" (LowType.debug abbreviation.Abbreviation) (LowType.debug abbreviation.Original))
      lowTypeMatcher.Test abbreviation.Original other ctx
    | _ -> Continue

  let testTypeInfo (nameEquality: TypeNameEquality.Equality) (left: Identifier) (right: Identifier) ctx =
    match nameEquality left right with
    | Ok distance ->
      EngineDebug.WriteLine("There are same type.")
      Matched (ctx |> Context.addDistance "substring matching of type name" distance)
    | failed ->
      EngineDebug.WriteLine(sprintf "There are deferent type. The reason is %A" failed)
      Failure FailureInfo.None

  let typeInfoRule nameEquality _ left right ctx =
    match left, right with
    | Identifier (left, _), Identifier (right, _) ->
      EngineDebug.WriteLine("type info rule.")
      testTypeInfo nameEquality left right ctx
    | _ -> Continue

  let variableRule lowTypeMatcher left right ctx =
    match left, right with
    | Variable _, Variable _ ->
      EngineDebug.WriteLine("variable rule.")
      if Equations.containsEquality left right ctx.Equations then
        EngineDebug.WriteLine("The equality already exists.")
        Matched ctx
      else
        testVariableEquality lowTypeMatcher left right ctx
    | _ -> Continue

  let rec distanceFromVariable = function
    | Wildcard _ -> 0
    | Variable _ -> 0
    | Identifier _ -> 1
    | Arrow ((ps, ret), _) -> seqDistance ps + distanceFromVariable ret
    | Tuple _ -> 1
    | Generic _ -> 1
    | TypeAbbreviation (x, _) -> distanceFromVariable x.Original
    | Delegate _ -> 1
    | ByRef _ -> 1
    | LowType.Subtype _ -> 0
    | Choice _ -> 1
    | LoadingType _ -> Name.loadingNameError()
  and seqDistance xs = xs |> Seq.sumBy (distanceFromVariable >> max 1)

  let greedyVariableRule lowTypeMatcher left right ctx =
    match left, right with
    | (Variable _ as variable), other
    | other, (Variable _ as variable) ->
      EngineDebug.WriteLine("greedy variable rule.")
      if Equations.containsEquality variable other ctx.Equations then
        EngineDebug.WriteLine("The equality already exists.")
        Matched ctx
      else
        testVariableEquality lowTypeMatcher variable other ctx
        |> MatchingResult.mapMatched (Context.addDistance "greedy variable" (distanceFromVariable other))
    | _ -> Continue

  let tupleRule (lowTypeMatcher: ILowTypeMatcher) left right ctx =
    match left, right with
    | Tuple (left, _), Tuple (right, _) ->
      EngineDebug.WriteLine("tuple rule.")
      lowTypeMatcher.TestAll left.Elements right.Elements ctx
      |> MatchingResult.mapMatched (Context.addDistance "tuple type difference" (if left.IsStruct <> right.IsStruct then 1 else 0))
    | Tuple (tuple, _), other
    | other, Tuple (tuple, _) ->
      EngineDebug.WriteLine("tuple rule.")
      let other = [ other ]
      lowTypeMatcher.TestAll tuple.Elements other ctx
    | _ -> Continue

  let testArrow (lowTypeMatcher: ILowTypeMatcher) leftElems rightElems ctx =
    lowTypeMatcher.TestArrow leftElems rightElems ctx

  let arrowRule lowTypeMatcher left right ctx =
    match left, right with
    | Arrow (leftElems, _), Arrow (rightElems, _) ->
      EngineDebug.WriteLine("arrow rule.")
      testArrow lowTypeMatcher leftElems rightElems ctx
    | _ -> Continue

  let testArrow_IgnoreParameterStyle (lowTypeMatcher: ILowTypeMatcher) (left: Arrow) (right: Arrow) ctx =
    match left, right with
    | ([ _ ], _), ([ _ ], _) ->
      lowTypeMatcher.TestArrow left right ctx
    | ([ Tuple ({ Elements = leftArgs }, _) ], leftRet), _ ->
      let left = leftArgs, leftRet
      lowTypeMatcher.TestArrow left right ctx
      |> MatchingResult.mapMatched (Context.addDistance "parameter style" 1)
    | _, ([ Tuple ({ Elements = rightArgs }, _) ], rightRet) ->
      let right = rightArgs, rightRet
      lowTypeMatcher.TestArrow left right ctx
      |> MatchingResult.mapMatched (Context.addDistance "parameter style" 1)
    | _, _ ->
      lowTypeMatcher.TestArrow left right ctx

  let arrowRule_IgnoreParameterStyle lowTypeMatcher left right ctx =
    match left, right with
    | Arrow (leftElems, _), Arrow (rightElems, _) ->
      EngineDebug.WriteLine("arrow rule (ignore parameter style).")
      testArrow_IgnoreParameterStyle lowTypeMatcher leftElems rightElems ctx
    | _ -> Continue

  let genericRule (lowTypeMatcher: ILowTypeMatcher) left right ctx =
    match left, right with
    | Generic (leftId, leftArgs, _), Generic (rightId, rightArgs, _) ->
      EngineDebug.WriteLine("generic rule.")
      lowTypeMatcher.Test leftId rightId ctx
      |> MatchingResult.bindMatched (fun ctx ->
        match lowTypeMatcher.TestAllExactly leftArgs rightArgs ctx with
        | Failure _ -> Failure FailureInfo.GenericArgumentsMismatch
        | (Matched _ | Continue) as result -> result
      )
    | _ -> Continue

  let wildcardRule _ left right ctx =
    match left, right with
    | Wildcard (None, _), _
    | _, Wildcard (None, _) ->
      EngineDebug.WriteLine("wildcard rule.")
      Matched ctx
    | _ -> Continue

  let wildcardGroupRule lowTypeMatcher left right ctx =
    match left, right with
    | (Wildcard (Some _, _)), _
    | _, (Wildcard (Some _, _))->
      EngineDebug.WriteLine("wildcard group rule.")
      if Equations.containsEquality left right ctx.Equations then
        EngineDebug.WriteLine("The equality already exists.")
        Matched ctx
      else
        testVariableEquality lowTypeMatcher left right ctx
    | _ -> Continue

  let delegateRule nameEquality (lowTypeMatcher: ILowTypeMatcher) left right ctx =
    match left, right with
    | Delegate (Identifier (leftId, _), _, _), Delegate (Identifier (rightId, _), _, _)
    | Delegate (Identifier (leftId, _), _, _), Identifier (rightId, _)
    | Identifier (leftId, _), Delegate (Identifier (rightId, _), _, _) ->
      EngineDebug.WriteLine("deligate rule.")
      testTypeInfo nameEquality leftId rightId ctx
    | Delegate (Generic (leftId, leftArgs, _), _, _), Delegate (Generic (rightId, rightArgs, _), _, _)
    | Delegate (Generic (leftId, leftArgs, _), _, _), Generic (rightId, rightArgs, _)
    | Generic (leftId, leftArgs, _), Delegate (Generic (rightId, rightArgs, _), _, _) ->
      EngineDebug.WriteLine("generic delegate rule.")
      lowTypeMatcher.Test leftId rightId ctx
      |> MatchingResult.bindMatched (lowTypeMatcher.TestAllExactly leftArgs rightArgs)
    | _ -> Continue

  let delegateAndArrowRule lowTypeMatcher left right ctx =
    match left, right with
    | Delegate (_, leftElems, _), Arrow (rightElems, _)
    | Arrow (leftElems, _), Delegate (_, rightElems, _) ->
      EngineDebug.WriteLine("delegate and arrow rule.")
      testArrow lowTypeMatcher leftElems rightElems ctx
      |> MatchingResult.mapMatched (Context.addDistance "delegate and arrow" 1)
    | _ -> Continue

  let delegateAndArrowRule_IgnoreParameterStyle lowTypeMatcher left right ctx =
    match left, right with
    | Delegate (_, leftElems, _), Arrow (rightElems, _)
    | Arrow (leftElems, _), Delegate (_, rightElems, _) ->
      EngineDebug.WriteLine("delegate and arrow rule (ignore parameter style).")
      testArrow_IgnoreParameterStyle lowTypeMatcher leftElems rightElems ctx
      |> MatchingResult.mapMatched (Context.addDistance "delegate and arrow" 1)
    | _ -> Continue

  let byrefRule (lowTypeMatcher: ILowTypeMatcher) left right ctx =
    match left, right with
    | ByRef (_, left, _), ByRef (_, right, _) ->
      EngineDebug.WriteLine("byref rule.")
      lowTypeMatcher.Test left right ctx
    | ByRef (_, left, _), right
    | left, ByRef (_, right, _) ->
      EngineDebug.WriteLine("byref rule (byref and type).")
      lowTypeMatcher.Test left right ctx
      |> MatchingResult.mapMatched (Context.addDistance "byref and type" 1)
    | _ -> Continue

  let rec subtypeTarget ctx = function
    | Identifier (id, _) -> Some (id, [])
    | Generic (Identifier (id, _), args, _) -> Some (id, args)
    | ByRef _ as t -> subtypeTarget ctx t
    | Delegate (t, _, _) -> subtypeTarget ctx t
    | LowType.Subtype (t, _) -> subtypeTarget ctx t
    | TypeAbbreviation _ as t -> (|AbbreviationRoot|_|) t |> Option.bind (subtypeTarget ctx)
    | Generic _ | Variable _ | Wildcard _ | Tuple _ | Arrow _ | Choice _ -> None
    | LoadingType _ -> Name.loadingNameError()

  let (|SubtypeTarget|_|) ctx = subtypeTarget ctx

  let rec containsGenericArgumentsMismatch = function
    | FailureInfo.GenericArgumentsMismatch -> true
    | FailureInfo.Many xs -> List.exists containsGenericArgumentsMismatch xs
    | FailureInfo.None -> false

  let findBaseTypeInTarget dependsOnVariable (lowTypeMatcher: ILowTypeMatcher) baseType targetId targetArgs ctx =
    let targetIdDef = TypeHierarchy.fullTypeDef ctx targetId

    let targetBaseTypes = targetIdDef |> Seq.collect (fun td -> TypeHierarchy.getBaseTypes ctx td targetArgs)

    let mutable result = None
    let mutable found = false
    use iterator = targetBaseTypes.GetEnumerator()

    while (iterator.MoveNext() && not found) do
      let target = iterator.Current

      match lowTypeMatcher.Test baseType target ctx with
      | Matched _ ->
        result <- Some target
        found <- true
      | Failure failureInfo when dependsOnVariable && containsGenericArgumentsMismatch failureInfo ->
        result <- Some target
      | Failure _ | Continue -> ()

    result
    
  let testSubtype dependsOnVariable (lowTypeMatcher: ILowTypeMatcher) baseType targetId targetArgs ctx =
    match findBaseTypeInTarget dependsOnVariable lowTypeMatcher baseType targetId targetArgs ctx with
    | None -> SubtypeResult.NotSubtype
    | Some target ->
      if dependsOnVariable then
        SubtypeResult.MaybeSubtype target
      else
        SubtypeResult.Subtype target

  let subtypeRule dependsOnVariable (lowTypeMatcher: ILowTypeMatcher) left right ctx =
    match left, right with
    | LowType.Subtype (baseType, _), target
    | target, LowType.Subtype (baseType, _) ->
      match target with
      | SubtypeTarget ctx (targetId, targetArgs) ->
        EngineDebug.WriteLine("subtype rule.")

        let valueFactory _ = testSubtype (dependsOnVariable baseType || dependsOnVariable target) lowTypeMatcher baseType targetId targetArgs ctx

        let key = (baseType, target)
        let result = ctx.SubtypeCache.GetOrAdd(key, valueFactory)
        match result with
        | SubtypeResult.Subtype target | SubtypeResult.MaybeSubtype target -> lowTypeMatcher.Test baseType target ctx
        | SubtypeResult.NotSubtype -> Failure FailureInfo.None
      | _ -> Continue

    | _ -> Continue

let instance options =
  let nameEquality = TypeNameEquality.equalityFromOptions options

  let dependsOnVariable =
    let f =
      match options.GreedyMatching with
      | Enabled -> LowType.collectVariableOrWildcardGroup
      | Disabled -> LowType.collectWildcardGroup
    f >> Array.isEmpty >> not

  let rule =
    Rule.compose [|
      yield Rules.choiceRule
      
      yield Rules.wildcardGroupRule
      yield Rules.wildcardRule
    
      match options.GreedyMatching with
      | Enabled -> yield Rules.greedyVariableRule
      | Disabled -> yield Rules.variableRule

      yield Rules.typeInfoRule nameEquality
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
      yield Rules.subtypeRule dependsOnVariable

      yield Rules.typeAbbreviationRule

      yield Rule.terminator
    |]

  { new ILowTypeMatcher with
      member this.Test left right ctx =
        EngineDebug.WriteLine(sprintf "Test \"%s\" and \"%s\". Equations: %s"
          (left.Debug())
          (right.Debug())
          (Equations.debug ctx.Equations))
        EngineDebug.WriteLine(sprintf "Positions are left:%A right:%A" left.Position right.Position)
        EngineDebug.Indent()
        let result = Rule.run rule this left right ctx |> MatchingResult.mapMatched (MatchPositions.update left right)
        EngineDebug.Unindent()
        result
      member this.TestAll left right ctx = Rules.testAllWithComplementAndSwap this options.ComplementDepth options.SwapOrderDepth left right ctx
      member this.TestAllExactly left right ctx = Rules.testAllExactly this left right ctx}