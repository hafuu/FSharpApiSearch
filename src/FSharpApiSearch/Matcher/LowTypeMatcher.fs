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

  let testAll (lowTypeMatcher: ILowTypeMatcher) (leftTypes: LowType seq) (rightTypes: LowType seq) (ctx: Context) =
    Debug.WriteLine(sprintf "Test %A and %A." (Seq.map LowType.debug leftTypes |> Seq.toList) (Seq.map LowType.debug rightTypes |> Seq.toList))
    if Seq.length leftTypes <> Seq.length rightTypes then
      Debug.WriteLine("The numbers of the parameters are different.")
      Failure
    else
      Seq.zip leftTypes rightTypes
      |> Seq.fold (fun result (left, right) ->
        Debug.WriteLine(sprintf "Test %s and %s." (LowType.debug left) (LowType.debug right))
        Debug.Indent()
        let result = MatchingResult.bindMatched (lowTypeMatcher.Test left right) result
        Debug.Unindent()
        result
      ) (Matched ctx)

  let typeAbbreviationRule (lowTypeMatcher: ILowTypeMatcher) left right ctx =
    match left, right with
    | (TypeAbbreviation abbreviation), other
    | other, (TypeAbbreviation abbreviation) ->
      Debug.WriteLine("type abbreviation rule.")
      Debug.WriteLine(sprintf "(%s) -> (%s)" (LowType.debug abbreviation.Abbreviation) (LowType.debug abbreviation.Original))
      lowTypeMatcher.Test abbreviation.Original other ctx
    | _ -> Continue ctx

  let identityRule _ left right ctx =
    match left, right with
    | Identity leftIdentity, Identity rightIdentity ->
      Debug.WriteLine("identity rule.")
      if Identity.sameName leftIdentity rightIdentity then
        Debug.WriteLine("There are same identities.")
        Matched ctx
      else
        Debug.WriteLine("There are deferent identities.")
        Failure
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

  let tupleRule lowTypeMatcher left right ctx =
    match left, right with
    | Tuple leftElems, Tuple rightElems ->
      Debug.WriteLine("tuple rule.")
      testAll lowTypeMatcher leftElems rightElems ctx
    | _ -> Continue ctx

  let arrowRule lowTypeMatcher left right ctx =
    match left, right with
    | Arrow leftElems, Arrow rightElems ->
      Debug.WriteLine("arrow rule.")
      testAll lowTypeMatcher leftElems rightElems ctx
    | _ -> Continue ctx

  let arrowRule_IgnoreParameterStyle lowTypeMatcher left right ctx =
    match left, right with
    | Arrow leftElems, Arrow rightElems ->
      Debug.WriteLine("arrow rule (ignore parameter style).")
      match leftElems, rightElems with
      | [ Tuple _; _ ], [ Tuple _; _ ]
      | [ Wildcard _; _ ], _
      | _, [ Wildcard _; _ ] ->
        testAll lowTypeMatcher leftElems rightElems ctx
      | [ Tuple leftArgs; leftRet ], _ ->
        let leftElems = seq { yield! leftArgs; yield leftRet }
        testAll lowTypeMatcher leftElems rightElems ctx
        |> MatchingResult.mapMatched (Context.addDistance 1)
      | _, [ Tuple rightArgs; rightRet ] ->
        let rightElems = seq { yield! rightArgs; yield rightRet }
        testAll lowTypeMatcher leftElems rightElems ctx
        |> MatchingResult.mapMatched (Context.addDistance 1)
      | _, _ -> testAll lowTypeMatcher leftElems rightElems ctx 
    | _ -> Continue ctx

  let genericRule lowTypeMatcher left right ctx =
    match left, right with
    | Generic (leftId, leftArgs), Generic (rightId, rightArgs) ->
      Debug.WriteLine("generic rule.")
      testAll lowTypeMatcher (leftId :: leftArgs) (rightId :: rightArgs) ctx
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

let instance options =
  let rule =
    Rule.compose [
      yield Rules.typeAbbreviationRule
      yield Rules.wildcardGroupRule
      yield Rules.wildcardRule
    
      match options.GreedyMatching with
      | Enabled -> yield Rules.greedyVariableRule
      | Disabled -> yield Rules.variableRule

      yield Rules.identityRule
      yield Rules.tupleRule
      yield Rules.genericRule

      match options.IgnoreParameterStyle with
      | Enabled -> yield Rules.arrowRule_IgnoreParameterStyle
      | Disabled -> yield Rules.arrowRule
        
      yield Rule.terminator
    ]
  { new ILowTypeMatcher with member this.Test left right ctx = Rule.run rule this left right ctx }