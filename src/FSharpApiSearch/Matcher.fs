module FSharpApiSearch.Matcher

open System.Diagnostics

type Equations = {
  Equalities: (Signature * Signature) list
  Inequalities: (Signature * Signature) list
}

type Context = {
  Distance: int
  Equations: Equations
}

type Result =
  | Matched of Context
  | Continue of Context
  | Failure

type SignatureTestFunction = Signature -> Signature -> Context -> Result
type SignatureRule = SignatureTestFunction -> SignatureTestFunction

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Result =
  let inline bindContinue f = function Continue x -> f x | r -> r
  let inline bindMatched f = function Matched x -> f x | r -> r
  let inline mapMatched f = function Matched x -> Matched (f x) | Continue x -> Continue x | Failure -> Failure
  let inline continueFailure ctx = function Failure -> Continue ctx | x -> x

  let toBool = function Matched _ -> true | _ -> false
  let distance = function
    | Matched { Distance = d } -> d
    | _ -> 0

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module SignatureRule =
  let run (rule: SignatureRule) test left right ctx = rule test left right ctx
  let inline compose (xs: SignatureRule seq): SignatureRule =
    fun test left right ctx ->
      xs
      |> Seq.fold (fun result rule -> result |> Result.bindContinue (run rule test left right)) (Continue ctx)

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Context =
  let setEquations eqs ctx = { ctx with Equations = eqs }
  let addDistance x (ctx: Context) =
    let newDistance = ctx.Distance + x
    Debug.WriteLine(sprintf "Update distance from %d to %d" ctx.Distance newDistance)
    { ctx with Distance = newDistance }

  let initialize eqs = { Distance = 0; Equations = eqs }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Equations =
  let debugDisplayEquality (left, right) = Signature.debugDisplay left + " = " + Signature.debugDisplay right
  let debugDisplayInequality (left, right) = Signature.debugDisplay left + " <> " + Signature.debugDisplay right
  let debugDisplay x = sprintf "Equalities = %A, Inequalities = %A" (List.map debugDisplayEquality x.Equalities) (List.map debugDisplayInequality x.Inequalities)
  
  let sortTerm x y = if x <= y then (x, y) else (y, x)

  let containsEquality left right eqs =
    let x = sortTerm left right
    eqs.Equalities |> List.contains x

  let containsInequality left right eqs =
    let x = sortTerm left right
    eqs.Inequalities |> List.contains x

  let findEqualities left eqs = eqs.Equalities |> List.filter (fst >> ((=)left))

  let testInequality left right eqs =
    Debug.WriteLine(sprintf "Test inequality between %s and %s." (Signature.debugDisplay left) (Signature.debugDisplay right))
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
                (Signature.debugDisplay left)
                (Signature.debugDisplay right)
                (debugDisplayInequality inequality)
                (debugDisplayEquality equality))
            false
          | None -> true
        )
      Debug.WriteLine("It passed the inequality test.")
      result

  let isRecirsive left right =
    match left, right with
    | (Variable _ as variable), other
    | other, (Variable _ as variable) ->
      if Signature.collectVariableOrWildcardGroup other |> List.exists ((=)variable) then
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
          for otherVariable in Signature.collectVariableOrWildcardGroup other |> List.distinct do
            if x = otherVariable then
              yield (y, (x, y))
            elif y = otherVariable then
              yield (x, (x, y))
      }
      oneSteps
      |> Seq.exists (fun (other, xy) ->
        let isCircular =  Signature.collectVariableOrWildcardGroup other |> Seq.exists ((=)variable)
        if isCircular then Debug.WriteLine(sprintf "It is the circular type. It was derived from the following equation. : %s" (debugDisplayEquality xy))
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

  let empty = { Equalities = []; Inequalities = [] }

  let signatureFromQuery q =
    match q.Method with
    | ByName (_, SignatureQuery s) -> Some s
    | BySignature s -> Some s
    | _ -> None

  let variableGroup signature =
    signature
    |> Signature.collectVariables
    |> List.distinct
    |> List.groupBy (function Variable (_, n) | StrongVariable (_, n) -> n | _ -> "")

  let initialize query =
    match signatureFromQuery query with
    | Some querySig ->
      let groups = variableGroup querySig
      let equalities = [
        for (_, group) in groups do
          for x in group do
          for y in group do
            if x < y then yield (x, y)
      ]
      { Equalities = equalities; Inequalities = [] }
    | None -> { Equalities = []; Inequalities = [] }

  let strictVariables query x =
    match signatureFromQuery query with
    | Some querySig ->
      let inequalities = [
        let variableGroup = variableGroup querySig
        for (xname, xs) in variableGroup do
          for (yname, ys) in variableGroup do
            if xname <> yname then
              for x in xs do
                for y in ys do
                  let left, right = if x < y then (x, y) else (y, x)
                  yield (left, right)

        let wildcards = Signature.collectWildcardGroup querySig |> List.distinct
        for x in wildcards do
          for y in wildcards do
            if x < y then yield (x, y)
      ]
      { x with Inequalities = List.distinct inequalities }
    | None -> x

module SignatureRules =
  let tryAddEquality test left right (ctx: Context) =
    let left, right = Equations.sortTerm left right
    Debug.WriteLine(sprintf "Test equaliity of \"%s\" and \"%s\"." (Signature.debugDisplay left) (Signature.debugDisplay right))
    if Equations.isRecirsive left right then
      Failure
    elif Equations.isCircular left right ctx.Equations then
      Failure
    else
      let leftEqualities = ctx.Equations |> Equations.findEqualities left
      Debug.WriteLine(
        match leftEqualities with
        | [] -> sprintf "It didn't find known equalities of \"%s\"." (Signature.debugDisplay left)
        | _ -> sprintf "It found known equalities of \"%s\". It begins the testing the \"%s\" and %A."
                (Signature.debugDisplay left)
                (Signature.debugDisplay right)
                (List.map Equations.debugDisplayEquality leftEqualities))
      let result =
        leftEqualities
        |> List.fold (fun result (_, x) -> Result.bindMatched (test right x) result) (Matched ctx)
        |> Result.bindMatched (Equations.tryAddEquality left right)
      Debug.WriteLine(
        match result with
        | Matched _ -> sprintf "It passed the test. The equality has been added.: \"%s\" = \"%s\"" (Signature.debugDisplay left) (Signature.debugDisplay right)
        | Continue _ | Failure -> sprintf "It failed to add the equality.: \"%s\" = \"%s\"" (Signature.debugDisplay left) (Signature.debugDisplay right))
      result

  let testAll test (leftTypes: Signature list) (rightTypes: Signature list) (ctx: Context) =
    Debug.WriteLine(sprintf "Test %A and %A." (List.map Signature.debugDisplay leftTypes) (List.map Signature.debugDisplay rightTypes))
    if leftTypes.Length <> rightTypes.Length then
      Debug.WriteLine("The number of the parameters is different.")
      Failure
    else
      List.zip leftTypes rightTypes
      |> List.fold (fun result (left, right) -> Result.bindMatched (test left right) result) (Matched ctx)

  let addLast x xs = [ yield! xs; yield x ]

  let terminator (_: SignatureTestFunction) (_: Signature) (_: Signature) (_: Context) = Failure

  [<RequireQualifiedAccess>]
  module Patterns =
    let (|Identity'|_|) = function
      | Identity n -> Some n
      | StrongIdentity n -> Some n
      | _ -> None

    let (|Variable'|_|) = function
      | Variable (s, n) -> Some (s, n)
      | StrongVariable (s, n) -> Some (s, n)
      | _ -> None

    let (|NonVariable|_|) = function
      | Variable _ -> None
      | StrongVariable _ -> None
      | _ -> Some ()

    let (|QueryInstanceMember|_|) = function
      | InstanceMember ({ Source = Source.Query} as x) -> Some x
      | _ -> None

    let (|NoArguments|_|) (x: InstanceMemberInfo) =
      match x with
      | { Arguments = [] } -> Some ()
      | _ -> None

    let (|Unit'|_|) = function
      | Identity' "unit" -> Some ()
      | _ -> None

    let (|OnlyUnitArgument|_|) (x: InstanceMemberInfo) =
      match x with
      | { Arguments = [ Unit' ] } -> Some ()
      | _ -> None


  let identityRule _ left right ctx =
    match left, right with
    | Patterns.Identity' leftName, Patterns.Identity' rightName ->
      Debug.WriteLine("identity type")
      if leftName = rightName then
        Debug.WriteLine("There are same identities.")
        Matched ctx
      else
        Debug.WriteLine("There are deferent identities.")
        Failure
    | StrongIdentity _, _
    | _, StrongIdentity _ ->
      Debug.WriteLine("Strong identity matches only with identity.")
      Failure
    | _ -> Continue ctx

  let hardVariableRule test left right ctx =
    match left, right with
    | Patterns.Variable' _, Patterns.Variable' _ ->
      Debug.WriteLine("both variable")
      if Equations.containsEquality left right ctx.Equations then
        Debug.WriteLine("The equality already exists.")
        Matched ctx
      else
        tryAddEquality test left right ctx
    | _ -> Continue ctx

  let rec distanceFromVariable = function
    | Wildcard -> 0
    | WildcardGroup _ -> 0
    | Identity _ -> 1
    | StrongIdentity _ -> 1
    | Variable _ -> 0
    | StrongVariable _ -> 0
    | Generic _ -> 1
    | Arrow xs -> seqDistance xs
    | StaticMethod x -> seqDistance (x.ReturnType :: x.Arguments)
    | InstanceMember x -> seqDistance (x.Receiver :: x.ReturnType :: x.Arguments)
  and seqDistance xs = xs |> Seq.sumBy (distanceFromVariable >> max 1)

  let similarityVariableRule test left right ctx =
    match left, right with
    | StrongVariable _, Patterns.NonVariable
    | Patterns.NonVariable, StrongVariable _ ->
      Debug.WriteLine("Strong variable matches only with variable.")
      Failure
    | (Patterns.Variable' _ as left), right
    | right, (Patterns.Variable' _ as left) ->
      Debug.WriteLine("variable")
      if Equations.containsEquality left right ctx.Equations then
        Debug.WriteLine("The equality already exists.")
        Matched ctx
      else
        tryAddEquality test left right ctx
        |> Result.mapMatched (Context.addDistance (distanceFromVariable right))
    | _ -> Continue ctx

  let genericRule test left right ctx =
    match left, right with
    | Generic (leftId, leftParams), Generic (rightId, rightParams) ->
      Debug.WriteLine("generic type")
      testAll test (leftId :: leftParams) (rightId :: rightParams) ctx
    | _ -> Continue ctx

  let arrowRule test left right ctx =
    match left, right with
    | Arrow leftTypes, Arrow rightTypes ->
      Debug.WriteLine("arrow type")
      testAll test leftTypes rightTypes ctx
    | _ -> Continue ctx

  let staticMethodRule test left right ctx =
    match left, right with
    | Arrow arrowTypes, StaticMethod (staticMethod)
    | StaticMethod (staticMethod), Arrow arrowTypes ->
      Debug.WriteLine("static method and arrow")
      testAll test arrowTypes (addLast staticMethod.ReturnType staticMethod.Arguments) ctx
    | StaticMethod (leftStaticMethod), StaticMethod (rightStaticMethod) ->
      Debug.WriteLine("both static method")
      testAll test (leftStaticMethod.ReturnType :: leftStaticMethod.Arguments) (rightStaticMethod.ReturnType :: rightStaticMethod.Arguments) ctx
    | _ -> Continue ctx

  let instanceFieldOrPropertyAndOnlyUnitArgumentMemberRule test left right ctx =
    match left, right with
    | (Patterns.QueryInstanceMember (queryMember & Patterns.NoArguments)), (InstanceMember (otherMember & Patterns.OnlyUnitArgument))
    | (InstanceMember (otherMember & Patterns.OnlyUnitArgument)), (Patterns.QueryInstanceMember (queryMember & Patterns.NoArguments)) ->
      testAll test [ queryMember.Receiver; queryMember.ReturnType ] [ otherMember.Receiver; otherMember.ReturnType ] ctx
      |> Result.mapMatched (Context.addDistance 1)
      |> Result.continueFailure ctx
    | _ -> Continue ctx

  let instanceMemberAndArrowRule test left right ctx =
    match left, right with
    | (Patterns.QueryInstanceMember queryMember), Arrow arrow
    | Arrow arrow, (Patterns.QueryInstanceMember queryMember) ->
      let xs = [
        match queryMember with
        | Patterns.NoArguments -> ()
        | Patterns.OnlyUnitArgument -> ()
        | _ -> yield! queryMember.Arguments
        yield queryMember.Receiver
        yield queryMember.ReturnType
      ]
      testAll test xs arrow ctx
      |> Result.mapMatched (Context.addDistance 1)
      |> Result.continueFailure ctx
    | _ -> Continue ctx

  let instanceMemberRule test left right ctx =
    match left, right with
    | InstanceMember leftMember, InstanceMember rightMember ->
      testAll test (leftMember.Receiver :: leftMember.ReturnType :: leftMember.Arguments) (rightMember.Receiver :: rightMember.ReturnType :: rightMember.Arguments) ctx
    | _ -> Continue ctx

  let wildcardRule _ left right ctx =
    match left, right with
    | Wildcard, _ | _, Wildcard ->
      Debug.WriteLine("wildcard")
      Matched ctx
    | _ -> Continue ctx

  let wildcardGroupRule test left right ctx =
    match left, right with
    | (WildcardGroup _ as left), right
    | right, (WildcardGroup _ as left) ->
      Debug.WriteLine("either wildcard group or other")
      if Equations.containsEquality left right ctx.Equations then
        Debug.WriteLine("The equality already exists.")
        Matched ctx
      else
        tryAddEquality test left right ctx
    | _ -> Continue ctx

let defaultRule =
  SignatureRule.compose [
    SignatureRules.wildcardRule
    SignatureRules.wildcardGroupRule
    SignatureRules.identityRule
    SignatureRules.hardVariableRule
    SignatureRules.genericRule
    SignatureRules.arrowRule
    SignatureRules.staticMethodRule
    SignatureRules.instanceMemberAndArrowRule
    SignatureRules.instanceFieldOrPropertyAndOnlyUnitArgumentMemberRule
    SignatureRules.instanceMemberRule
    SignatureRules.terminator
  ]

let similaritySearchingRule =
  SignatureRule.compose [
    SignatureRules.wildcardRule
    SignatureRules.wildcardGroupRule
    SignatureRules.identityRule
    SignatureRules.similarityVariableRule
    SignatureRules.genericRule
    SignatureRules.arrowRule
    SignatureRules.staticMethodRule
    SignatureRules.instanceMemberAndArrowRule
    SignatureRules.instanceFieldOrPropertyAndOnlyUnitArgumentMemberRule
    SignatureRules.instanceMemberRule
    SignatureRules.terminator
  ]

let testSignature rule left right ctx =
  let rec test left right ctx =
    Debug.WriteLine(sprintf "Test signature \"%s\" and \"%s\". Equations: %s"
      (Signature.debugDisplay left)
      (Signature.debugDisplay right)
      (Equations.debugDisplay ctx.Equations))
    Debug.Indent()
    let result = SignatureRule.run rule test left right ctx
    Debug.Unindent()
    result
  test left right ctx

let testName queryName targetApi =
  let targetName = targetApi.Name.Split('.') |> Seq.last
  queryName = targetName

let matches query targetApi rule ctx =
  let result =
    match query.Method with
    | ByName (name, _) when testName name targetApi = false -> Failure
    | ByName (_, AnySignature) -> Matched ctx
    | ByName (_, SignatureQuery signature) 
    | BySignature signature -> testSignature rule signature targetApi.Signature ctx
  result