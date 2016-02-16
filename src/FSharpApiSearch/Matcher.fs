module FSharpApiSearch.Matcher

open System.Diagnostics
open FSharpApiSearch.SpecialTypes

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

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module MatchingResult =
  let inline bindContinue f = function Continue x -> f x | r -> r
  let inline bindMatched f = function Matched x -> f x | r -> r
  let inline mapMatched f = function Matched x -> Matched (f x) | Continue x -> Continue x | Failure -> Failure

  let toBool = function Matched _ -> true | _ -> false

type ILowTypeMatcher =
  abstract Test: LowType -> LowType -> Context -> MatchingResult

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

module LowTypeMatcher =
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
        |> Seq.fold (fun result (left, right) -> MatchingResult.bindMatched (lowTypeMatcher.Test left right) result) (Matched ctx)

    let typeAbbreviationRule (lowTypeMatcher: ILowTypeMatcher) left right ctx =
      match left, right with
      | (TypeAbbreviation abbreviation), other
      | other, (TypeAbbreviation abbreviation) ->
        Debug.Write("type abbreviation rule.")
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

    let similarityVariableRule lowTypeMatcher left right ctx =
      match left, right with
      | (Variable _ as variable), other
      | other, (Variable _ as variable) ->
        Debug.WriteLine("similarity variable rule.")
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

  let instance searchOption =
    let rule =
      Rule.compose [
        yield Rules.typeAbbreviationRule
        yield Rules.wildcardGroupRule
        yield Rules.wildcardRule
    
        match searchOption with
        | { SimilaritySearching = Enabled } -> yield Rules.similarityVariableRule
        | { SimilaritySearching = Disabled } -> yield Rules.variableRule

        yield Rules.identityRule
        yield Rules.tupleRule
        yield Rules.genericRule
        yield Rules.arrowRule
        yield Rule.terminator
      ]
    { new ILowTypeMatcher with member this.Test left right ctx = Rule.run rule this left right ctx }

module PublishedApi =
  let test (api: Api) ctx =
    match api.Signature with
    | ApiSignature.FullTypeDefinition _ -> Failure
    | ApiSignature.TypeAbbreviation _ -> Failure
    | _ -> Matched ctx

  let instance options =
    { new IApiMatcher with
        member this.Name = "Published Api Matcher"
        member this.Test lowTypeMatcher query api ctx = test api ctx }

module NameMatcher =
  let test query (api: Api) ctx =
    match query with
    | QueryMethod.ByName (expectedName, _) ->
      if expectedName = api.Name.Head then
        Matched ctx
      else
        Failure
    | _ -> Matched ctx
  let instance options =
    { new IApiMatcher with
        member this.Name = "Name Matcher"
        member this.Test lowTypeMatcher query api ctx = test query api ctx }

module SignatureMatcher =
  module Rules =
    let testAll (lowTypeMatcher: ILowTypeMatcher) (leftTypes: LowType seq) (rightTypes: LowType seq) (ctx: Context) =
      Debug.WriteLine(sprintf "Test %A and %A." (Seq.map LowType.debug leftTypes |> Seq.toList) (Seq.map LowType.debug rightTypes |> Seq.toList))
      if Seq.length leftTypes <> Seq.length rightTypes then
        Debug.WriteLine("The numbers of the parameters are different.")
        Failure
      else
        Seq.zip leftTypes rightTypes
        |> Seq.fold (fun result (left, right) -> MatchingResult.bindMatched (lowTypeMatcher.Test left right) result) (Matched ctx)
  
    let moduleValueRule (lowTypeMatcher: ILowTypeMatcher) (left: SignatureQuery) (right: ApiSignature) ctx =
      match left, right with
      | SignatureQuery.Signature (Arrow _), ApiSignature.ModuleValue _ -> Continue ctx
      | SignatureQuery.Signature left, ApiSignature.ModuleValue right ->
        Debug.WriteLine("module value rule.")
        lowTypeMatcher.Test left right ctx
      | _ -> Continue ctx

    let moduleFunctionRule (lowTypeMatcher: ILowTypeMatcher) (left: SignatureQuery) (right: ApiSignature) ctx =
      match left, right with
      | SignatureQuery.Signature (Arrow _ as left), ApiSignature.ModuleFunction xs ->
        let right = Arrow xs
        Debug.WriteLine("module function rule.")
        lowTypeMatcher.Test left right ctx
      | _ -> Continue ctx

    let breakArrow = function
      | Arrow xs -> xs
      | other -> [ other ]

    let staticMemberRule (lowTypeMatcher: ILowTypeMatcher) (left: SignatureQuery) (right: ApiSignature) ctx =
      match left, right with
      | SignatureQuery.Signature left, ApiSignature.StaticMember (_, member') ->
        Debug.WriteLine("static member rule.")
        let leftElems = breakArrow left
        let rightElems = seq { yield! member'.Arguments; yield member'.ReturnType }
        testAll lowTypeMatcher leftElems rightElems ctx
      | _ -> Continue ctx

    let constructorRule (lowTypeMatcher: ILowTypeMatcher) (left: SignatureQuery) (right: ApiSignature) ctx =
      match left, right with
      | SignatureQuery.Signature left, ApiSignature.Constructor (_, member') ->
        Debug.WriteLine("constructor rule.")
        let leftElems = breakArrow left
        let rightElems = seq { yield! member'.Arguments; yield member'.ReturnType }
        testAll lowTypeMatcher leftElems rightElems ctx
      | _ -> Continue ctx

    let instanceMemberRule (lowTypeMatcher: ILowTypeMatcher) (left: SignatureQuery) (right: ApiSignature) ctx =
      match left, right with
      | SignatureQuery.InstanceMember (Receiver = queryReceiver; Arguments = queryArguments; ReturnType = queryReturnType), ApiSignature.InstanceMember (declaringType, member') ->
        Debug.WriteLine("instance member rule.")
        let leftElems = [
          yield queryReceiver
          yield! queryArguments
          yield queryReturnType
        ]
        let rightElems = [
          yield declaringType
          yield! member'.Arguments
          yield member'.ReturnType
        ]
        testAll lowTypeMatcher leftElems rightElems ctx
      | _ -> Continue ctx

    let instanceMemberUnitArgumentRule (lowTypeMatcher: ILowTypeMatcher) (left: SignatureQuery) (right: ApiSignature) ctx =
      match left, right with
      | SignatureQuery.InstanceMember (Receiver = queryReceiver; Arguments = []; ReturnType = queryReturnType), ApiSignature.InstanceMember (declaringType, ({ Arguments = [ LowType.Patterns.Unit ] } as member')) ->
        Debug.WriteLine("instance member unit argument rule.")
        let leftElems = [
          yield queryReceiver
          yield queryReturnType
        ]
        let rightElems = [
          yield declaringType
          yield member'.ReturnType
        ]
        testAll lowTypeMatcher leftElems rightElems ctx
        |> MatchingResult.mapMatched (Context.addDistance 1)
      | _ -> Continue ctx

    let instanceMemberAndFunctionRule (lowTypeMatcher: ILowTypeMatcher) (left: SignatureQuery) (right: ApiSignature) ctx =
      match left, right with
      | SignatureQuery.InstanceMember (Receiver = queryReceiver; Arguments = queryArguments; ReturnType = queryReturnType), ApiSignature.ModuleFunction rightElems ->
        let leftElems = [
          yield! queryArguments
          yield queryReceiver
          yield queryReturnType
        ]
        testAll lowTypeMatcher leftElems rightElems ctx
        |> MatchingResult.mapMatched (Context.addDistance 1)
      | _ -> Continue ctx

  let tryGetSignatureQuery = function
    | QueryMethod.BySignature s -> Some s
    | QueryMethod.ByName (_, s) -> Some s

  let instance (_: SearchOptions) =
    let rule =
      Rule.compose [
        yield Rules.moduleValueRule
        yield Rules.moduleFunctionRule
        yield Rules.staticMemberRule
        yield Rules.constructorRule
        yield Rules.instanceMemberUnitArgumentRule
        yield Rules.instanceMemberAndFunctionRule
        yield Rules.instanceMemberRule
        yield Rule.terminator
      ]
    { new IApiMatcher with
        member this.Name = "Signature Matcher"
        member this.Test lowTypeMatcher query api ctx =
          match tryGetSignatureQuery query with
          | Some (SignatureQuery.Wildcard) -> Matched ctx
          | Some s -> Rule.run rule lowTypeMatcher s api.Signature ctx
          | None -> Matched ctx }

module ConstraintSolver =
  let getFullTypeDefinition (ctx: Context) (baseType: LowType) =
    let rec getIdentity = function
      | Identity i -> i
      | Generic (x, _) -> getIdentity x
      | Tuple xs -> Identity.tupleN xs.Length
      | TypeAbbreviation { Original = o } -> getIdentity o
      | _ -> failwith "invalid base type."
    match getIdentity baseType with
    | FullIdentity full ->
      ctx.ApiDictionaries.[full.AssemblyName].TypeDefinitions |> Seq.find (fun td -> td.FullIdentity = full)
      |> Array.singleton
    | PartialIdentity partial ->
      ctx.QueryTypes.[partial]

  let rec (|ConstraintTestee|_|) = function
    | Identity id -> Some (id, [])
    | Generic (Identity id, args) -> Some (id, args)
    | Tuple xs -> Some (Identity.tupleN xs.Length, xs)
    | TypeAbbreviation { Original = o } -> (|ConstraintTestee|_|) o
    | _ -> None 

  let createConstraintSolver title testConstraint (testeeType: LowType) ctx = seq {
    match testeeType with
    | ConstraintTestee (testeeIdentity, testTypeArgs) ->
      let testees =
        match testeeIdentity with
        | FullIdentity i -> ctx.ApiDictionaries.[i.AssemblyName].TypeDefinitions |> Seq.find (fun td -> td.FullIdentity = i) |> Array.singleton
        | PartialIdentity i -> ctx.QueryTypes.[i]
      for typeDef in testees do
        Debug.WriteLine(sprintf "Test %s: %s" title (typeDef.Debug()))
        Debug.Indent()
        let nextCtx =
          match testeeIdentity with
          | FullIdentity _ -> ctx
          | PartialIdentity i -> { ctx with QueryTypes = ctx.QueryTypes |> Map.add i [| typeDef |] }
        let results = testConstraint typeDef testTypeArgs nextCtx |> Seq.cache
        Debug.Unindent()
        Debug.WriteLine(
          if Seq.isEmpty results = false then
            sprintf "Success %s, %d branches." title (Seq.length results)
          else
            sprintf "Failure %s." title
        )
        yield! results
    | Variable _ -> yield ctx
    | _ -> ()
  }

  let transferVariableArgument (inheritArgs: Map<TypeVariable, LowType>) (baseType: LowType): LowType list =
    let rec genericArguments = function
      | Identity _ -> []
      | Generic (_, args) -> args
      | TypeAbbreviation { Original = o } -> genericArguments o
      | _ -> failwith "invalid base type."
    genericArguments baseType
    |> List.map (function
      | Variable (VariableSource.Target, v) -> inheritArgs.[v]
      | a -> a)

  let instantiate (t: FullTypeDefinition) (args: LowType list) =
    let id = Identity (FullIdentity t.FullIdentity)
    match args with
    | [] -> id
    | _ -> Generic (id, args)

  let rec getInheritTypes (ctx: Context) (t: FullTypeDefinition) (args: LowType list): LowType seq = seq {
    let argPair = List.zip t.GenericParameters args |> Map.ofList

    let thisType = instantiate t args
    yield thisType 

    let parents = seq {
      match t.BaseType with
      | Some baseType -> yield baseType
      | None -> ()

      yield! t.AllInterfaces
    }

    for p in parents do
      let baseTypeArgs = transferVariableArgument argPair p
      let baseTypeDef =
        let rec getFullIdentity = function
          | Identity (FullIdentity full) -> full
          | Generic (Identity (FullIdentity full), _) -> full
          | TypeAbbreviation { Original = o } -> getFullIdentity o
          | _ -> failwith "It is not full identity."
        let full = getFullIdentity p
        ctx.ApiDictionaries.[full.AssemblyName].TypeDefinitions |> Seq.find (fun td -> td.FullIdentity = full)
      yield! getInheritTypes ctx baseTypeDef baseTypeArgs
  }

  let firstMatched f xs =
    xs
    |> Seq.tryPick (fun x -> match f x with Matched ctx -> Some ctx | _ -> None)
    |> function
      | Some ctx -> Seq.singleton ctx
      | None -> Seq.empty

  let testSubtypeConstraint (lowTypeMatcher: ILowTypeMatcher) (parentType: LowType) =
    createConstraintSolver
      "subtype constrints"
      (fun testeeTypeDef testeeArgs ctx ->
        let testees =
          match parentType with
          | Variable _ -> Seq.singleton (instantiate testeeTypeDef testeeArgs)
          | _ -> getInheritTypes ctx testeeTypeDef testeeArgs
        testees
        |> firstMatched (fun t -> lowTypeMatcher.Test t parentType ctx)
      )

  let addGenericMemberReplacements (m: Member) replacements =
    m.GenericParameters
    |> Seq.fold (fun replacements v ->
      replacements |> Map.add v (Wildcard None)
    ) replacements

  let normalizeGetterMethod (m: Member) =
    let args =
      match m.Arguments with
      | [] -> [ LowType.Unit ]
      | args -> args
    Arrow [ yield! args; yield m.ReturnType ]

  let normalizeSetterMethod (m: Member) =
    let args = [
      yield! m.Arguments
      yield m.ReturnType
    ]
    Arrow [ yield! args; yield LowType.Unit ]

  let normalizeMethod (m: Member) =
    Arrow [ yield! m.Arguments; yield m.ReturnType ]

  let testMemberConstraint (lowTypeMatcher: ILowTypeMatcher) modifier (expectedMember: Member) =
    let normalizedExpectedMember  =
      let xs = [ yield! expectedMember.Arguments; yield expectedMember.ReturnType ]
      Arrow xs

    createConstraintSolver
      "member constraints"
      (fun testeeTypeDef testeeArgs ctx ->
        Debug.WriteLine("Member normalize to arrow or function.")
        let members =
          match modifier with
          | MemberModifier.Static -> testeeTypeDef.StaticMembers
          | MemberModifier.Instance -> testeeTypeDef.InstanceMembers
        let genericTypeReplacements = List.zip testeeTypeDef.GenericParameters testeeArgs |> Map.ofList
        members
        |> Seq.choose (fun member' ->
          let normalized =
            match member' with
            | { Kind = MemberKind.Property PropertyKind.Get } ->
              if "get_" + member'.Name = expectedMember.Name then Some (normalizeGetterMethod member') else None
            | { Kind = MemberKind.Property PropertyKind.Set } ->
              if "set_" + member'.Name = expectedMember.Name then Some (normalizeSetterMethod member') else None
            | { Kind = MemberKind.Property PropertyKind.GetSet } ->
              if "get_" + member'.Name = expectedMember.Name then Some (normalizeGetterMethod member')
              elif "set_" + member'.Name = expectedMember.Name then Some (normalizeSetterMethod member')
              else None
            | _->
              if member'.Name = expectedMember.Name && member'.IsCurried = false && member'.Arguments.Length = expectedMember.Arguments.Length then
                Some (normalizeMethod member')
              else None
          normalized |> Option.map (fun x -> (x, addGenericMemberReplacements member' genericTypeReplacements))
        )
        |> Seq.map (fun (x, replacements) -> LowType.applyVariable VariableSource.Target replacements x)
        |> firstMatched (fun x -> lowTypeMatcher.Test x normalizedExpectedMember ctx)
      )

  let createConstraintStatusSolver name (get: _ -> ConstraintStatus) =
    let rec testConstraint testeeSignature ctx =
      let test =
        createConstraintSolver
          (sprintf "%s constraints" name)
          (fun testeeTypeDef testeeArgs ctx ->
            match get testeeTypeDef with
            | Satisfy -> Seq.singleton ctx
            | NotSatisfy -> Seq.empty
            | Dependence xs -> fold testeeTypeDef testeeArgs xs ctx)
      test testeeSignature ctx
    and fold (typeDef: FullTypeDefinition) args (dependentVariables: TypeVariable list) ctx =
      let testArgs =
        typeDef.GenericParameters
        |> List.map (fun p -> List.exists ((=)p) dependentVariables)
        |> List.zip args
        |> List.choose (fun (arg, isDependent) -> if isDependent then Some arg else None)
      Debug.WriteLine(sprintf "Test %s of dependent types: %A" name (testArgs |> List.map (fun x -> x.Debug())))
      let branches =
        testArgs
        |> Seq.fold (fun contextBranches testeeSignature ->
          seq {
            for ctxBranch in contextBranches do
              yield! testConstraint testeeSignature ctxBranch
          }
        ) (Seq.singleton ctx)
        |> Seq.cache
      Debug.WriteLine(sprintf "%d branches from dependent types." (Seq.length branches))
      branches
    testConstraint

  let testNullnessConstraint = createConstraintStatusSolver "nullness" (fun x -> x.SupportNull)
  let testDefaultConstructorConstriant = createConstraintStatusSolver "default constructor" (fun x -> x.DefaultConstructor)
  let testValueTypeConstraint = createConstraintStatusSolver "value type" (fun x -> x.ValueType)
  let testReferenceTypeConstraint = createConstraintStatusSolver "reference type" (fun x -> x.ReferenceType)
  let testEqualityConstraint = createConstraintStatusSolver "equality" (fun x -> x.Equality)
  let testComparisonConstraint = createConstraintStatusSolver "comparison" (fun x -> x.Comparison)

  let rec solve' (lowTypeMatcher: ILowTypeMatcher) (constraints: TypeConstraint list) (initialCtx: Context) (testEqualities: (LowType * LowType) list) =
    let getTestSignatures variable =
      let variable = Variable (VariableSource.Target, variable)
      testEqualities |> List.choose (fun (left, right) -> if left = variable then Some right elif right = variable then Some left else None)
    
    Debug.WriteLine("Begin solving type constraints.")
    Debug.WriteLine(sprintf "Equalities: %A" (List.map Equations.debugEquality testEqualities))
    Debug.WriteLine(sprintf "Constraints: %A" (constraints |> List.map TypeConstraint.debug))
    Debug.Indent()

    let testConstraint constraint' contextBranches testeeSignature: Context seq = seq {
      let inline pass ctx = Seq.singleton ctx
      for ctx in contextBranches do
        match constraint' with
        | SubtypeConstraints parentType ->
          yield! testSubtypeConstraint lowTypeMatcher parentType testeeSignature ctx
        | NullnessConstraints ->
          yield! testNullnessConstraint testeeSignature ctx
        | MemberConstraints (modifier, member') ->
          yield! testMemberConstraint lowTypeMatcher modifier member' testeeSignature ctx
        | DefaultConstructorConstraints ->
          yield! testDefaultConstructorConstriant testeeSignature ctx
        | ValueTypeConstraints ->
          yield! testValueTypeConstraint testeeSignature ctx
        | ReferenceTypeConstraints ->
          yield! testReferenceTypeConstraint testeeSignature ctx
        | EnumerationConstraints ->
          yield! pass ctx
        | DelegateConstraints ->
          yield! pass ctx
        | UnmanagedConstraints ->
          yield! pass ctx
        | EqualityConstraints ->
          yield! testEqualityConstraint testeeSignature ctx
        | ComparisonConstraints ->
          yield! testComparisonConstraint testeeSignature ctx
    }
    let result =
      constraints
      |> Seq.fold (fun contextBranches constraint' ->
        seq {
          for variable in constraint'.Variables do
            Debug.WriteLine(sprintf "Constraint test: %s" (constraint'.Debug()))
            let testSignatures = getTestSignatures variable
            Debug.WriteLine(sprintf "Constraint test signatures: %A" (testSignatures |> List.map (fun x -> x.Debug())))
            yield! testSignatures |> List.fold (testConstraint constraint'.Constraint) contextBranches
        }
      ) (Seq.singleton initialCtx)
      |> Seq.tryHead
      |> function
        | Some ctx ->
          match ctx.Equations.Equalities |> List.take (ctx.Equations.Equalities.Length - initialCtx.Equations.Equalities.Length) with
          | [] -> Matched ctx
          | newEqualities ->
            Debug.WriteLine(sprintf "There are new equalities." )
            Debug.Indent()
            let result = solve' lowTypeMatcher constraints ctx newEqualities
            Debug.Unindent()
            result
        | None -> Failure
        
    Debug.Unindent()
    Debug.WriteLine(sprintf "End solving type constraints. Result=%b" (MatchingResult.toBool result))
    result

  let solve lowTypeMatcher constraints ctx = solve' lowTypeMatcher constraints ctx ctx.Equations.Equalities

  let instance (_: SearchOptions) =
    { new IApiMatcher with
        member this.Name = "Constraint Solver"
        member this.Test lowTypeMatcher query api ctx = solve lowTypeMatcher api.TypeConstraints ctx }

module Initializer =
  let matchers options =
    let lowTypeMatcher = LowTypeMatcher.instance options
    let apiMatchers =
      [
        PublishedApi.instance
        NameMatcher.instance
        SignatureMatcher.instance
        ConstraintSolver.instance
      ]
      |> List.map (fun f -> f options)
    (lowTypeMatcher, apiMatchers)

  let collectFromSignatureQuery getTarget query =
    let (|Target|_|) t = getTarget t
    let rec f = function
      | Target x -> Seq.singleton x
      | Arrow xs -> Seq.collect f xs
      | Tuple xs -> Seq.collect f xs
      | Generic (id, args) ->
        Seq.concat [
          f id
          Seq.collect f args
        ]
      | TypeAbbreviation { Original = o } -> f o
      | _ -> Seq.empty
    let results = 
      match query with
      | { Query.Method = QueryMethod.ByName (_, sigQuery) }
      | { Query.Method = QueryMethod.BySignature sigQuery } ->
        match sigQuery with
        | SignatureQuery.Wildcard -> Seq.empty
        | SignatureQuery.Signature lt -> f lt
        | SignatureQuery.InstanceMember (receiver, arguments, returnType) ->
          Seq.concat [
            f receiver
            Seq.collect f arguments
            f returnType
          ]
    results |> Seq.distinct |> Seq.toList
    
  let collectVariables = collectFromSignatureQuery (function Variable _ as v -> Some v | _ -> None)
  let collectWildcardGroups = collectFromSignatureQuery (function Wildcard (Some _) as w -> Some w | _ -> None)
  let collectPartialIdentities = collectFromSignatureQuery (function Identity (PartialIdentity id) -> Some id | _ -> None)

  let initialEquations options query eqs =
    match options.StrictQueryVariable with
    | Enabled ->
      let variables = collectVariables query
      let wildcards = collectWildcardGroups query
      let inequalities =
        [
          for x in variables do
            for y in variables do
              if x < y then yield (x, y)

          for x in wildcards do
            for y in wildcards do
              if x < y then yield (x, y)
        ]
      { eqs with Inequalities = inequalities }
    | Disabled -> eqs

  let queryTypes query (dictionaries: ApiDictionary[]) =
    collectPartialIdentities query
    |> Seq.map (fun id ->
      let types = dictionaries |> Seq.collect (fun d -> d.TypeDefinitions) |> Seq.filter (fun td -> Identity.sameName (PartialIdentity id) (FullIdentity td.FullIdentity)) |> Seq.toArray
      (id, types)
    )
    |> Map.ofSeq

  let initializeContext (dictionaries: ApiDictionary[]) (options: SearchOptions) (query: Query) =
    {
      Distance = 0
      Equations = Equations.empty |> initialEquations options query
      QueryTypes = queryTypes query dictionaries
      ApiDictionaries = dictionaries |> Seq.map (fun d -> (d.AssemblyName, d)) |> Map.ofSeq
    }

  let replaceTypeAbbreviation (dictionaries: ApiDictionary seq) (query: Query) =
    let table = dictionaries |> Seq.collect (fun x -> x.TypeAbbreviations) |> Seq.toList
    let rec replace = function
      | Identity id as i ->
        let replacement = table |> List.tryFindBack (function { Abbreviation = Identity abbId } -> Identity.sameName abbId id | _ -> false)
        match replacement with
        | Some replacement -> TypeAbbreviation { Abbreviation = i; Original = replacement.Original }
        | None -> i
      | Generic (Identity id, args) as generic ->
        let replacedArgs = args |> List.map replace
        let idReplacement = table |> List.tryFindBack (function { Abbreviation = Generic (Identity abbId, _) } -> Identity.sameName abbId id | _ -> false)
        match idReplacement with
        | Some { Abbreviation = Generic (_, abbArgs); Original = original } ->
          let applyTable =
            List.zip abbArgs replacedArgs
            |> List.map (function Variable (_, v), arg -> (v, arg) | _ -> failwith "Parameters of generic type abbreviation should be variable.")
            |> Map.ofList
          let replacedGeneric = LowType.applyVariable VariableSource.Target applyTable original
          TypeAbbreviation { Abbreviation = generic; Original = replacedGeneric }
        | Some _ -> generic
        | None -> Generic (Identity id, replacedArgs)
      | Arrow xs -> Arrow (List.map replace xs)
      | Tuple xs -> Tuple (List.map replace xs)
      | other -> other
    let replaceSignatureQuery = function
      | SignatureQuery.Wildcard -> SignatureQuery.Wildcard
      | SignatureQuery.Signature lt -> SignatureQuery.Signature (replace lt)
      | SignatureQuery.InstanceMember (receiver, args, returnType) -> SignatureQuery.InstanceMember (replace receiver, List.map replace args, replace returnType)
    match query with
    | { Method = QueryMethod.ByName (name, sigQuery) } -> { query with Method = QueryMethod.ByName (name, replaceSignatureQuery sigQuery) }
    | { Method = QueryMethod.BySignature sigQuery } -> { query with Method = QueryMethod.BySignature (replaceSignatureQuery sigQuery) }
          

  let initializeQuery (dictionaries: ApiDictionary seq) (query: Query) =
    query
    |> replaceTypeAbbreviation dictionaries

let test (lowTypeMatcher: ILowTypeMatcher) (apiMatchers: IApiMatcher list) (query: Query) (ctx: Context) (api: Api) =
  apiMatchers
  |> Seq.fold (fun state m ->
    match state with
    | Matched ctx ->
      Debug.WriteLine(sprintf "Test \"%s\" and \"%s\" by %s. Equations: %s"
        query.OriginalString
        (ApiSignature.debug api.Signature)
        m.Name
        (Equations.debug ctx.Equations))
      Debug.Indent()
      let result = m.Test lowTypeMatcher query.Method api ctx
      Debug.Unindent()
      result
    | _ -> Failure
  ) (Matched ctx)

let search (dictionaries: ApiDictionary[]) (options: SearchOptions) (targets: Api seq) (queryStr: string) =
  let lowTypeMatcher, apiMatchers = Initializer.matchers options
  let query = QueryParser.parse queryStr |> Initializer.initializeQuery dictionaries
  let initialContext = Initializer.initializeContext dictionaries options query
  targets
  |> Seq.choose (fun api ->
    match test lowTypeMatcher apiMatchers query initialContext api with
    | Matched ctx -> Some { Distance = ctx.Distance; Api = api }
    | _ -> None)
  |> Seq.sortBy (fun x -> (x.Distance, ReverseName.toString x.Api.Name))
  |> Seq.cache