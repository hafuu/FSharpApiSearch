module internal FSharpApiSearch.ConstraintSolver

open System.Diagnostics
open FSharpApiSearch.EngineTypes
open FSharpApiSearch.SpecialTypes
open FSharpApiSearch.StringPrinter
open FSharpApiSearch.TypeHierarchy

let rec (|ConstraintTestee|_|) = function
  | Identifier (id, _) -> Some (id, [])
  | Generic (Identifier (id, _), args, _) -> Some (id, args)
  | Tuple ({ Elements = xs }, _) -> Some (Identifier.tupleN xs.Length, xs)
  | TypeAbbreviation ({ Original = o }, _) -> (|ConstraintTestee|_|) o
  | _ -> None 

let createConstraintSolver title testConstraint (testeeType: LowType) ctx = seq {
  match testeeType with
  | ConstraintTestee (testeeType, testTypeArgs) ->
    let testees =
      match testeeType with
      | ConcreteType a -> ctx.ApiDictionaries.[a.AssemblyName].TypeDefinitions.[a] |> Array.singleton
      | UserInputType u -> ctx.QueryTypes.[u]
    for typeDef in testees do
      Debug.WriteLine(sprintf "Test %s: %s" title (typeDef.Debug()))
      Debug.Indent()
      let nextCtx =
        match testeeType with
        | ConcreteType _ -> ctx
        | UserInputType u -> { ctx with QueryTypes = ctx.QueryTypes |> Map.add u [| typeDef |] }
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
  | Wildcard _ -> yield ctx
  | _ -> ()
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
        | _ -> getBaseTypes ctx testeeTypeDef testeeArgs
      testees
      |> firstMatched (fun t -> lowTypeMatcher.Test t parentType ctx)
    )

let addGenericMemberReplacements (m: Member) replacements =
  m.GenericParameters
  |> Seq.fold (fun replacements v ->
    replacements |> Map.add v (Wildcard.create None)
  ) replacements

let normalizeGetterMethod (m: Member) =
  let indexOrUnit =
    match m.Parameters with
    | [] -> LowType.Unit
    | [ propertyIndex ] ->
      let elems = List.map (fun x -> x.Type) propertyIndex
      Tuple.create { Elements = elems; IsStruct = false }
    | _ -> failwith "Curried getter is invalid."
  Arrow.create ([ indexOrUnit ], m.ReturnParameter.Type)

let normalizeSetterMethod (m: Member) =
  let parameters =
    match m.Parameters with
    | [] -> m.ReturnParameter.Type
    | [ propertyIndex ] ->
      let elements = [ yield! propertyIndex; yield m.ReturnParameter ] |> List.map (fun x -> x.Type)
      Tuple.create { Elements = elements; IsStruct = false }
    | _ -> failwith "Curried setter is invalid."
  Arrow.create ([ parameters ], LowType.Unit)

let normalizeMethod (m: Member) = Arrow.create (Member.toArrow m)

let testMemberConstraint (lowTypeMatcher: ILowTypeMatcher) modifier (expectedMember: Member) =
  let normalizedExpectedMember = Arrow.create (Member.toArrow expectedMember)

  createConstraintSolver
    "member constraints"
    (fun testeeTypeDef testeeArgs ctx ->
      Debug.WriteLine("Member normalize to arrow or function.")
      let members =
        match modifier with
        | MemberModifier.Static -> Seq.append testeeTypeDef.StaticMembers testeeTypeDef.ImplicitStaticMembers
        | MemberModifier.Instance -> Seq.append testeeTypeDef.InstanceMembers testeeTypeDef.ImplicitInstanceMembers
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
            if member'.Name = expectedMember.Name && member'.IsCurried = false && member'.Parameters.Length = expectedMember.Parameters.Length then
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
    let variable = Variable.create (VariableSource.Target, variable)
    testEqualities |> List.choose (fun (left, right) -> if left = variable then Some right elif right = variable then Some left else None)
    
  Debug.WriteLine("Begin solving type constraints.")
  Debug.WriteLine(sprintf "Equalities: %A" (List.map Equations.debugEquality testEqualities))
  Debug.WriteLine(sprintf "Constraints: %A" (constraints |> List.map (fun c -> c.Debug())))
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
        match Context.newEquations initialCtx ctx with
        | [] -> Matched ctx
        | newEqualities ->
          Debug.WriteLine(sprintf "There are new equalities." )
          Debug.Indent()
          let result = solve' lowTypeMatcher constraints ctx newEqualities
          Debug.Unindent()
          result
      | None -> Failure FailureInfo.None
        
  Debug.Unindent()
  Debug.WriteLine(sprintf "End solving type constraints. Result=%b" (MatchingResult.toBool result))
  result

let solve lowTypeMatcher constraints ctx = solve' lowTypeMatcher constraints ctx ctx.Equations.Equalities

let instance (_: SearchOptions) =
  { new IApiMatcher with
      member this.Name = "Constraint Solver"
      member this.Test lowTypeMatcher query api ctx = solve lowTypeMatcher api.TypeConstraints ctx }