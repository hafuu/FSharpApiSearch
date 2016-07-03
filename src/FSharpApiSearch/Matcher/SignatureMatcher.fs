module internal FSharpApiSearch.SignatureMatcher

open System.Diagnostics
open FSharpApiSearch.MatcherTypes
open FSharpApiSearch.SpecialTypes

module Rules =
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
  
  let moduleValueRule (lowTypeMatcher: ILowTypeMatcher) (left: SignatureQuery) (right: ApiSignature) ctx =
    match left, right with
    | SignatureQuery.Signature (Arrow _), ApiSignature.ModuleValue _ -> Continue ctx
    | SignatureQuery.Signature left, ApiSignature.ModuleValue right ->
      Debug.WriteLine("module value rule.")
      lowTypeMatcher.Test left right ctx
    | _ -> Continue ctx

  let moduleFunctionRule (lowTypeMatcher: ILowTypeMatcher) (left: SignatureQuery) (right: ApiSignature) ctx =
    match left, right with
    | SignatureQuery.Signature (Arrow _ as left), ApiSignature.ModuleFunction xs
    | SignatureQuery.Signature (LowType.Patterns.AbbreviationRoot (Arrow _ as left)), ApiSignature.ModuleFunction xs
    | SignatureQuery.Signature (Arrow _ as left), ApiSignature.ModuleValue (LowType.Patterns.AbbreviationRoot (Arrow xs)) ->
      let right = Arrow xs
      Debug.WriteLine("module function rule.")
      lowTypeMatcher.Test left right ctx
    | _ -> Continue ctx

  let activePatternRule (lowTypeMatcher: ILowTypeMatcher) (left: SignatureQuery) (right: ApiSignature) ctx =
    match left, right with
    | SignatureQuery.Signature (Arrow _ as left), ApiSignature.ActivePatten (_, right) ->
      Debug.WriteLine("active pattern rule.")
      lowTypeMatcher.Test left right ctx
    | _ -> Continue ctx

  let breakArrow = function
    | Arrow xs -> xs
    | other -> [ other ]

  let normalizeMember member' = seq {
    match member'.IsCurried, member'.Arguments with
    | _, [] -> ()
    | true, args -> yield! args
    | false, [ one ] -> yield one
    | false, many -> yield Tuple many
    yield member'.ReturnType
  }

  let testMemberArgAndReturn (lowTypeMatcher: ILowTypeMatcher) (left: LowType) (member': Member) ctx =
    let leftElems = breakArrow left
    if member'.IsCurried then
      let rightElems = seq { yield! member'.Arguments; yield member'.ReturnType }
      testAll lowTypeMatcher leftElems rightElems ctx
    else
      match leftElems, member' with
      | [ leftOne ], { Arguments = []; ReturnType = rightRet } -> lowTypeMatcher.Test leftOne rightRet ctx
      | [ _ ], { Arguments = _ } -> Failure
      | [ _; _ ], { Arguments = [ rightArg ]; ReturnType = rightRet } ->
        let rightElems = [ rightArg; rightRet ]
        testAll lowTypeMatcher leftElems rightElems ctx
      | [ _; _ ], { Arguments = [] } -> Failure
      | [ Tuple _; _ ], { Arguments = _ } ->
        let rightElems = [ Tuple member'.Arguments; member'.ReturnType ]
        testAll lowTypeMatcher leftElems rightElems ctx
      | _ -> Failure

  let testMemberArgAndReturn_IgnoreArgumentStyle (lowTypeMatcher: ILowTypeMatcher) (left: LowType) (member': Member) ctx =
    match left, member' with
    | Arrow [ Tuple leftArgs; leftRet ], { IsCurried = true } ->
      let leftElems = seq { yield! leftArgs; yield leftRet }
      let rightElems = seq { yield! member'.Arguments; yield member'.ReturnType }
      testAll lowTypeMatcher leftElems rightElems ctx
      |> MatchingResult.mapMatched (Context.addDistance 1)
    | Arrow [ _; _ ], { IsCurried = false } ->
      testMemberArgAndReturn lowTypeMatcher left member' ctx
    | Arrow leftElems, { IsCurried = false; Arguments = [ Tuple rightArgs ] } ->
      let rightElems = seq { yield! rightArgs; yield member'.ReturnType }
      testAll lowTypeMatcher leftElems rightElems ctx
      |> MatchingResult.mapMatched (Context.addDistance 1)
    | Arrow leftElems, { IsCurried = false } ->
      let rightElems = seq { yield! member'.Arguments; yield member'.ReturnType }
      testAll lowTypeMatcher leftElems rightElems ctx
      |> MatchingResult.mapMatched (Context.addDistance 1)
    | _ -> testMemberArgAndReturn lowTypeMatcher left member' ctx

  let (|StaticMember|_|) = function
    | ApiSignature.StaticMember (_, member') -> Some member'
    | ApiSignature.TypeExtension { MemberModifier = MemberModifier.Static; Member = member' } -> Some member'
    | ApiSignature.ExtensionMember member' -> Some member'
    | _ -> None

  let staticMemberRule (lowTypeMatcher: ILowTypeMatcher) (left: SignatureQuery) (right: ApiSignature) ctx =
    match left, right with
    | SignatureQuery.Signature left, StaticMember member' ->
      Debug.WriteLine("static member rule.")
      testMemberArgAndReturn lowTypeMatcher left member' ctx
    | _ -> Continue ctx

  let staticMemberRule_IgnoreArgumentStyle (lowTypeMatcher: ILowTypeMatcher) (left: SignatureQuery) (right: ApiSignature) ctx =
    match left, right with
    | SignatureQuery.Signature left, StaticMember member' ->
      Debug.WriteLine("static member rule (ignore argument style).")
      testMemberArgAndReturn_IgnoreArgumentStyle lowTypeMatcher left member' ctx
    | _ -> Continue ctx

  let constructorRule (lowTypeMatcher: ILowTypeMatcher) (left: SignatureQuery) (right: ApiSignature) ctx =
    match left, right with
    | SignatureQuery.Signature left, ApiSignature.Constructor (_, member') ->
      Debug.WriteLine("constructor rule.")
      testMemberArgAndReturn lowTypeMatcher left member' ctx
    | _ -> Continue ctx

  let constructorRule_IgnoreArgumentStyle (lowTypeMatcher: ILowTypeMatcher) (left: SignatureQuery) (right: ApiSignature) ctx =
    match left, right with
    | SignatureQuery.Signature left, ApiSignature.Constructor (_, member') ->
      Debug.WriteLine("constructor rule (ignore argument style).")
      testMemberArgAndReturn_IgnoreArgumentStyle lowTypeMatcher left member' ctx
    | _ -> Continue ctx

  let methodPart queryArguments queryReturnType = // receiver => {methodPart}
    match queryArguments with
    | [] -> queryReturnType
    | _ -> Arrow [ yield! queryArguments; yield queryReturnType ]

  let (|InstanceMember|_|) = function
    | ApiSignature.InstanceMember (declaringType, member') -> Some (declaringType, member')
    | ApiSignature.TypeExtension { MemberModifier = MemberModifier.Instance; ExistingType = declaringType; Member = member' } -> Some (declaringType, member')
    | ApiSignature.ExtensionMember ({ Arguments = declaringType :: arguments } as member') ->
      let member' = { member' with Arguments = arguments }
      Some (declaringType, member')
    | _ -> None

  let instanceMemberRule (lowTypeMatcher: ILowTypeMatcher) (left: SignatureQuery) (right: ApiSignature) ctx =
    match left, right with
    | SignatureQuery.InstanceMember (Receiver = queryReceiver; Arguments = queryArguments; ReturnType = queryReturnType), InstanceMember (declaringType, member') ->
      Debug.WriteLine("instance member rule.")
      lowTypeMatcher.Test queryReceiver declaringType ctx
      |> MatchingResult.bindMatched (fun ctx ->
        let left = methodPart queryArguments queryReturnType
        testMemberArgAndReturn lowTypeMatcher left member' ctx
      )
    | _ -> Continue ctx

  let instanceMemberRule_IgnoreArgumentStyle (lowTypeMatcher: ILowTypeMatcher) (left: SignatureQuery) (right: ApiSignature) ctx =
    match left, right with
    | SignatureQuery.InstanceMember (Receiver = queryReceiver; Arguments = queryArguments; ReturnType = queryReturnType), InstanceMember (declaringType, member') ->
      Debug.WriteLine("instance member rule (ignore argument style).")
      lowTypeMatcher.Test queryReceiver declaringType ctx
      |> MatchingResult.bindMatched (fun ctx ->
        let left = methodPart queryArguments queryReturnType
        testMemberArgAndReturn_IgnoreArgumentStyle lowTypeMatcher left member' ctx
      )
    | _ -> Continue ctx

  let instanceMemberUnitArgumentRule (lowTypeMatcher: ILowTypeMatcher) (left: SignatureQuery) (right: ApiSignature) ctx =
    match left, right with
    | SignatureQuery.InstanceMember (Receiver = queryReceiver; Arguments = []; ReturnType = queryReturnType), InstanceMember (declaringType, ({ Arguments = [ LowType.Patterns.Unit ] } as member')) ->
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
      Debug.WriteLine("instance member and function rule.")
      let leftElems = [
        yield! queryArguments
        yield queryReceiver
        yield queryReturnType
      ]
      testAll lowTypeMatcher leftElems rightElems ctx
      |> MatchingResult.mapMatched (Context.addDistance 1)
    | _ -> Continue ctx

  let arrowAndInstanceMemberRule (lowTypeMatcher: ILowTypeMatcher) (left: SignatureQuery) (right: ApiSignature) ctx =
    match left, right with
    | SignatureQuery.Signature (Arrow (leftReceiver :: leftMemberPart)), InstanceMember (declaringType, member') ->
      Debug.WriteLine("arrow and instance member rule.")
      lowTypeMatcher.Test leftReceiver declaringType ctx
      |> MatchingResult.bindMatched (testMemberArgAndReturn lowTypeMatcher (Arrow leftMemberPart) member')
      |> MatchingResult.mapMatched (Context.addDistance 1)
    | _ -> Continue ctx

  let arrowAndInstanceMemberRule_IgnoreArgumentStyle (lowTypeMatcher: ILowTypeMatcher) (left: SignatureQuery) (right: ApiSignature) ctx =
    match left, right with
    | SignatureQuery.Signature (Arrow (leftReceiver :: leftArgAndRet)), InstanceMember (declaringType, member') ->
      let leftArgAndRet =
        match leftArgAndRet with
        | [ one ] -> one
        | many -> Arrow many
      Debug.WriteLine("arrow and instance member rule (ignore argument style).")
      lowTypeMatcher.Test leftReceiver declaringType ctx
      |> MatchingResult.bindMatched (testMemberArgAndReturn_IgnoreArgumentStyle lowTypeMatcher leftArgAndRet member')
      |> MatchingResult.mapMatched (Context.addDistance 1)
    | _ -> Continue ctx

let tryGetSignatureQuery = function
  | QueryMethod.BySignature s -> Some s
  | QueryMethod.ByName (_, s) -> Some s
  | QueryMethod.ByActivePattern _ -> None

let instance (options: SearchOptions) =
  let rule =
    Rule.compose [
      yield Rules.moduleValueRule
      yield Rules.moduleFunctionRule
      yield Rules.activePatternRule

      match options with
      | { IgnoreArgumentStyle = Enabled } ->
        yield Rules.staticMemberRule_IgnoreArgumentStyle
        yield Rules.constructorRule_IgnoreArgumentStyle
      | { IgnoreArgumentStyle = Disabled } ->
        yield Rules.staticMemberRule
        yield Rules.constructorRule
        
      yield Rules.instanceMemberUnitArgumentRule
      yield Rules.instanceMemberAndFunctionRule
      match options with
      | { IgnoreArgumentStyle = Enabled } ->
        yield Rules.instanceMemberRule_IgnoreArgumentStyle
        yield Rules.arrowAndInstanceMemberRule_IgnoreArgumentStyle
      | { IgnoreArgumentStyle = Disabled } ->
        yield Rules.instanceMemberRule
        yield Rules.arrowAndInstanceMemberRule
        
      yield Rule.terminator
    ]
  { new IApiMatcher with
      member this.Name = "Signature Matcher"
      member this.Test lowTypeMatcher query api ctx =
        match tryGetSignatureQuery query with
        | Some (SignatureQuery.Wildcard) -> Matched ctx
        | Some s -> Rule.run rule lowTypeMatcher s api.Signature ctx
        | None -> Matched ctx }