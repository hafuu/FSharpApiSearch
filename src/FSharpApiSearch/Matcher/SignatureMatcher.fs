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
    match member'.IsCurried, member'.Parameters with
    | _, [] -> ()
    | true, parameters -> yield! parameters
    | false, [ one ] -> yield one
    | false, many -> yield Tuple many
    yield member'.ReturnType
  }

  let testMemberParamAndReturn (lowTypeMatcher: ILowTypeMatcher) (left: LowType) (member': Member) ctx =
    let leftElems = breakArrow left
    if member'.IsCurried then
      let rightElems = seq { yield! member'.Parameters; yield member'.ReturnType }
      testAll lowTypeMatcher leftElems rightElems ctx
    else
      match leftElems, member' with
      | [ leftOne ], { Parameters = []; ReturnType = rightRet } -> lowTypeMatcher.Test leftOne rightRet ctx
      | [ _ ], { Parameters = _ } -> Failure
      | [ _; _ ], { Parameters = [ rightParams ]; ReturnType = rightRet } ->
        let rightElems = [ rightParams; rightRet ]
        testAll lowTypeMatcher leftElems rightElems ctx
      | [ _; _ ], { Parameters = [] } -> Failure
      | [ Tuple _; _ ], { Parameters = _ } ->
        let rightElems = [ Tuple member'.Parameters; member'.ReturnType ]
        testAll lowTypeMatcher leftElems rightElems ctx
      | _ -> Failure

  let testMemberParamAndReturn_IgnoreParameterStyle (lowTypeMatcher: ILowTypeMatcher) (left: LowType) (member': Member) ctx =
    match left, member' with
    | Arrow [ Tuple leftParams; leftRet ], { IsCurried = true } ->
      let leftElems = seq { yield! leftParams; yield leftRet }
      let rightElems = seq { yield! member'.Parameters; yield member'.ReturnType }
      testAll lowTypeMatcher leftElems rightElems ctx
      |> MatchingResult.mapMatched (Context.addDistance 1)
    | Arrow [ _; _ ], { IsCurried = false } ->
      testMemberParamAndReturn lowTypeMatcher left member' ctx
    | Arrow leftElems, { IsCurried = false; Parameters = [ Tuple rightParams ] } ->
      let rightElems = seq { yield! rightParams; yield member'.ReturnType }
      testAll lowTypeMatcher leftElems rightElems ctx
      |> MatchingResult.mapMatched (Context.addDistance 1)
    | Arrow leftElems, { IsCurried = false } ->
      let rightElems = seq { yield! member'.Parameters; yield member'.ReturnType }
      testAll lowTypeMatcher leftElems rightElems ctx
      |> MatchingResult.mapMatched (Context.addDistance 1)
    | _ -> testMemberParamAndReturn lowTypeMatcher left member' ctx

  let (|StaticMember|_|) = function
    | ApiSignature.StaticMember (_, member') -> Some member'
    | ApiSignature.TypeExtension { MemberModifier = MemberModifier.Static; Member = member' } -> Some member'
    | ApiSignature.ExtensionMember member' -> Some member'
    | _ -> None

  let staticMemberRule (lowTypeMatcher: ILowTypeMatcher) (left: SignatureQuery) (right: ApiSignature) ctx =
    match left, right with
    | SignatureQuery.Signature left, StaticMember member' ->
      Debug.WriteLine("static member rule.")
      testMemberParamAndReturn lowTypeMatcher left member' ctx
    | _ -> Continue ctx

  let staticMemberRule_IgnoreParameterStyle (lowTypeMatcher: ILowTypeMatcher) (left: SignatureQuery) (right: ApiSignature) ctx =
    match left, right with
    | SignatureQuery.Signature left, StaticMember member' ->
      Debug.WriteLine("static member rule (ignore parameter style).")
      testMemberParamAndReturn_IgnoreParameterStyle lowTypeMatcher left member' ctx
    | _ -> Continue ctx

  let constructorRule (lowTypeMatcher: ILowTypeMatcher) (left: SignatureQuery) (right: ApiSignature) ctx =
    match left, right with
    | SignatureQuery.Signature left, ApiSignature.Constructor (_, member') ->
      Debug.WriteLine("constructor rule.")
      testMemberParamAndReturn lowTypeMatcher left member' ctx
    | _ -> Continue ctx

  let constructorRule_IgnoreParameterStyle (lowTypeMatcher: ILowTypeMatcher) (left: SignatureQuery) (right: ApiSignature) ctx =
    match left, right with
    | SignatureQuery.Signature left, ApiSignature.Constructor (_, member') ->
      Debug.WriteLine("constructor rule (ignore parameter style).")
      testMemberParamAndReturn_IgnoreParameterStyle lowTypeMatcher left member' ctx
    | _ -> Continue ctx

  let methodPart queryParams queryReturnType = // receiver => {methodPart}
    match queryParams with
    | [] -> queryReturnType
    | _ -> Arrow [ yield! queryParams; yield queryReturnType ]

  let (|InstanceMember|_|) = function
    | ApiSignature.InstanceMember (declaringType, member') -> Some (declaringType, member')
    | ApiSignature.TypeExtension { MemberModifier = MemberModifier.Instance; ExistingType = declaringType; Member = member' } -> Some (declaringType, member')
    | ApiSignature.ExtensionMember ({ Parameters = declaringType :: parameters } as member') ->
      let member' = { member' with Parameters = parameters }
      Some (declaringType, member')
    | _ -> None

  let instanceMemberRule (lowTypeMatcher: ILowTypeMatcher) (left: SignatureQuery) (right: ApiSignature) ctx =
    match left, right with
    | SignatureQuery.InstanceMember (Receiver = queryReceiver; Parameters = queryParams; ReturnType = queryReturnType), InstanceMember (declaringType, member') ->
      Debug.WriteLine("instance member rule.")
      lowTypeMatcher.Test queryReceiver declaringType ctx
      |> MatchingResult.bindMatched (fun ctx ->
        let left = methodPart queryParams queryReturnType
        testMemberParamAndReturn lowTypeMatcher left member' ctx
      )
    | _ -> Continue ctx

  let instanceMemberRule_IgnoreParameterStyle (lowTypeMatcher: ILowTypeMatcher) (left: SignatureQuery) (right: ApiSignature) ctx =
    match left, right with
    | SignatureQuery.InstanceMember (Receiver = queryReceiver; Parameters = queryParams; ReturnType = queryReturnType), InstanceMember (declaringType, member') ->
      Debug.WriteLine("instance member rule (ignore parameter style).")
      lowTypeMatcher.Test queryReceiver declaringType ctx
      |> MatchingResult.bindMatched (fun ctx ->
        let left = methodPart queryParams queryReturnType
        testMemberParamAndReturn_IgnoreParameterStyle lowTypeMatcher left member' ctx
      )
    | _ -> Continue ctx

  let instanceMemberUnitParameterRule (lowTypeMatcher: ILowTypeMatcher) (left: SignatureQuery) (right: ApiSignature) ctx =
    match left, right with
    | SignatureQuery.InstanceMember (Receiver = queryReceiver; Parameters = []; ReturnType = queryReturnType), InstanceMember (declaringType, ({ Parameters = [ LowType.Patterns.Unit ] } as member')) ->
      Debug.WriteLine("instance member unit parameter rule.")
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
    | SignatureQuery.InstanceMember (Receiver = queryReceiver; Parameters = queryParams; ReturnType = queryReturnType), ApiSignature.ModuleFunction rightElems ->
      Debug.WriteLine("instance member and function rule.")
      let leftElems = [
        yield! queryParams
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
      |> MatchingResult.bindMatched (testMemberParamAndReturn lowTypeMatcher (Arrow leftMemberPart) member')
      |> MatchingResult.mapMatched (Context.addDistance 1)
    | _ -> Continue ctx

  let arrowAndInstanceMemberRule_IgnoreParameterStyle (lowTypeMatcher: ILowTypeMatcher) (left: SignatureQuery) (right: ApiSignature) ctx =
    match left, right with
    | SignatureQuery.Signature (Arrow (leftReceiver :: leftParamAndRet)), InstanceMember (declaringType, member') ->
      let leftParamAndRet =
        match leftParamAndRet with
        | [ one ] -> one
        | many -> Arrow many
      Debug.WriteLine("arrow and instance member rule (ignore parameter style).")
      lowTypeMatcher.Test leftReceiver declaringType ctx
      |> MatchingResult.bindMatched (testMemberParamAndReturn_IgnoreParameterStyle lowTypeMatcher leftParamAndRet member')
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
      | { IgnoreParameterStyle = Enabled } ->
        yield Rules.staticMemberRule_IgnoreParameterStyle
        yield Rules.constructorRule_IgnoreParameterStyle
      | { IgnoreParameterStyle = Disabled } ->
        yield Rules.staticMemberRule
        yield Rules.constructorRule
        
      yield Rules.instanceMemberUnitParameterRule
      yield Rules.instanceMemberAndFunctionRule
      match options with
      | { IgnoreParameterStyle = Enabled } ->
        yield Rules.instanceMemberRule_IgnoreParameterStyle
        yield Rules.arrowAndInstanceMemberRule_IgnoreParameterStyle
      | { IgnoreParameterStyle = Disabled } ->
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