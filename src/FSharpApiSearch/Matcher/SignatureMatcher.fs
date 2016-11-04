module internal FSharpApiSearch.SignatureMatcher

open System.Diagnostics
open FSharpApiSearch.MatcherTypes
open FSharpApiSearch.SpecialTypes

module Rules =
  let choiceRule runRules (lowTypeMatcher: ILowTypeMatcher) (left: SignatureQuery) (right: ApiSignature) ctx =
    match left, right with
    | SignatureQuery.Signature (Choice choices), _ ->
      Debug.WriteLine("choice rule.")
      Debug.WriteLine(sprintf "test %A" (choices |> List.map (fun x -> x.Debug())))
      choices
      |> Seq.map (fun c ->
        Debug.WriteLine(sprintf "try test %s" (c.Debug()))
        Debug.Indent()
        let result = runRules lowTypeMatcher (SignatureQuery.Signature c) right ctx
        Debug.Unindent()
        result
      )
      |> Seq.tryPick (fun result -> match result with Matched _ as m -> Some m | _ -> None)
      |> function
        | Some matched -> matched
        | None -> Failure
    | _ -> Continue ctx

  let moduleValueRule (lowTypeMatcher: ILowTypeMatcher) (left: SignatureQuery) (right: ApiSignature) ctx =
    match left, right with
    | SignatureQuery.Signature (Arrow _), ApiSignature.ModuleValue _ -> Continue ctx
    | SignatureQuery.Signature left, ApiSignature.ModuleValue right ->
      Debug.WriteLine("module value rule.")
      lowTypeMatcher.Test left right ctx
    | _ -> Continue ctx

  let (|WildcardOrVariable|_|) = function
    | Wildcard _ -> Some ()
    | Variable _ -> Some ()
    | _ -> None

  let trimOptionalParameters (leftElems: LowType list) (rightElems: Parameter list list) =
    match rightElems with
    | [ nonCurriedParameters; [ ret ] ] ->
      let leftLength = (leftElems |> List.sumBy (function Tuple xs -> xs.Length | _ -> 1)) - 1 // subtract return parameter (1)
      let rightLength = nonCurriedParameters.Length
      if leftLength < rightLength then
        let trimedParameters, extraParameters = List.splitAt leftLength nonCurriedParameters
        if List.forall (fun x -> x.IsOptional) extraParameters then
          Debug.WriteLine(sprintf "trimed %d parameters." (rightLength - leftLength))
          [ trimedParameters; [ ret ] ]
        else
          rightElems
      else
        rightElems
    | _ ->
      rightElems

  let testArrow (lowTypeMatcher: ILowTypeMatcher) (leftElems: LowType list) (rightElems: Parameter list list) ctx =
    Debug.WriteLine("test arrow.")
    let rightElems = trimOptionalParameters leftElems rightElems
    let test ctx =
      let rightElems = (Function.toLowTypeList rightElems)
      lowTypeMatcher.TestArrow leftElems rightElems ctx
    match leftElems, rightElems with
    | [ WildcardOrVariable; _ ], [ [ _ ]; _ ] -> test ctx
    | [ WildcardOrVariable; _ ], [ _; _ ] -> Failure
    | _ -> test ctx

  let (|Right_CurriedFunction|_|) (xs: Parameter list list) =
    if xs |> List.forall (function [ _ ] -> true | _ -> false) then
      Some (xs |> List.map (fun x -> x.Head.Type))
    else
      None

  let (|Right_NonCurriedFunction|_|) (xs: Parameter list list) =
    match xs with
    | [ parameters; [ ret ] ] -> Some [ yield! parameters |> List.map (fun x -> x.Type); yield ret.Type ]
    | _ -> None

  let (|Right_TupleFunction|_|) (xs: Parameter list list) =
    match xs with
    | [ [ { Type = Tuple parameters } ]; [ { Type = ret } ] ] -> Some [ yield! parameters; yield ret ]
    | _ -> None

  let (|Left_CurriedFunction|_|) (xs: LowType list) =
    match xs with
    | [ Tuple _; _ ] -> None
    | _ -> Some xs

  let (|Left_NonCurriedFunction|_|) (xs: LowType list) =
    match xs with
    | [ Tuple parameters; ret ] -> Some [ yield! parameters; yield ret ]
    | _ -> None

  let testArrow_IgnoreParamStyle (lowTypeMatcher: ILowTypeMatcher) (leftElems: LowType list) (rightElems: Parameter list list) ctx =
    Debug.WriteLine("test arrow (ignore parameter style).")
    let rightElems = trimOptionalParameters leftElems rightElems
    match leftElems, rightElems with
    | [ leftParam; leftRet ], [ [ rightParam ]; [ rightRet ] ] ->
      Debug.WriteLine("pattern 1")
      let leftElems = [ leftParam; leftRet ]
      let rightElems = [ rightParam.Type; rightRet.Type ]
      lowTypeMatcher.TestArrow leftElems rightElems ctx
    | Left_NonCurriedFunction leftElems, Right_CurriedFunction rightElems ->
      Debug.WriteLine("pattern 2")
      lowTypeMatcher.TestArrow leftElems rightElems ctx
      |> MatchingResult.mapMatched (Context.addDistance 1)
    | Left_CurriedFunction leftElems, (Right_TupleFunction rightElems | Right_NonCurriedFunction rightElems) ->
      Debug.WriteLine("pattern 3")
      lowTypeMatcher.TestArrow leftElems rightElems ctx
      |> MatchingResult.mapMatched (Context.addDistance 1)
    | _, _ ->
      Debug.WriteLine("pattern 4")
      testArrow lowTypeMatcher leftElems rightElems ctx
      
  let moduleFunctionRule testArrow (lowTypeMatcher: ILowTypeMatcher) (left: SignatureQuery) (right: ApiSignature) ctx =
    match left, right with
    | SignatureQuery.Signature (Arrow leftElems), ApiSignature.ModuleFunction rightFun
    | SignatureQuery.Signature (LowType.Patterns.AbbreviationRoot (Arrow leftElems)), ApiSignature.ModuleFunction rightFun ->
      Debug.WriteLine("module function rule.")
      testArrow lowTypeMatcher leftElems rightFun ctx
    | SignatureQuery.Signature (Arrow _ as left), ApiSignature.ModuleValue (LowType.Patterns.AbbreviationRoot (Arrow _ as right)) ->
      Debug.WriteLine("module function rule.")
      lowTypeMatcher.Test left right ctx
    | _ -> Continue ctx

  let arrowQueryAndDelegateRule (lowTypeMatcher: ILowTypeMatcher) (left: SignatureQuery) (right: ApiSignature) ctx =
    match left, right with
    | SignatureQuery.Signature (Arrow _ as left), ApiSignature.ModuleValue (Delegate (_, xs)) ->
      let right = Arrow xs
      Debug.WriteLine("arrow query and delegate rule.")
      lowTypeMatcher.Test left right ctx
      |> MatchingResult.mapMatched (Context.addDistance 1)
    | _ -> Continue ctx

  let activePatternRule testArrow (lowTypeMatcher: ILowTypeMatcher) (left: SignatureQuery) (right: ApiSignature) ctx =
    match left, right with
    | SignatureQuery.Signature (Arrow leftElems), ApiSignature.ActivePatten (_, rightElems) ->
      Debug.WriteLine("active pattern rule.")
      testArrow lowTypeMatcher leftElems rightElems ctx
    | _ -> Continue ctx

  let breakArrow = function
    | Arrow xs -> xs
    | other -> [ other ]

  let (|StaticMember|_|) = function
    | ApiSignature.StaticMember (_, member') -> Some member'
    | ApiSignature.TypeExtension { MemberModifier = MemberModifier.Static; Member = member' } -> Some member'
    | ApiSignature.ExtensionMember member' -> Some member'
    | _ -> None

  let staticMemberRule testArrow (lowTypeMatcher: ILowTypeMatcher) (left: SignatureQuery) (right: ApiSignature) ctx =
    match left, right with
    | SignatureQuery.Signature left, StaticMember member' ->
      Debug.WriteLine("static member rule.")
      testArrow lowTypeMatcher (breakArrow left) (Member.toFunction member') ctx
    | _ -> Continue ctx

  let constructorRule testArrow (lowTypeMatcher: ILowTypeMatcher) (left: SignatureQuery) (right: ApiSignature) ctx =
    match left, right with
    | SignatureQuery.Signature left, ApiSignature.Constructor (_, member') ->
      Debug.WriteLine("constructor rule.")
      testArrow lowTypeMatcher (breakArrow left) (Member.toFunction member') ctx 
    | _ -> Continue ctx

  let methodPart queryParams queryReturnType = // receiver => {methodPart}
    match queryParams with
    | [] -> [ queryReturnType ]
    | _ -> [ yield! queryParams; yield queryReturnType ]

  let (|InstanceMember|_|) = function
    | ApiSignature.InstanceMember (declaringType, member') -> Some (declaringType, member')
    | ApiSignature.TypeExtension { MemberModifier = MemberModifier.Instance; ExistingType = declaringType; Member = member' } -> Some (declaringType, member')
    | ApiSignature.ExtensionMember ({ Parameters = [ declaringType :: parameters ] } as member') ->
      let member' =
        let ps =
          match parameters with
          | [] -> []
          | _ -> [ parameters ]
        { member' with Parameters = ps }
      Some (declaringType.Type, member')
    | _ -> None

  let instanceMemberRule testArrow (lowTypeMatcher: ILowTypeMatcher) (left: SignatureQuery) (right: ApiSignature) ctx =
    match left, right with
    | SignatureQuery.InstanceMember (Receiver = queryReceiver; Parameters = queryParams; ReturnType = queryReturnType), InstanceMember (declaringType, member') ->
      Debug.WriteLine("instance member rule.")
      lowTypeMatcher.TestReceiver queryReceiver declaringType ctx
      |> MatchingResult.bindMatched (fun ctx ->
        let left = methodPart queryParams queryReturnType
        testArrow lowTypeMatcher left (Member.toFunction member') ctx
      )
    | _ -> Continue ctx

  let instanceMemberUnitParameterRule (lowTypeMatcher: ILowTypeMatcher) (left: SignatureQuery) (right: ApiSignature) ctx =
    match left, right with
    | SignatureQuery.InstanceMember (Receiver = queryReceiver; Parameters = []; ReturnType = queryReturnType), InstanceMember (declaringType, ({ Parameters = [ [ { Type = LowType.Patterns.Unit } ] ] } as member')) ->
      Debug.WriteLine("instance member unit parameter rule.")
      lowTypeMatcher.TestReceiver queryReceiver declaringType ctx
      |> MatchingResult.bindMatched (lowTypeMatcher.Test queryReturnType member'.ReturnParameter.Type)
      |> MatchingResult.mapMatched (Context.addDistance 1)
    | _ -> Continue ctx

  let instanceMemberAndFunctionRule (lowTypeMatcher: ILowTypeMatcher) (left: SignatureQuery) (right: ApiSignature) ctx =
    match left, right with
    | SignatureQuery.InstanceMember (Receiver = queryReceiver; Parameters = queryParams; ReturnType = queryReturnType), ApiSignature.ModuleFunction right ->
      Debug.WriteLine("instance member and function rule.")
      let leftElems =
        [
          match queryParams with
          | [] -> ()
          | _ -> yield! queryParams
          yield queryReceiver
          yield queryReturnType
        ]
      let rightElems = Function.toLowTypeList right
      lowTypeMatcher.TestArrow leftElems rightElems ctx
      |> MatchingResult.mapMatched (Context.addDistance 1)
    | _ -> Continue ctx

  let arrowAndInstanceMemberRule testArrow (lowTypeMatcher: ILowTypeMatcher) (left: SignatureQuery) (right: ApiSignature) ctx =
    match left, right with
    | SignatureQuery.Signature (Arrow (leftReceiver :: leftMemberPart)), InstanceMember (declaringType, member') ->
      Debug.WriteLine("arrow and instance member rule.")
      lowTypeMatcher.TestReceiver leftReceiver declaringType ctx
      |> MatchingResult.bindMatched (testArrow lowTypeMatcher leftMemberPart (Member.toFunction member'))
      |> MatchingResult.mapMatched (Context.addDistance 1)
    | _ -> Continue ctx

  let unionCaseRule testArrow (lowTypeMatcher: ILowTypeMatcher) (left: SignatureQuery) (right: ApiSignature) ctx =
    match left, right with
    | SignatureQuery.Signature (Arrow leftElems), ApiSignature.UnionCase uc
    | SignatureQuery.Signature (LowType.Patterns.AbbreviationRoot (Arrow leftElems)), ApiSignature.UnionCase uc when uc.Fields.IsEmpty = false ->
      Debug.WriteLine("union case rule.")
      let caseAsFunc = UnionCase.toFunction uc
      testArrow lowTypeMatcher leftElems caseAsFunc ctx
    | SignatureQuery.Signature left, ApiSignature.UnionCase { DeclaringType = right; Fields = [] } ->
      Debug.WriteLine("union case rule.")
      lowTypeMatcher.Test left right ctx
    | _ -> Continue ctx

  let typeDefRule (lowTypeMatcher: ILowTypeMatcher) (left: SignatureQuery) (right: ApiSignature) ctx =
    match left, right with
    | SignatureQuery.Signature (Arrow _ | Wildcard _ | Variable _), ApiSignature.FullTypeDefinition _ -> Failure
    | SignatureQuery.Signature left, ApiSignature.FullTypeDefinition typeDef ->
      Debug.WriteLine("type def rule.")
      let right = typeDef.LowType
      lowTypeMatcher.Test left right ctx
    | _ -> Continue ctx

  let moduleDefRule (lowTypeMatcher: ILowTypeMatcher) (left: SignatureQuery) (right: ApiSignature) ctx =
    match left, right with
    | SignatureQuery.Signature (Arrow _ | Wildcard _ | Variable _), ApiSignature.ModuleDefinition _ -> Failure
    | SignatureQuery.Signature left, ApiSignature.ModuleDefinition moduleDef ->
      Debug.WriteLine("module def rule.")
      let right = moduleDef.LowType
      lowTypeMatcher.Test left right ctx
    | _ -> Continue ctx

  let typeAbbreviationRule (lowTypeMatcher: ILowTypeMatcher) (left: SignatureQuery) (right: ApiSignature) ctx =
    match left, right with
    | SignatureQuery.Signature (Arrow _ | Wildcard _ | Variable _), ApiSignature.TypeAbbreviation _ -> Failure
    | SignatureQuery.Signature left, ApiSignature.TypeAbbreviation abbreviationDef ->
      Debug.WriteLine("type abbreviation rule.")
      let abbreviation = abbreviationDef.TypeAbbreviation
      let right = Choice [ abbreviation.Abbreviation; abbreviation.Original ]
      lowTypeMatcher.Test left right ctx
    | _ -> Continue ctx

let tryGetSignatureQuery = function
  | QueryMethod.BySignature s -> Some s
  | QueryMethod.ByName (_, s) -> Some s
  | QueryMethod.ByActivePattern _ -> None
  | QueryMethod.ByComputationExpression _ -> None

let instance (options: SearchOptions) =
  let testArrow =
    match options.IgnoreParameterStyle with
    | Enabled -> Rules.testArrow_IgnoreParamStyle
    | Disabled -> Rules.testArrow

  let rec run (lowTypeMatcher: ILowTypeMatcher) (left: SignatureQuery) (right: ApiSignature) ctx =
    let rule =
      Rule.compose [
        yield Rules.choiceRule run

        yield Rules.moduleValueRule
        yield Rules.moduleFunctionRule testArrow
        yield Rules.activePatternRule testArrow

        yield Rules.staticMemberRule testArrow
        yield Rules.constructorRule testArrow
        
        yield Rules.instanceMemberUnitParameterRule
        yield Rules.instanceMemberAndFunctionRule
        yield Rules.instanceMemberRule testArrow
        yield Rules.arrowAndInstanceMemberRule testArrow
        
        yield Rules.arrowQueryAndDelegateRule

        yield Rules.unionCaseRule testArrow

        yield Rules.typeDefRule
        yield Rules.moduleDefRule
        yield Rules.typeAbbreviationRule

        yield Rule.terminator
      ]
    Rule.run rule lowTypeMatcher left right ctx

  { new IApiMatcher with
      member this.Name = "Signature Matcher"
      member this.Test lowTypeMatcher query api ctx =
        match tryGetSignatureQuery query with
        | Some (SignatureQuery.Wildcard) -> Matched ctx
        | Some s -> run lowTypeMatcher s api.Signature ctx
        | None -> Matched ctx }