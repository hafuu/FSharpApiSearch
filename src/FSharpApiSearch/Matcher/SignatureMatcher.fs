module internal FSharpApiSearch.SignatureMatcher

open System.Diagnostics
open FSharpApiSearch.MatcherTypes
open FSharpApiSearch.SpecialTypes
open FSharpApiSearch.Printer

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

  let trimOptionalParameters ((leftParams, _): Arrow) (right: Function) : Function =
    match right with
    | [ nonCurriedParameters ], ret ->
      let leftLength = (leftParams |> List.sumBy (function Tuple { Elements = xs } -> xs.Length | _ -> 1))
      let rightLength = nonCurriedParameters.Length
      if leftLength < rightLength then
        let trimedParameters, extraParameters = List.splitAt leftLength nonCurriedParameters
        if List.forall (fun x -> x.IsOptional) extraParameters then
          Debug.WriteLine(sprintf "trimed %d parameters." (rightLength - leftLength))
          [ trimedParameters ], ret
        else
          right
      else
        right
    | _ ->
      right

  type TestArrow = ILowTypeMatcher -> Arrow -> Function -> Context -> MatchingResult

  let testArrow (lowTypeMatcher: ILowTypeMatcher) (left: Arrow) (right: Function) ctx =
    Debug.WriteLine("test arrow.")
    let right = trimOptionalParameters left right
    let test ctx =
      let right = Function.toArrow right
      lowTypeMatcher.TestArrow left right ctx
    match (fst left), (fst right) with
    | [ WildcardOrVariable ], [ [ _ ] ] -> test ctx
    | [ WildcardOrVariable ], [ _ ] -> Failure
    | _ -> test ctx

  let (|Right_CurriedFunction|_|) (right: Function) =
    let ps, ret = right
    if ps |> List.forall (function [ _ ] -> true | _ -> false) then
      Some (ps |> List.map (fun x -> x.Head.Type), ret.Type)
    else
      None

  let (|Right_NonCurriedFunction|_|) (right: Function) =
    match right with
    | ([ parameters ], ret) when parameters.Length >= 2 -> Some (parameters |> List.map (fun x -> x.Type), ret.Type)
    | _ -> None

  let (|Right_TupleFunction|_|) (right: Function) =
    match right with
    | [ [ { Type = Tuple { Elements = parameters } } ] ], { Type = ret } -> Some (parameters, ret)
    | _ -> None

  let (|Left_CurriedFunction|_|) (left: Arrow) =
    match left with
    | [ Tuple _ ], _ -> None
    | _ -> Some left

  let (|Left_NonCurriedFunction|_|) (left: Arrow) =
    match left with
    | [ Tuple { Elements = parameters } ], ret -> Some (parameters, ret)
    | _ -> None

  let testArrow_IgnoreParamStyle (lowTypeMatcher: ILowTypeMatcher) (left: Arrow) (right: Function) ctx =
    Debug.WriteLine("test arrow (ignore parameter style).")
    let right = trimOptionalParameters left right
    match left, right with
    | ([ _ ], _), ([ [ _ ] ], _) ->
      Debug.WriteLine("pattern 1")
      lowTypeMatcher.TestArrow left (Function.toArrow right) ctx
    | Left_NonCurriedFunction left, Right_CurriedFunction right ->
      Debug.WriteLine("pattern 2")
      lowTypeMatcher.TestArrow left right ctx
      |> MatchingResult.mapMatched (Context.addDistance "parameter style" 1)
    | Left_CurriedFunction left, (Right_TupleFunction right | Right_NonCurriedFunction right) ->
      Debug.WriteLine("pattern 3")
      lowTypeMatcher.TestArrow left right ctx
      |> MatchingResult.mapMatched (Context.addDistance "parameter style" 1)
    | _, _ ->
      Debug.WriteLine("pattern 4")
      testArrow lowTypeMatcher left right ctx
      
  let moduleFunctionRule (testArrow: TestArrow) (lowTypeMatcher: ILowTypeMatcher) (left: SignatureQuery) (right: ApiSignature) ctx =
    match left, right with
    | SignatureQuery.Signature (Arrow left), ApiSignature.ModuleFunction right
    | SignatureQuery.Signature (LowType.Patterns.AbbreviationRoot (Arrow left)), ApiSignature.ModuleFunction right ->
      Debug.WriteLine("module function rule.")
      testArrow lowTypeMatcher left right ctx
    | SignatureQuery.Signature leftRet, ApiSignature.ModuleFunction right ->
      Debug.WriteLine("module function rule.")
      let left = [], leftRet
      testArrow lowTypeMatcher left right ctx
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
      |> MatchingResult.mapMatched (Context.addDistance "arrow and delegate" 1)
    | _ -> Continue ctx

  let activePatternRule (testArrow: TestArrow) (lowTypeMatcher: ILowTypeMatcher) (left: SignatureQuery) (right: ApiSignature) ctx =
    match left, right with
    | SignatureQuery.Signature (Arrow leftElems), ApiSignature.ActivePatten (_, rightElems) ->
      Debug.WriteLine("active pattern rule.")
      testArrow lowTypeMatcher leftElems rightElems ctx
    | _ -> Continue ctx

  let breakArrow = function
    | Arrow xs -> xs
    | other -> [], other

  let (|StaticMember|_|) = function
    | ApiSignature.StaticMember (_, member') -> Some member'
    | ApiSignature.TypeExtension { MemberModifier = MemberModifier.Static; Member = member' } -> Some member'
    | ApiSignature.ExtensionMember member' -> Some member'
    | _ -> None

  let (|NoArgsMember|_|) = function
    | { Parameters = [] } as m -> Some m
    | _ -> None

  let extensionMemberRule (testArrow: TestArrow) (lowTypeMatcher: ILowTypeMatcher) (left: SignatureQuery) (right: ApiSignature) ctx =
    match left, right with
    | SignatureQuery.Signature (Arrow ([ leftReceiver; leftParams ], leftReturnType)), ApiSignature.ExtensionMember ({ Parameters = [ rightReceiver :: rightParams ] } as member' ) ->
      Debug.WriteLine("extension member rule.")
      let left = [ leftParams ], leftReturnType
      let right = { member' with Parameters = [ rightParams ] }
      lowTypeMatcher.TestReceiver leftReceiver rightReceiver.Type ctx
      |> MatchingResult.bindMatched (testArrow lowTypeMatcher left (Member.toFunction right))
    | _ -> Continue ctx

  let staticMemberRule (testArrow: TestArrow) (lowTypeMatcher: ILowTypeMatcher) (left: SignatureQuery) (right: ApiSignature) ctx =
    match left, right with
    | SignatureQuery.Signature (Arrow _), StaticMember (NoArgsMember _) ->
      Debug.WriteLine("Arrow and static no args member do not match.")
      Failure
    | SignatureQuery.Signature left, StaticMember member' ->
      Debug.WriteLine("static member rule.")
      testArrow lowTypeMatcher (breakArrow left) (Member.toFunction member') ctx
    | _ -> Continue ctx

  let constructorRule (testArrow: TestArrow) (lowTypeMatcher: ILowTypeMatcher) (left: SignatureQuery) (right: ApiSignature) ctx =
    match left, right with
    | SignatureQuery.Signature left, ApiSignature.Constructor (_, member') ->
      Debug.WriteLine("constructor rule.")
      testArrow lowTypeMatcher (breakArrow left) (Member.toFunction member') ctx 
    | _ -> Continue ctx

  let (|InstanceMember|_|) = function
    | ApiSignature.InstanceMember (declaringType, member') -> Some (declaringType, member')
    | ApiSignature.TypeExtension { MemberModifier = MemberModifier.Instance; ExistingType = declaringType; Member = member' } -> Some (declaringType, member')
    | _ -> None

  let arrowAndInstanceMemberRule (testArrow: TestArrow) (lowTypeMatcher: ILowTypeMatcher) (left: SignatureQuery) (right: ApiSignature) ctx =
    match left, right with
    | SignatureQuery.Signature (Arrow ((leftReceiver :: leftMemberParams), leftMemberRet)), InstanceMember (declaringType, member') ->
      Debug.WriteLine("arrow and instance member rule.")
      lowTypeMatcher.TestReceiver leftReceiver declaringType ctx
      |> MatchingResult.bindMatched (testArrow lowTypeMatcher (leftMemberParams, leftMemberRet) (Member.toFunction member'))
    | _ -> Continue ctx

  let unionCaseRule (testArrow: TestArrow) (lowTypeMatcher: ILowTypeMatcher) (left: SignatureQuery) (right: ApiSignature) ctx =
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
    | SignatureQuery.Signature (Arrow _ | Wildcard _ | Variable _ | LowType.Subtype _), ApiSignature.ModuleDefinition _ -> Failure
    | SignatureQuery.Signature left, ApiSignature.ModuleDefinition moduleDef ->
      Debug.WriteLine("module def rule.")
      let right = moduleDef.LowType
      lowTypeMatcher.Test left right ctx
    | _ -> Continue ctx

  let typeAbbreviationRule (lowTypeMatcher: ILowTypeMatcher) (left: SignatureQuery) (right: ApiSignature) ctx =
    match left, right with
    | SignatureQuery.Signature (Arrow _ | Wildcard _ | Variable _), ApiSignature.TypeAbbreviation _ -> Failure
    | SignatureQuery.Signature (LowType.Subtype _ as left), ApiSignature.TypeAbbreviation { Original = right } ->
      Debug.WriteLine("type abbreviation rule.")
      lowTypeMatcher.Test left right ctx
    | SignatureQuery.Signature left, ApiSignature.TypeAbbreviation abbreviationDef ->
      Debug.WriteLine("type abbreviation rule.")
      let abbreviation = abbreviationDef.TypeAbbreviation
      let right = Choice [ abbreviation.Abbreviation; abbreviation.Original ]
      lowTypeMatcher.Test left right ctx
    | _ -> Continue ctx

let tryGetSignatureQuery = function
  | QueryMethod.BySignature s -> Some s
  | QueryMethod.ByName (_, s) -> Some s
  | QueryMethod.ByNameOrSignature (_, s) -> Some s
  | QueryMethod.ByActivePattern _ -> None
  | QueryMethod.ByComputationExpression _ -> None

let instance (options: SearchOptions) =
  let testArrow : Rules.TestArrow =
    match options.IgnoreParameterStyle with
    | Enabled -> Rules.testArrow_IgnoreParamStyle
    | Disabled -> Rules.testArrow

  let rec run (lowTypeMatcher: ILowTypeMatcher) (left: SignatureQuery) (right: ApiSignature) ctx =
    let rule =
      Rule.compose [|
        yield Rules.choiceRule run

        yield Rules.moduleValueRule
        yield Rules.moduleFunctionRule testArrow
        yield Rules.activePatternRule testArrow

        yield Rule.continueFailure (Rules.extensionMemberRule testArrow)
        yield Rules.staticMemberRule testArrow
        yield Rules.constructorRule testArrow
        
        yield Rules.arrowAndInstanceMemberRule testArrow
        
        yield Rules.arrowQueryAndDelegateRule

        yield Rules.unionCaseRule testArrow

        yield Rules.typeDefRule
        yield Rules.moduleDefRule
        yield Rules.typeAbbreviationRule

        yield Rule.terminator
      |]
    Rule.run rule lowTypeMatcher left right ctx

  { new IApiMatcher with
      member this.Name = "Signature Matcher"
      member this.Test lowTypeMatcher query api ctx =
        match tryGetSignatureQuery query.Method with
        | Some (SignatureQuery.Wildcard) -> Matched ctx
        | Some s -> run lowTypeMatcher s api.Signature ctx
        | None -> Matched ctx }