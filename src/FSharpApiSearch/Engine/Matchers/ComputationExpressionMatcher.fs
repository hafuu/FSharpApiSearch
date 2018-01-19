module internal FSharpApiSearch.ComputationExpressionMatcher

open EngineTypes

module Filter =
  let instance (_: SearchOptions) =
    { new IApiMatcher with
        member this.Name = "Computation Expression Filter"
        member this.Test lowTypeMatcher query api ctx =
          match api.Kind with
          | ApiKind.ComputationExpressionBuilder -> Failure
          | _ -> Matched ctx }

type ComputationExpressionBuilderRule = ILowTypeMatcher -> ComputationExpressionQuery -> ComputationExpressionBuilder -> Context -> MatchingResult

let ceBuilderTypeRule (lowTypeMatcher: ILowTypeMatcher) (query: ComputationExpressionQuery) (builder: ComputationExpressionBuilder) ctx =
  let right = Choice.create (builder.BuilderType, builder.ComputationExpressionTypes)
  lowTypeMatcher.Test query.Type right ctx

let syntaxRule (_: ILowTypeMatcher) (query: ComputationExpressionQuery) (builder: ComputationExpressionBuilder) ctx =
  let builderSyntaxes = builder.Syntaxes |> List.map (fun s -> s.Syntax, s.Position) |> dict
  query.Syntaxes
  |> List.fold (fun result querySyntax ->
    result
    |> MatchingResult.bindMatched (fun ctx ->
      match builderSyntaxes.TryGetValue querySyntax.Syntax with
      | true, sigPos ->
        let newCtx =
          match querySyntax.Position, sigPos with
          | AtQuery (Some queryId, _), AtSignature sigId -> { ctx with MatchPositions = Map.add sigId queryId ctx.MatchPositions }
          | _ -> ctx
        Matched newCtx
      | _ -> Failure
    )
  ) (Matched ctx)

let ceBuilderRules : ComputationExpressionBuilderRule =
  Rule.compose [|
    ceBuilderTypeRule |> Rule.matchedToContinue
    syntaxRule
  |]

let test (lowTypeMatcher: ILowTypeMatcher) (builderTypes: LowType) (ctx: Context) (api: Api) =
  match api.Signature with
  | ApiSignature.ModuleValue (TypeAbbreviation ({ Original = Arrow ((_, ret), _) }, _)) -> lowTypeMatcher.Test builderTypes ret ctx
  | ApiSignature.ModuleValue value -> lowTypeMatcher.Test builderTypes value ctx
  | ApiSignature.ModuleFunction (_, ret) -> lowTypeMatcher.Test builderTypes ret.Type ctx
  | _ -> Failure

let search (seqFunc: SeqFunctions) (targets: ApiDictionary seq) (lowTypeMatcher: ILowTypeMatcher) (query: ComputationExpressionQuery) (initialContext: Context) =
  let builderTypes =
    targets
    |> seqFunc.Collect (fun target -> seq {
      for api in target.Api do
        match api.Signature with
        | ApiSignature.ComputationExpressionBuilder builder ->
          match Rule.run ceBuilderRules lowTypeMatcher query builder initialContext with
          | Matched ctx ->
            let result = { Distance = ctx.Distance; Api = api; AssemblyName = target.AssemblyName; MatchPositions = ctx.MatchPositions }
            yield (result, builder.BuilderType)
          | _ -> ()
        | _ -> ()
    })
    |> Seq.toList


  if List.isEmpty builderTypes then
    Seq.empty
  else
    let builderResults = builderTypes |> List.map fst

    let builderTypes = Choice.create (query.Type, builderTypes |> List.map snd)

    let apiResults =
      targets
      |> Seq.collect (fun dic -> dic.Api |> Seq.map (fun api -> (dic, api)))
      |> seqFunc.Choose (fun (dic, api) ->
        match test lowTypeMatcher builderTypes initialContext api with
        | Matched ctx -> Some { Distance = ctx.Distance; Api = api; AssemblyName = dic.AssemblyName; MatchPositions = ctx.MatchPositions }
        | _ -> None
      )

    Seq.append builderResults apiResults