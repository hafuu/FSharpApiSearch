module internal FSharpApiSearch.ComputationExpressionMatcher

open MatcherTypes
open FSharp.Collections.ParallelSeq

module Filter =
  let instance (_: SearchOptions) =
    { new IApiMatcher with
        member this.Name = "Computation Expression Filter"
        member this.Test lowTypeMatcher query api ctx =
          match api.Kind with
          | ApiKind.ComputationExpressionBuilder -> Failure
          | _ -> Matched ctx }

let private collect (options: SearchOptions) f (xs: #seq<_>) =
  match options.Parallel with
  | Enabled -> PSeq.collect f xs :> seq<_>
  | Disabled -> Seq.collect f xs

let private choose (options: SearchOptions) f xs =
  match options.Parallel with
  | Enabled -> PSeq.choose f xs :> seq<_>
  | Disabled -> Seq.choose f xs

let private append options xs ys =
  match options.Parallel with
  | Enabled -> PSeq.append xs ys :> seq<_>
  | Disabled -> Seq.append xs ys

let test (lowTypeMatcher: ILowTypeMatcher) (builderTypes: LowType) (ctx: Context) (api: Api) =
  match api.Signature with
  | ApiSignature.ModuleValue (TypeAbbreviation { Original = Arrow (_, ret) }) -> lowTypeMatcher.Test builderTypes ret ctx
  | ApiSignature.ModuleValue value -> lowTypeMatcher.Test builderTypes value ctx
  | ApiSignature.ModuleFunction (_, ret) -> lowTypeMatcher.Test builderTypes ret.Type ctx
  | _ -> Failure

let testComputationExpressionTypes (lowTypeMatcher: ILowTypeMatcher) ctx queryCeType ceTypes =
  ceTypes |> Seq.exists (fun t -> lowTypeMatcher.Test t queryCeType ctx |> MatchingResult.toBool)

let search (options: SearchOptions) (targets: ApiDictionary seq) (lowTypeMatcher: ILowTypeMatcher) (query: ComputationExpressionQuery) (initialContext: Context) =
  let querySyntaxes = Set.ofList query.Syntaxes

  let testSyntaxes =
    if query.Syntaxes.IsEmpty then
      fun syntaxes -> Set.isEmpty syntaxes = false
    else
      fun syntaxes -> Set.intersect syntaxes querySyntaxes = querySyntaxes

  let builderTypes =
    targets
    |> collect options (fun target ->
      target.Api
      |> Seq.choose (fun api ->
        match api.Signature with
        | ApiSignature.ComputationExpressionBuilder builder ->
          Some (api, builder)
        | _ -> None
      )
      |> Seq.filter (fun (_, builder) -> testComputationExpressionTypes lowTypeMatcher initialContext query.Type builder.ComputationExpressionTypes)
      |> Seq.filter (fun (_, builder) -> testSyntaxes (Set.ofList builder.Syntaxes))
      |> Seq.map (fun (api, builder) ->
        let result = { Distance = 0; Api = api; AssemblyName = target.AssemblyName }
        (result, builder.BuilderType)
      )
    )
    |> Seq.toList

  let builderResults = builderTypes |> Seq.map fst

  let builderTypes = Choice (builderTypes |> List.map snd)

  let apiResults =
    targets
    |> Seq.collect (fun dic -> dic.Api |> Seq.map (fun api -> (dic, api)))
    |> choose options (fun (dic, api) ->
      match test lowTypeMatcher builderTypes initialContext api with
      | Matched ctx -> Some { Distance = ctx.Distance; Api = api; AssemblyName = dic.AssemblyName }
      | _ -> None
    )

  append options builderResults apiResults