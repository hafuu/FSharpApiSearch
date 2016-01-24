module FSharpApiSearch.Matcher

open FSharpApiSearch.Types

type Equations = {
  Equalities: (Signature * Signature) list
  Inequalities: (Signature * Signature) list
}

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Equations =
  let sortTerm x y = if x <= y then (x, y) else (y, x)

  let containsEquality left right eqs =
    let x = sortTerm left right
    eqs.Equalities |> List.contains x
  let findEqualities left eqs = eqs.Equalities |> List.filter (fst >> ((=)left))

  let testInequality left right eqs =
    eqs.Inequalities
    |> List.choose (fun (x, y) -> if x = left then Some y elif y = left then Some x else None)
    |> List.forall (fun inequalityTerm ->
      let xy = sortTerm right inequalityTerm
      eqs.Equalities |> List.exists ((=)xy) |> not
    )

  let tryAddEquality left right eqs =
    let left, right = sortTerm left right
    if testInequality left right eqs then
      Some { eqs with Equalities = (left, right) :: eqs.Equalities }
    else
      None

  let empty = { Equalities = []; Inequalities = [] }

  let strict { Query = t } =
    let nonEqualities =
      [
        let variables = Signature.collectVariables t
        for x in variables do
          for y in variables do
            if x < y then yield (x, y)
      ]
    { Equalities = []; Inequalities = nonEqualities }

type MatchResult =
  | Success of Equations
  | Failure

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module MatchResult =
  let ofOption = function
    | Some newEqs -> Success newEqs
    | None -> Failure

  let inline bind f = function Success x -> f x | Failure -> Failure

let rec run (left: Signature) (right: Signature) (eqs: Equations): MatchResult =
  match left, right with
  | _, Unknown
  | Unknown, _ ->
    Failure
  | Identity leftName, Identity rightName ->
    if leftName = rightName then
      Success eqs
    else
      Failure
  | Arrow leftTypes, Arrow rightTypes ->
    runGeneric leftTypes rightTypes eqs
  | Generic (leftId, leftParams), Generic (rightId, rightParams) ->
    runGeneric (leftId :: leftParams) (rightId :: rightParams) eqs
  | Tuple leftTypes, Tuple rightTypes ->
    runGeneric leftTypes rightTypes eqs
  | Variable _, Variable _ ->
    let left, right = Equations.sortTerm left right
    if Equations.containsEquality left right eqs then
      Success eqs
    else
      attemptToAddEquality left right eqs
  | (Variable _ as left), right
  | right, (Variable _ as left) ->
    attemptToAddEquality left right eqs
  | _ ->
    Failure
and runGeneric (leftTypes: Signature list) (rightTypes: Signature list) (eqs: Equations): MatchResult =
  if leftTypes.Length <> rightTypes.Length then
    Failure
  else
    List.zip leftTypes rightTypes
    |> List.fold (fun result (left, right) -> MatchResult.bind (run left right) result) (Success eqs)
and attemptToAddEquality left right eqs =
  eqs
  |> Equations.findEqualities left
  |> List.fold (fun result (_, x) -> MatchResult.bind (run right x) result) (Success eqs)
  |> MatchResult.bind (Equations.tryAddEquality left right >> MatchResult.ofOption)

let matches { Query = query } target initialEquations =
  match run query target initialEquations with
  | Success _ -> true
  | Failure -> false