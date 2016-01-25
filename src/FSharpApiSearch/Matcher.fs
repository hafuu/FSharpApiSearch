module FSharpApiSearch.Matcher

open FSharpApiSearch.Types
open System.Diagnostics

type Equations = {
  Equalities: (Signature * Signature) list
  Inequalities: (Signature * Signature) list
}

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Equations =
  let debugDisplayEquality (left, right) = Signature.debugDisplay left + " = " + Signature.debugDisplay right
  let debugDisplayInequality (left, right) = Signature.debugDisplay left + " <> " + Signature.debugDisplay right
  let debugDisplay x = sprintf "Equalities = %A, Inequalities = %A" (List.map debugDisplayEquality x.Equalities) (List.map debugDisplayInequality x.Inequalities)
  
  let sortTerm x y = if x <= y then (x, y) else (y, x)

  let containsEquality left right eqs =
    let x = sortTerm left right
    eqs.Equalities |> List.contains x
  let findEqualities left eqs = eqs.Equalities |> List.filter (fst >> ((=)left))

  let testInequality left right eqs =
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

  let isRecirsive left right =
    match left, right with
    | (Variable _ as variable), other
    | other, (Variable _ as variable) ->
      if Signature.collectVariables other |> List.exists ((=)variable) then
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
          for otherVariable in Signature.collectVariables other |> List.distinct do
            if x = otherVariable then
              yield (y, (x, y))
            elif y = otherVariable then
              yield (x, (x, y))
      }
      oneSteps
      |> Seq.exists (fun (other, xy) ->
        if Signature.collectVariables other |> Seq.exists ((=)variable) then
          Debug.WriteLine(sprintf "It is the circular type. It was derived from the following equation. : %s" (debugDisplayEquality xy))
          true
        else
          false)
    | _ -> false

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
        let variables = Signature.collectUniqueVariables t |> List.sort
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
  Debug.WriteLine(sprintf "begin test: %s, %s" (Signature.debugDisplay left) (Signature.debugDisplay right))
  Debug.Indent()
  try
    match left, right with
    | _, Unknown
    | Unknown, _ ->
      Failure
    | Identity leftName, Identity rightName ->
      Debug.WriteLine("identity type")
      if leftName = rightName then
        Debug.WriteLine("There are same identities.")
        Success eqs
      else
        Debug.WriteLine("There are deferent identities.")
        Failure
    | Arrow leftTypes, Arrow rightTypes ->
      Debug.WriteLine("arrow type")
      runGeneric leftTypes rightTypes eqs
    | Generic (leftId, leftParams), Generic (rightId, rightParams) ->
      Debug.WriteLine("generic type")
      runGeneric (leftId :: leftParams) (rightId :: rightParams) eqs
    | Tuple leftTypes, Tuple rightTypes ->
      Debug.WriteLine("tuple type")
      runGeneric leftTypes rightTypes eqs
    | Variable _, Variable _ ->
      Debug.WriteLine("both variable")
      Debug.WriteLine(sprintf "equations: %s" (Equations.debugDisplay eqs))
      if Equations.containsEquality left right eqs then
        Debug.WriteLine("The equality already exists.")
        Success eqs
      else
        attemptToAddEquality left right eqs
    | (Variable _ as left), right
    | right, (Variable _ as left) ->
      Debug.WriteLine("either variable or other")
      Debug.WriteLine(sprintf "equations: %s" (Equations.debugDisplay eqs))
      attemptToAddEquality left right eqs
    | _ ->
      Failure
  finally
    Debug.Unindent()
and runGeneric (leftTypes: Signature list) (rightTypes: Signature list) (eqs: Equations): MatchResult =
  Debug.WriteLine(sprintf "test parameters: %A, %A" (List.map Signature.debugDisplay leftTypes) (List.map Signature.debugDisplay rightTypes))
  if leftTypes.Length <> rightTypes.Length then
    Debug.WriteLine("The number of the parameters is different.")
    Failure
  else
    List.zip leftTypes rightTypes
    |> List.fold (fun result (left, right) -> MatchResult.bind (run left right) result) (Success eqs)
and attemptToAddEquality left right eqs =
  let left, right = Equations.sortTerm left right
  Debug.WriteLine(sprintf "test equaliity: %s = %s" (Signature.debugDisplay left) (Signature.debugDisplay right))
  if Equations.isRecirsive left right then
    Failure
  elif Equations.isCircular left right eqs then
    Failure
  else
    let leftEqualities = eqs |> Equations.findEqualities left
    Debug.WriteLine(sprintf "It found known equalities of %s.: %A" (Signature.debugDisplay left) (List.map Equations.debugDisplayEquality leftEqualities))
    let result =
      leftEqualities
      |> List.fold (fun result (_, x) -> MatchResult.bind (run right x) result) (Success eqs)
      |> MatchResult.bind (Equations.tryAddEquality left right >> MatchResult.ofOption)
    Debug.WriteLine(
      match result with
      | Success _ -> sprintf "It was added the equality.: %s = %s" (Signature.debugDisplay left) (Signature.debugDisplay right)
      | Failure -> sprintf "It failed to add the equality.: %s = %s" (Signature.debugDisplay left) (Signature.debugDisplay right))
    result

let matches { Query = query } target initialEquations =
  match run query target initialEquations with
  | Success _ -> true
  | Failure -> false