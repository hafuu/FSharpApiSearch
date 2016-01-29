module FSharpApiSearch.Matcher

open FSharpApiSearch.Types
open System.Diagnostics

type Equations = {
  Equalities: (Signature * Signature) list
  Inequalities: (Signature * Signature) list
}

type Context = {
  Distance: int
  Equations: Equations
}

type MatchResult =
  | Success of Context
  | Failure

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Context =
  let setEquations eqs ctx = { ctx with Equations = eqs }
  let addDistance x (ctx: Context) =
    let newDistance = ctx.Distance + x
    Debug.WriteLine(sprintf "Update distance from %d to %d" ctx.Distance newDistance)
    { ctx with Distance = newDistance }

  let initialize eqs = { Distance = 0; Equations = eqs }

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

  let tryAddEquality left right (ctx: Context) =
    let left, right = sortTerm left right
    if testInequality left right ctx.Equations then
      let eqs = ctx.Equations
      let newEqs = { eqs with Equalities = (left, right) :: eqs.Equalities }
      Success (Context.setEquations newEqs ctx)
    else
      Failure

  let empty = { Equalities = []; Inequalities = [] }

  let strict query =
    let t =
      match query.Method with
      | ByName (_, SignatureQuery s) -> s
      | BySignature s -> s
      | _ -> Unknown
    let nonEqualities =
      [
        let variables = Signature.collectUniqueVariables t |> List.sort
        for x in variables do
          for y in variables do
            if x < y then yield (x, y)
      ]
    { Equalities = []; Inequalities = nonEqualities }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module MatchResult =
  let toBool = function
    | Success _ -> true
    | Failure -> false

  let inline bind f = function Success x -> f x | Failure -> Failure
  let inline map f = function Success x -> Success (f x) | Failure -> Failure

  let distance = function
    | Success { Distance = distance } -> distance
    | Failure -> System.Int32.MaxValue

let rec distanceFromVariable = function
  | Identity _ -> 1
  | Variable _ -> 0
  | Generic _ -> 1
  | Tuple _ -> 1
  | Arrow xs -> List.sumBy (distanceFromVariable >> max 1) xs
  | StaticMethod (xs, y) -> List.sumBy (distanceFromVariable >> max 1) (y :: xs)
  | Unknown -> 0

let addLast x xs = [ yield! xs; yield x ]

let rec matchesSignature (left: Signature) (right: Signature) (ctx: Context): MatchResult =
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
        Success ctx
      else
        Debug.WriteLine("There are deferent identities.")
        Failure
    | Arrow leftTypes, Arrow rightTypes ->
      Debug.WriteLine("arrow type")
      runGeneric leftTypes rightTypes ctx
    | Arrow arrowTypes, StaticMethod (methodArguments, returnType)
    | StaticMethod (methodArguments, returnType), Arrow arrowTypes ->
      Debug.WriteLine("static method and arrow")
      runGeneric arrowTypes (addLast returnType methodArguments) ctx
    | StaticMethod (leftArguments, leftReturnType), StaticMethod (rightArguments, rightReturnType) ->
      Debug.WriteLine("both static method")
      runGeneric (leftReturnType :: leftArguments) (rightReturnType :: rightArguments) ctx
    | Generic (leftId, leftParams), Generic (rightId, rightParams) ->
      Debug.WriteLine("generic type")
      runGeneric (leftId :: leftParams) (rightId :: rightParams) ctx
    | Tuple leftTypes, Tuple rightTypes ->
      Debug.WriteLine("tuple type")
      runGeneric leftTypes rightTypes ctx
    | Variable _, Variable _ ->
      Debug.WriteLine("both variable")
      Debug.WriteLine(sprintf "equations: %s" (Equations.debugDisplay ctx.Equations))
      if Equations.containsEquality left right ctx.Equations then
        Debug.WriteLine("The equality already exists.")
        Success ctx
      else
        attemptToAddEquality left right ctx
    | (Variable _ as left), right
    | right, (Variable _ as left) ->
      Debug.WriteLine("either variable or other")
      Debug.WriteLine(sprintf "equations: %s" (Equations.debugDisplay ctx.Equations))
      if Equations.containsEquality left right ctx.Equations then
        Debug.WriteLine("The equality already exists.")
        Success ctx
      else
        attemptToAddEquality left right ctx
        |> MatchResult.map (Context.addDistance (distanceFromVariable right))
    | _ ->
      Failure
  finally
    Debug.Unindent()
and runGeneric (leftTypes: Signature list) (rightTypes: Signature list) (ctx: Context): MatchResult =
  Debug.WriteLine(sprintf "test parameters: %A, %A" (List.map Signature.debugDisplay leftTypes) (List.map Signature.debugDisplay rightTypes))
  if leftTypes.Length <> rightTypes.Length then
    Debug.WriteLine("The number of the parameters is different.")
    Failure
  else
    List.zip leftTypes rightTypes
    |> List.fold (fun result (left, right) -> MatchResult.bind (matchesSignature left right) result) (Success ctx)
and attemptToAddEquality left right (ctx: Context) =
  let left, right = Equations.sortTerm left right
  Debug.WriteLine(sprintf "test equaliity: %s = %s" (Signature.debugDisplay left) (Signature.debugDisplay right))
  if Equations.isRecirsive left right then
    Failure
  elif Equations.isCircular left right ctx.Equations then
    Failure
  else
    let leftEqualities = ctx.Equations |> Equations.findEqualities left
    Debug.WriteLine(sprintf "It found known equalities of %s.: %A" (Signature.debugDisplay left) (List.map Equations.debugDisplayEquality leftEqualities))
    let result =
      leftEqualities
      |> List.fold (fun result (_, x) -> MatchResult.bind (matchesSignature right x) result) (Success ctx)
      |> MatchResult.bind (Equations.tryAddEquality left right)
    Debug.WriteLine(
      match result with
      | Success _ -> sprintf "It was added the equality.: %s = %s" (Signature.debugDisplay left) (Signature.debugDisplay right)
      | Failure -> sprintf "It failed to add the equality.: %s = %s" (Signature.debugDisplay left) (Signature.debugDisplay right))
    result

let matchesName queryName targetApi =
  let targetName = targetApi.Name.Split('.') |> Seq.last
  queryName = targetName

let matches query targetApi initialEquations =
  let result =
    match query.Method with
    | ByName (name, _) when matchesName name targetApi = false -> Failure
    | ByName (_, Wildcard) -> Success initialEquations
    | ByName (_, SignatureQuery signature) 
    | BySignature signature -> matchesSignature signature targetApi.Signature initialEquations
  result