module FSharpApiSearch.Types

type Source = Query | Target

type Signature =
  | Variable of Source * string
  | Identity of string
  | Arrow of Signature list
  | Generic of Signature * Signature list
  | Tuple of Signature list
  | Unknown

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Signature =
  let rec private collectVariables' = function
    | Variable _ as v -> [ v ]
    | (Tuple xs | Arrow xs) -> List.collect collectVariables' xs
    | Identity _ -> []
    | Generic (x, ys) -> List.collect collectVariables' (x :: ys)
    | Unknown -> []

  let collectVariables t = collectVariables' t |> List.distinct |> List.sort

  let rec display = function
    | Variable (_, name) -> "'" + name
    | Identity name -> name
    | Arrow xs ->
      xs
      |> Seq.map (function
        | Arrow _ as a -> sprintf "(%s)" (display a)
        | x -> display x
      )
      |> String.concat " -> "
    | Generic (x, ys) -> sprintf "%s<%s>" (display x) (ys |> Seq.map display |> String.concat ", ")
    | Tuple xs ->
      xs
      |> Seq.map (function
        | Tuple _ as t -> sprintf "(%s)" (display t)
        | x -> display x)
      |> String.concat " * "
    | Unknown -> "Unknown"

type Query = {
  OriginalString: string
  Query: Signature
}

type Api = {
  Name: string
  Signature: Signature
}