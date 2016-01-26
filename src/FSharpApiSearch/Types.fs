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

  let collectVariables t = collectVariables' t

  let collectUniqueVariables t = collectVariables t |> List.distinct

  let rec private display' prefix = function
    | Variable (source, name) -> prefix source name
    | Identity name -> name
    | Arrow xs ->
      xs
      |> Seq.map (function
        | Arrow _ as a -> sprintf "(%s)" (display' prefix a)
        | x -> display' prefix x
      )
      |> String.concat " -> "
    | Generic (x, ys) -> sprintf "%s<%s>" (display' prefix x) (ys |> Seq.map (display' prefix) |> String.concat ", ")
    | Tuple xs ->
      xs
      |> Seq.map (function
        | Tuple _ as t -> sprintf "(%s)" (display' prefix t)
        | x -> display' prefix x)
      |> String.concat " * "
    | Unknown -> "Unknown"

  let rec display = display' (fun _ name -> "'" + name)

  let rec debugDisplay = display' (fun source name -> match source with Query -> "'q" + name | Target -> "'t" + name)

type SignaturePart =
  | SignatureQuery of Signature
  | Wildcard

type QueryMethod =
  | ByName of string * SignaturePart
  | BySignature of Signature

type Query = {
  OriginalString: string
  Method: QueryMethod
}

type Api = {
  Name: string
  Signature: Signature
}