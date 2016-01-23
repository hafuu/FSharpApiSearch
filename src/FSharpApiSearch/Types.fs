module FSharpApiSearch.Types

type Source = Query | Target

type Signature =
  | Variable of Source * string
  | Identity of string
  | Arrow of Signature list
  | Generic of Signature * Signature list
  | Tuple of Signature list

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Type =
  let rec private collectVariables' = function
    | Variable _ as v -> [ v ]
    | (Tuple xs | Arrow xs) -> List.collect collectVariables' xs
    | Identity _ -> []
    | Generic (x, ys) -> List.collect collectVariables' (x :: ys)
    
  let collectVariables t = collectVariables' t |> List.distinct |> List.sort

type Query = {
  OriginalString: string
  Query: Signature
}