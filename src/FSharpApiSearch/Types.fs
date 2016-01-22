module FSharpApiSearch.Types

type Source = Query | Target

type TypeIdentity =
  | Variable of Source * string
  | Type of string
type Type =
  | TypeIdentity of TypeIdentity
  | Arrow of Type list
  | Generic of TypeIdentity * Type list
  | Tuple of Type list

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Type =
  let rec private collectVariables' = function
    | TypeIdentity (Variable _) as v -> [ v ]
    | (Tuple xs | Arrow xs) -> List.collect collectVariables' xs
    | TypeIdentity (Type _) -> []
    | Generic (x, ys) -> List.concat (collectVariables' (TypeIdentity x) :: (List.map collectVariables' ys))
    
  let collectVariables t = collectVariables' t |> List.distinct |> List.sort

type Query = {
  OriginalString: string
  Query: Type
}