[<AutoOpen>]
module FSharpApiSearch.Types

type Source = Query | Target

type Signature =
  | WildcardGroup of string
  | Wildcard
  | Variable of Source * string
  | StrongVariable of Source * string
  | Identity of string
  | StrongIdentity of string
  | Arrow of Signature list
  | Generic of Signature * Signature list
  | Tuple of Signature list
  | StaticMethod of arguments: Signature list * returnType: Signature
  | Unknown

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Signature =
  let rec collectVariables = function
    | Variable _ as v -> [ v ]
    | StrongVariable _ as v -> [ v ]
    | (Tuple xs | Arrow xs) -> List.collect collectVariables xs
    | Identity _ -> []
    | StrongIdentity _ -> []
    | Wildcard -> []
    | WildcardGroup _ -> []
    | Generic (x, ys) -> List.collect collectVariables (x :: ys)
    | StaticMethod (parameters, returnType) -> List.collect collectVariables (returnType :: parameters)
    | Unknown -> []

  let rec collectWildcardGroup = function
    | Variable _ -> []
    | StrongVariable _ -> []
    | (Tuple xs | Arrow xs) -> List.collect collectWildcardGroup xs
    | Identity _ -> []
    | StrongIdentity _ -> []
    | Wildcard -> []
    | WildcardGroup _ as w -> [ w ]
    | Generic (x, ys) -> List.collect collectWildcardGroup (x :: ys)
    | StaticMethod (parameters, returnType) -> List.collect collectWildcardGroup (returnType :: parameters)
    | Unknown -> []

  let collectVariableOrWildcardGroup x = List.concat [ collectVariables x; collectWildcardGroup x ]

  let rec private display' prefix = function
    | Variable (source, name) -> prefix source name
    | StrongVariable (source, name) -> "!" + prefix source name
    | Identity name -> name
    | StrongIdentity name -> "!" + name
    | Wildcard -> "?"
    | WildcardGroup name -> "?" + name
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
    | StaticMethod (parameters, returnType) -> sprintf "%s -> %s" (display' prefix (Tuple parameters)) (display' prefix returnType)
    | Unknown -> "Unknown"

  let rec display = display' (fun _ name -> "'" + name)

  let rec debugDisplay = display' (fun source name -> match source with Query -> "'q" + name | Target -> "'t" + name)

type SignaturePart =
  | SignatureQuery of Signature
  | AnySignature

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