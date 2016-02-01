[<AutoOpen>]
module FSharpApiSearch.Types

open System.Text.RegularExpressions

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
  | StaticMethod of StaticMethodInfo
  | InstanceMember of InstanceMemberInfo
  | Unknown
and InstanceMemberInfo = {
  Source: Source
  Receiver: Signature
  Arguments: Signature list
  ReturnType: Signature
}
and StaticMethodInfo = {
  Arguments: Signature list
  ReturnType: Signature
}

let internal arrayRegexPattern = @"\[,*\]"

let (|Array|_|) = function
  | Generic (Identity name, [ t ]) when Regex.IsMatch(name, arrayRegexPattern) ->
    let dimension = name.Split(',').Length
    Some (dimension, name, t)
  | _ -> None

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
    | StaticMethod x -> List.collect collectVariables (x.ReturnType :: x.Arguments)
    | InstanceMember x -> List.collect collectVariables (x.Receiver :: x.ReturnType :: x.Arguments)
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
    | StaticMethod x -> List.collect collectWildcardGroup (x.ReturnType :: x.Arguments)
    | InstanceMember x -> List.collect collectWildcardGroup (x.Receiver :: x.ReturnType :: x.Arguments)
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
    | Array (_, name, x) -> display' prefix x + name
    | Generic (x, ys) -> sprintf "%s<%s>" (display' prefix x) (ys |> Seq.map (display' prefix) |> String.concat ", ")
    | Tuple xs ->
      xs
      |> Seq.map (function
        | Tuple _ as t -> sprintf "(%s)" (display' prefix t)
        | x -> display' prefix x)
      |> String.concat " * "
    | StaticMethod x -> sprintf "%s -> %s" (display' prefix (Tuple x.Arguments)) (display' prefix x.ReturnType)
    | InstanceMember x ->
      match x.Arguments with
      | [] -> sprintf "%s" (display' prefix x.ReturnType)
      | _ -> sprintf "%s -> %s" (display' prefix (Tuple x.Arguments)) (display' prefix x.ReturnType)
    | Unknown -> "Unknown"

  let display = display' (fun _ name -> "'" + name)

  let debugDisplay signature =
    let display = display' (fun source name -> match source with Query -> "'q" + name | Target -> "'t" + name)
    match signature with
    | InstanceMember x -> sprintf "%s => %s" (display x.Receiver) (display signature)
    | _ -> display signature

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