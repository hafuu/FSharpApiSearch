[<AutoOpen>]
module FSharpApiSearch.Types

open System.Text.RegularExpressions

type Source = Query | Target

type ReverseName = string list
type Identity =
  | PartialName of ReverseName
  | FullName of ReverseName

type Signature =
  | WildcardGroup of string
  | Wildcard
  | Variable of Source * string
  | StrongVariable of Source * string
  | Identity of Identity
  | StrongIdentity of Identity
  | Arrow of Signature list
  | Generic of Signature * Signature list
  | StaticMethod of StaticMethod
  | InstanceMember of InstanceMember
  | TypeAbbreviation of TypeAbbreviation
and InstanceMember = {
  Source: Source
  Receiver: Signature
  Arguments: Signature list
  ReturnType: Signature
}
and StaticMethod = {
  Arguments: Signature list
  ReturnType: Signature
}
and TypeAbbreviation = {
  Abbreviation: Signature
  Original: Signature
}

let internal arrayRegexPattern = @"\[,*\]"

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Signature =
  let private splitName (name: string) = name.Split('.') |> List.ofArray |> List.rev
  let partialName name = PartialName (splitName name)
  let fullName name = FullName (splitName name)

  [<Literal>]
  let TupleTypeName = "System.Tuple"
  let TupleIdentity = fullName TupleTypeName
  let tuple xs = Generic (Identity TupleIdentity, xs)

  let UnitIdentity = fullName "Microsoft.FSharp.Core.Unit"
  let UnitAbbreviationIdentity = fullName "Microsoft.FSharp.Core.unit"
  
  let testIdentity x y =
    match x, y with
    | FullName xs, FullName ys ->
      xs = ys
    | (PartialName xs | FullName xs), (PartialName ys | FullName ys) ->
      let testLength = min xs.Length ys.Length
      List.take testLength xs = List.take testLength ys

  module Patterns =
    let (|TypeName|) = function
      | PartialName xs -> List.head xs
      | FullName xs -> List.head xs

    let (|AnyIdentity|_|) = function
      | Identity n -> Some n
      | StrongIdentity n -> Some n
      | _ -> None

    let (|Array|_|) = function
      | Generic (AnyIdentity (TypeName name), [ t ]) when Regex.IsMatch(name, arrayRegexPattern) ->
        let dimension = name.Split(',').Length
        Some (dimension, name, t)
      | _ -> None

    let (|Tuple|_|) = function
      | Generic ((AnyIdentity id), xs) when id = TupleIdentity -> Some xs
      | _ -> None

    let (|AnyVariable|_|) = function
      | Variable (s, n) -> Some (s, n)
      | StrongVariable (s, n) -> Some (s, n)
      | _ -> None

    let (|NonVariable|_|) = function
      | Variable _ -> None
      | StrongVariable _ -> None
      | _ -> Some ()

    let (|QueryInstanceMember|_|) = function
      | InstanceMember ({ Source = Source.Query} as x) -> Some x
      | _ -> None

    let (|NoArguments|_|) (x: InstanceMember) =
      match x with
      | { Arguments = [] } -> Some ()
      | _ -> None

    let (|Unit|_|) = function
      | AnyIdentity id when testIdentity id UnitIdentity || testIdentity id UnitAbbreviationIdentity -> Some ()
      | TypeAbbreviation { Original = AnyIdentity id } when id = UnitIdentity -> Some ()
      | _ -> None

    let (|OnlyUnitArgument|_|) (x: InstanceMember) =
      match x with
      | { Arguments = [ Unit ] } -> Some ()
      | _ -> None

  open Patterns

  let rec collectVariables = function
    | Variable _ as v -> [ v ]
    | StrongVariable _ as v -> [ v ]
    | Arrow xs -> List.collect collectVariables xs
    | Identity _ -> []
    | StrongIdentity _ -> []
    | Wildcard -> []
    | WildcardGroup _ -> []
    | Generic (x, ys) -> List.collect collectVariables (x :: ys)
    | StaticMethod x -> List.collect collectVariables (x.ReturnType :: x.Arguments)
    | InstanceMember x -> List.collect collectVariables (x.Receiver :: x.ReturnType :: x.Arguments)
    | TypeAbbreviation x -> List.concat [ collectVariables x.Abbreviation; collectVariables x.Original ]

  let rec collectWildcardGroup = function
    | Variable _ -> []
    | StrongVariable _ -> []
    | Arrow xs -> List.collect collectWildcardGroup xs
    | Identity _ -> []
    | StrongIdentity _ -> []
    | Wildcard -> []
    | WildcardGroup _ as w -> [ w ]
    | Generic (x, ys) -> List.collect collectWildcardGroup (x :: ys)
    | StaticMethod x -> List.collect collectWildcardGroup (x.ReturnType :: x.Arguments)
    | InstanceMember x -> List.collect collectWildcardGroup (x.Receiver :: x.ReturnType :: x.Arguments)
    | TypeAbbreviation x -> List.concat [ collectWildcardGroup x.Abbreviation; collectWildcardGroup x.Original ]

  let collectVariableOrWildcardGroup x = List.concat [ collectVariables x; collectWildcardGroup x ]

  let rec private display' prefix = function
    | Variable (source, name) -> prefix source name
    | StrongVariable (source, name) -> "!" + prefix source name
    | Identity (TypeName name) -> name
    | StrongIdentity (TypeName name) -> "!" + name
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
    | Tuple xs ->
      xs
      |> Seq.map (function
        | Tuple _ as t -> sprintf "(%s)" (display' prefix t)
        | x -> display' prefix x)
      |> String.concat " * "
    | Generic (x, ys) -> sprintf "%s<%s>" (display' prefix x) (ys |> Seq.map (display' prefix) |> String.concat ", ")
    | StaticMethod x -> sprintf "%s -> %s" (display' prefix (tuple x.Arguments)) (display' prefix x.ReturnType)
    | InstanceMember x ->
      match x.Arguments with
      | [] -> sprintf "%s" (display' prefix x.ReturnType)
      | _ -> sprintf "%s -> %s" (display' prefix (tuple x.Arguments)) (display' prefix x.ReturnType)
    | TypeAbbreviation x -> display' prefix x.Abbreviation

  let display = display' (fun _ name -> "'" + name)

  let debugDisplay signature =
    let display = display' (fun source name -> match source with Query -> "'q" + name | Target -> "'t" + name)
    match signature with
    | InstanceMember x -> sprintf "%s => %s" (display x.Receiver) (display signature)
    | _ -> display signature

  let internal transferGenericArgument sourceArgs abbreviationArgs destArgs =
    let newArgs = Array.ofList destArgs
    List.zip sourceArgs abbreviationArgs
    |> List.choose (function
      | arg, (AnyVariable abbArgVariableName) ->
        let indexes =
          destArgs
          |> List.mapi (fun i o ->
            match o with
            | AnyVariable originalArgVariableName when abbArgVariableName = originalArgVariableName -> Some i
            | _ -> None)
          |> List.choose (fun x -> x)
        Some (arg, indexes)
      | _ -> None
    )
    |> List.iter (fun (arg, indexes) -> indexes |> List.iter (fun i -> newArgs.[i] <- arg))
    List.ofArray newArgs

  let internal tryFindGenericAbbreviation table genericIdName genericArguments =
    table
    |> List.tryFindBack (function
      | { Abbreviation = Generic (AnyIdentity abbIdName, abbArgs) } -> testIdentity abbIdName genericIdName && (List.length abbArgs) = (List.length genericArguments)
      | _ -> false)

  let rec replaceAbbreviation table = function
    | AnyIdentity idName as i ->
      let replacement = table |> List.tryFindBack (function { Abbreviation = AnyIdentity abbIdName } -> testIdentity abbIdName idName | _ -> false)
      match replacement with
      | Some replace -> TypeAbbreviation { Abbreviation = i; Original = replace.Original }
      | None -> i
    | Generic (AnyIdentity idName as id, args) as generic ->
      let replacedArgs = args |> List.map (replaceAbbreviation table)
      let abb = tryFindGenericAbbreviation table idName replacedArgs
      match abb with
      | Some ({ Abbreviation = Generic (_, abbArgs); Original = Generic (originalId, originalArgs) }) ->
        let newArgs = transferGenericArgument replacedArgs abbArgs originalArgs
        TypeAbbreviation { Abbreviation = generic; Original = Generic (originalId, newArgs) }
      | Some _ -> Generic (id, replacedArgs)
      | None -> Generic (id, replacedArgs)
    | Arrow xs -> Arrow (List.map (replaceAbbreviation table) xs)
    | StaticMethod x -> StaticMethod { Arguments = List.map (replaceAbbreviation table) x.Arguments; ReturnType = replaceAbbreviation table x.ReturnType }
    | InstanceMember x ->
      let x = { x with
                    Receiver = replaceAbbreviation table x.Receiver
                    Arguments = List.map (replaceAbbreviation table) x.Arguments
                    ReturnType = replaceAbbreviation table x.ReturnType }
      InstanceMember x
    | x -> x

  let rec replaceVariable variableName replacement x =
    let inline replace x = replaceVariable variableName replacement x
    match x with
    | AnyVariable (_, name) when name = variableName -> replacement
    | Generic(id, args) -> Generic (replace id, List.map replace args)
    | Arrow xs -> Arrow (List.map replace xs)
    | StaticMethod x -> StaticMethod { x with Arguments = List.map replace x.Arguments; ReturnType = replace x.ReturnType }
    | InstanceMember x ->
      let x = { x with
                    Receiver = replace x.Receiver
                    Arguments = List.map replace x.Arguments
                    ReturnType = replace x.ReturnType }
      InstanceMember x
    | x -> x

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

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Query =
  let replaceAbbreviation table = function
    | { Method = BySignature s } as x -> { x with Method = BySignature (Signature.replaceAbbreviation table s) }
    | { Method = ByName (n, SignatureQuery s) } as x -> { x with Method = ByName (n, SignatureQuery (Signature.replaceAbbreviation table s)) }
    | x -> x

type Api = {
  Name: string
  Signature: Signature
}

type ApiDictionary = {
  AssemblyName: string
  Api: Api list
  TypeAbbreviations: TypeAbbreviation list
}