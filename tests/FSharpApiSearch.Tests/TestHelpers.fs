module TestHelpers
open FSharpApiSearch
open FSharpApiSearch.Types.Signature.Patterns

let rec updateSource newSource = function
  | Variable (_, name) -> Variable (newSource, name)
  | StrongVariable (_, name) -> StrongVariable(newSource, name)
  | Identity _ as x -> x
  | StrongIdentity _ as x -> x
  | Wildcard as x -> x
  | WildcardGroup _ as x -> x
  | Arrow xs -> Arrow (List.map (updateSource newSource) xs)
  | Generic (id, xs) -> Generic (updateSource newSource id, List.map (updateSource newSource) xs)
  | StaticMethod x ->
    StaticMethod {
      Arguments = List.map (updateSource newSource) x.Arguments
      ReturnType = updateSource newSource x.ReturnType
    }
  | InstanceMember x ->
    InstanceMember {
      Source = newSource
      Receiver = updateSource newSource x.Receiver
      Arguments = List.map (updateSource newSource) x.Arguments
      ReturnType = updateSource newSource x.ReturnType
    }
  | TypeAbbreviation x -> TypeAbbreviation { Abbreviation = updateSource newSource x.Abbreviation; Original = updateSource newSource x.Original }

let toStaticMethod = function
  | Arrow xs ->
    match xs with
    | [ Tuple arguments; returnType ] -> StaticMethod { Arguments = arguments; ReturnType = returnType }
    | [ argument; returnType ] -> StaticMethod { Arguments = [ argument ]; ReturnType = returnType }
    | _ -> failwith "failed to convert static method"
  | _ -> failwith "failed to convert static method"

module DSL =
  let private source = Source.Query
  let identity name = Identity (Signature.partialName name)
  let fullIdentity name = Identity (Signature.fullName name)
  let strongIdentity name = StrongIdentity (Signature.partialName name)
  let variable name = Variable (source, name)
  let strongVariable name = StrongVariable (source, name)
  let wildcard = Wildcard
  let wildcardGroup name = WildcardGroup name
  let arrow xs = Arrow xs
  let generic id xs = Generic (id, xs)
  let tuple xs = Signature.tuple xs
  let staticMethod args ret = StaticMethod { Arguments = args; ReturnType = ret }
  let instanceMember receiver args ret = InstanceMember { Source = source; Receiver = receiver; Arguments = args; ReturnType = ret }
  let array x = generic (identity "[]") [ x ]
  let array2d x = generic (identity "[,]") [ x ]
  let array3d x = generic (identity "[,,]") [ x ]
  let abbreviation original abbreviation = TypeAbbreviation { Abbreviation = abbreviation; Original = original }

open DSL

let fsharpAbbreviationTable = [
  { Abbreviation = fullIdentity "Microsoft.FSharp.Core.int"; Original = fullIdentity "System.Int32" }
  { Abbreviation = fullIdentity "Microsoft.FSharp.Core.float"; Original = fullIdentity "System.Double" }
  { Abbreviation = fullIdentity "Microsoft.FSharp.Core.single"; Original = fullIdentity "System.Single" }
  { Abbreviation = fullIdentity "Microsoft.FSharp.Core.string"; Original = fullIdentity "System.String" }
  { Abbreviation = fullIdentity "Microsoft.FSharp.Core.unit"; Original = fullIdentity "Microsoft.FSharp.Core.Unit" }
  { Abbreviation = generic (fullIdentity "Microsoft.FSharp.Collections.list") [ variable "a" ]; Original = generic (fullIdentity "Microsoft.FSharp.Collections.List") [ variable "a" ] }
  { Abbreviation = generic (fullIdentity "Microsoft.FSharp.Core.option") [ variable "a" ]; Original = generic (fullIdentity "Microsoft.FSharp.Core.Option") [ variable "a" ] }
]

let replaceAbbreviation = Signature.replaceAbbreviation fsharpAbbreviationTable

let rec toFullName = function
  | Identity (PartialName xs) when xs.Length > 1 -> Identity (FullName xs)
  | StrongIdentity (PartialName xs) when xs.Length > 1 -> Identity (FullName xs)
  | Array (_, name, x) -> Generic (Identity (Signature.fullName name), [ toFullName x ])
  | Generic (id, xs) -> Generic (toFullName id, List.map toFullName xs)
  | Arrow xs -> Arrow (List.map toFullName xs)
  | InstanceMember x -> InstanceMember { x with Receiver = toFullName x.Receiver; Arguments = List.map toFullName x.Arguments; ReturnType = toFullName x.ReturnType }
  | StaticMethod x -> StaticMethod { x with Arguments = List.map toFullName x.Arguments; ReturnType = toFullName x.ReturnType }
  | TypeAbbreviation x -> TypeAbbreviation { x with Abbreviation = toFullName x.Abbreviation; Original = toFullName x.Original }
  | x -> x

let replaceFSharpTypes (query: string) =
  let replace (oldValue: string) (newValue: string) (str: string) = str.Replace(oldValue, newValue)
  query
  |> replace "int" "Microsoft.FSharp.Core.int"
  |> replace "unit" "Microsoft.FSharp.Core.unit"
  |> replace "float" "Microsoft.FSharp.Core.float"
  |> replace "single" "Microsoft.FSharp.Core.single"
  |> replace "string" "Microsoft.FSharp.Core.string"
  |> replace "option" "Microsoft.FSharp.Core.option"
  |> replace "Map" "Microsoft.FSharp.Collections.Map"
  |> replace "list" "Microsoft.FSharp.Collections.list"