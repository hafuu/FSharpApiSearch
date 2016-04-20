module FSharpApiSearch.CommandLine

let (|Status|_|) (name: string) (str: string) =
  if str.StartsWith(name) then
    match str.Substring(name.Length) with
    | "+" -> Some true
    | "-" -> Some false
    | _ -> None
  else
    None

let (|KeyValue|_|) key (str: string) =
  match str.Split([| ':' |], 2) |> Array.toList with
  | [ k; v ] when key = k -> Some v
  | _ -> None