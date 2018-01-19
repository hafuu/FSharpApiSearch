[<Persimmon.Category("printer", "html_printer")>]
module HtmlPrintHelperTest

open Persimmon
open Persimmon.Syntax.UseTestNameByReflection
//open Persimmon.MuscleAssert
open FSharpApiSearch
open FSharpApiSearch.HtmlPrintHelper

let write str (x: StructuredWriter) = x.Write(str); x
let begin' color (x: StructuredWriter) = x.BeginColor(color); x
let end' color (x: StructuredWriter) = x.EndColor(color); x

let writerTest = parameterize {
  source [
    write "ab", [| ("ab", None) |]
    write "ab" >> write "cd", [| ("abcd", None) |]
    write "ab" >> begin' 1 >> write "cd" >> end' 1, [| ("ab", None); ("cd", Some 1) |]
    write "ab" >> begin' 1 >> write "cd" >> end' 1 >> write "ef", [| ("ab", None); ("cd", Some 1); ("ef", None) |]
    write "ab" >> begin' 1 >> write "cd" >> begin' 2 >> write "ef" >> end' 2 >> end' 1, [| ("ab", None); ("cd", Some 1); ("ef", Some 2) |]
    write "ab" >> begin' 1 >> write "cd" >> begin' 2 >> write "ef" >> end' 2 >> write "gh" >> end' 1, [| ("ab", None); ("cd", Some 1); ("ef", Some 2); ("gh", Some 1) |]
    begin' 1 >> write "ab" >> end' 1 >> write "cd", [| ("ab", Some 1); ("cd", None) |]
    begin' 1 >> write "a" >> end' 1 >> write " -> " >> begin' 2 >> write "b" >> end' 2 >> write " -> " >> begin' 3 >> write "c" >> end' 3, [| ("a", Some 1); (" -> ", None); ("b", Some 2); (" -> ", None); ("c", Some 3) |]
  ]
  run (fun (steps : StructuredWriter -> StructuredWriter, expected) -> test {
    let writer = StructuredWriter()
    let actual = (steps writer).Result.Text
    do! actual |> assertEquals expected
  })
}

let printQueryTest = parameterize {
  source [
    "a -> b -> c", [| ("a", Some 0); (" -> ", None); ("b", Some 1); (" -> ", None); ("c", Some 2) |]
  ]
  run (fun (input, expected) -> test {
    let query = QueryParser.FSharp.parse input |> QueryInitializer.queryPosition
    let actual = HtmlPrintHelper.query (QueryPrinter.print query)
    do! actual.Text |> assertEquals expected
  })
}