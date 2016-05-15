namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("FSharpApiSearch.Console")>]
[<assembly: AssemblyProductAttribute("FSharpApiSearch")>]
[<assembly: AssemblyDescriptionAttribute("F# API search engine")>]
[<assembly: AssemblyVersionAttribute("0.2.1")>]
[<assembly: AssemblyFileVersionAttribute("0.2.1")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "0.2.1"
    let [<Literal>] InformationalVersion = "0.2.1"
