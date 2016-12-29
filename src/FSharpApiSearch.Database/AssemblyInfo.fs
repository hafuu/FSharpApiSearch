namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("FSharpApiSearch.Database")>]
[<assembly: AssemblyProductAttribute("FSharpApiSearch")>]
[<assembly: AssemblyDescriptionAttribute("F# API search engine")>]
[<assembly: AssemblyVersionAttribute("2.1.1")>]
[<assembly: AssemblyFileVersionAttribute("2.1.1")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "2.1.1"
    let [<Literal>] InformationalVersion = "2.1.1"
