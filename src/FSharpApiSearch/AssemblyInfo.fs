namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("FSharpApiSearch")>]
[<assembly: AssemblyProductAttribute("FSharpApiSearch")>]
[<assembly: AssemblyDescriptionAttribute("F# API search engine")>]
[<assembly: AssemblyVersionAttribute("2.1.0")>]
[<assembly: AssemblyFileVersionAttribute("2.1.0")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "2.1.0"
    let [<Literal>] InformationalVersion = "2.1.0"
