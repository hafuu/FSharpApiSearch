namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("FSharpApiSearch")>]
[<assembly: AssemblyProductAttribute("FSharpApiSearch")>]
[<assembly: AssemblyDescriptionAttribute("F# API search engine")>]
[<assembly: AssemblyVersionAttribute("0.1.0")>]
[<assembly: AssemblyFileVersionAttribute("0.1.0")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "0.1.0"
