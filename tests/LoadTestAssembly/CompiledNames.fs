module CompiledNames

let withoutCompiledName () = ()

[<CompiledName("FUNC_NAME")>]
let funcName () = ()

[<CompiledName("TYPE_NAME")>]
type TypeName() = class end

type T() =
  [<CompiledName("METHOD_NAME")>]
  static member MethodName() = ()

  static member WithoutCompiledNameProperty = 3

  [<CompiledName("PROPERTY_NAME")>]
  static member PropertyName = 3

type Record = {
  [<CompiledName("it_is_ignored")>]
  FieldName: int
}

type Union = | [<CompiledName("it_is_ignored")>] CaseName of int

[<CompiledName("it_is_ignored")>]
module ModuleName = ()

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module WithModuleSuffix =
  let f () = ()