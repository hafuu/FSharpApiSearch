module FSharpApiSearch.Printer

module FSharp =
  let printFullName (api: Api) (p: SignaturePrinter<_>) = p.Append(FSharpFormat.printName_full (ApiName.toName api.Name))
  let printApiName (api: Api) (p: SignaturePrinter<_>) = p.Append(FSharpFormat.printApiName (ApiName.toName api.Name))
  let printAccessPath (depth: int option) (api: Api) (p: SignaturePrinter<_>) = p.Append(FSharpFormat.printAccessPath depth (ApiName.toName api.Name))

  let printSignature (api:Api) (p: SignaturePrinter<_>) = p.Append(FSharpFormat.printApiSignature false api.Signature)
  let printKind (api: Api) (p: SignaturePrinter<_>) = p.Append(FSharpFormat.printApiKind api.Kind)

  let hasTypeConstraints (api: Api) = not api.TypeConstraints.IsEmpty
  let printTypeConstraints (api: Api) (p: SignaturePrinter<_>) =
    p.Append("when ")
     .AppendJoin(" and ", api.TypeConstraints, FSharpFormat.printConstraint false)
      

module CSharp =
  let printFullName (api: Api) (p: SignaturePrinter<_>) = p.Append(CSharpFormat.printName_full (ApiName.toName api.Name))
  let printApiName (api: Api) (p: SignaturePrinter<_>) = p.Append(CSharpFormat.printApiName (ApiName.toName api.Name))
  let printAccessPath (depth: int option) (api: Api) (p: SignaturePrinter<_>) = p.Append(CSharpFormat.printAccessPath depth (ApiName.toName api.Name))

  let printSignature (api:Api) (p: SignaturePrinter<_>) = p.Append(CSharpFormat.printApiSignature api.Signature)
  let printKind (api: Api) (p: SignaturePrinter<_>) = p.Append(CSharpFormat.printApiKind api.Kind)
  
  let hasTypeConstraints (api: Api) = api.TypeConstraints |> List.exists CSharpFormat.csharpTypeConstraintPred
  let printTypeConstraints (api: Api) (p: SignaturePrinter<_>) =
    p.Append(CSharpFormat.printConstraints api.TypeConstraints)