module InterfaceInheritance

type GrandParentInterface = interface
  abstract member GrandParentMethod: unit -> int
end

type ParentInterface =
  inherit GrandParentInterface

  abstract member ParentMethod: unit -> string

type ChildInterface =
  inherit ParentInterface

  abstract member ChildMethod: unit -> float

type GenericGrandParentInterface<'t> = interface
  abstract member GrandParentMethod: 't -> 'u
end

type GenericParentInterface<'a> =
  inherit GenericGrandParentInterface<'a>
  abstract member ParentMethod: 'a -> 'b

type GenericChildInterface<'a> = inherit GenericParentInterface<'a>
type ConflictParameterInterface<'b> = inherit GenericParentInterface<'b>
type IntChildInterface = inherit GenericParentInterface<int>