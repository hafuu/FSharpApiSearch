[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module FSharpApiSearch.Database

open System.IO
open MessagePack.Resolvers
open MessagePack
open MessagePack.FSharp

let databaseName = "database"

module internal Serialization =
  type T = (string * Api[])[]
  let toDumpObj (xs: Database) : T = xs |> Array.map (fun x -> x.AssemblyName, x.Api)

  let fromDumpObj (xs: T) : Database =
    xs
    |> Array.map (fun (name, apis) ->
      { AssemblyName = name; Api = apis; TypeDefinitions = IDictionary.empty; TypeAbbreviations = Array.empty }
      |> ApiLoader.Impl.makeDefAndAbb
    )

let internal initMessagePack = lazy(
  CompositeResolver.RegisterAndSetAsDefault(FSharpResolver.Instance, StandardResolver.Instance)
)

let saveStream (stream: Stream) (database: Database) : unit =
  initMessagePack.Force()

  MessagePackSerializer.Serialize(stream, Serialization.toDumpObj database)

let save (path: string) (database: Database) : unit =
  if File.Exists(path) then File.Delete(path)
  use file = File.OpenWrite(path)
  saveStream file database

let loadFromStream (stream: Stream) : Database =
  initMessagePack.Force()
  MessagePackSerializer.Deserialize<Serialization.T>(stream) |> Serialization.fromDumpObj

let loadFromFile (path: string) : Database =
  use file = File.OpenRead(path)
  loadFromStream file