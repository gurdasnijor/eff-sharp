/// @title Integrating with non-Effect code (a managed Runtime)
///
/// `Runtime` bridges effectful domain logic with imperative hosts — web handlers,
/// framework hooks, worker loops, legacy callbacks. You build ONE runtime from
/// your application `Context` (assembled by `Layer`s), then call `Runtime.runSync`
/// / `runSyncExit` from any synchronous edge.
///
/// Mirrors upstream `03_integration/10_managed-runtime` (the Hono `TodoRepo`
/// example), minus the HTTP framework (not ported). It also shows `Schema`
/// validating an inbound payload — the eff-sharp equivalent of
/// `Schema.decodeUnknownSync`.
module EffSharp.AiDocs.ManagedRuntime

open Effect
open EffSharp.AiDocs.Shared

// ---------------------------------------------------------------------------
// Domain model (records/DUs) + a tagged-style error.
// ---------------------------------------------------------------------------

type Todo = { Id: int; Title: string; Completed: bool }
type CreateTodo = { Title: string }
type TodoError = TodoNotFound of id: int

// ---------------------------------------------------------------------------
// The service: a record of effects, all requiring `Context`.
// ---------------------------------------------------------------------------

type TodoRepo =
    { GetAll: Effect<Todo list, TodoError, Context>
      GetById: int -> Effect<Todo, TodoError, Context>
      Create: CreateTodo -> Effect<Todo, TodoError, Context> }

let repoTag: Tag<TodoRepo> = Tag.make<TodoRepo> "app/TodoRepo"

/// A Layer that builds the repo over an in-memory store. `Layer.effect` lets the
/// construction itself be effectful (here it allocates a `Ref` for the id seed).
let repoLayer: Layer<TodoError, unit> =
    Layer.effect
        repoTag
        (effect {
            let! nextId = Ref.make 1
            let store = System.Collections.Generic.Dictionary<int, Todo>()

            let getAll: Effect<Todo list, TodoError, Context> =
                Effect.sync (fun () -> store.Values |> List.ofSeq)

            let getById (id: int) : Effect<Todo, TodoError, Context> =
                effect {
                    // F# auto-tuples the TryGetValue out-param into (found, value).
                    match store.TryGetValue id with
                    | true, todo -> return todo
                    | _ -> return! Effect.fail (TodoNotFound id)
                }

            let create (payload: CreateTodo) : Effect<Todo, TodoError, Context> =
                effect {
                    let! id = Ref.getAndUpdate nextId (fun c -> c + 1)
                    let todo = { Id = id; Title = payload.Title; Completed = false }
                    store.[id] <- todo
                    return todo
                }

            return { GetAll = getAll; GetById = getById; Create = create }
        })

// ---------------------------------------------------------------------------
// Build the runtime ONCE by materialising the layer's Context. The layer has no
// finalizers here, but we still build it inside a scope (the general pattern).
// ---------------------------------------------------------------------------

let makeRuntime () : Runtime =
    let build =
        Scope.scoped (fun (scope: Scope<TodoError, unit>) -> Layer.buildWithScope scope repoLayer)

    match Effect.runSync () build with
    | Success ctx -> Runtime.make ctx
    | Failure cause -> failwithf "failed to build runtime: %s" (Cause.render cause)

// ---------------------------------------------------------------------------
// Validate inbound data with Schema — the boundary between untyped JSON and the
// domain model. `Json` is the leaf6 value type reused by Schema.
// ---------------------------------------------------------------------------

let createSchema: Schema<CreateTodo> =
    Schema.object {
        let! title = Schema.field "title" Schema.string (fun (c: CreateTodo) -> c.Title)
        return { Title = title }
    }

// ---------------------------------------------------------------------------
// "Handlers" — plain synchronous functions, exactly what a web framework calls.
// Each one runs an effect through the shared runtime.
// ---------------------------------------------------------------------------

/// POST /todos — decode the body, then create. Returns Result for the host to
/// map to 201 / 400.
let handleCreate (rt: Runtime) (body: Json) : Result<Todo, string> =
    match Schema.decode createSchema body with
    | Error issue -> Error(Schema.format issue) // -> HTTP 400
    | Ok payload ->
        Ok(Runtime.runSync rt (Effect.service repoTag |> Effect.flatMap (fun repo -> repo.Create payload)))

/// GET /todos/:id — run for the full Exit so we can map a typed miss to 404.
let handleGet (rt: Runtime) (id: int) : Result<Todo, string> =
    let exit = Runtime.runSyncExit rt (Effect.service repoTag |> Effect.flatMap (fun repo -> repo.GetById id))

    match exit with
    | Success todo -> Ok todo
    | Failure { Reasons = [ Reason.Fail(TodoNotFound id) ] } -> Error(sprintf "todo %d not found" id) // -> 404
    | Failure cause -> Error(Cause.render cause)

let demo () =
    banner "03 · Integration — managed Runtime + Schema"
    let rt = makeRuntime ()

    // Imperative edge #1: a valid POST body (as JSON).
    match handleCreate rt (JObject(Map [ "title", JString "write eff-sharp docs" ])) with
    | Ok todo -> printfn "  created => #%d %s" todo.Id todo.Title
    | Error msg -> printfn "  400 => %s" msg

    // Imperative edge #2: an invalid POST body — Schema rejects it.
    match handleCreate rt (JObject(Map [ "name", JString "oops" ])) with
    | Ok todo -> printfn "  created => #%d" todo.Id
    | Error msg -> printfn "  400 => %s" msg

    // Imperative edge #3: a miss maps to a typed error -> 404.
    match handleGet rt 99 with
    | Ok todo -> printfn "  got => %s" todo.Title
    | Error msg -> printfn "  404 => %s" msg
