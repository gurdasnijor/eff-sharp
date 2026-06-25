module RefSpec

open Effect
open Effect.Vitest

let private current = "value"
let private update = "new value"

type private State =
    | Active
    | Changed
    | Closed

let private isActive =
    function
    | Active -> true
    | _ -> false

let private isChanged =
    function
    | Changed -> true
    | _ -> false

let private isClosed =
    function
    | Closed -> true
    | _ -> false

let private run (eff: Effect<'A, 'E, unit>) : 'A =
    match Effect.runSync () eff with
    | Success a -> a
    | Failure cause -> failwithf "unexpected failure: %s" (Cause.render cause)

describe "Ref" (fun () ->
    test "get returns the current value" (fun () ->
        toBe (run (Ref.get (Ref.makeUnsafe current))) current)

    test "getAndSet and getAndUpdate return previous values" (fun () ->
        let ref = run (Ref.make current)
        toBe (run (Ref.getAndSet ref update)) current
        toBe (run (Ref.get ref)) update
        toBe (run (Ref.getAndUpdate ref (fun _ -> current))) update
        toBe (run (Ref.get ref)) current)

    test "getAndUpdateSome handles matching and missing transitions" (fun () ->
        let ref = run (Ref.make Active)
        toEqual (run (Ref.getAndUpdateSome ref (fun s -> if isClosed s then Some Changed else None))) Active
        toEqual (run (Ref.get ref)) Active
        toEqual (run (Ref.getAndUpdateSome ref (fun s -> if isActive s then Some Changed else None))) Active
        toEqual (run (Ref.getAndUpdateSome ref (fun s -> if isChanged s then Some Closed else None))) Changed
        toEqual (run (Ref.get ref)) Closed)

    test "set and update variants store values" (fun () ->
        let ref = run (Ref.make current)
        run (Ref.set ref update)
        toBe (run (Ref.get ref)) update
        run (Ref.update ref (fun _ -> current))
        toBe (run (Ref.get ref)) current
        toBe (run (Ref.updateAndGet ref (fun _ -> update))) update)

    test "updateSome variants" (fun () ->
        let ref = run (Ref.make Active)
        run (Ref.updateSome ref (fun s -> if isClosed s then Some Changed else None))
        toEqual (run (Ref.get ref)) Active
        run (Ref.updateSome ref (fun s -> if isActive s then Some Changed else None))
        toEqual (run (Ref.get ref)) Changed
        toEqual (run (Ref.updateSomeAndGet ref (fun s -> if isChanged s then Some Closed else None))) Closed)

    test "modify returns a result while storing the new value" (fun () ->
        let ref = run (Ref.make current)
        toBe (run (Ref.modify ref (fun _ -> ("hello", update)))) "hello"
        toBe (run (Ref.get ref)) update)

    test "modifySome applies matching transitions" (fun () ->
        let ref = run (Ref.make Active)

        let unchanged =
            run (
                Ref.modifySome ref (fun s ->
                    if isClosed s then ("active", Some Active) else ("state does not change", None))
            )

        toBe unchanged "state does not change"

        let changed =
            run (
                Ref.modifySome ref (fun s ->
                    if isActive s then ("changed", Some Changed) else ("state does not change", None))
            )

        toBe changed "changed"
        toEqual (run (Ref.get ref)) Changed))

