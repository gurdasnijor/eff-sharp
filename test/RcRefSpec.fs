module RcRefSpec

open Effect
open Effect.Vitest

let private voidExit: Exit<obj, obj> = Success(box ())

let private run (eff: Effect<'A, 'E, unit>) : 'A =
    match Effect.runSync () eff with
    | Success a -> a
    | Failure cause -> failwithf "unexpected failure: %s" (Cause.render cause)

describe "RcRef" (fun () ->
    test "deallocation" (fun () ->
        let acquired = ref 0
        let released = ref 0

        let acquire (scope: Scope<string, unit>) : Effect<string, string, unit> =
            Scope.acquireRelease
                scope
                (Effect.sync (fun () ->
                    acquired.Value <- acquired.Value + 1
                    "foo"))
                (fun _ _ -> Effect.sync (fun () -> released.Value <- released.Value + 1))

        let refScope = Scope.make<string, unit> ()
        let rcRef = run (RcRef.make refScope acquire)
        toBe acquired.Value 0

        let value = run (Scope.scoped (fun s -> RcRef.get rcRef s))
        toBe value "foo"
        toBe acquired.Value 1
        toBe released.Value 1

        let scopeA = Scope.make<string, unit> ()
        let scopeB = Scope.make<string, unit> ()
        run (RcRef.get rcRef scopeA) |> ignore
        run (RcRef.get rcRef scopeB) |> ignore
        toBe acquired.Value 2
        toBe released.Value 1

        run (Scope.close scopeB voidExit)
        toBe released.Value 1
        run (Scope.close scopeA voidExit)
        toBe released.Value 2

        let scopeC = Scope.make<string, unit> ()
        run (RcRef.get rcRef scopeC) |> ignore
        toBe acquired.Value 3
        toBe released.Value 2

        run (Scope.close refScope voidExit)
        toBe released.Value 3

        match Effect.runSync () (Scope.scoped (fun s -> RcRef.get rcRef s)) with
        | Failure cause ->
            let hasInterrupt =
                cause.Reasons
                |> List.exists (function
                    | Reason.Interrupt _ -> true
                    | _ -> false)

            toBe hasInterrupt true
        | Success _ -> failwith "expected an interrupted failure"))
