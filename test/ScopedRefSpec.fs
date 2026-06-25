module ScopedRefSpec

open Effect
open Effect.Vitest

let private run (eff: Effect<'A, 'E, unit>) : 'A =
    match Effect.runSync () eff with
    | Success a -> a
    | Failure cause -> failwithf "unexpected failure: %s" (Cause.render cause)

type private Counter =
    { Acquired: Ref<int>
      Released: Ref<int> }

let private makeCounter: Effect<Counter, string, unit> =
    Effect.sync (fun () ->
        { Acquired = Ref.makeUnsafe 0
          Released = Ref.makeUnsafe 0 })

let private acquire (counter: Counter) (scope: Scope<string, unit>) : Effect<int, string, unit> =
    Ref.update counter.Acquired (fun n -> n + 1)
    |> Effect.zipRight (Scope.addFinalizer scope (Ref.update counter.Released (fun n -> n + 1)))
    |> Effect.zipRight (Ref.get counter.Acquired)

describe "ScopedRef" (fun () ->
    test "single and dual set update current value" (fun () ->
        let single =
            run (
                Scope.scoped (fun parent ->
                    effect {
                        let! counter = makeCounter
                        let! ref = ScopedRef.make parent (fun () -> 0)
                        do! ScopedRef.set ref (acquire counter)
                        return! ScopedRef.get ref
                    })
            )

        toBe single 1

        let dual =
            run (
                Scope.scoped (fun parent ->
                    effect {
                        let! counter = makeCounter
                        let! ref = ScopedRef.make parent (fun () -> 0)
                        do! ScopedRef.set ref (acquire counter)
                        do! ScopedRef.set ref (acquire counter)
                        return! ScopedRef.get ref
                    })
            )

        toBe dual 2)

    test "releases previous resources when replaced" (fun () ->
        let acquired, released =
            run (
                Scope.scoped (fun parent ->
                    effect {
                        let! counter = makeCounter
                        let! ref = ScopedRef.make parent (fun () -> 0)
                        do! ScopedRef.set ref (acquire counter)
                        do! ScopedRef.set ref (acquire counter)
                        do! ScopedRef.set ref (acquire counter)
                        let! a = Ref.get counter.Acquired
                        let! r = Ref.get counter.Released
                        return a, r
                    })
            )

        toBe acquired 3
        toBe released 2)

    test "releases current resource when scoped ref scope closes" (fun () ->
        let acquired, released =
            run (
                effect {
                    let! counter = makeCounter

                    do!
                        Scope.scoped (fun parent ->
                            effect {
                                let! ref = ScopedRef.make parent (fun () -> 0)
                                do! ScopedRef.set ref (acquire counter)
                                do! ScopedRef.set ref (acquire counter)
                                do! ScopedRef.set ref (acquire counter)
                            })

                    let! a = Ref.get counter.Acquired
                    let! r = Ref.get counter.Released
                    return a, r
                }
            )

        toBe acquired 3
        toBe released 3)

    test "getUnsafe reads the current value synchronously" (fun () ->
        let value =
            run (
                Scope.scoped (fun parent ->
                    effect {
                        let! ref = ScopedRef.make parent (fun () -> 42)
                        return ScopedRef.getUnsafe ref
                    })
            )

        toBe value 42))
