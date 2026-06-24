module Effect.Tests.ScopedRefTests

open Xunit
open Effect

// Ported from repos/effect-smol/packages/effect/test/ScopedRef.test.ts, adapted
// to the port's explicit-`Scope` model (see ScopedRef.fs). The upstream
// `Counter` test util is reproduced with two `Ref`s; `acquire scope` increments
// `acquired` and registers a `released` finalizer on the given scope.

let private run (eff: Effect<'A, 'E, unit>) : 'A =
    match Effect.runSync () eff with
    | Success a -> a
    | Failure c -> failwithf "unexpected failure: %s" (Cause.render c)

type private Counter = { Acquired: Ref<int>; Released: Ref<int> }

let private makeCounter: Effect<Counter, string, unit> =
    Effect.sync (fun () -> { Acquired = Ref.makeUnsafe 0; Released = Ref.makeUnsafe 0 })

let private acquire (c: Counter) (scope: Scope<string, unit>) : Effect<int, string, unit> =
    Ref.update c.Acquired (fun n -> n + 1)
    |> Effect.zipRight (Scope.addFinalizer scope (Ref.update c.Released (fun n -> n + 1)))
    |> Effect.zipRight (Ref.get c.Acquired)

[<Fact>]
let ``single set`` () =
    let result =
        run (
            Scope.scoped (fun parent ->
                effect {
                    let! counter = makeCounter
                    let! ref = ScopedRef.make parent (fun () -> 0)
                    do! ScopedRef.set ref (acquire counter)
                    return! ScopedRef.get ref
                })
        )

    Assert.Equal(1, result)

[<Fact>]
let ``dual set`` () =
    let result =
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

    Assert.Equal(2, result)

[<Fact>]
let ``releases the previous resource when replaced`` () =
    let acquired, released =
        run (
            Scope.scoped (fun parent ->
                effect {
                    let! counter = makeCounter
                    let! ref = ScopedRef.make parent (fun () -> 0)
                    do! ScopedRef.set ref (acquire counter)
                    do! ScopedRef.set ref (acquire counter)
                    let! a = Ref.get counter.Acquired
                    let! r = Ref.get counter.Released
                    return a, r
                })
        )

    Assert.Equal(2, acquired)
    Assert.Equal(1, released)

[<Fact>]
let ``releases each previous resource across multiple replacements`` () =
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

    Assert.Equal(3, acquired)
    Assert.Equal(2, released)

[<Fact>]
let ``releases the current resource when the scoped ref scope closes`` () =
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

    Assert.Equal(3, acquired)
    Assert.Equal(3, released)

[<Fact>]
let ``getUnsafe reads the current value synchronously`` () =
    let value =
        run (
            Scope.scoped (fun parent ->
                effect {
                    let! ref = ScopedRef.make parent (fun () -> 42)
                    return ScopedRef.getUnsafe ref
                })
        )

    Assert.Equal(42, value)
