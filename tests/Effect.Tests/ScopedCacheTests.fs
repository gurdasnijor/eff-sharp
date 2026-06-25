module Effect.Tests.ScopedCacheTests

open Xunit
open Effect

// Ported from repos/effect-smol/packages/effect/test/ScopedCache.test.ts.
//
// Same controllable-clock driving as CacheTests. The scoped behaviour is that a
// lookup registers finalizers on its *entry scope*, which is closed (releasing
// the resource) when the entry is invalidated, refreshed, evicted, or the cache
// is fully invalidated. The lookup receives that `Scope` explicitly (the F# port
// has no `Scope.Scope` env service).

let private mkClock (time: int64 ref) : Clock =
    { CurrentTimeMillisUnsafe = fun () -> time.Value
      SleepUnsafe = fun _ -> System.Threading.Tasks.Task.CompletedTask }

let private run (time: int64 ref) (eff: Effect<'A, 'E, Context>) : Exit<'A, 'E> =
    Effect.runSync (Context.make Clock.tag (mkClock time)) eff

let private get (time: int64 ref) (eff: Effect<'A, 'E, Context>) : 'A =
    match run time eff with
    | Success a -> a
    | Failure c -> failwithf "unexpected failure: %s" (Cause.render c)

/// A lookup that acquires a "resource" (increments `acquired`) and registers its
/// release (increments `released`) on the entry scope.
let private resourceLookup (acquired: int ref) (released: int ref) =
    fun (k: string) (scope: Scope<string, Context>) ->
        Scope.addFinalizer scope (Effect.sync (fun () -> released.Value <- released.Value + 1))
        |> Effect.map (fun () ->
            acquired.Value <- acquired.Value + 1
            k.Length)

// ----------------------------------------------------------------------------
// hit / miss
// ----------------------------------------------------------------------------

[<Fact>]
let ``get - miss invokes lookup, hit serves from cache (lookup runs once)`` () =
    let time = ref 0L
    let count = ref 0

    let cache: ScopedCache<string, int, string> =
        get
            time
            (ScopedCache.make 10 (fun (k: string) (_: Scope<string, Context>) ->
                Effect.sync (fun () ->
                    count.Value <- count.Value + 1
                    k.Length)))

    Assert.Equal<Exit<int, string>>(Exit.succeed 5, run time (ScopedCache.get cache "hello"))
    Assert.Equal<Exit<int, string>>(Exit.succeed 5, run time (ScopedCache.get cache "hello"))
    Assert.Equal(1, count.Value)

[<Fact>]
let ``getOption / has / size`` () =
    let time = ref 0L

    let cache: ScopedCache<string, int, string> =
        get time (ScopedCache.make 10 (fun (k: string) (_: Scope<string, Context>) -> Effect.succeed k.Length))

    Assert.Equal<Exit<int option, string>>(Exit.succeed None, run time (ScopedCache.getOption cache "hello"))
    Assert.False(get time (ScopedCache.has cache "hello"))
    run time (ScopedCache.get cache "hello") |> ignore
    Assert.Equal<Exit<int option, string>>(Exit.succeed (Some 5), run time (ScopedCache.getOption cache "hello"))
    Assert.True(get time (ScopedCache.has cache "hello"))
    Assert.Equal(1, get time (ScopedCache.size cache))

// ----------------------------------------------------------------------------
// scoped resource release
// ----------------------------------------------------------------------------

[<Fact>]
let ``invalidate - releases the entry's scoped resource`` () =
    let time = ref 0L
    let acquired = ref 0
    let released = ref 0

    let cache: ScopedCache<string, int, string> =
        get time (ScopedCache.make 10 (resourceLookup acquired released))

    run time (ScopedCache.get cache "x") |> ignore
    Assert.Equal(1, acquired.Value)
    Assert.Equal(0, released.Value) // still alive while cached

    run time (ScopedCache.invalidate cache "x") |> ignore
    Assert.Equal(1, released.Value) // released on invalidate
    Assert.False(get time (ScopedCache.has cache "x"))

[<Fact>]
let ``invalidateAll - releases every entry's scope`` () =
    let time = ref 0L
    let acquired = ref 0
    let released = ref 0

    let cache: ScopedCache<string, int, string> =
        get time (ScopedCache.make 10 (resourceLookup acquired released))

    run time (ScopedCache.get cache "a") |> ignore
    run time (ScopedCache.get cache "bb") |> ignore
    Assert.Equal(2, acquired.Value)
    Assert.Equal(0, released.Value)

    run time (ScopedCache.invalidateAll cache) |> ignore
    Assert.Equal(2, released.Value)
    Assert.Equal(0, get time (ScopedCache.size cache))

[<Fact>]
let ``refresh - releases the old scope and re-acquires`` () =
    let time = ref 0L
    let acquired = ref 0
    let released = ref 0

    let cache: ScopedCache<string, int, string> =
        get time (ScopedCache.make 10 (resourceLookup acquired released))

    run time (ScopedCache.get cache "x") |> ignore
    Assert.Equal(1, acquired.Value)
    Assert.Equal(0, released.Value)

    run time (ScopedCache.refresh cache "x") |> ignore
    Assert.Equal(2, acquired.Value) // re-acquired
    Assert.Equal(1, released.Value) // old released

[<Fact>]
let ``capacity eviction - releases the evicted entry's scope`` () =
    let time = ref 0L
    let acquired = ref 0
    let released = ref 0

    let cache: ScopedCache<string, int, string> =
        get time (ScopedCache.make 1 (resourceLookup acquired released))

    run time (ScopedCache.get cache "a") |> ignore
    run time (ScopedCache.get cache "b") |> ignore // evicts "a"
    Assert.Equal(2, acquired.Value)
    Assert.Equal(1, released.Value) // "a" released on eviction
    Assert.Equal(1, get time (ScopedCache.size cache))
