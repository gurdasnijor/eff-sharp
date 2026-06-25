module ScopedCacheSpec

open Effect
open Effect.Vitest

let private mkClock (time: int64 ref) : Clock =
    { CurrentTimeMillisUnsafe = fun () -> time.Value
      SleepUnsafe = fun _ -> async { return () } }

let private run (time: int64 ref) (eff: Effect<'A, 'E, Context>) : Exit<'A, 'E> =
    Effect.runSync (Context.make Clock.tag (mkClock time)) eff

let private get (time: int64 ref) (eff: Effect<'A, 'E, Context>) : 'A =
    match run time eff with
    | Success a -> a
    | Failure cause -> failwithf "unexpected failure: %s" (Cause.render cause)

let private resourceLookup (acquired: int ref) (released: int ref) =
    fun (k: string) (scope: Scope<string, Context>) ->
        Scope.addFinalizer scope (Effect.sync (fun () -> released.Value <- released.Value + 1))
        |> Effect.map (fun () ->
            acquired.Value <- acquired.Value + 1
            k.Length)

describe "ScopedCache" (fun () ->
    test "get miss invokes lookup and hit serves from cache" (fun () ->
        let time = ref 0L
        let count = ref 0

        let cache: ScopedCache<string, int, string> =
            get
                time
                (ScopedCache.make 10 (fun (k: string) (_: Scope<string, Context>) ->
                    Effect.sync (fun () ->
                        count.Value <- count.Value + 1
                        k.Length)))

        toBe (run time (ScopedCache.get cache "hello") = Exit.succeed 5) true
        toBe (run time (ScopedCache.get cache "hello") = Exit.succeed 5) true
        toBe count.Value 1)

    test "getOption has and size inspect cached entries" (fun () ->
        let time = ref 0L

        let cache: ScopedCache<string, int, string> =
            get time (ScopedCache.make 10 (fun (k: string) (_: Scope<string, Context>) -> Effect.succeed k.Length))

        toBe (run time (ScopedCache.getOption cache "hello") = Exit.succeed None) true
        toBe (get time (ScopedCache.has cache "hello")) false
        run time (ScopedCache.get cache "hello") |> ignore
        toBe (run time (ScopedCache.getOption cache "hello") = Exit.succeed (Some 5)) true
        toBe (get time (ScopedCache.has cache "hello")) true
        toBe (get time (ScopedCache.size cache)) 1)

    test "invalidate releases the entry scoped resource" (fun () ->
        let time = ref 0L
        let acquired = ref 0
        let released = ref 0

        let cache: ScopedCache<string, int, string> =
            get time (ScopedCache.make 10 (resourceLookup acquired released))

        run time (ScopedCache.get cache "x") |> ignore
        toBe acquired.Value 1
        toBe released.Value 0
        run time (ScopedCache.invalidate cache "x") |> ignore
        toBe released.Value 1
        toBe (get time (ScopedCache.has cache "x")) false)

    test "invalidateAll releases every entry scope" (fun () ->
        let time = ref 0L
        let acquired = ref 0
        let released = ref 0

        let cache: ScopedCache<string, int, string> =
            get time (ScopedCache.make 10 (resourceLookup acquired released))

        run time (ScopedCache.get cache "a") |> ignore
        run time (ScopedCache.get cache "bb") |> ignore
        toBe acquired.Value 2
        toBe released.Value 0
        run time (ScopedCache.invalidateAll cache) |> ignore
        toBe released.Value 2
        toBe (get time (ScopedCache.size cache)) 0)

    test "refresh releases old scope and re-acquires" (fun () ->
        let time = ref 0L
        let acquired = ref 0
        let released = ref 0

        let cache: ScopedCache<string, int, string> =
            get time (ScopedCache.make 10 (resourceLookup acquired released))

        run time (ScopedCache.get cache "x") |> ignore
        run time (ScopedCache.refresh cache "x") |> ignore
        toBe acquired.Value 2
        toBe released.Value 1)

    test "capacity eviction releases the evicted entry scope" (fun () ->
        let time = ref 0L
        let acquired = ref 0
        let released = ref 0

        let cache: ScopedCache<string, int, string> =
            get time (ScopedCache.make 1 (resourceLookup acquired released))

        run time (ScopedCache.get cache "a") |> ignore
        run time (ScopedCache.get cache "b") |> ignore
        toBe acquired.Value 2
        toBe released.Value 1
        toBe (get time (ScopedCache.size cache)) 1))
