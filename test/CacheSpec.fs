module CacheSpec

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

describe "Cache" (fun () ->
    test "get miss invokes lookup and hit serves from cache" (fun () ->
        let time = ref 0L
        let count = ref 0

        let cache: Cache<string, int, string> =
            get
                time
                (Cache.make 10 (fun (k: string) ->
                    Effect.sync (fun () ->
                        count.Value <- count.Value + 1
                        k.Length)))

        toBe (run time (Cache.get cache "hello") = Exit.succeed 5) true
        toBe (run time (Cache.get cache "hello") = Exit.succeed 5) true
        toBe count.Value 1)

    test "get caches failures" (fun () ->
        let time = ref 0L
        let count = ref 0

        let cache: Cache<string, int, string> =
            get
                time
                (Cache.make 10 (fun (k: string) ->
                    Effect.suspend (fun () ->
                        count.Value <- count.Value + 1

                        if k = "error" then
                            Effect.fail "boom"
                        else
                            Effect.succeed k.Length)))

        toBe (run time (Cache.get cache "error") = Exit.fail "boom") true
        toBe (run time (Cache.get cache "error") = Exit.fail "boom") true
        toBe count.Value 1)

    test "getOption and getSuccess inspect resolved entries" (fun () ->
        let time = ref 0L

        let cache: Cache<string, int, string> =
            get
                time
                (Cache.make 10 (fun (k: string) ->
                    if k = "error" then
                        Effect.fail "boom"
                    else
                        Effect.succeed k.Length))

        toBe (run time (Cache.getOption cache "hello") = Exit.succeed None) true
        run time (Cache.get cache "hello") |> ignore
        run time (Cache.get cache "error") |> ignore
        toBe (run time (Cache.getOption cache "hello") = Exit.succeed (Some 5)) true
        toBe (run time (Cache.getSuccess cache "hello") = Exit.succeed (Some 5)) true
        toBe (run time (Cache.getSuccess cache "error") = Exit.succeed None) true)

    test "set overrides lookup value" (fun () ->
        let time = ref 0L

        let cache: Cache<string, int, string> =
            get time (Cache.make 100 (fun (k: string) -> Effect.succeed k.Length))

        toBe (get time (Cache.get cache "test")) 4
        run time (Cache.set cache "test" 999) |> ignore
        toBe (get time (Cache.get cache "test")) 999)

    test "has invalidate invalidateWhen and invalidateAll" (fun () ->
        let time = ref 0L

        let cache: Cache<string, int, string> =
            get time (Cache.make 10 (fun (k: string) -> Effect.succeed k.Length))

        toBe (get time (Cache.has cache "hello")) false
        run time (Cache.get cache "hello") |> ignore
        toBe (get time (Cache.has cache "hello")) true
        run time (Cache.invalidate cache "hello") |> ignore
        toBe (get time (Cache.has cache "hello")) false

        run time (Cache.get cache "hello") |> ignore
        run time (Cache.get cache "hi") |> ignore
        toBe (get time (Cache.invalidateWhen cache "hello" (fun v -> v = 5))) true
        toBe (get time (Cache.has cache "hello")) false
        toBe (get time (Cache.invalidateWhen cache "hi" (fun v -> v = 5))) false
        toBe (get time (Cache.has cache "hi")) true
        toBe (get time (Cache.size cache)) 1
        run time (Cache.invalidateAll cache) |> ignore
        toBe (get time (Cache.size cache)) 0)

    test "invalidate makes lookup run again" (fun () ->
        let time = ref 0L
        let count = ref 0

        let cache: Cache<string, int, string> =
            get
                time
                (Cache.make 10 (fun (k: string) ->
                    Effect.sync (fun () ->
                        count.Value <- count.Value + 1
                        k.Length)))

        run time (Cache.get cache "test") |> ignore
        run time (Cache.invalidate cache "test") |> ignore
        run time (Cache.get cache "test") |> ignore
        toBe count.Value 2)

    test "refresh always reruns lookup and overwrites" (fun () ->
        let time = ref 0L
        let counter = ref 0

        let cache: Cache<string, string, string> =
            get
                time
                (Cache.make 10 (fun (k: string) ->
                    Effect.sync (fun () ->
                        counter.Value <- counter.Value + 1
                        sprintf "%s-%d" k counter.Value)))

        toBe (get time (Cache.get cache "user")) "user-1"
        toBe (get time (Cache.get cache "user")) "user-1"
        toBe (get time (Cache.refresh cache "user")) "user-2"
        toBe (get time (Cache.get cache "user")) "user-2")

    test "TTL policies expire entries" (fun () ->
        let time = ref 0L

        let fixedCache: Cache<string, int, string> =
            get time (Cache.makeWithTtl 10 (Duration.hours 1.0) (fun (k: string) -> Effect.succeed k.Length))

        run time (Cache.get fixedCache "test") |> ignore
        time.Value <- int64 (30.0 * 60.0 * 1000.0)
        toBe (get time (Cache.has fixedCache "test")) true
        time.Value <- int64 (61.0 * 60.0 * 1000.0)
        toBe (get time (Cache.has fixedCache "test")) false

        let ttl (exit: Exit<int, string>) (_key: string) =
            match exit with
            | Success _ -> Duration.hours 1.0
            | Failure _ -> Duration.seconds 1.0

        let dynamic: Cache<string, int, string> =
            get
                time
                (Cache.makeWith 10 ttl (fun (k: string) ->
                    if k = "fail" then
                        Effect.fail "boom"
                    else
                        Effect.succeed k.Length))

        time.Value <- 0L
        run time (Cache.get dynamic "ok") |> ignore
        run time (Cache.get dynamic "fail") |> ignore
        time.Value <- int64 (2.0 * 1000.0)
        toBe (get time (Cache.has dynamic "fail")) false
        toBe (get time (Cache.has dynamic "ok")) true)

    test "capacity exceeding evicts the oldest entry" (fun () ->
        let time = ref 0L

        let cache: Cache<string, int, string> =
            get time (Cache.make 2 (fun (k: string) -> Effect.succeed k.Length))

        run time (Cache.set cache "a" 1) |> ignore
        run time (Cache.set cache "b" 2) |> ignore
        toBe (get time (Cache.size cache)) 2
        run time (Cache.set cache "c" 3) |> ignore
        toBe (get time (Cache.size cache)) 2
        toBe (get time (Cache.has cache "a")) false
        toBe (get time (Cache.has cache "c")) true))
