module RandomSpec

open Effect
open Effect.Vitest

let private run (ctx: Context) (eff: Effect<'A, 'E, Context>) : 'A =
    match Effect.runSync ctx eff with
    | Success a -> a
    | Failure c -> failwithf "unexpected failure: %s" (Cause.render c)

let private mockDoubles (values: float list) : Context =
    let arr = List.toArray values
    let mutable i = 0

    let nextDouble () =
        let v = if i < arr.Length then arr.[i] else 0.0
        i <- i + 1
        v

    Context.make Random.tag (Random.ofDouble nextDouble)

let private empty = Context.empty

let private seq5: Effect<float list, string, Context> =
    effect {
        let! a = Random.next
        let! b = Random.next
        let! c = Random.next
        let! d = Random.next
        let! e = Random.next
        return [ a; b; c; d; e ]
    }

describe "Random" (fun () ->
    test "next and nextBoolean produce values in range" (fun () ->
        let value = run empty Random.next
        toBe (value >= 0.0 && value < 1.0) true

        let boolValue = run empty Random.nextBoolean
        toBe (boolValue = true || boolValue = false) true)

    test "nextBoolean is deterministic with seeds and uses a strict threshold" (fun () ->
        let program =
            effect {
                let! a = Random.nextBoolean
                let! b = Random.nextBoolean
                let! c = Random.nextBoolean
                return [ a; b; c ]
            }

        let r1 = run empty (Random.withSeed (SeedString "next-boolean-seed") program)
        let r2 = run empty (Random.withSeed (SeedString "next-boolean-seed") program)
        toEqual r1 r2

        toEqual (run (mockDoubles [ 0.5; 0.500001; 0.1 ]) program) [ false; true; false ])

    test "nextInt and bounded numbers stay inside requested ranges" (fun () ->
        let value = run empty Random.nextInt
        toBe (value >= -9_007_199_254_740_991.0 && value <= 9_007_199_254_740_991.0) true
        toBe value (floor value)

        for _ in 1..100 do
            let between = run empty (Random.nextBetween 10.0 20.0)
            toBe (between >= 10.0 && between < 20.0) true

            let closed = run empty (Random.nextIntBetween 1.0 6.0 false)
            toBe closed (floor closed)
            toBe (closed >= 1.0 && closed <= 6.0) true

            let halfOpen = run empty (Random.nextIntBetween 1.0 6.0 true)
            toBe (halfOpen >= 1.0 && halfOpen < 6.0) true

        toBe (run (mockDoubles [ 0.75 ]) (Random.nextBetween 10.5 20.5)) 18.0
        let negative = run empty (Random.nextBetween -10.0 10.0)
        toBe (negative >= -10.0 && negative < 10.0) true
        toBe (run (mockDoubles [ 0.9999999999999999 ]) (Random.nextIntBetween 1.0 6.0 true)) 5.0)

    test "shuffle returns all values and is deterministic with the same seed" (fun () ->
        let input = [ 1; 2; 3; 4; 5 ]
        let output = run empty (Random.shuffle input)
        toEqual input [ 1; 2; 3; 4; 5 ]
        toEqual (output |> Array.toList |> List.sort) [ 1; 2; 3; 4; 5 ]

        let program = Random.shuffle [ 1; 2; 3; 4; 5 ]
        let r1 = run empty (Random.withSeed (SeedString "shuffle-seed") program)
        let r2 = run empty (Random.withSeed (SeedString "shuffle-seed") program)
        toEqual r1 r2)

    test "choice selects values and fails for empty iterables" (fun () ->
        toBe (run (mockDoubles [ 0.75 ]) (Random.choice [ "a"; "b"; "c" ])) "c"

        match Effect.runSync empty (Random.choice ([]: int list)) with
        | Failure c ->
            match Cause.failures c with
            | (e: NoSuchElementError) :: _ -> toBe e.Message "Cannot select a random element from an empty array"
            | [] -> failwith "expected a typed failure"
        | Success _ -> failwith "expected failure"

        let program =
            effect {
                let! a = Random.choice [ 1; 2; 3; 4; 5 ]
                let! b = Random.choice [ 1; 2; 3; 4; 5 ]
                let! c = Random.choice [ 1; 2; 3; 4; 5 ]
                return [ a; b; c ]
            }

        let r1 = run empty (Random.withSeed (SeedString "choice-seed") program)
        let r2 = run empty (Random.withSeed (SeedString "choice-seed") program)
        toEqual r1 r2)

    test "withSeed is deterministic and distinguishes seed values" (fun () ->
        toEqual (run empty (Random.withSeed (SeedString "test-seed") seq5)) (run empty (Random.withSeed (SeedString "test-seed") seq5))
        toBe (run empty (Random.withSeed (SeedInt 12345) seq5) = run empty (Random.withSeed (SeedInt 67890) seq5)) false

        let e = run empty (Random.withSeed (SeedString "") seq5)
        let a = run empty (Random.withSeed (SeedString "a") seq5)
        let b = run empty (Random.withSeed (SeedString "b") seq5)
        toBe (a = e) false
        toBe (b = e) false
        toBe (a = b) false

        for s1, s2 in [ "1234a", "1234b"; "1234ab", "1234ac"; "1234abc", "1234abd" ] do
            toBe (run empty (Random.withSeed (SeedString s1) seq5) = run empty (Random.withSeed (SeedString s2) seq5)) false

        let r1 = run empty (Random.withSeed (SeedString "\U0001F600") seq5)
        let r2 = run empty (Random.withSeed (SeedString "\U0001F601") seq5)
        let r3 = run empty (Random.withSeed (SeedString "\U0001F400") seq5)
        toBe (r1 = e) false
        toBe (r2 = e) false
        toBe (r1 = r2) false
        toBe (r1 = r3) false
        toEqual (run empty (Random.withSeed (SeedInt 12345) seq5)) (run empty (Random.withSeed (SeedInt 12345) seq5))))
