module DifferSpec

open Effect
open Effect.Vitest

let private IntDiffer: Differ<int, int> =
    Differ.make 0 (fun oldV newV -> newV - oldV) (+) (+)

describe "Differ" (fun () ->
    test "empty patch is a no-op" (fun () -> toBe (IntDiffer.patch 5 IntDiffer.empty) 5)

    test "diff then patch round-trips" (fun () ->
        let p = IntDiffer.diff 3 10
        toBe p 7
        toBe (IntDiffer.patch 3 p) 10)

    test "combine composes patches in order" (fun () ->
        let p1 = IntDiffer.diff 0 2
        let p2 = IntDiffer.diff 0 3
        let combined = IntDiffer.combine p1 p2
        toBe combined 5
        toBe (IntDiffer.patch 10 combined) 15))

