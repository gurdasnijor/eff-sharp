module CauseSpec

open Effect
open Effect.Vitest

describe "Cause" (fun () ->
    test "TypeId constants match upstream" (fun () ->
        toBe Cause.TypeId "~effect/Cause"
        toBe Cause.ReasonTypeId "~effect/Cause/Reason")

    test "empty has no reasons" (fun () -> toEqual Cause.empty<string>.Reasons [])

    test "fail creates a single Fail reason preserving the error" (fun () ->
        let cause = Cause.fail "error"
        toBe (List.length cause.Reasons) 1
        toBe (Cause.isFailReason cause.Reasons.[0]) true
        toEqual (Cause.failures cause) [ "error" ])

    test "die creates a single Die reason preserving the defect" (fun () ->
        let cause = Cause.die (box "defect")
        toBe (List.length cause.Reasons) 1
        toBe (Cause.isDieReason cause.Reasons.[0]) true
        toEqual (Cause.defects cause) [ box "defect" ])

    test "interrupt creates a single Interrupt reason" (fun () ->
        let cause = Cause.interrupt (Some 1)
        toBe (List.length cause.Reasons) 1
        toBe (Cause.isInterruptReason cause.Reasons.[0]) true)

    test "reason guards narrow exactly one variant" (fun () ->
        let fail = (Cause.fail "error").Reasons.[0]
        let die = (Cause.die (box "defect")).Reasons.[0]
        let intr = (Cause.interrupt (Some 1)).Reasons.[0]
        toBe (Cause.isFailReason fail) true
        toBe (Cause.isDieReason fail) false
        toBe (Cause.isInterruptReason fail) false
        toBe (Cause.isDieReason die) true
        toBe (Cause.isFailReason die) false
        toBe (Cause.isInterruptReason die) false
        toBe (Cause.isInterruptReason intr) true
        toBe (Cause.isFailReason intr) false
        toBe (Cause.isDieReason intr) false)

    test "makeReason helpers build the matching variant" (fun () ->
        toBe (Cause.isFailReason (Cause.makeFailReason "error")) true
        toBe (Cause.isDieReason (Cause.makeDieReason (box "defect"))) true
        toBe (Cause.isInterruptReason (Cause.makeInterruptReason (Some 1))) true)

    test "render matches Effect's toString" (fun () ->
        toBe (Cause.render (Cause.fail "error")) "Cause([Fail(\"error\")])"
        toBe (Cause.render (Cause.die (box "defect"))) "Cause([Die(\"defect\")])"
        toBe (Cause.render (Cause.interrupt (Some 1))) "Cause([Interrupt(1)])"
        toBe (Cause.render (Cause.interrupt None)) "Cause([Interrupt(undefined)])"
        toBe (Cause.render Cause.empty<string>) "Cause([])")

    test "map transforms Fail reasons and leaves Die/Interrupt untouched" (fun () ->
        let cause =
            Cause.combine (Cause.fail 1) (Cause.combine (Cause.die (box "d")) (Cause.interrupt (Some 2)))

        let mapped = Cause.map (fun n -> n + 10) cause
        toEqual (Cause.failures mapped) [ 11 ]
        toEqual (Cause.defects mapped) [ box "d" ]
        toBe (List.length mapped.Reasons) 3)

    test "map leaves a Fail-less cause's reasons intact" (fun () ->
        let mapped =
            Cause.map (fun (_: int) -> failwith "must not be called") (Cause.die (box "d"))

        toBe (Cause.isDieReason mapped.Reasons.[0]) true)

    test "combine concatenates reasons and is empty-absorbing" (fun () ->
        toBe (List.length (Cause.combine (Cause.fail "a") (Cause.fail "b")).Reasons) 2
        let c = Cause.fail "x"
        toBe (List.length (Cause.combine c Cause.empty<string>).Reasons) 1
        toBe (List.length (Cause.combine Cause.empty<string> c).Reasons) 1)

    test "combine de-duplicates reasons by value" (fun () ->
        let combined = Cause.combine (Cause.fail "dup") (Cause.fail "dup")
        toBe (List.length combined.Reasons) 1)

    test "combine preserves order: first then second" (fun () ->
        let combined = Cause.combine (Cause.die (box "first")) (Cause.fail "second")
        toEqual (Cause.defects combined) [ box "first" ]
        toEqual (Cause.failures combined) [ "second" ]))

