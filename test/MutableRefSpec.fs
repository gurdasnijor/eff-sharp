module MutableRefSpec

open Effect
open Effect.Vitest

describe "MutableRef" (fun () ->
    test "get and set read and write the current value" (fun () ->
        let ref = MutableRef.make 42
        toBe (MutableRef.get ref) 42
        MutableRef.set ref 100 |> ignore
        toBe (MutableRef.get ref) 100)

    test "set returns the same reference" (fun () ->
        let ref = MutableRef.make "initial"
        toBe (MutableRef.set ref "updated") ref)

    test "setAndGet returns the new value" (fun () ->
        let ref = MutableRef.make "old"
        toBe (MutableRef.setAndGet ref "new") "new"
        toBe (MutableRef.get ref) "new")

    test "getAndSet returns the previous value" (fun () ->
        let ref = MutableRef.make "old"
        toBe (MutableRef.getAndSet ref "new") "old"
        toBe (MutableRef.get ref) "new")

    test "update applies a function and returns the reference" (fun () ->
        let ref = MutableRef.make 5
        toBe (MutableRef.update ref (fun n -> n + 1)) ref
        toBe (MutableRef.get ref) 6)

    test "updateAndGet and getAndUpdate" (fun () ->
        let ref = MutableRef.make 5
        toBe (MutableRef.updateAndGet ref (fun n -> n + 1)) 6
        toBe (MutableRef.getAndUpdate ref (fun n -> n * 2)) 6
        toBe (MutableRef.get ref) 12)

    test "compareAndSet swaps only on a matching current value" (fun () ->
        let ref = MutableRef.make "initial"
        toBe (MutableRef.compareAndSet ref "initial" "updated") true
        toBe (MutableRef.get ref) "updated"
        toBe (MutableRef.compareAndSet ref "initial" "failed") false
        toBe (MutableRef.get ref) "updated")

    test "increment and decrement" (fun () ->
        let ref = MutableRef.make 5
        MutableRef.increment ref |> ignore
        toBe (MutableRef.get ref) 6
        MutableRef.decrement ref |> ignore
        toBe (MutableRef.get ref) 5)

    test "incrementAndGet, decrementAndGet, getAndIncrement, getAndDecrement" (fun () ->
        let ref = MutableRef.make 5
        toBe (MutableRef.incrementAndGet ref) 6
        toBe (MutableRef.decrementAndGet ref) 5
        toBe (MutableRef.getAndIncrement ref) 5
        toBe (MutableRef.get ref) 6
        toBe (MutableRef.getAndDecrement ref) 6
        toBe (MutableRef.get ref) 5)

    test "toggle flips a boolean" (fun () ->
        let flag = MutableRef.make false
        MutableRef.toggle flag |> ignore
        toBe (MutableRef.get flag) true
        MutableRef.toggle flag |> ignore
        toBe (MutableRef.get flag) false))

