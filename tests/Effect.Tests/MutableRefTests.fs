module Effect.Tests.MutableRefTests

open Xunit
open Effect

// No upstream test file exists for MutableRef; these cases cover the documented
// behaviour from repos/effect-smol/packages/effect/src/MutableRef.ts.

[<Fact>]
let ``get and set read and write the current value`` () =
    let ref = MutableRef.make 42
    Assert.Equal(42, MutableRef.get ref)
    MutableRef.set ref 100 |> ignore
    Assert.Equal(100, MutableRef.get ref)

[<Fact>]
let ``set returns the same reference`` () =
    let ref = MutableRef.make "initial"
    Assert.Same(ref, MutableRef.set ref "updated")

[<Fact>]
let ``setAndGet returns the new value`` () =
    let ref = MutableRef.make "old"
    Assert.Equal("new", MutableRef.setAndGet ref "new")
    Assert.Equal("new", MutableRef.get ref)

[<Fact>]
let ``getAndSet returns the previous value`` () =
    let ref = MutableRef.make "old"
    Assert.Equal("old", MutableRef.getAndSet ref "new")
    Assert.Equal("new", MutableRef.get ref)

[<Fact>]
let ``update applies a function and returns the reference`` () =
    let ref = MutableRef.make 5
    Assert.Same(ref, MutableRef.update ref (fun n -> n + 1))
    Assert.Equal(6, MutableRef.get ref)

[<Fact>]
let ``updateAndGet and getAndUpdate`` () =
    let ref = MutableRef.make 5
    Assert.Equal(6, MutableRef.updateAndGet ref (fun n -> n + 1))
    Assert.Equal(6, MutableRef.getAndUpdate ref (fun n -> n * 2))
    Assert.Equal(12, MutableRef.get ref)

[<Fact>]
let ``compareAndSet swaps only on a matching current value`` () =
    let ref = MutableRef.make "initial"
    Assert.True(MutableRef.compareAndSet ref "initial" "updated")
    Assert.Equal("updated", MutableRef.get ref)
    Assert.False(MutableRef.compareAndSet ref "initial" "failed")
    Assert.Equal("updated", MutableRef.get ref)

[<Fact>]
let ``increment and decrement`` () =
    let ref = MutableRef.make 5
    MutableRef.increment ref |> ignore
    Assert.Equal(6, MutableRef.get ref)
    MutableRef.decrement ref |> ignore
    Assert.Equal(5, MutableRef.get ref)

[<Fact>]
let ``incrementAndGet, decrementAndGet, getAndIncrement, getAndDecrement`` () =
    let ref = MutableRef.make 5
    Assert.Equal(6, MutableRef.incrementAndGet ref)
    Assert.Equal(5, MutableRef.decrementAndGet ref)
    Assert.Equal(5, MutableRef.getAndIncrement ref)
    Assert.Equal(6, MutableRef.get ref)
    Assert.Equal(6, MutableRef.getAndDecrement ref)
    Assert.Equal(5, MutableRef.get ref)

[<Fact>]
let ``toggle flips a boolean`` () =
    let flag = MutableRef.make false
    MutableRef.toggle flag |> ignore
    Assert.True(MutableRef.get flag)
    MutableRef.toggle flag |> ignore
    Assert.False(MutableRef.get flag)
