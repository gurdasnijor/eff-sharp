module Effect.Tests.DurationTests

open System.Numerics
open Xunit
open Effect

// Ported from repos/effect-smol/packages/effect/test/Duration.test.ts
//
// `deepStrictEqual` maps to F# structural `Assert.Equal`. JS-only behaviours
// (toJSON, NodeInspect, .pipe()) are dropped per Duration.fs doc comment; the
// `Equal.equals` cases map to `Duration.equals`.

let private inf = System.Double.PositiveInfinity
let private ninf = System.Double.NegativeInfinity
let private nan = System.Double.NaN

let private obj fields = Duration.fromInputUnsafe (FromObject fields)
let private E = DurationObject.Empty

[<Fact>]
let ``fromInputUnsafe`` () =
    let millis100 = Duration.millis 100.0
    Assert.Equal(millis100, Duration.fromInputUnsafe (FromDuration millis100))
    Assert.Equal(millis100, Duration.fromInputUnsafe (FromMillisNumber 100.0))
    Assert.Equal(Duration.nanos (BigInteger 10), Duration.fromInputUnsafe (FromNanosBigInt(BigInteger 10)))

    Assert.Equal(Duration.nanos (BigInteger 1), Duration.fromInputUnsafe (FromString "1 nano"))
    Assert.Equal(Duration.nanos (BigInteger 10), Duration.fromInputUnsafe (FromString "10 nanos"))
    Assert.Equal(Duration.nanos (BigInteger 2), Duration.fromInputUnsafe (FromString "1.5 nanos"))
    Assert.Equal(Duration.nanos (BigInteger -2), Duration.fromInputUnsafe (FromString "-1.5 nanos"))
    Assert.Equal(Duration.micros (BigInteger 1), Duration.fromInputUnsafe (FromString "1 micro"))
    Assert.Equal(Duration.micros (BigInteger 10), Duration.fromInputUnsafe (FromString "10 micros"))
    Assert.Equal(Duration.nanos (BigInteger 1500), Duration.fromInputUnsafe (FromString "1.5 micros"))
    Assert.Equal(Duration.nanos (BigInteger -1500), Duration.fromInputUnsafe (FromString "-1.5 micros"))
    Assert.Equal(Duration.millis 1.0, Duration.fromInputUnsafe (FromString "1 milli"))
    Assert.Equal(Duration.millis 10.0, Duration.fromInputUnsafe (FromString "10 millis"))
    Assert.Equal(Duration.seconds 1.0, Duration.fromInputUnsafe (FromString "1 second"))
    Assert.Equal(Duration.seconds 10.0, Duration.fromInputUnsafe (FromString "10 seconds"))
    Assert.Equal(Duration.minutes 1.0, Duration.fromInputUnsafe (FromString "1 minute"))
    Assert.Equal(Duration.minutes 10.0, Duration.fromInputUnsafe (FromString "10 minutes"))
    Assert.Equal(Duration.hours 1.0, Duration.fromInputUnsafe (FromString "1 hour"))
    Assert.Equal(Duration.hours 10.0, Duration.fromInputUnsafe (FromString "10 hours"))
    Assert.Equal(Duration.days 1.0, Duration.fromInputUnsafe (FromString "1 day"))
    Assert.Equal(Duration.days 10.0, Duration.fromInputUnsafe (FromString "10 days"))
    Assert.Equal(Duration.weeks 1.0, Duration.fromInputUnsafe (FromString "1 week"))
    Assert.Equal(Duration.weeks 10.0, Duration.fromInputUnsafe (FromString "10 weeks"))

    Assert.Equal(Duration.seconds 1.5, Duration.fromInputUnsafe (FromString "1.5 seconds"))
    Assert.Equal(Duration.seconds -1.5, Duration.fromInputUnsafe (FromString "-1.5 seconds"))
    Assert.Equal(Duration.infinity, Duration.fromInputUnsafe (FromString "Infinity"))
    Assert.Equal(Duration.negativeInfinity, Duration.fromInputUnsafe (FromString "-Infinity"))

    Assert.Equal(Duration.nanos (BigInteger 500123456789L), Duration.fromInputUnsafe (FromHrTime(500.0, 123456789.0)))
    Assert.Equal(Duration.nanos (BigInteger -500000000000L + BigInteger 123456789), Duration.fromInputUnsafe (FromHrTime(-500.0, 123456789.0)))
    Assert.Equal(Duration.nanos (BigInteger 2), Duration.fromInputUnsafe (FromHrTime(0.0, 1.5)))
    Assert.Equal(Duration.nanos (BigInteger -2), Duration.fromInputUnsafe (FromHrTime(0.0, -1.5)))
    Assert.Equal(Duration.nanos (BigInteger 1), Duration.fromInputUnsafe (FromHrTime(0.0000000005, 0.5)))
    Assert.Equal(Duration.infinity, Duration.fromInputUnsafe (FromHrTime(inf, 0.0)))
    Assert.Equal(Duration.negativeInfinity, Duration.fromInputUnsafe (FromHrTime(ninf, 0.0)))
    Assert.Equal(Duration.zero, Duration.fromInputUnsafe (FromHrTime(nan, 0.0)))
    Assert.Equal(Duration.infinity, Duration.fromInputUnsafe (FromHrTime(0.0, inf)))
    Assert.Equal(Duration.negativeInfinity, Duration.fromInputUnsafe (FromHrTime(0.0, ninf)))
    Assert.Equal(Duration.zero, Duration.fromInputUnsafe (FromHrTime(0.0, nan)))

    // object input
    Assert.Equal(Duration.zero, obj E)
    Assert.Equal(Duration.hours 2.0, obj { E with Hours = Some 2.0 })
    Assert.Equal(Duration.weeks 1.0, obj { E with Weeks = Some 1.0 })
    Assert.Equal(Duration.days 3.0, obj { E with Days = Some 3.0 })
    Assert.Equal(Duration.minutes 45.0, obj { E with Minutes = Some 45.0 })
    Assert.Equal(Duration.seconds 30.0, obj { E with Seconds = Some 30.0 })
    Assert.Equal(Duration.millis 500.0, obj { E with Milliseconds = Some 500.0 })
    Assert.Equal(Duration.sum (Duration.hours 1.0) (Duration.minutes 30.0), obj { E with Hours = Some 1.0; Minutes = Some 30.0 })
    Assert.Equal(Duration.hours -1.0, obj { E with Hours = Some -1.0 })
    Assert.Equal(Duration.nanos (BigInteger 1000000500L), obj { E with Seconds = Some 1.0; Nanoseconds = Some 500.0 })
    Assert.Equal(Duration.micros (BigInteger 100), obj { E with Microseconds = Some 100.0 })
    Assert.Equal(Duration.nanos (BigInteger -1500), obj { E with Microseconds = Some -1.5 })
    Assert.Equal(Duration.nanos (BigInteger 2), obj { E with Nanoseconds = Some 1.5 })
    Assert.Equal(Duration.nanos (BigInteger -2), obj { E with Nanoseconds = Some -1.5 })
    Assert.Equal(Duration.nanos (BigInteger 1), obj { E with Milliseconds = Some 0.0000005; Nanoseconds = Some 0.5 })
    Assert.Equal(
        Duration.sum (Duration.sum (Duration.days 1.0) (Duration.hours 2.0)) (Duration.sum (Duration.minutes 30.0) (Duration.seconds 15.0)),
        obj { E with Days = Some 1.0; Hours = Some 2.0; Minutes = Some 30.0; Seconds = Some 15.0 })

[<Fact>]
let ``fromInput`` () =
    Assert.Equal(Some(Duration.millis 100.0), Duration.fromInput (FromMillisNumber 100.0))
    Assert.Equal(Some(Duration.nanos (BigInteger 10)), Duration.fromInput (FromNanosBigInt(BigInteger 10)))
    Assert.Equal(Some(Duration.seconds 1.0), Duration.fromInput (FromString "1 second"))
    Assert.Equal(Some(Duration.infinity), Duration.fromInput (FromString "Infinity"))
    Assert.Equal(None, Duration.fromInput (FromString "invalid"))

[<Fact>]
let ``Order`` () =
    Assert.Equal(-1, Duration.order (Duration.millis 1.0) (Duration.millis 2.0))
    Assert.Equal(1, Duration.order (Duration.millis 2.0) (Duration.millis 1.0))
    Assert.Equal(0, Duration.order (Duration.millis 2.0) (Duration.millis 2.0))
    Assert.Equal(-1, Duration.order (Duration.millis 1.0) (Duration.nanos (BigInteger 2000000)))

    Assert.Equal(-1, Duration.order (Duration.nanos (BigInteger 1)) (Duration.nanos (BigInteger 2)))
    Assert.Equal(1, Duration.order (Duration.nanos (BigInteger 2)) (Duration.nanos (BigInteger 1)))
    Assert.Equal(0, Duration.order (Duration.nanos (BigInteger 2)) (Duration.nanos (BigInteger 2)))
    Assert.Equal(1, Duration.order (Duration.nanos (BigInteger 2000000)) (Duration.millis 1.0))

    Assert.Equal(0, Duration.order Duration.infinity Duration.infinity)
    Assert.Equal(1, Duration.order Duration.infinity (Duration.millis 1.0))
    Assert.Equal(1, Duration.order Duration.infinity (Duration.nanos (BigInteger 1)))
    Assert.Equal(-1, Duration.order (Duration.millis 1.0) Duration.infinity)
    Assert.Equal(-1, Duration.order (Duration.nanos (BigInteger 1)) Duration.infinity)

[<Fact>]
let ``Equivalence`` () =
    Assert.True(Duration.equivalence (Duration.millis 1.0) (Duration.millis 1.0))
    Assert.True(Duration.equivalence (Duration.millis 1.0) (Duration.nanos (BigInteger 1000000)))
    Assert.False(Duration.equivalence (Duration.millis 1.0) (Duration.millis 2.0))

    Assert.True(Duration.equivalence (Duration.nanos (BigInteger 1)) (Duration.nanos (BigInteger 1)))
    Assert.True(Duration.equivalence (Duration.nanos (BigInteger 1000000)) (Duration.millis 1.0))
    Assert.False(Duration.equivalence (Duration.nanos (BigInteger 1)) (Duration.nanos (BigInteger 2)))

    Assert.True(Duration.equivalence Duration.infinity Duration.infinity)
    Assert.False(Duration.equivalence Duration.infinity (Duration.millis 1.0))
    Assert.False(Duration.equivalence Duration.infinity (Duration.nanos (BigInteger 1)))
    Assert.False(Duration.equivalence (Duration.millis 1.0) Duration.infinity)
    Assert.False(Duration.equivalence (Duration.nanos (BigInteger 1)) Duration.infinity)

[<Fact>]
let ``max`` () =
    Assert.Equal(Duration.millis 2.0, Duration.max (Duration.millis 1.0) (Duration.millis 2.0))
    Assert.Equal(Duration.minutes 1.0, Duration.max (Duration.minutes 1.0) (Duration.millis 2.0))

[<Fact>]
let ``min`` () =
    Assert.Equal(Duration.millis 1.0, Duration.min (Duration.millis 1.0) (Duration.millis 2.0))
    Assert.Equal(Duration.millis 2.0, Duration.min (Duration.minutes 1.0) (Duration.millis 2.0))

[<Fact>]
let ``clamp`` () =
    Assert.Equal(Duration.millis 2.0, Duration.clamp (Duration.millis 2.0) (Duration.millis 3.0) (Duration.millis 1.0))
    Assert.Equal(Duration.minutes 1.5, Duration.clamp (Duration.minutes 1.0) (Duration.minutes 2.0) (Duration.minutes 1.5))

[<Fact>]
let ``equals`` () =
    Assert.True(Duration.equals (Duration.hours 1.0) (Duration.minutes 60.0))

[<Fact>]
let ``between`` () =
    Assert.True(Duration.between (Duration.minutes 59.0) (Duration.minutes 61.0) (Duration.hours 1.0))
    Assert.True(Duration.between (Duration.seconds 59.0) (Duration.seconds 61.0) (Duration.minutes 1.0))

[<Fact>]
let ``divide`` () =
    Assert.Equal(Some(Duration.seconds 30.0), Duration.divide (Duration.minutes 1.0) 2.0)
    Assert.Equal(Some(Duration.nanos (BigInteger 333333333)), Duration.divide (Duration.seconds 1.0) 3.0)
    Assert.Equal(Some(Duration.zero), Duration.divide Duration.zero 2.0)
    Assert.Equal(Some(Duration.minutes 2.0), Duration.divide (Duration.minutes 1.0) 0.5)
    Assert.Equal(Some(Duration.seconds 40.0), Duration.divide (Duration.minutes 1.0) 1.5)

    Assert.Equal(Some(Duration.nanos (BigInteger 1)), Duration.divide (Duration.nanos (BigInteger 2)) 2.0)
    Assert.Equal(Some(Duration.zero), Duration.divide (Duration.nanos (BigInteger 1)) 3.0)
    Assert.Equal(None, Duration.divide (Duration.nanos (BigInteger 1)) 0.5)
    Assert.Equal(None, Duration.divide (Duration.nanos (BigInteger 1)) 1.5)

    Assert.Equal(Some(Duration.infinity), Duration.divide Duration.infinity 2.0)
    Assert.Equal(Some(Duration.negativeInfinity), Duration.divide Duration.infinity -2.0)
    Assert.Equal(Some(Duration.infinity), Duration.divide Duration.negativeInfinity -2.0)

    Assert.Equal(None, Duration.divide (Duration.minutes 1.0) 0.0)
    Assert.Equal(None, Duration.divide (Duration.minutes 1.0) -0.0)
    Assert.Equal(None, Duration.divide (Duration.nanos (BigInteger 1)) 0.0)

    Assert.Equal(None, Duration.divide (Duration.minutes 1.0) nan)
    Assert.Equal(None, Duration.divide (Duration.nanos (BigInteger 1)) nan)
    Assert.Equal(None, Duration.divide Duration.infinity nan)
    Assert.Equal(None, Duration.divide (Duration.minutes 1.0) inf)
    Assert.Equal(None, Duration.divide (Duration.minutes 1.0) ninf)

[<Fact>]
let ``divideUnsafe`` () =
    Assert.Equal(Duration.seconds 30.0, Duration.divideUnsafe (Duration.minutes 1.0) 2.0)
    Assert.Equal(Duration.nanos (BigInteger 333333333), Duration.divideUnsafe (Duration.seconds 1.0) 3.0)
    Assert.Equal(Duration.zero, Duration.divideUnsafe Duration.zero 2.0)
    Assert.Equal(Duration.minutes 2.0, Duration.divideUnsafe (Duration.minutes 1.0) 0.5)
    Assert.Equal(Duration.seconds 40.0, Duration.divideUnsafe (Duration.minutes 1.0) 1.5)

    Assert.Equal(Duration.nanos (BigInteger 1), Duration.divideUnsafe (Duration.nanos (BigInteger 2)) 2.0)
    Assert.Equal(Duration.zero, Duration.divideUnsafe (Duration.nanos (BigInteger 1)) 3.0)
    Assert.Throws<System.Exception>(fun () -> Duration.divideUnsafe (Duration.nanos (BigInteger 1)) 0.5 |> ignore) |> ignore
    Assert.Throws<System.Exception>(fun () -> Duration.divideUnsafe (Duration.nanos (BigInteger 1)) 1.5 |> ignore) |> ignore

    Assert.Equal(Duration.infinity, Duration.divideUnsafe Duration.infinity 2.0)

    // IEEE 754 sign rules for divide by zero
    Assert.Equal(Duration.infinity, Duration.divideUnsafe (Duration.minutes 1.0) 0.0)
    Assert.Equal(Duration.negativeInfinity, Duration.divideUnsafe (Duration.minutes 1.0) -0.0)
    Assert.Equal(Duration.infinity, Duration.divideUnsafe (Duration.nanos (BigInteger 1)) 0.0)
    Assert.Equal(Duration.negativeInfinity, Duration.divideUnsafe (Duration.nanos (BigInteger 1)) -0.0)
    Assert.Equal(Duration.negativeInfinity, Duration.divideUnsafe (Duration.nanos (BigInteger -1)) 0.0)
    Assert.Equal(Duration.infinity, Duration.divideUnsafe (Duration.nanos (BigInteger -1)) -0.0)
    Assert.Equal(Duration.zero, Duration.divideUnsafe Duration.zero 0.0)
    Assert.Equal(Duration.zero, Duration.divideUnsafe Duration.zero -0.0)

    Assert.Equal(Duration.zero, Duration.divideUnsafe (Duration.minutes 1.0) nan)
    Assert.Equal(Duration.zero, Duration.divideUnsafe (Duration.infinity) nan)
    Assert.Equal(Duration.zero, Duration.divideUnsafe (Duration.minutes 1.0) inf)
    Assert.Equal(Duration.zero, Duration.divideUnsafe (Duration.infinity) inf)
    Assert.Equal(Duration.zero, Duration.divideUnsafe (Duration.minutes 1.0) ninf)

[<Fact>]
let ``times`` () =
    Assert.Equal(Duration.minutes 1.0, Duration.times (Duration.seconds 1.0) 60.0)
    Assert.Equal(Duration.nanos (BigInteger 20), Duration.times (Duration.nanos (BigInteger 2)) 10.0)
    Assert.Equal(Duration.infinity, Duration.times Duration.infinity 60.0)

[<Fact>]
let ``sum`` () =
    Assert.Equal(Duration.minutes 1.0, Duration.sum (Duration.seconds 30.0) (Duration.seconds 30.0))
    Assert.Equal(Duration.nanos (BigInteger 60), Duration.sum (Duration.nanos (BigInteger 30)) (Duration.nanos (BigInteger 30)))
    Assert.Equal(Duration.infinity, Duration.sum Duration.infinity (Duration.seconds 30.0))
    Assert.Equal(Duration.infinity, Duration.sum (Duration.seconds 30.0) Duration.infinity)
    Assert.Equal(Duration.infinity, Duration.sum Duration.infinity (Duration.nanos (BigInteger 1)))
    Assert.Equal(Duration.infinity, Duration.sum Duration.infinity Duration.infinity)

[<Fact>]
let ``subtract`` () =
    Assert.Equal(Duration.seconds 20.0, Duration.subtract (Duration.seconds 30.0) (Duration.seconds 10.0))
    Assert.Equal(Duration.zero, Duration.subtract (Duration.seconds 30.0) (Duration.seconds 30.0))
    Assert.Equal(Duration.seconds -10.0, Duration.subtract (Duration.seconds 30.0) (Duration.seconds 40.0))

    Assert.Equal(Duration.nanos (BigInteger 20), Duration.subtract (Duration.nanos (BigInteger 30)) (Duration.nanos (BigInteger 10)))
    Assert.Equal(Duration.zero, Duration.subtract (Duration.nanos (BigInteger 30)) (Duration.nanos (BigInteger 30)))
    Assert.Equal(Duration.nanos (BigInteger -10), Duration.subtract (Duration.nanos (BigInteger 30)) (Duration.nanos (BigInteger 40)))

    Assert.Equal(Duration.infinity, Duration.subtract Duration.infinity (Duration.seconds 30.0))
    Assert.Equal(Duration.negativeInfinity, Duration.subtract (Duration.seconds 30.0) Duration.infinity)
    Assert.Equal(Duration.zero, Duration.subtract Duration.infinity Duration.infinity)
    Assert.Equal(Duration.infinity, Duration.subtract Duration.infinity Duration.negativeInfinity)
    Assert.Equal(Duration.negativeInfinity, Duration.subtract Duration.negativeInfinity Duration.infinity)
    Assert.Equal(Duration.zero, Duration.subtract Duration.negativeInfinity Duration.negativeInfinity)
    Assert.Equal(Duration.negativeInfinity, Duration.subtract Duration.negativeInfinity (Duration.seconds 5.0))
    Assert.Equal(Duration.infinity, Duration.subtract (Duration.seconds 5.0) Duration.negativeInfinity)

[<Fact>]
let ``isGreaterThan`` () =
    Assert.True(Duration.isGreaterThan (Duration.seconds 30.0) (Duration.seconds 20.0))
    Assert.False(Duration.isGreaterThan (Duration.seconds 30.0) (Duration.seconds 30.0))
    Assert.False(Duration.isGreaterThan (Duration.seconds 30.0) (Duration.seconds 60.0))
    Assert.True(Duration.isGreaterThan (Duration.millis 1.0) (Duration.nanos (BigInteger 1)))
    Assert.True(Duration.isGreaterThan Duration.infinity (Duration.seconds 20.0))
    Assert.False(Duration.isGreaterThan (Duration.nanos (BigInteger 1)) Duration.infinity)

[<Fact>]
let ``isLessThan`` () =
    Assert.True(Duration.isLessThan (Duration.seconds 20.0) (Duration.seconds 30.0))
    Assert.False(Duration.isLessThan (Duration.seconds 30.0) (Duration.seconds 30.0))
    Assert.False(Duration.isLessThan (Duration.seconds 60.0) (Duration.seconds 30.0))
    Assert.True(Duration.isLessThan (Duration.nanos (BigInteger 1)) (Duration.millis 1.0))

[<Fact>]
let ``toString`` () =
    Assert.Equal("Infinity", Duration.toString Duration.infinity)
    Assert.Equal("10 nanos", Duration.toString (Duration.nanos (BigInteger 10)))
    Assert.Equal("2 millis", Duration.toString (Duration.millis 2.0))
    Assert.Equal("2125000 nanos", Duration.toString (Duration.millis 2.125))
    Assert.Equal("2000 millis", Duration.toString (Duration.seconds 2.0))
    Assert.Equal("2500 millis", Duration.toString (Duration.seconds 2.5))

[<Fact>]
let ``format`` () =
    Assert.Equal("Infinity", Duration.format Duration.infinity)
    Assert.Equal("5m", Duration.format (Duration.minutes 5.0))
    Assert.Equal("5m 19s 500ms", Duration.format (Duration.minutes 5.325))
    Assert.Equal("3h", Duration.format (Duration.hours 3.0))
    Assert.Equal("3h 6m 40s 500ms", Duration.format (Duration.hours 3.11125))
    Assert.Equal("2d", Duration.format (Duration.days 2.0))
    Assert.Equal("2d 6h", Duration.format (Duration.days 2.25))
    Assert.Equal("7d", Duration.format (Duration.weeks 1.0))
    Assert.Equal("0", Duration.format Duration.zero)

[<Fact>]
let ``parts`` () =
    Assert.Equal({ Days = inf; Hours = inf; Minutes = inf; Seconds = inf; Millis = inf; Nanos = inf }, Duration.parts Duration.infinity)
    Assert.Equal({ Days = 0.0; Hours = 0.0; Minutes = 5.0; Seconds = 19.0; Millis = 500.0; Nanos = 0.0 }, Duration.parts (Duration.minutes 5.325))
    Assert.Equal({ Days = 0.0; Hours = 0.0; Minutes = 3.0; Seconds = 6.0; Millis = 675.0; Nanos = 0.0 }, Duration.parts (Duration.minutes 3.11125))

[<Fact>]
let ``isDuration`` () =
    Assert.True(Duration.isDuration (box (Duration.millis 100.0)))
    Assert.False(Duration.isDuration (box null))

[<Fact>]
let ``zero`` () =
    Assert.Equal(Duration.seconds 1.0, Duration.sum (Duration.seconds 1.0) Duration.zero)

[<Fact>]
let ``weeks`` () =
    Assert.True(Duration.equals (Duration.weeks 1.0) (Duration.days 7.0))
    Assert.False(Duration.equals (Duration.weeks 1.0) (Duration.days 1.0))

[<Fact>]
let ``toMillis`` () =
    Assert.Equal(1.0, Duration.toMillis (Duration.millis 1.0))
    Assert.Equal(0.000001, Duration.toMillis (Duration.nanos (BigInteger 1)))
    Assert.Equal(inf, Duration.toMillis Duration.infinity)

[<Fact>]
let ``toSeconds`` () =
    Assert.Equal(0.001, Duration.toSeconds (Duration.millis 1.0))
    Assert.Equal(1e-9, Duration.toSeconds (Duration.nanos (BigInteger 1)))
    Assert.Equal(inf, Duration.toSeconds Duration.infinity)

[<Fact>]
let ``toNanos`` () =
    Assert.Equal(Some(BigInteger 1), Duration.toNanos (Duration.nanos (BigInteger 1)))
    Assert.Equal(None, Duration.toNanos Duration.infinity)
    Assert.Equal(None, Duration.toNanos Duration.negativeInfinity)
    Assert.Equal(Some(BigInteger 1000500), Duration.toNanos (Duration.millis 1.0005))
    Assert.Equal(Some(BigInteger 100000000), Duration.toNanos (Duration.millis 100.0))

[<Fact>]
let ``toNanosUnsafe`` () =
    Assert.Equal(BigInteger 1, Duration.toNanosUnsafe (Duration.nanos (BigInteger 1)))
    Assert.Throws<System.Exception>(fun () -> Duration.toNanosUnsafe Duration.infinity |> ignore) |> ignore
    Assert.Equal(BigInteger 1000500, Duration.toNanosUnsafe (Duration.millis 1.0005))
    Assert.Equal(BigInteger 2, Duration.toNanosUnsafe (Duration.millis 0.0000015))
    Assert.Equal(BigInteger -2, Duration.toNanosUnsafe (Duration.millis -0.0000015))
    Assert.Equal(BigInteger 100000000, Duration.toNanosUnsafe (Duration.millis 100.0))

[<Fact>]
let ``toHrTime`` () =
    Assert.Equal((0.0, 1000000.0), Duration.toHrTime (Duration.millis 1.0))
    Assert.Equal((0.0, 1.0), Duration.toHrTime (Duration.nanos (BigInteger 1)))
    Assert.Equal((1.0, 1.0), Duration.toHrTime (Duration.nanos (BigInteger 1000000001)))
    Assert.Equal((1.0, 1000000.0), Duration.toHrTime (Duration.millis 1001.0))
    Assert.Equal((inf, 0.0), Duration.toHrTime Duration.infinity)

[<Fact>]
let ``negative values`` () =
    Assert.True(Duration.isNegative (Duration.millis -1.0))
    Assert.True(Duration.isNegative (Duration.nanos (BigInteger -1)))
    Assert.Equal(-1.0, Duration.toMillis (Duration.millis -1.0))
    Assert.Equal(BigInteger -1, Duration.toNanosUnsafe (Duration.nanos (BigInteger -1)))

    Assert.True(Duration.isNegative (Duration.seconds -5.0))
    Assert.False(Duration.isNegative Duration.zero)
    Assert.False(Duration.isNegative (Duration.seconds 5.0))
    Assert.False(Duration.isNegative Duration.infinity)
    Assert.True(Duration.isNegative Duration.negativeInfinity)

    Assert.True(Duration.isPositive (Duration.seconds 5.0))
    Assert.False(Duration.isPositive Duration.zero)
    Assert.False(Duration.isPositive (Duration.seconds -5.0))
    Assert.True(Duration.isPositive Duration.infinity)
    Assert.False(Duration.isPositive Duration.negativeInfinity)

    Assert.Equal(Duration.seconds 5.0, Duration.abs (Duration.seconds -5.0))
    Assert.Equal(Duration.seconds 5.0, Duration.abs (Duration.seconds 5.0))
    Assert.Equal(Duration.zero, Duration.abs Duration.zero)
    Assert.Equal(Duration.infinity, Duration.abs Duration.negativeInfinity)
    Assert.Equal(Duration.infinity, Duration.abs Duration.infinity)

    Assert.Equal(Duration.seconds -5.0, Duration.negate (Duration.seconds 5.0))
    Assert.Equal(Duration.seconds 5.0, Duration.negate (Duration.seconds -5.0))
    Assert.Equal(Duration.zero, Duration.negate Duration.zero)
    Assert.Equal(Duration.negativeInfinity, Duration.negate Duration.infinity)
    Assert.Equal(Duration.infinity, Duration.negate Duration.negativeInfinity)

    Assert.False(Duration.isFinite Duration.negativeInfinity)
    Assert.False(Duration.isZero Duration.negativeInfinity)
    Assert.Equal(ninf, Duration.toMillis Duration.negativeInfinity)
    Assert.Equal(ninf, Duration.toSeconds Duration.negativeInfinity)
    Assert.Equal((ninf, 0.0), Duration.toHrTime Duration.negativeInfinity)

    Assert.Equal("-5s", Duration.format (Duration.seconds -5.0))
    Assert.Equal("-Infinity", Duration.format Duration.negativeInfinity)

    Assert.Equal(0, Duration.order Duration.negativeInfinity Duration.negativeInfinity)
    Assert.Equal(-1, Duration.order Duration.negativeInfinity (Duration.seconds -5.0))
    Assert.Equal(1, Duration.order (Duration.seconds -5.0) Duration.negativeInfinity)
    Assert.Equal(-1, Duration.order Duration.negativeInfinity Duration.infinity)
    Assert.Equal(1, Duration.order Duration.infinity Duration.negativeInfinity)
    Assert.Equal(-1, Duration.order (Duration.seconds -5.0) (Duration.seconds -3.0))
    Assert.Equal(1, Duration.order (Duration.seconds -3.0) (Duration.seconds -5.0))

    Assert.True(Duration.equivalence Duration.negativeInfinity Duration.negativeInfinity)
    Assert.False(Duration.equivalence Duration.negativeInfinity Duration.infinity)

    Assert.Equal(Duration.zero, Duration.sum Duration.infinity Duration.negativeInfinity)
    Assert.Equal(Duration.zero, Duration.sum Duration.negativeInfinity Duration.infinity)
    Assert.Equal(Duration.negativeInfinity, Duration.sum Duration.negativeInfinity Duration.negativeInfinity)
    Assert.Equal(Duration.negativeInfinity, Duration.sum Duration.negativeInfinity (Duration.seconds 5.0))

    Assert.Equal(Duration.negativeInfinity, Duration.times Duration.infinity -1.0)
    Assert.Equal(Duration.infinity, Duration.times Duration.negativeInfinity -1.0)
    Assert.Equal(Duration.zero, Duration.times Duration.infinity 0.0)
    Assert.Equal(Duration.seconds -10.0, Duration.times (Duration.seconds 5.0) -2.0)

    Assert.Equal({ Days = ninf; Hours = ninf; Minutes = ninf; Seconds = ninf; Millis = ninf; Nanos = ninf }, Duration.parts Duration.negativeInfinity)

    Assert.Equal("-Infinity", Duration.toString Duration.negativeInfinity)
    Assert.Equal("-5 millis", Duration.toString (Duration.millis -5.0))
    Assert.Equal("-10 nanos", Duration.toString (Duration.nanos (BigInteger -10)))

[<Fact>]
let ``match`` () =
    let m (d: Duration) =
        Duration.matchWith
            (fun millis -> sprintf "millis: %d" (int64 millis))
            (fun nanos -> sprintf "nanos: %O" nanos)
            (fun () -> "infinity")
            (fun () -> "infinity")
            d
    Assert.Equal("millis: 100", m (Duration.millis 100.0))
    Assert.Equal("nanos: 10", m (Duration.nanos (BigInteger 10)))
    Assert.Equal("infinity", m Duration.infinity)

[<Fact>]
let ``isFinite`` () =
    Assert.True(Duration.isFinite (Duration.millis 100.0))
    Assert.True(Duration.isFinite (Duration.nanos (BigInteger 100)))
    Assert.False(Duration.isFinite Duration.infinity)

[<Fact>]
let ``isZero`` () =
    Assert.True(Duration.isZero Duration.zero)
    Assert.True(Duration.isZero (Duration.millis 0.0))
    Assert.True(Duration.isZero (Duration.nanos (BigInteger 0)))
    Assert.False(Duration.isZero Duration.infinity)
    Assert.False(Duration.isZero (Duration.millis 1.0))
    Assert.False(Duration.isZero (Duration.nanos (BigInteger 1)))

[<Fact>]
let ``toMinutes`` () =
    Assert.Equal(1.0, Duration.toMinutes (Duration.millis 60000.0))
    Assert.Equal(1.0, Duration.toMinutes (Duration.nanos (BigInteger 60000000000L)))
    Assert.Equal(inf, Duration.toMinutes Duration.infinity)

[<Fact>]
let ``toHours`` () =
    Assert.Equal(1.0, Duration.toHours (Duration.millis 3600000.0))
    Assert.Equal(1.0, Duration.toHours (Duration.nanos (BigInteger 3600000000000L)))
    Assert.Equal(inf, Duration.toHours Duration.infinity)

[<Fact>]
let ``toDays`` () =
    Assert.Equal(1.0, Duration.toDays (Duration.millis 86400000.0))
    Assert.Equal(1.0, Duration.toDays (Duration.nanos (BigInteger 86400000000000L)))
    Assert.Equal(inf, Duration.toDays Duration.infinity)

[<Fact>]
let ``toWeeks`` () =
    Assert.Equal(1.0, Duration.toWeeks (Duration.millis 604800000.0))
    Assert.Equal(1.0, Duration.toWeeks (Duration.nanos (BigInteger 604800000000000L)))
    Assert.Equal(inf, Duration.toWeeks Duration.infinity)

[<Fact>]
let ``ReducerSum`` () =
    Assert.Equal(Duration.millis 3.0, Duration.ReducerSum.Combine (Duration.millis 1.0) (Duration.millis 2.0))

[<Fact>]
let ``CombinerMax`` () =
    Assert.Equal(Duration.millis 2.0, Duration.CombinerMax.Combine (Duration.millis 1.0) (Duration.millis 2.0))

[<Fact>]
let ``CombinerMin`` () =
    Assert.Equal(Duration.millis 1.0, Duration.CombinerMin.Combine (Duration.millis 1.0) (Duration.millis 2.0))
