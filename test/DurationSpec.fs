module DurationSpec

open System.Numerics
open Effect
open Effect.Vitest

let private inf = System.Double.PositiveInfinity
let private ninf = System.Double.NegativeInfinity
let private nan = System.Double.NaN

let private bi (n: int) = BigInteger n
let private obj fields = Duration.fromInputUnsafe (FromObject fields)
let private E = DurationObject.Empty

let private same actual expected =
    toBe (actual = expected) true

describe "Duration" (fun () ->
    test "fromInputUnsafe decodes primitive string hrtime and object inputs" (fun () ->
        let millis100 = Duration.millis 100.0
        same (Duration.fromInputUnsafe (FromDuration millis100)) millis100
        same (Duration.fromInputUnsafe (FromMillisNumber 100.0)) millis100
        same (Duration.fromInputUnsafe (FromNanosBigInt(bi 10))) (Duration.nanos (bi 10))

        same (Duration.fromInputUnsafe (FromString "1 nano")) (Duration.nanos (bi 1))
        same (Duration.fromInputUnsafe (FromString "10 nanos")) (Duration.nanos (bi 10))
        same (Duration.fromInputUnsafe (FromString "1.5 nanos")) (Duration.nanos (bi 2))
        same (Duration.fromInputUnsafe (FromString "-1.5 nanos")) (Duration.nanos (bi -2))
        same (Duration.fromInputUnsafe (FromString "1 micro")) (Duration.micros (bi 1))
        same (Duration.fromInputUnsafe (FromString "10 micros")) (Duration.micros (bi 10))
        same (Duration.fromInputUnsafe (FromString "1.5 micros")) (Duration.nanos (bi 1500))
        same (Duration.fromInputUnsafe (FromString "-1.5 micros")) (Duration.nanos (bi -1500))
        same (Duration.fromInputUnsafe (FromString "1 milli")) (Duration.millis 1.0)
        same (Duration.fromInputUnsafe (FromString "10 millis")) (Duration.millis 10.0)
        same (Duration.fromInputUnsafe (FromString "1 second")) (Duration.seconds 1.0)
        same (Duration.fromInputUnsafe (FromString "10 seconds")) (Duration.seconds 10.0)
        same (Duration.fromInputUnsafe (FromString "1 minute")) (Duration.minutes 1.0)
        same (Duration.fromInputUnsafe (FromString "10 minutes")) (Duration.minutes 10.0)
        same (Duration.fromInputUnsafe (FromString "1 hour")) (Duration.hours 1.0)
        same (Duration.fromInputUnsafe (FromString "10 hours")) (Duration.hours 10.0)
        same (Duration.fromInputUnsafe (FromString "1 day")) (Duration.days 1.0)
        same (Duration.fromInputUnsafe (FromString "10 days")) (Duration.days 10.0)
        same (Duration.fromInputUnsafe (FromString "1 week")) (Duration.weeks 1.0)
        same (Duration.fromInputUnsafe (FromString "10 weeks")) (Duration.weeks 10.0)
        same (Duration.fromInputUnsafe (FromString "1.5 seconds")) (Duration.seconds 1.5)
        same (Duration.fromInputUnsafe (FromString "-1.5 seconds")) (Duration.seconds -1.5)
        same (Duration.fromInputUnsafe (FromString "Infinity")) Duration.infinity
        same (Duration.fromInputUnsafe (FromString "-Infinity")) Duration.negativeInfinity

        same (Duration.fromInputUnsafe (FromHrTime(500.0, 123456789.0))) (Duration.nanos (BigInteger 500123456789L))
        same
            (Duration.fromInputUnsafe (FromHrTime(-500.0, 123456789.0)))
            (Duration.nanos (BigInteger -500000000000L + BigInteger 123456789))
        same (Duration.fromInputUnsafe (FromHrTime(0.0, 1.5))) (Duration.nanos (bi 2))
        same (Duration.fromInputUnsafe (FromHrTime(0.0, -1.5))) (Duration.nanos (bi -2))
        same (Duration.fromInputUnsafe (FromHrTime(0.0000000005, 0.5))) (Duration.nanos (bi 1))
        same (Duration.fromInputUnsafe (FromHrTime(inf, 0.0))) Duration.infinity
        same (Duration.fromInputUnsafe (FromHrTime(ninf, 0.0))) Duration.negativeInfinity
        same (Duration.fromInputUnsafe (FromHrTime(nan, 0.0))) Duration.zero
        same (Duration.fromInputUnsafe (FromHrTime(0.0, inf))) Duration.infinity
        same (Duration.fromInputUnsafe (FromHrTime(0.0, ninf))) Duration.negativeInfinity
        same (Duration.fromInputUnsafe (FromHrTime(0.0, nan))) Duration.zero

        same (obj E) Duration.zero
        same (obj { E with Hours = Some 2.0 }) (Duration.hours 2.0)
        same (obj { E with Weeks = Some 1.0 }) (Duration.weeks 1.0)
        same (obj { E with Days = Some 3.0 }) (Duration.days 3.0)
        same (obj { E with Minutes = Some 45.0 }) (Duration.minutes 45.0)
        same (obj { E with Seconds = Some 30.0 }) (Duration.seconds 30.0)
        same (obj { E with Milliseconds = Some 500.0 }) (Duration.millis 500.0)
        same
            (obj
                { E with
                    Hours = Some 1.0
                    Minutes = Some 30.0 })
            (Duration.sum (Duration.hours 1.0) (Duration.minutes 30.0))
        same (obj { E with Hours = Some -1.0 }) (Duration.hours -1.0)
        same
            (obj
                { E with
                    Seconds = Some 1.0
                    Nanoseconds = Some 500.0 })
            (Duration.nanos (BigInteger 1000000500L))
        same (obj { E with Microseconds = Some 100.0 }) (Duration.micros (bi 100))
        same (obj { E with Microseconds = Some -1.5 }) (Duration.nanos (bi -1500))
        same (obj { E with Nanoseconds = Some 1.5 }) (Duration.nanos (bi 2))
        same (obj { E with Nanoseconds = Some -1.5 }) (Duration.nanos (bi -2))
        same
            (obj
                { E with
                    Milliseconds = Some 0.0000005
                    Nanoseconds = Some 0.5 })
            (Duration.nanos (bi 1)))

    test "fromInput returns options for safe decoding" (fun () ->
        same (Duration.fromInput (FromMillisNumber 100.0)) (Some(Duration.millis 100.0))
        same (Duration.fromInput (FromNanosBigInt(bi 10))) (Some(Duration.nanos (bi 10)))
        same (Duration.fromInput (FromString "1 second")) (Some(Duration.seconds 1.0))
        same (Duration.fromInput (FromString "Infinity")) (Some Duration.infinity)
        same (Duration.fromInput (FromString "invalid")) None)

    test "orders and compares finite and infinite durations" (fun () ->
        toBe (Duration.order (Duration.millis 1.0) (Duration.millis 2.0)) -1
        toBe (Duration.order (Duration.millis 2.0) (Duration.millis 1.0)) 1
        toBe (Duration.order (Duration.millis 2.0) (Duration.millis 2.0)) 0
        toBe (Duration.order (Duration.millis 1.0) (Duration.nanos (bi 2000000))) -1
        toBe (Duration.order (Duration.nanos (bi 1)) (Duration.nanos (bi 2))) -1
        toBe (Duration.order (Duration.nanos (bi 2)) (Duration.nanos (bi 1))) 1
        toBe (Duration.order Duration.infinity Duration.infinity) 0
        toBe (Duration.order Duration.infinity (Duration.millis 1.0)) 1
        toBe (Duration.order (Duration.nanos (bi 1)) Duration.infinity) -1

        toBe (Duration.equivalence (Duration.millis 1.0) (Duration.nanos (bi 1000000))) true
        toBe (Duration.equivalence (Duration.millis 1.0) (Duration.millis 2.0)) false
        toBe (Duration.equivalence Duration.infinity Duration.infinity) true
        toBe (Duration.equivalence Duration.infinity (Duration.millis 1.0)) false
        toBe (Duration.equals (Duration.hours 1.0) (Duration.minutes 60.0)) true
        toBe (Duration.between (Duration.minutes 59.0) (Duration.minutes 61.0) (Duration.hours 1.0)) true
        same (Duration.max (Duration.millis 1.0) (Duration.millis 2.0)) (Duration.millis 2.0)
        same (Duration.min (Duration.minutes 1.0) (Duration.millis 2.0)) (Duration.millis 2.0)
        same
            (Duration.clamp (Duration.minutes 1.0) (Duration.minutes 2.0) (Duration.minutes 1.5))
            (Duration.minutes 1.5))

    test "divides durations safely and unsafely" (fun () ->
        same (Duration.divide (Duration.minutes 1.0) 2.0) (Some(Duration.seconds 30.0))
        same (Duration.divide (Duration.seconds 1.0) 3.0) (Some(Duration.nanos (bi 333333333)))
        same (Duration.divide Duration.zero 2.0) (Some Duration.zero)
        same (Duration.divide (Duration.minutes 1.0) 0.5) (Some(Duration.minutes 2.0))
        same (Duration.divide (Duration.minutes 1.0) 1.5) (Some(Duration.seconds 40.0))
        same (Duration.divide (Duration.nanos (bi 2)) 2.0) (Some(Duration.nanos (bi 1)))
        same (Duration.divide (Duration.nanos (bi 1)) 3.0) (Some Duration.zero)
        same (Duration.divide (Duration.nanos (bi 1)) 0.5) None
        same (Duration.divide Duration.infinity 2.0) (Some Duration.infinity)
        same (Duration.divide Duration.infinity -2.0) (Some Duration.negativeInfinity)
        same (Duration.divide Duration.negativeInfinity -2.0) (Some Duration.infinity)
        same (Duration.divide (Duration.minutes 1.0) 0.0) None
        same (Duration.divide (Duration.minutes 1.0) nan) None
        same (Duration.divide (Duration.minutes 1.0) inf) None

        same (Duration.divideUnsafe (Duration.minutes 1.0) 2.0) (Duration.seconds 30.0)
        same (Duration.divideUnsafe (Duration.seconds 1.0) 3.0) (Duration.nanos (bi 333333333))
        same (Duration.divideUnsafe (Duration.minutes 1.0) 0.5) (Duration.minutes 2.0)
        same (Duration.divideUnsafe (Duration.nanos (bi 2)) 2.0) (Duration.nanos (bi 1))
        toThrow (fun () -> Duration.divideUnsafe (Duration.nanos (bi 1)) 0.5 |> ignore)
        same (Duration.divideUnsafe Duration.infinity 2.0) Duration.infinity
        same (Duration.divideUnsafe (Duration.minutes 1.0) 0.0) Duration.infinity
        same (Duration.divideUnsafe (Duration.minutes 1.0) -0.0) Duration.negativeInfinity
        same (Duration.divideUnsafe (Duration.nanos (bi -1)) 0.0) Duration.negativeInfinity
        same (Duration.divideUnsafe (Duration.nanos (bi -1)) -0.0) Duration.infinity
        same (Duration.divideUnsafe Duration.zero 0.0) Duration.zero
        same (Duration.divideUnsafe (Duration.minutes 1.0) nan) Duration.zero
        same (Duration.divideUnsafe Duration.infinity inf) Duration.zero)

    test "arithmetic handles finite and infinite durations" (fun () ->
        same (Duration.times (Duration.seconds 1.0) 60.0) (Duration.minutes 1.0)
        same (Duration.times (Duration.nanos (bi 2)) 10.0) (Duration.nanos (bi 20))
        same (Duration.times Duration.infinity 60.0) Duration.infinity
        same (Duration.times Duration.infinity -1.0) Duration.negativeInfinity
        same (Duration.times Duration.negativeInfinity -1.0) Duration.infinity
        same (Duration.times Duration.infinity 0.0) Duration.zero

        same (Duration.sum (Duration.seconds 30.0) (Duration.seconds 30.0)) (Duration.minutes 1.0)
        same (Duration.sum (Duration.nanos (bi 30)) (Duration.nanos (bi 30))) (Duration.nanos (bi 60))
        same (Duration.sum Duration.infinity (Duration.seconds 30.0)) Duration.infinity
        same (Duration.sum Duration.infinity Duration.negativeInfinity) Duration.zero
        same (Duration.sum Duration.negativeInfinity Duration.infinity) Duration.zero
        same (Duration.sum Duration.negativeInfinity Duration.negativeInfinity) Duration.negativeInfinity

        same (Duration.subtract (Duration.seconds 30.0) (Duration.seconds 10.0)) (Duration.seconds 20.0)
        same (Duration.subtract (Duration.seconds 30.0) (Duration.seconds 30.0)) Duration.zero
        same (Duration.subtract (Duration.seconds 30.0) (Duration.seconds 40.0)) (Duration.seconds -10.0)
        same (Duration.subtract (Duration.nanos (bi 30)) (Duration.nanos (bi 10))) (Duration.nanos (bi 20))
        same (Duration.subtract Duration.infinity (Duration.seconds 30.0)) Duration.infinity
        same (Duration.subtract (Duration.seconds 30.0) Duration.infinity) Duration.negativeInfinity
        same (Duration.subtract Duration.infinity Duration.infinity) Duration.zero
        same (Duration.subtract Duration.infinity Duration.negativeInfinity) Duration.infinity)

    test "formats parts and scalar conversions" (fun () ->
        toBe (Duration.toString Duration.infinity) "Infinity"
        toBe (Duration.toString Duration.negativeInfinity) "-Infinity"
        toBe (Duration.toString (Duration.nanos (bi 10))) "10 nanos"
        toBe (Duration.toString (Duration.millis 2.0)) "2 millis"
        toBe (Duration.toString (Duration.millis 2.125)) "2125000 nanos"
        toBe (Duration.toString (Duration.seconds 2.0)) "2000 millis"
        toBe (Duration.toString (Duration.seconds 2.5)) "2500 millis"

        toBe (Duration.format Duration.infinity) "Infinity"
        toBe (Duration.format Duration.negativeInfinity) "-Infinity"
        toBe (Duration.format (Duration.minutes 5.0)) "5m"
        toBe (Duration.format (Duration.minutes 5.325)) "5m 19s 500ms"
        toBe (Duration.format (Duration.hours 3.11125)) "3h 6m 40s 500ms"
        toBe (Duration.format (Duration.days 2.25)) "2d 6h"
        toBe (Duration.format (Duration.weeks 1.0)) "7d"
        toBe (Duration.format Duration.zero) "0"

        same
            (Duration.parts Duration.infinity)
            { Days = inf
              Hours = inf
              Minutes = inf
              Seconds = inf
              Millis = inf
              Nanos = inf }
        same
            (Duration.parts (Duration.minutes 5.325))
            { Days = 0.0
              Hours = 0.0
              Minutes = 5.0
              Seconds = 19.0
              Millis = 500.0
              Nanos = 0.0 }
        same
            (Duration.parts (Duration.minutes 3.11125))
            { Days = 0.0
              Hours = 0.0
              Minutes = 3.0
              Seconds = 6.0
              Millis = 675.0
              Nanos = 0.0 }

        toBe (Duration.toMillis (Duration.millis 1.0)) 1.0
        toBe (Duration.toMillis (Duration.nanos (bi 1))) 0.000001
        toBe (Duration.toSeconds (Duration.millis 1.0)) 0.001
        toBe (Duration.toSeconds (Duration.nanos (bi 1))) 1e-9
        toBe (Duration.toMinutes (Duration.millis 60000.0)) 1.0
        toBe (Duration.toHours (Duration.millis 3600000.0)) 1.0
        toBe (Duration.toDays (Duration.millis 86400000.0)) 1.0
        toBe (Duration.toWeeks (Duration.millis 604800000.0)) 1.0)

    test "converts to nanos and hrtime" (fun () ->
        same (Duration.toNanos (Duration.nanos (bi 1))) (Some(bi 1))
        same (Duration.toNanos Duration.infinity) None
        same (Duration.toNanos Duration.negativeInfinity) None
        same (Duration.toNanos (Duration.millis 1.0005)) (Some(BigInteger 1000500))
        same (Duration.toNanos (Duration.millis 100.0)) (Some(BigInteger 100000000))
        same (Duration.toNanosUnsafe (Duration.nanos (bi 1))) (bi 1)
        toThrow (fun () -> Duration.toNanosUnsafe Duration.infinity |> ignore)
        same (Duration.toNanosUnsafe (Duration.millis 1.0005)) (BigInteger 1000500)
        same (Duration.toNanosUnsafe (Duration.millis 0.0000015)) (bi 2)
        same (Duration.toNanosUnsafe (Duration.millis -0.0000015)) (bi -2)
        same (Duration.toHrTime (Duration.millis 1.0)) (0.0, 1000000.0)
        same (Duration.toHrTime (Duration.nanos (bi 1))) (0.0, 1.0)
        same (Duration.toHrTime (Duration.nanos (BigInteger 1000000001))) (1.0, 1.0)
        same (Duration.toHrTime Duration.infinity) (inf, 0.0)
        same (Duration.toHrTime Duration.negativeInfinity) (ninf, 0.0))

    test "negative values keep signs through helpers" (fun () ->
        toBe (Duration.isNegative (Duration.millis -1.0)) true
        toBe (Duration.isNegative (Duration.nanos (bi -1))) true
        toBe (Duration.isNegative Duration.negativeInfinity) true
        toBe (Duration.isPositive (Duration.seconds 5.0)) true
        toBe (Duration.isPositive Duration.infinity) true
        toBe (Duration.isPositive Duration.negativeInfinity) false
        same (Duration.abs (Duration.seconds -5.0)) (Duration.seconds 5.0)
        same (Duration.abs Duration.negativeInfinity) Duration.infinity
        same (Duration.negate (Duration.seconds 5.0)) (Duration.seconds -5.0)
        same (Duration.negate Duration.infinity) Duration.negativeInfinity
        toBe (Duration.format (Duration.seconds -5.0)) "-5s"
        toBe (Duration.order Duration.negativeInfinity (Duration.seconds -5.0)) -1
        toBe (Duration.order (Duration.seconds -3.0) (Duration.seconds -5.0)) 1
        toBe (Duration.equivalence Duration.negativeInfinity Duration.infinity) false
        same
            (Duration.parts Duration.negativeInfinity)
            { Days = ninf
              Hours = ninf
              Minutes = ninf
              Seconds = ninf
              Millis = ninf
              Nanos = ninf })

    test "guards matchers and combiners work" (fun () ->
        toBe (Duration.isDuration (box (Duration.millis 100.0))) true
        toBe (Duration.isDuration (box null)) false
        toBe (Duration.isFinite (Duration.millis 100.0)) true
        toBe (Duration.isFinite Duration.infinity) false
        toBe (Duration.isZero Duration.zero) true
        toBe (Duration.isZero (Duration.nanos (bi 1))) false

        let m (d: Duration) =
            Duration.matchWith
                (fun millis -> sprintf "millis: %d" (int64 millis))
                (fun nanos -> sprintf "nanos: %O" nanos)
                (fun () -> "infinity")
                (fun () -> "-infinity")
                d

        toBe (m (Duration.millis 100.0)) "millis: 100"
        toBe (m (Duration.nanos (bi 10))) "nanos: 10"
        toBe (m Duration.infinity) "infinity"
        toBe (m Duration.negativeInfinity) "-infinity"

        same (Duration.ReducerSum.Combine (Duration.millis 1.0) (Duration.millis 2.0)) (Duration.millis 3.0)
        same (Duration.CombinerMax.Combine (Duration.millis 1.0) (Duration.millis 2.0)) (Duration.millis 2.0)
        same (Duration.CombinerMin.Combine (Duration.millis 1.0) (Duration.millis 2.0)) (Duration.millis 1.0)))
