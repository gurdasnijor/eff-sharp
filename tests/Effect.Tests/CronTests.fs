module Effect.Tests.CronTests

open Xunit
open Effect

// Ported from repos/effect-smol/packages/effect/test/Cron.test.ts
//
// Only the UTC-portable subset is ported (see Cron.fs doc comment). Named-zone
// and DST test cases (Europe/Berlin, offset zones, makeZonedFromString, etc.)
// are skipped because the zoned/DST DateTime surface is out of scope for this
// slice. UTC `parseUnsafe(expr, Some "UTC")` is treated as plain UTC stepping.

let private mk (values: CronValues) = Cron.make values
let private cv =
    { Seconds = None; Minutes = []; Hours = []; Days = []; Months = []; Weekdays = []; Tz = None }

let private dt s = DateTime.makeUnsafe (InputString s)
let private parseU s = Cron.parseUnsafe s None
let private parseUtc s = Cron.parseUnsafe s (Some "UTC")

[<Fact>]
let ``isCronParseError`` () =
    match Cron.parse "" None with
    | Error e -> Assert.True(Cron.isCronParseError (box e))
    | Ok _ -> Assert.Fail "expected failure"
    Assert.False(Cron.isCronParseError (box (System.Exception "regular error")))
    Assert.False(Cron.isCronParseError (box "not an error"))

[<Fact>]
let ``CronParseError constructor`` () =
    let error = CronParseError("boom", Some "0 0 * * *")
    Assert.True((box error) :? System.Exception)
    Assert.True(Cron.isCronParseError (box error))
    Assert.Equal("CronParseError", error.Tag)
    Assert.Equal("boom", error.Message)
    Assert.Equal(Some "0 0 * * *", error.Input)

[<Fact>]
let ``parse`` () =
    Assert.Equal(
        Ok(mk { cv with Minutes = [ 0 ]; Hours = [ 4 ]; Days = [ 8; 9; 10; 11; 12; 13; 14 ]; Months = []; Weekdays = [] }),
        Cron.parse "0 4 8-14 * 0-6" None)
    Assert.Equal(
        Ok(mk { cv with Minutes = [ 0 ]; Hours = [ 0 ]; Days = [ 1; 15 ]; Months = []; Weekdays = [ 3 ] }),
        Cron.parse "0 0 1,15 * 3" None)
    Assert.Equal(
        Ok(mk { cv with Minutes = [ 23 ]; Hours = [ 0; 2; 4; 6; 8; 10; 12; 14; 16; 18; 20 ]; Days = []; Months = []; Weekdays = [] }),
        Cron.parse "23 0-20/2 * * *" None)
    Assert.True((parseU "23 0-20/2 * * *").Tz.IsNone)

[<Fact>]
let ``parseUnsafe errors`` () =
    let e1 = Assert.Throws<CronParseError>(fun () -> Cron.parseUnsafe "" None |> ignore)
    Assert.Equal("Invalid number of segments in cron expression", e1.Message)
    Assert.Equal(Some "", e1.Input)
    let e2 = Assert.Throws<CronParseError>(fun () -> Cron.parseUnsafe "0 0 4 8-14 * *" (Some "") |> ignore)
    Assert.Equal("Invalid time zone in cron expression", e2.Message)

[<Fact>]
let ``match`` () =
    let m expr s = Cron.matches (parseU expr) (dt s)
    Assert.True(m "5 0 * 8 *" "2024-08-01 00:05:00")
    Assert.False(m "5 0 * 8 *" "2024-09-01 00:05:00")
    Assert.False(m "5 0 * 8 *" "2024-08-01 01:05:00")
    Assert.True(m "15 14 1 * *" "2024-02-01 14:15:00")
    Assert.False(m "15 14 1 * *" "2024-02-01 15:15:00")
    Assert.False(m "15 14 1 * *" "2024-02-02 14:15:00")
    Assert.True(m "23 0-20/2 * * 0" "2024-01-07 00:23:00")
    Assert.False(m "23 0-20/2 * * 0" "2024-01-07 03:23:00")
    Assert.False(m "23 0-20/2 * * 0" "2024-01-08 00:23:00")
    Assert.True(m "5 4 * * SUN" "2024-01-07 04:05:00")
    Assert.False(m "5 4 * * SUN" "2024-01-08 04:05:00")
    Assert.False(m "5 4 * * SUN" "2025-01-07 04:05:00")
    Assert.True(m "5 4 * DEC SUN" "2024-12-01 04:05:00")
    Assert.False(m "5 4 * DEC SUN" "2024-12-01 04:06:00")
    Assert.False(m "5 4 * DEC SUN" "2024-12-02 04:05:00")
    Assert.True(m "42 5 0 * 8 *" "2024-08-01 00:05:42")
    Assert.False(m "42 5 0 * 8 *" "2024-09-01 00:05:42")
    Assert.False(m "42 5 0 * 8 *" "2024-08-01 01:05:42")

[<Fact>]
let ``next`` () =
    let after = dt "2024-01-04 16:21:00"
    let n expr = Cron.next (parseU expr) after
    Assert.Equal(dt "2024-02-08 00:05:00", n "5 0 8 2 *")
    Assert.Equal(dt "2024-02-01 14:15:00", n "15 14 1 * *")
    Assert.Equal(dt "2024-01-07 00:23:00", n "23 0-20/2 * * 0")
    Assert.Equal(dt "2024-01-07 04:05:00", n "5 4 * * SUN")
    Assert.Equal(dt "2024-12-01 04:05:00", n "5 4 * DEC SUN")
    Assert.Equal(dt "2024-02-08 00:05:30", n "30 5 0 8 2 *")

[<Fact>]
let ``next does not skip earlier days when the upcoming day is missing`` () =
    let cron = parseUtc "0 0 1,16,31 * *"
    Assert.Equal(dt "2020-03-01T00:00:00.000Z", Cron.next cron (dt "2020-02-18T00:00:00.000Z"))
    Assert.Equal(dt "2020-03-01T00:00:00.000Z", Cron.next (parseUtc "0 0 */15 * *") (dt "2020-02-18T00:00:00.000Z"))
    Assert.Equal(dt "2024-07-01T00:00:00.000Z", Cron.next cron (dt "2024-06-20T00:00:00.000Z"))

[<Fact>]
let ``prev`` () =
    let before = dt "2024-01-04T16:21:00Z"
    let p expr = Cron.prev (parseUtc expr) before
    Assert.Equal(dt "2023-02-08T00:05:00.000Z", p "5 0 8 2 *")
    Assert.Equal(dt "2024-01-01T14:15:00.000Z", p "15 14 1 * *")
    Assert.Equal(dt "2023-12-31T23:20:23.000Z", p "23 0-20/2 * * * 0")
    Assert.Equal(dt "2023-12-31T04:05:00.000Z", p "5 4 * * SUN")
    Assert.Equal(dt "2023-12-31T04:05:00.000Z", p "5 4 * DEC SUN")
    Assert.Equal(dt "2023-02-08T00:05:30.000Z", p "30 5 0 8 2 *")

    let wednesday = dt "2025-10-22T01:00:00.000Z"
    Assert.Equal(dt "2025-10-20T01:00:00.000Z", Cron.prev (parseUtc "0 1 * * MON") wednesday)
    Assert.Equal(dt "2025-10-27T01:00:00.000Z", Cron.next (parseUtc "0 1 * * MON") wednesday)
    Assert.Equal(dt "2025-10-21T01:00:00.000Z", Cron.prev (parseUtc "0 1 * * TUE") wednesday)
    Assert.Equal(dt "2025-10-28T01:00:00.000Z", Cron.next (parseUtc "0 1 * * TUE") wednesday)

[<Fact>]
let ``returns the latest second when rolling back a minute`` () =
    let expr = parseUtc "10,30 * * * * *"
    Assert.Equal(dt "2023-12-31T23:59:30.000Z", Cron.prev expr (dt "2024-01-01T00:00:05.000Z"))

[<Fact>]
let ``forward and reverse sequences stay aligned`` () =
    let cases =
        [ "5 2 * * 1", "2020-01-01T00:00:01Z", "2021-01-01T00:00:01Z"
          "0 12 1 * *", "2020-01-01T00:00:01Z", "2021-01-01T00:00:01Z"
          "10,30 * * * * *", "2024-01-01T00:00:00Z", "2024-01-02T00:00:00Z" ]

    let gatherForward cron (lower: DateTime) (upper: DateTime) =
        let rec loop (cur: DateTime) acc =
            let n = Cron.next cron cur
            if n.EpochMillis >= upper.EpochMillis then List.rev acc else loop n (n :: acc)
        loop lower []

    let gatherReverse cron (lower: DateTime) (upper: DateTime) =
        let rec loop (cur: DateTime) acc =
            let p = Cron.prev cron cur
            if p.EpochMillis <= lower.EpochMillis then acc else loop p (p :: acc)
        loop upper []

    for expr, lowerStr, upperStr in cases do
        let lower = dt lowerStr
        let upper = dt upperStr
        let cron = parseUtc expr
        Assert.Equal<DateTime list>(gatherForward cron lower upper, gatherReverse cron lower upper)

[<Fact>]
let ``prev prefers the latest matching day within the previous month`` () =
    let cron = parseUtc "0 0 8 5,20 * *"
    Assert.Equal(dt "2024-05-20T08:00:00.000Z", Cron.prev cron (dt "2024-06-03T00:00:00.000Z"))

[<Fact>]
let ``prev wraps weekday using the last allowed value`` () =
    let cron = parseUtc "0 1 * * MON,FRI"
    Assert.Equal(dt "2025-10-17T01:00:00.000Z", Cron.prev cron (dt "2025-10-19T12:00:00.000Z"))

[<Fact>]
let ``prev respects combined day-of-month and weekday constraints`` () =
    let cron = parseUtc "0 0 9 1,15 * MON"
    Assert.Equal(dt "2024-04-01T09:00:00.000Z", Cron.prev cron (dt "2024-04-02T12:00:00.000Z"))

[<Fact>]
let ``prev handles step expressions across day boundary`` () =
    let cron = parseUtc "0 */7 8-10 * * *"
    Assert.Equal(dt "2024-01-01T08:00:00.000Z", Cron.prev cron (dt "2024-01-01T08:01:00.000Z"))

[<Fact>]
let ``prev wraps across year boundary`` () =
    let cron = parseUtc "0 0 1 1 *"
    Assert.Equal(dt "2023-01-01T00:00:00.000Z", Cron.prev cron (dt "2024-01-01T00:00:00.000Z"))

[<Fact>]
let ``prev handles day 31 skipping months without it`` () =
    let cron = parseUtc "0 0 31 * *"
    Assert.Equal(dt "2024-01-31T00:00:00.000Z", Cron.prev cron (dt "2024-03-01T00:00:00.000Z"))

[<Fact>]
let ``prev clamps to the last valid day when rolling back a month`` () =
    let cron = parseUtc "0 0 0 * FEB *"
    Assert.Equal(dt "2024-02-29T00:00:00.000Z", Cron.prev cron (dt "2024-03-31T12:00:00.000Z"))

[<Fact>]
let ``prev with multiple months specified`` () =
    let cron = parseUtc "0 0 15 1,4,7,10 *"
    Assert.Equal(dt "2024-04-15T00:00:00.000Z", Cron.prev cron (dt "2024-05-01T00:00:00.000Z"))

[<Fact>]
let ``sequence`` () =
    let gen = Cron.sequence (parseU "23 0-20/2 * * 0") (dt "2024-01-01 00:00:00") |> Seq.take 5 |> Seq.toList
    Assert.Equal<DateTime list>(
        [ dt "2024-01-07 00:23:00"; dt "2024-01-07 02:23:00"; dt "2024-01-07 04:23:00"; dt "2024-01-07 06:23:00"; dt "2024-01-07 08:23:00" ],
        gen)

[<Fact>]
let ``equal`` () =
    let cron = parseU "23 0-20/2 * * 0"
    Assert.True(Cron.equals cron cron)
    Assert.True(Cron.equals cron (parseU "23 0-20/2 * * 0"))
    Assert.False(Cron.equals cron (parseU "23 0-20/2 * * 1"))
    Assert.False(Cron.equals cron (parseU "23 0-20/2 * * 0-6"))
    Assert.False(Cron.equals cron (parseU "23 0-20/2 1 * 0"))

[<Fact>]
let ``handles leap years`` () =
    let m s = Cron.matches (parseU "0 0 29 2 *") (dt s)
    Assert.True(m "2024-02-29 00:00:00")
    Assert.False(m "2025-03-01 00:00:00")
    Assert.True(m "2028-02-29 00:00:00")
    Assert.Equal(dt "2024-02-29 00:00:00", Cron.next (parseU "0 0 29 2 *") (dt "2024-01-01 00:00:00"))
    Assert.Equal(dt "2028-02-29 00:00:00", Cron.next (parseU "0 0 29 2 *") (dt "2025-01-01 00:00:00"))
