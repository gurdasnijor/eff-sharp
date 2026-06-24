module Effect.Tests.DateTimeTests

open Xunit
open Effect

// Ported from repos/effect-smol/packages/effect/test/DateTime.test.ts
//
// Only the UTC-core, non-Effect, non-zoned, non-Intl test cases are ported here
// (see DateTime.fs doc comment for the skipped surface: Zoned/timezones/DST,
// the Effect-runtime `now`/layers/TestClock, and Intl `format`).

let private S s = DateTime.makeUnsafe (InputString s)
let private parts p = DateTime.makeUnsafe (InputParts p)
let private E = DateTimeParts.Empty
let private M = DateTimeMath.Zero

// --- add / subtract ---

[<Fact>]
let ``add clamps to the last valid day when changing months`` () =
    let jan = parts { E with Year = Some 2023; Month = Some 1; Day = Some 31 }
    let feb = DateTime.add { M with Months = 1 } jan
    Assert.Equal("2023-02-28T00:00:00.000Z", DateTime.toJSON feb)

    let mar = parts { E with Year = Some 2023; Month = Some 3; Day = Some 31 }
    let feb2 = DateTime.subtract { M with Months = 1 } mar
    Assert.Equal("2023-02-28T00:00:00.000Z", DateTime.toJSON feb2)

[<Fact>]
let ``add days`` () =
    let d = S "2024-03-15T12:00:00.000Z"
    Assert.Equal("2024-03-16T12:00:00.000Z", DateTime.toJSON (DateTime.add { M with Days = 1 } d))

[<Fact>]
let ``add leap year rolls Feb 29 to Feb 28`` () =
    let now = S "2024-02-29T00:00:00.000Z"
    let future = DateTime.add { M with Years = 1 } now
    Assert.Equal("2025-02-28T00:00:00.000Z", DateTime.formatIso future)

// --- endOf ---

[<Fact>]
let ``endOf month`` () =
    let mar = S "2024-03-15T12:00:00.000Z"
    Assert.Equal("2024-03-31T23:59:59.999Z", DateTime.toJSON (DateTime.endOf UnitMonth 0 mar))

[<Fact>]
let ``endOf feb leap year`` () =
    let feb = S "2024-02-15T12:00:00.000Z"
    Assert.Equal("2024-02-29T23:59:59.999Z", DateTime.toJSON (DateTime.endOf UnitMonth 0 feb))

[<Fact>]
let ``endOf week`` () =
    let start = S "2024-03-15T12:00:00.000Z"
    let endD = DateTime.endOf UnitWeek 0 start
    Assert.Equal("2024-03-16T23:59:59.999Z", DateTime.toJSON endD)
    Assert.Equal(6, DateTime.getPartUtc PartWeekDay endD)

[<Fact>]
let ``endOf week last day`` () =
    let start = S "2024-03-16T12:00:00.000Z"
    Assert.Equal("2024-03-16T23:59:59.999Z", DateTime.toJSON (DateTime.endOf UnitWeek 0 start))

[<Fact>]
let ``endOf week with options`` () =
    let start = S "2024-03-15T12:00:00.000Z"
    Assert.Equal("2024-03-17T23:59:59.999Z", DateTime.toJSON (DateTime.endOf UnitWeek 1 start))

// --- startOf ---

[<Fact>]
let ``startOf month`` () =
    let mar = S "2024-03-15T12:00:00.000Z"
    Assert.Equal("2024-03-01T00:00:00.000Z", DateTime.toJSON (DateTime.startOf UnitMonth 0 mar))

[<Fact>]
let ``startOf month duplicated`` () =
    let mar = S "2024-03-15T12:00:00.000Z"
    let endD = mar |> DateTime.startOf UnitMonth 0 |> DateTime.startOf UnitMonth 0
    Assert.Equal("2024-03-01T00:00:00.000Z", DateTime.toJSON endD)

[<Fact>]
let ``startOf feb leap year`` () =
    let feb = S "2024-02-15T12:00:00.000Z"
    Assert.Equal("2024-02-01T00:00:00.000Z", DateTime.toJSON (DateTime.startOf UnitMonth 0 feb))

[<Fact>]
let ``startOf week`` () =
    let start = S "2024-03-15T12:00:00.000Z"
    let endD = DateTime.startOf UnitWeek 0 start
    Assert.Equal("2024-03-10T00:00:00.000Z", DateTime.toJSON endD)
    Assert.Equal(0, DateTime.getPartUtc PartWeekDay endD)

[<Fact>]
let ``startOf week first day`` () =
    let start = S "2024-03-10T12:00:00.000Z"
    Assert.Equal("2024-03-10T00:00:00.000Z", DateTime.toJSON (DateTime.startOf UnitWeek 0 start))

[<Fact>]
let ``startOf week with options`` () =
    let start = S "2024-03-15T12:00:00.000Z"
    Assert.Equal("2024-03-11T00:00:00.000Z", DateTime.toJSON (DateTime.startOf UnitWeek 1 start))

// --- nearest ---

[<Fact>]
let ``nearest month up`` () =
    let mar = S "2024-03-16T12:00:00.000Z"
    Assert.Equal("2024-04-01T00:00:00.000Z", DateTime.toJSON (DateTime.nearest UnitMonth 0 mar))

[<Fact>]
let ``nearest month down`` () =
    let mar = S "2024-03-16T11:00:00.000Z"
    Assert.Equal("2024-03-01T00:00:00.000Z", DateTime.toJSON (DateTime.nearest UnitMonth 0 mar))

[<Fact>]
let ``nearest second up`` () =
    let mar = S "2024-03-20T12:00:00.500Z"
    Assert.Equal("2024-03-20T12:00:01.000Z", DateTime.toJSON (DateTime.nearest UnitSecond 0 mar))

[<Fact>]
let ``nearest second down`` () =
    let mar = S "2024-03-20T12:00:00.400Z"
    Assert.Equal("2024-03-20T12:00:00.000Z", DateTime.toJSON (DateTime.nearest UnitSecond 0 mar))

// --- fromParts ---

[<Fact>]
let ``fromParts partial`` () =
    let date = parts { E with Year = Some 2024; Month = Some 12; Day = Some 25 }
    Assert.Equal("2024-12-25T00:00:00.000Z", DateTime.toJSON date)

[<Fact>]
let ``fromParts month is set correctly`` () =
    let date = parts { E with Year = Some 2024 }
    Assert.Equal("2024-01-01T00:00:00.000Z", DateTime.toJSON date)

// --- setPartsUtc / setParts ---

[<Fact>]
let ``setPartsUtc partial`` () =
    let date = parts { E with Year = Some 2024; Month = Some 12; Day = Some 25 }
    Assert.Equal("2024-12-25T00:00:00.000Z", DateTime.toJSON date)
    let updated = DateTime.setPartsUtc { E with Year = Some 2023; Month = Some 1 } date
    Assert.Equal("2023-01-25T00:00:00.000Z", DateTime.toJSON updated)

[<Fact>]
let ``setParts partial`` () =
    let date = parts { E with Year = Some 2024; Month = Some 12; Day = Some 25 }
    let updated = DateTime.setParts { E with Year = Some 2023; Month = Some 1 } date
    Assert.Equal("2023-01-25T00:00:00.000Z", DateTime.toJSON updated)

// --- formatIso ---

[<Fact>]
let ``formatIso full`` () =
    let now = S "2024-03-15T12:00:00.000Z"
    Assert.Equal("2024-03-15T12:00:00.000Z", DateTime.formatIso now)

// --- makeUnsafe ---

[<Fact>]
let ``makeUnsafe treats strings without zone info as UTC`` () =
    Assert.Equal("2024-01-01T01:00:00.000Z", DateTime.toJSON (S "2024-01-01 01:00:00"))

[<Fact>]
let ``makeUnsafe does not append Z to strings ending with Z`` () =
    Assert.Equal("2026-01-27T17:14:06.000Z", DateTime.toJSON (S "2026-01-27T17:14:06.000Z"))

[<Fact>]
let ``makeUnsafe does not append Z to strings with explicit offset`` () =
    Assert.Equal("2020-02-01T00:17:00.000Z", DateTime.toJSON (S "2020-02-01T11:17:00+1100"))

[<Fact>]
let ``makeUnsafe does not append Z to strings containing GMT`` () =
    Assert.Equal("2026-01-27T17:14:06.000Z", DateTime.toJSON (S "Tue, 27 Jan 2026 17:14:06 GMT"))

// --- misc ---

[<Fact>]
let ``parts equality`` () =
    let d1 = S "2025-01-01"
    let d2 = S "2025-01-01"
    Assert.Equal(d1, d2)
    DateTime.toPartsUtc d2 |> ignore
    Assert.Equal(d1, d2)

[<Fact>]
let ``toUtc with a Utc`` () =
    let dt = S "2024-01-01T01:00:00Z"
    Assert.Equal("2024-01-01T01:00:00.000Z", DateTime.toJSON dt)

[<Fact>]
let ``distance is one day`` () =
    let now = S "2024-03-15T00:00:00.000Z"
    let tomorrow = DateTime.add { M with Days = 1 } now
    Assert.True(Duration.equals (DateTime.distance now tomorrow) (Duration.fromInputUnsafe (FromString "1 day")))

[<Fact>]
let ``Order and comparisons`` () =
    let a = S "2024-01-01T00:00:00.000Z"
    let b = S "2024-01-02T00:00:00.000Z"
    Assert.Equal(-1, DateTime.order a b)
    Assert.Equal(1, DateTime.order b a)
    Assert.Equal(0, DateTime.order a a)
    Assert.True(DateTime.isLessThan a b)
    Assert.True(DateTime.equivalence a a)
    Assert.Equal(a, DateTime.min a b)
    Assert.Equal(b, DateTime.max a b)
