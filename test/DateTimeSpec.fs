module DateTimeSpec

open Effect
open Effect.Vitest

let private S s = DateTime.makeUnsafe (InputString s)
let private parts p = DateTime.makeUnsafe (InputParts p)
let private E = DateTimeParts.Empty
let private M = DateTimeMath.Zero

let private same actual expected =
    toBe (actual = expected) true

describe "DateTime" (fun () ->
    test "adds and subtracts calendar parts with month-end clamping" (fun () ->
        let jan31 =
            parts
                { E with
                    Year = Some 2023
                    Month = Some 1
                    Day = Some 31 }

        toBe (DateTime.toJSON (DateTime.add { M with Months = 1 } jan31)) "2023-02-28T00:00:00.000Z"

        let mar31 =
            parts
                { E with
                    Year = Some 2023
                    Month = Some 3
                    Day = Some 31 }

        toBe (DateTime.toJSON (DateTime.subtract { M with Months = 1 } mar31)) "2023-02-28T00:00:00.000Z"
        toBe (DateTime.toJSON (DateTime.add { M with Days = 1 } (S "2024-03-15T12:00:00.000Z"))) "2024-03-16T12:00:00.000Z"
        toBe (DateTime.formatIso (DateTime.add { M with Years = 1 } (S "2024-02-29T00:00:00.000Z"))) "2025-02-28T00:00:00.000Z")

    test "startOf and endOf handle month and week boundaries" (fun () ->
        toBe (DateTime.toJSON (DateTime.endOf UnitMonth 0 (S "2024-03-15T12:00:00.000Z"))) "2024-03-31T23:59:59.999Z"
        toBe (DateTime.toJSON (DateTime.endOf UnitMonth 0 (S "2024-02-15T12:00:00.000Z"))) "2024-02-29T23:59:59.999Z"

        let weekEnd = DateTime.endOf UnitWeek 0 (S "2024-03-15T12:00:00.000Z")
        toBe (DateTime.toJSON weekEnd) "2024-03-16T23:59:59.999Z"
        toBe (DateTime.getPartUtc PartWeekDay weekEnd) 6
        toBe (DateTime.toJSON (DateTime.endOf UnitWeek 0 (S "2024-03-16T12:00:00.000Z"))) "2024-03-16T23:59:59.999Z"
        toBe (DateTime.toJSON (DateTime.endOf UnitWeek 1 (S "2024-03-15T12:00:00.000Z"))) "2024-03-17T23:59:59.999Z"

        let mar = S "2024-03-15T12:00:00.000Z"
        toBe (DateTime.toJSON (DateTime.startOf UnitMonth 0 mar)) "2024-03-01T00:00:00.000Z"
        toBe (DateTime.toJSON (mar |> DateTime.startOf UnitMonth 0 |> DateTime.startOf UnitMonth 0)) "2024-03-01T00:00:00.000Z"
        toBe (DateTime.toJSON (DateTime.startOf UnitMonth 0 (S "2024-02-15T12:00:00.000Z"))) "2024-02-01T00:00:00.000Z"

        let weekStart = DateTime.startOf UnitWeek 0 mar
        toBe (DateTime.toJSON weekStart) "2024-03-10T00:00:00.000Z"
        toBe (DateTime.getPartUtc PartWeekDay weekStart) 0
        toBe (DateTime.toJSON (DateTime.startOf UnitWeek 0 (S "2024-03-10T12:00:00.000Z"))) "2024-03-10T00:00:00.000Z"
        toBe (DateTime.toJSON (DateTime.startOf UnitWeek 1 mar)) "2024-03-11T00:00:00.000Z")

    test "nearest rounds to the closest unit boundary" (fun () ->
        toBe (DateTime.toJSON (DateTime.nearest UnitMonth 0 (S "2024-03-16T12:00:00.000Z"))) "2024-04-01T00:00:00.000Z"
        toBe (DateTime.toJSON (DateTime.nearest UnitMonth 0 (S "2024-03-16T11:00:00.000Z"))) "2024-03-01T00:00:00.000Z"
        toBe (DateTime.toJSON (DateTime.nearest UnitSecond 0 (S "2024-03-20T12:00:00.500Z"))) "2024-03-20T12:00:01.000Z"
        toBe (DateTime.toJSON (DateTime.nearest UnitSecond 0 (S "2024-03-20T12:00:00.400Z"))) "2024-03-20T12:00:00.000Z")

    test "constructs and updates from partial UTC parts" (fun () ->
        let christmas =
            parts
                { E with
                    Year = Some 2024
                    Month = Some 12
                    Day = Some 25 }

        toBe (DateTime.toJSON christmas) "2024-12-25T00:00:00.000Z"
        toBe (DateTime.toJSON (parts { E with Year = Some 2024 })) "2024-01-01T00:00:00.000Z"

        let update =
            { E with
                Year = Some 2023
                Month = Some 1 }

        toBe (DateTime.toJSON (DateTime.setPartsUtc update christmas)) "2023-01-25T00:00:00.000Z"
        toBe (DateTime.toJSON (DateTime.setParts update christmas)) "2023-01-25T00:00:00.000Z")

    test "formats and parses UTC, offset, and GMT inputs" (fun () ->
        toBe (DateTime.formatIso (S "2024-03-15T12:00:00.000Z")) "2024-03-15T12:00:00.000Z"
        toBe (DateTime.toJSON (S "2024-01-01 01:00:00")) "2024-01-01T01:00:00.000Z"
        toBe (DateTime.toJSON (S "2026-01-27T17:14:06.000Z")) "2026-01-27T17:14:06.000Z"
        toBe (DateTime.toJSON (S "2020-02-01T11:17:00+1100")) "2020-02-01T00:17:00.000Z"
        toBe (DateTime.toJSON (S "Tue, 27 Jan 2026 17:14:06 GMT")) "2026-01-27T17:14:06.000Z")

    test "compares, measures distance, and preserves value equality" (fun () ->
        let d1 = S "2025-01-01"
        let d2 = S "2025-01-01"
        same d1 d2
        DateTime.toPartsUtc d2 |> ignore
        same d1 d2

        let dt = S "2024-01-01T01:00:00Z"
        toBe (DateTime.toJSON (DateTime.toUtc dt)) "2024-01-01T01:00:00.000Z"

        let now = S "2024-03-15T00:00:00.000Z"
        let tomorrow = DateTime.add { M with Days = 1 } now
        toBe (Duration.equals (DateTime.distance now tomorrow) (Duration.fromInputUnsafe (FromString "1 day"))) true

        let a = S "2024-01-01T00:00:00.000Z"
        let b = S "2024-01-02T00:00:00.000Z"
        toBe (DateTime.order a b) -1
        toBe (DateTime.order b a) 1
        toBe (DateTime.order a a) 0
        toBe (DateTime.isLessThan a b) true
        toBe (DateTime.equivalence a a) true
        same (DateTime.min a b) a
        same (DateTime.max a b) b))
