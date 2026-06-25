module CronSpec

open Effect
open Effect.Vitest

let private mk (values: CronValues) = Cron.make values

let private cv =
    { Seconds = None
      Minutes = []
      Hours = []
      Days = []
      Months = []
      Weekdays = []
      Tz = None }

let private dt s = DateTime.makeUnsafe (InputString s)
let private parseU s = Cron.parseUnsafe s None
let private parseUtc s = Cron.parseUnsafe s (Some "UTC")

let private same actual expected =
    toBe (actual = expected) true

describe "Cron" (fun () ->
    test "constructs and identifies parse errors" (fun () ->
        match Cron.parse "" None with
        | Error e -> toBe (Cron.isCronParseError (box e)) true
        | Ok _ -> failwith "expected failure"

        toBe (Cron.isCronParseError (box (System.Exception "regular error"))) false
        toBe (Cron.isCronParseError (box "not an error")) false

        let error = CronParseError("boom", Some "0 0 * * *")
        toBe (box error :? System.Exception) true
        toBe (Cron.isCronParseError (box error)) true
        toBe error.Tag "CronParseError"
        toBe error.Message "boom"
        same error.Input (Some "0 0 * * *"))

    test "parses five and six segment expressions" (fun () ->
        same
            (Cron.parse "0 4 8-14 * 0-6" None)
            (Ok(
                mk
                    { cv with
                        Minutes = [ 0 ]
                        Hours = [ 4 ]
                        Days = [ 8; 9; 10; 11; 12; 13; 14 ] }
            ))

        same
            (Cron.parse "0 0 1,15 * 3" None)
            (Ok(
                mk
                    { cv with
                        Minutes = [ 0 ]
                        Hours = [ 0 ]
                        Days = [ 1; 15 ]
                        Weekdays = [ 3 ] }
            ))

        same
            (Cron.parse "23 0-20/2 * * *" None)
            (Ok(
                mk
                    { cv with
                        Minutes = [ 23 ]
                        Hours = [ 0; 2; 4; 6; 8; 10; 12; 14; 16; 18; 20 ] }
            ))

        same (parseU "23 0-20/2 * * *").Tz None)

    test "parseUnsafe raises CronParseError for invalid input" (fun () ->
        toThrow (fun () -> Cron.parseUnsafe "" None |> ignore)
        toThrow (fun () -> Cron.parseUnsafe "0 0 4 8-14 * *" (Some "") |> ignore)

        match Cron.parse "" None with
        | Error e ->
            toBe e.Message "Invalid number of segments in cron expression"
            same e.Input (Some "")
        | Ok _ -> failwith "expected failure"

        match Cron.parse "0 0 4 8-14 * *" (Some "") with
        | Error e -> toBe e.Message "Invalid time zone in cron expression"
        | Ok _ -> failwith "expected failure")

    test "matches cron expressions against UTC instants" (fun () ->
        let m expr s = Cron.matches (parseU expr) (dt s)
        toBe (m "5 0 * 8 *" "2024-08-01 00:05:00") true
        toBe (m "5 0 * 8 *" "2024-09-01 00:05:00") false
        toBe (m "5 0 * 8 *" "2024-08-01 01:05:00") false
        toBe (m "15 14 1 * *" "2024-02-01 14:15:00") true
        toBe (m "15 14 1 * *" "2024-02-01 15:15:00") false
        toBe (m "15 14 1 * *" "2024-02-02 14:15:00") false
        toBe (m "23 0-20/2 * * 0" "2024-01-07 00:23:00") true
        toBe (m "23 0-20/2 * * 0" "2024-01-07 03:23:00") false
        toBe (m "23 0-20/2 * * 0" "2024-01-08 00:23:00") false
        toBe (m "5 4 * * SUN" "2024-01-07 04:05:00") true
        toBe (m "5 4 * * SUN" "2024-01-08 04:05:00") false
        toBe (m "5 4 * * SUN" "2025-01-07 04:05:00") false
        toBe (m "5 4 * DEC SUN" "2024-12-01 04:05:00") true
        toBe (m "5 4 * DEC SUN" "2024-12-01 04:06:00") false
        toBe (m "5 4 * DEC SUN" "2024-12-02 04:05:00") false
        toBe (m "42 5 0 * 8 *" "2024-08-01 00:05:42") true
        toBe (m "42 5 0 * 8 *" "2024-09-01 00:05:42") false
        toBe (m "42 5 0 * 8 *" "2024-08-01 01:05:42") false)

    test "finds next scheduled instants" (fun () ->
        let after = dt "2024-01-04 16:21:00"
        let n expr = Cron.next (parseU expr) after
        same (n "5 0 8 2 *") (dt "2024-02-08 00:05:00")
        same (n "15 14 1 * *") (dt "2024-02-01 14:15:00")
        same (n "23 0-20/2 * * 0") (dt "2024-01-07 00:23:00")
        same (n "5 4 * * SUN") (dt "2024-01-07 04:05:00")
        same (n "5 4 * DEC SUN") (dt "2024-12-01 04:05:00")
        same (n "30 5 0 8 2 *") (dt "2024-02-08 00:05:30")

        let cron = parseUtc "0 0 1,16,31 * *"
        same (Cron.next cron (dt "2020-02-18T00:00:00.000Z")) (dt "2020-03-01T00:00:00.000Z")
        same (Cron.next (parseUtc "0 0 */15 * *") (dt "2020-02-18T00:00:00.000Z")) (dt "2020-03-01T00:00:00.000Z")
        same (Cron.next cron (dt "2024-06-20T00:00:00.000Z")) (dt "2024-07-01T00:00:00.000Z"))

    test "finds previous scheduled instants" (fun () ->
        let before = dt "2024-01-04T16:21:00Z"
        let p expr = Cron.prev (parseUtc expr) before
        same (p "5 0 8 2 *") (dt "2023-02-08T00:05:00.000Z")
        same (p "15 14 1 * *") (dt "2024-01-01T14:15:00.000Z")
        same (p "23 0-20/2 * * * 0") (dt "2023-12-31T23:20:23.000Z")
        same (p "5 4 * * SUN") (dt "2023-12-31T04:05:00.000Z")
        same (p "5 4 * DEC SUN") (dt "2023-12-31T04:05:00.000Z")
        same (p "30 5 0 8 2 *") (dt "2023-02-08T00:05:30.000Z")

        let wednesday = dt "2025-10-22T01:00:00.000Z"
        same (Cron.prev (parseUtc "0 1 * * MON") wednesday) (dt "2025-10-20T01:00:00.000Z")
        same (Cron.next (parseUtc "0 1 * * MON") wednesday) (dt "2025-10-27T01:00:00.000Z")
        same (Cron.prev (parseUtc "0 1 * * TUE") wednesday) (dt "2025-10-21T01:00:00.000Z")
        same (Cron.next (parseUtc "0 1 * * TUE") wednesday) (dt "2025-10-28T01:00:00.000Z"))

    test "handles reverse stepping edge cases" (fun () ->
        same
            (Cron.prev (parseUtc "10,30 * * * * *") (dt "2024-01-01T00:00:05.000Z"))
            (dt "2023-12-31T23:59:30.000Z")

        same
            (Cron.prev (parseUtc "0 0 8 5,20 * *") (dt "2024-06-03T00:00:00.000Z"))
            (dt "2024-05-20T08:00:00.000Z")
        same
            (Cron.prev (parseUtc "0 1 * * MON,FRI") (dt "2025-10-19T12:00:00.000Z"))
            (dt "2025-10-17T01:00:00.000Z")
        same
            (Cron.prev (parseUtc "0 0 9 1,15 * MON") (dt "2024-04-02T12:00:00.000Z"))
            (dt "2024-04-01T09:00:00.000Z")
        same
            (Cron.prev (parseUtc "0 */7 8-10 * * *") (dt "2024-01-01T08:01:00.000Z"))
            (dt "2024-01-01T08:00:00.000Z")
        same
            (Cron.prev (parseUtc "0 0 1 1 *") (dt "2024-01-01T00:00:00.000Z"))
            (dt "2023-01-01T00:00:00.000Z")
        same
            (Cron.prev (parseUtc "0 0 31 * *") (dt "2024-03-01T00:00:00.000Z"))
            (dt "2024-01-31T00:00:00.000Z")
        same
            (Cron.prev (parseUtc "0 0 0 * FEB *") (dt "2024-03-31T12:00:00.000Z"))
            (dt "2024-02-29T00:00:00.000Z")
        same
            (Cron.prev (parseUtc "0 0 15 1,4,7,10 *") (dt "2024-05-01T00:00:00.000Z"))
            (dt "2024-04-15T00:00:00.000Z"))

    test "forward and reverse sequences stay aligned" (fun () ->
        let cases =
            [ "5 2 * * 1", "2020-01-01T00:00:01Z", "2021-01-01T00:00:01Z"
              "0 12 1 * *", "2020-01-01T00:00:01Z", "2021-01-01T00:00:01Z"
              "10,30 * * * * *", "2024-01-01T00:00:00Z", "2024-01-02T00:00:00Z" ]

        let gatherForward cron (lower: DateTime) (upper: DateTime) =
            let rec loop (cur: DateTime) acc =
                let n = Cron.next cron cur

                if n.EpochMillis >= upper.EpochMillis then
                    List.rev acc
                else
                    loop n (n :: acc)

            loop lower []

        let gatherReverse cron (lower: DateTime) (upper: DateTime) =
            let rec loop (cur: DateTime) acc =
                let p = Cron.prev cron cur

                if p.EpochMillis <= lower.EpochMillis then
                    acc
                else
                    loop p (p :: acc)

            loop upper []

        for expr, lowerStr, upperStr in cases do
            let lower = dt lowerStr
            let upper = dt upperStr
            let cron = parseUtc expr
            same (gatherForward cron lower upper) (gatherReverse cron lower upper))

    test "sequence equal and leap-year behavior" (fun () ->
        let gen =
            Cron.sequence (parseU "23 0-20/2 * * 0") (dt "2024-01-01 00:00:00")
            |> Seq.take 5
            |> Seq.toList

        same
            gen
            [ dt "2024-01-07 00:23:00"
              dt "2024-01-07 02:23:00"
              dt "2024-01-07 04:23:00"
              dt "2024-01-07 06:23:00"
              dt "2024-01-07 08:23:00" ]

        let cron = parseU "23 0-20/2 * * 0"
        toBe (Cron.equals cron cron) true
        toBe (Cron.equals cron (parseU "23 0-20/2 * * 0")) true
        toBe (Cron.equals cron (parseU "23 0-20/2 * * 1")) false
        toBe (Cron.equals cron (parseU "23 0-20/2 * * 0-6")) false
        toBe (Cron.equals cron (parseU "23 0-20/2 1 * 0")) false

        let m s = Cron.matches (parseU "0 0 29 2 *") (dt s)
        toBe (m "2024-02-29 00:00:00") true
        toBe (m "2025-03-01 00:00:00") false
        toBe (m "2028-02-29 00:00:00") true
        same (Cron.next (parseU "0 0 29 2 *") (dt "2024-01-01 00:00:00")) (dt "2024-02-29 00:00:00")
        same (Cron.next (parseU "0 0 29 2 *") (dt "2025-01-01 00:00:00")) (dt "2028-02-29 00:00:00")))
