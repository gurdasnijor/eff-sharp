namespace Effect

open System.Globalization

/// Port of repos/effect-smol/packages/effect/src/Cron.ts.
///
/// SCOPE: the recurring-schedule core, evaluated in **UTC**. Field constraints
/// (seconds, minutes, hours, days, months, weekdays) are parsed and matched, and
/// `next`/`prev`/`sequence` step a UTC instant exactly like upstream (reusing
/// `DateTime`'s internal `JsDate` field arithmetic).
///
/// Deliberately SKIPPED (per CONVENTIONS.md "too large → core + skips"):
///   * Named IANA time zones and DST disambiguation in `next`/`prev` — upstream's
///     `adjustDst` is a no-op for UTC, which is all this port supports. The `tz`
///     argument is accepted and validated, but only UTC/none semantics are
///     honoured during stepping (DST/offset-zone test cases are not ported).
///   * JS-runtime machinery (`TypeId` brand, `pipe`, `Hash`/`Equal` symbols,
///     `toJSON`). `Equivalence`/`equals` lower to plain functions (sets only).
///
/// `CronParseError` is a `System.Exception` subclass (so it "is an Error").
/// `parse` returns an F# `Result<Cron, CronParseError>`.

/// A parsed cron error.
type CronParseError(message: string, input: string option) =
    inherit System.Exception(message)
    member _.Tag = "CronParseError"
    member _.Input = input

/// A recurring calendar schedule. Empty sets mean "match all" for that field.
/// Months are 1-based (1-12); weekdays 0-6 (0=Sunday).
type Cron =
    { Seconds: Set<int>
      Minutes: Set<int>
      Hours: Set<int>
      Days: Set<int>
      Months: Set<int>
      Weekdays: Set<int>
      Tz: string option }

/// Explicit field constraints for `Cron.make`.
type CronValues =
    { Seconds: int list option
      Minutes: int list
      Hours: int list
      Days: int list
      Months: int list
      Weekdays: int list
      Tz: string option }

[<RequireQualifiedAccess>]
module Cron =

    let private inv = CultureInfo.InvariantCulture

    // --- guards ---

    let isCron (u: obj) : bool =
        match u with
        | :? Cron -> true
        | _ -> false

    let isCronParseError (u: obj) : bool =
        match u with
        | :? CronParseError -> true
        | _ -> false

    // --- construction ---

    let make (values: CronValues) : Cron =
        { Seconds = Set.ofList (defaultArg values.Seconds [ 0 ])
          Minutes = Set.ofList values.Minutes
          Hours = Set.ofList values.Hours
          Days = Set.ofList values.Days
          Months = Set.ofList values.Months
          Weekdays = Set.ofList values.Weekdays
          Tz = values.Tz }

    // --- equality (field restrictions only, ignoring tz) ---

    let equivalence (self: Cron) (that: Cron) : bool =
        self.Seconds = that.Seconds
        && self.Minutes = that.Minutes
        && self.Hours = that.Hours
        && self.Days = that.Days
        && self.Months = that.Months
        && self.Weekdays = that.Weekdays

    let equals (self: Cron) (that: Cron) : bool = equivalence self that

    // --- parsing ---

    let private monthAliases =
        dict [ "jan", 1; "feb", 2; "mar", 3; "apr", 4; "may", 5; "jun", 6
               "jul", 7; "aug", 8; "sep", 9; "oct", 10; "nov", 11; "dec", 12 ]

    let private weekdayAliases =
        dict [ "sun", 0; "mon", 1; "tue", 2; "wed", 3; "thu", 4; "fri", 5; "sat", 6 ]

    type private SegOptions =
        { Min: int
          Max: int
          Aliases: System.Collections.Generic.IDictionary<string, int> }

    let private noAliases = dict []
    let private secondOptions = { Min = 0; Max = 59; Aliases = noAliases }
    let private minuteOptions = { Min = 0; Max = 59; Aliases = noAliases }
    let private hourOptions = { Min = 0; Max = 23; Aliases = noAliases }
    let private dayOptions = { Min = 1; Max = 31; Aliases = noAliases }
    let private monthOptions = { Min = 1; Max = 12; Aliases = monthAliases }
    let private weekdayOptions = { Min = 0; Max = 6; Aliases = weekdayAliases }

    let private parseNum (s: string) : float =
        match System.Double.TryParse(s, NumberStyles.Any, inv) with
        | true, v -> v
        | _ -> nan

    let private aliasOrValue (field: string) (aliases: System.Collections.Generic.IDictionary<string, int>) : float =
        match aliases.TryGetValue(field.ToLowerInvariant()) with
        | true, v -> float v
        | _ -> parseNum field

    let private splitStep (input: string) : string * float option =
        match input.IndexOf '/' with
        | -1 -> input, None
        | i -> input.Substring(0, i), Some(parseNum (input.Substring(i + 1)))

    let private splitRange (input: string) (aliases: System.Collections.Generic.IDictionary<string, int>) : float * float option =
        match input.IndexOf '-' with
        | -1 -> aliasOrValue input aliases, None
        | i -> aliasOrValue (input.Substring(0, i)) aliases, Some(aliasOrValue (input.Substring(i + 1)) aliases)

    let private isInt (x: float) = System.Double.IsInteger x

    let private parseSegment (input: string) (options: SegOptions) : Result<Set<int>, CronParseError> =
        let err msg = Error(CronParseError(msg, Some input))
        let capacity = options.Max - options.Min + 1
        let mutable values = Set.empty
        let mutable result: Result<Set<int>, CronParseError> option = None
        let fields = input.Split(',')
        let mutable fi = 0
        while result.IsNone && fi < fields.Length do
            let field = fields.[fi]
            fi <- fi + 1
            let raw, step = splitStep field
            if raw = "*" && step.IsNone then
                result <- Some(Ok Set.empty)
            else
                let stepValid =
                    match step with
                    | Some s when not (isInt s) -> Some(err "Expected step value to be a positive integer")
                    | Some s when s < 1.0 -> Some(err "Expected step value to be greater than 0")
                    | Some s when s > float options.Max -> Some(err (sprintf "Expected step value to be less than %d" options.Max))
                    | _ -> None
                match stepValid with
                | Some e -> result <- Some e
                | None ->
                    let stepN = match step with Some s -> int s | None -> 1
                    if raw = "*" then
                        let mutable i = options.Min
                        while i <= options.Max do
                            values <- Set.add i values
                            i <- i + stepN
                    else
                        let left, right = splitRange raw options.Aliases
                        if not (isInt left) then result <- Some(err "Expected a positive integer")
                        elif int left < options.Min || int left > options.Max then
                            result <- Some(err (sprintf "Expected a value between %d and %d" options.Min options.Max))
                        else
                            match right with
                            | None -> values <- Set.add (int left) values
                            | Some r when not (isInt r) -> result <- Some(err "Expected a positive integer")
                            | Some r when int r < options.Min || int r > options.Max ->
                                result <- Some(err (sprintf "Expected a value between %d and %d" options.Min options.Max))
                            | Some r when left > r -> result <- Some(err "Invalid value range")
                            | Some r ->
                                let mutable i = int left
                                while i <= int r do
                                    values <- Set.add i values
                                    i <- i + stepN
                    if result.IsNone && Set.count values >= capacity then
                        result <- Some(Ok Set.empty)
        match result with
        | Some r -> r
        | None -> Ok values

    /// Validate a tz string (UTC subset). Only rejects blatantly invalid zones
    /// (e.g. ""); during stepping all zones are treated as UTC.
    let private validateTz (tz: string) : bool = tz.Trim() <> ""

    let parse (cron: string) (tz: string option) : Result<Cron, CronParseError> =
        let segments = cron.Split(' ') |> Array.filter (fun s -> s <> "") |> Array.toList
        if segments.Length <> 5 && segments.Length <> 6 then
            Error(CronParseError("Invalid number of segments in cron expression", Some cron))
        else
            let segs = if segments.Length = 5 then "0" :: segments else segments
            match segs with
            | [ seconds; minutes; hours; days; months; weekdays ] ->
                let tzResult =
                    match tz with
                    | None -> Ok None
                    | Some t when validateTz t -> Ok(Some t)
                    | Some t -> Error(CronParseError("Invalid time zone in cron expression", Some t))
                match tzResult with
                | Error e -> Error e
                | Ok tzv ->
                    match parseSegment seconds secondOptions with
                    | Error e -> Error e
                    | Ok sec ->
                        match parseSegment minutes minuteOptions with
                        | Error e -> Error e
                        | Ok min ->
                            match parseSegment hours hourOptions with
                            | Error e -> Error e
                            | Ok hrs ->
                                match parseSegment days dayOptions with
                                | Error e -> Error e
                                | Ok dys ->
                                    match parseSegment months monthOptions with
                                    | Error e -> Error e
                                    | Ok mts ->
                                        match parseSegment weekdays weekdayOptions with
                                        | Error e -> Error e
                                        | Ok wds ->
                                            Ok
                                                { Seconds = sec
                                                  Minutes = min
                                                  Hours = hrs
                                                  Days = dys
                                                  Months = mts
                                                  Weekdays = wds
                                                  Tz = tzv }
            | _ -> Error(CronParseError("Invalid number of segments in cron expression", Some cron))

    let parseUnsafe (cron: string) (tz: string option) : Cron =
        match parse cron tz with
        | Ok c -> c
        | Error e -> raise e

    // --- matching ---

    let matches (cron: Cron) (date: DateTime) : bool =
        let parts = DateTime.toPartsUtc date
        if not (Set.isEmpty cron.Seconds) && not (Set.contains parts.Second cron.Seconds) then false
        elif not (Set.isEmpty cron.Minutes) && not (Set.contains parts.Minute cron.Minutes) then false
        elif not (Set.isEmpty cron.Hours) && not (Set.contains parts.Hour cron.Hours) then false
        elif not (Set.isEmpty cron.Months) && not (Set.contains parts.Month cron.Months) then false
        elif Set.isEmpty cron.Days && Set.isEmpty cron.Weekdays then true
        elif Set.isEmpty cron.Weekdays then Set.contains parts.Day cron.Days
        elif Set.isEmpty cron.Days then Set.contains parts.WeekDay cron.Weekdays
        else Set.contains parts.Day cron.Days || Set.contains parts.WeekDay cron.Weekdays

    // --- stepping (next / prev), UTC only ---

    type private Direction =
        | Next
        | Prev

    // calendar helpers delegate to the BCL (month0 is 0-indexed; BCL is 1-indexed)
    let private daysInMonth (year: int) (month0: int) : int =
        System.DateTime.DaysInMonth(year, month0 + 1)

    let private daysInPrevMonth (year: int) (month0: int) : int =
        if month0 = 0 then 31 else daysInMonth year (month0 - 1)

    type private Bound =
        { Second: int; Minute: int; Hour: int; Day: int; Month: int; Weekday: int }

    let private headOr (xs: int list) (dflt: int) = match xs with x :: _ -> x | [] -> dflt
    let private lastOr (xs: int list) (dflt: int) = match xs with [] -> dflt | _ -> List.last xs

    let private lookupTable (values: int list) (size: int) (dir: Direction) : int option [] =
        let result = Array.create size None
        if List.isEmpty values then
            result
        else
            let arr = List.toArray values
            let mutable current = None
            match dir with
            | Next ->
                let mutable index = arr.Length - 1
                for i in size - 1 .. -1 .. 0 do
                    while index >= 0 && arr.[index] >= i do
                        current <- Some arr.[index]
                        index <- index - 1
                    result.[i] <- current
            | Prev ->
                let mutable index = 0
                for i in 0 .. size - 1 do
                    while index < arr.Length && arr.[index] <= i do
                        current <- Some arr.[index]
                        index <- index + 1
                    result.[i] <- current
            result

    let private stepCron (cron: Cron) (now: DateTime) (direction: Direction) : DateTime =
        let reverse = (direction = Prev)
        let tick = if reverse then -1 else 1

        let seconds = Set.toList cron.Seconds
        let minutes = Set.toList cron.Minutes
        let hours = Set.toList cron.Hours
        let days = Set.toList cron.Days
        let months = Set.toList cron.Months
        let weekdays = Set.toList cron.Weekdays

        let first =
            { Second = headOr seconds 0
              Minute = headOr minutes 0
              Hour = headOr hours 0
              Day = headOr days 1
              Month = headOr months 1 - 1
              Weekday = headOr weekdays 0 }

        let last =
            { Second = lastOr seconds 59
              Minute = lastOr minutes 59
              Hour = lastOr hours 23
              Day = lastOr days 31
              Month = lastOr months 12 - 1
              Weekday = lastOr weekdays 6 }

        let boundary = if reverse then last else first

        let tSecond = lookupTable seconds 60 direction
        let tMinute = lookupTable minutes 60 direction
        let tHour = lookupTable hours 24 direction
        let tDay = lookupTable days 32 direction
        let tMonth = lookupTable months 13 direction
        let tWeekday = lookupTable weekdays 7 direction

        let needsStep (nextV: int) (current: int) = if reverse then nextV < current else nextV > current
        let big = 1_000_000

        let date = DateTime.JsDate(now.EpochMillis)
        date.SetUTCSeconds(date.Seconds + tick, 0)

        let secondsStage () =
            if Set.isEmpty cron.Seconds then false
            else
                let currentSecond = date.Seconds
                match tSecond.[currentSecond] with
                | None -> date.SetUTCMinutes(date.Minutes + tick, boundary.Second); true
                | Some nextSecond -> if needsStep nextSecond currentSecond then (date.SetUTCSeconds nextSecond; true) else false

        let minutesStage () =
            if Set.isEmpty cron.Minutes then false
            else
                let currentMinute = date.Minutes
                match tMinute.[currentMinute] with
                | None -> date.SetUTCHours(date.Hours + tick, boundary.Minute, boundary.Second); true
                | Some nextMinute -> if needsStep nextMinute currentMinute then (date.SetUTCMinutes(nextMinute, boundary.Second); true) else false

        let hoursStage () =
            if Set.isEmpty cron.Hours then false
            else
                let currentHour = date.Hours
                match tHour.[currentHour] with
                | None ->
                    date.SetUTCDate(date.Date + tick)
                    date.SetUTCHours(boundary.Hour, boundary.Minute, boundary.Second)
                    true
                | Some nextHour -> if needsStep nextHour currentHour then (date.SetUTCHours(nextHour, boundary.Minute, boundary.Second); true) else false

        let dayStage () =
            if Set.isEmpty cron.Weekdays && Set.isEmpty cron.Days then false
            else
                let mutable a = if reverse then -big else big
                let mutable b = if reverse then -big else big
                if not (Set.isEmpty cron.Weekdays) then
                    let currentWeekday = date.Day
                    match tWeekday.[currentWeekday] with
                    | None -> a <- if reverse then currentWeekday - 7 + boundary.Weekday else 7 - currentWeekday + boundary.Weekday
                    | Some nextWeekday -> a <- nextWeekday - currentWeekday
                if not (Set.isEmpty cron.Days) && a <> 0 then
                    let currentDay = date.Date
                    match tDay.[currentDay] with
                    | None ->
                        if reverse then
                            let prevMonthDays = daysInPrevMonth date.Year date.Month0
                            b <- -(currentDay + (prevMonthDays - boundary.Day))
                        else
                            b <- daysInMonth date.Year date.Month0 - currentDay + boundary.Day
                    | Some nextDay when (not reverse) && nextDay > daysInMonth date.Year date.Month0 ->
                        b <- daysInMonth date.Year date.Month0 - currentDay + boundary.Day
                    | Some nextDay -> b <- nextDay - currentDay
                let addDays = if reverse then max a b else min a b
                if addDays <> 0 then
                    date.SetUTCDate(date.Date + addDays)
                    date.SetUTCHours(boundary.Hour, boundary.Minute, boundary.Second)
                    true
                else false

        let monthStage () =
            if Set.isEmpty cron.Months then false
            else
                let currentMonth = date.Month0 + 1
                let clampBoundaryDay (targetMonthIndex: int) =
                    if not (Set.isEmpty cron.Days) then boundary.Day
                    else min boundary.Day (daysInMonth date.Year targetMonthIndex)
                match tMonth.[currentMonth] with
                | None ->
                    date.SetUTCFullYear(date.Year + tick)
                    date.SetUTCMonth(boundary.Month, clampBoundaryDay boundary.Month)
                    date.SetUTCHours(boundary.Hour, boundary.Minute, boundary.Second)
                    true
                | Some nextMonth ->
                    if needsStep nextMonth currentMonth then
                        let targetMonthIndex = nextMonth - 1
                        date.SetUTCMonth(targetMonthIndex, clampBoundaryDay targetMonthIndex)
                        date.SetUTCHours(boundary.Hour, boundary.Minute, boundary.Second)
                        true
                    else false

        let stages = [ secondsStage; minutesStage; hoursStage; dayStage; monthStage ]
        let rec runStages =
            function
            | [] -> false
            | stage :: rest -> if stage () then true else runStages rest

        let mutable proceed = true
        let mutable count = 0
        while proceed do
            if count >= 10_000 then
                raise (System.Exception(sprintf "Unable to find %s cron date" (if reverse then "prev" else "next")))
            count <- count + 1
            proceed <- runStages stages

        DateTime.makeUnsafe (InputEpochMillis(date.GetTime()))

    /// The next scheduled instant strictly after `now`.
    let next (cron: Cron) (now: DateTime) : DateTime = stepCron cron now Next

    /// The previous scheduled instant strictly before `now`.
    let prev (cron: Cron) (now: DateTime) : DateTime = stepCron cron now Prev

    /// An infinite sequence of scheduled instants after `start`.
    let sequence (cron: Cron) (start: DateTime) : DateTime seq =
        seq {
            let mutable current = start
            while true do
                current <- next cron current
                yield current
        }
