namespace Effect

open System.Globalization
open System.Text.RegularExpressions

/// Port of repos/effect-smol/packages/effect/src/DateTime.ts (+ internal/dateTime.ts).
///
/// SCOPE: this port covers the **UTC core** only. A `DateTime` here is the
/// upstream `Utc` variant — an instant stored as epoch milliseconds — and the
/// calendar arithmetic is performed by re-implementing the ECMAScript
/// `Date`-with-`setUTC*` overflow semantics in `JsDate` (so `add({ months })`,
/// `endOf("month")`, leap years, etc. behave exactly like upstream).
///
/// Deliberately SKIPPED (noted per CONVENTIONS.md "too large → core + skips"):
///   * The `Zoned` variant and all IANA time-zone / DST machinery
///     (`makeZoned*`, `setZone*`, disambiguation, `zonedOffset`, `toDate`).
///   * Effect-runtime integration (`now`, `nowAsDate`, layers, `CurrentTimeZone`,
///     `withCurrentZone*`, `isFuture`/`isPast`) — depends on the Clock/Effect/Layer
///     runtime owned by other slices.
///   * `Intl`-based locale formatting (`format`, `formatLocal`, `formatUtc`,
///     `formatIntl`) and the zoned ISO formatters.
///   * JS-runtime machinery (`TypeId` brand, `pipe`, `Hash`/`Equal` symbols).
///
/// Provided UTC surface: `makeUnsafe`/`make` (string + parts + epoch), `toJSON`/
/// `formatIso`, `toPartsUtc`/`getPartUtc`, `setPartsUtc`/`setParts`, `add`/
/// `subtract`/`addDuration`/`subtractDuration`, `startOf`/`endOf`/`nearest`,
/// `mutate`, `removeTime`, `distance`, `Order`/`Equivalence`/comparisons.

/// An instant (the upstream `Utc` variant), stored as epoch milliseconds.
type DateTime = { EpochMillis: int64 }

/// Partial calendar parts used to construct or update a `DateTime` (month is
/// 1-based, matching upstream `Parts`).
type DateTimeParts =
    { Year: int option
      Month: int option
      Day: int option
      WeekDay: int option
      Hour: int option
      Minute: int option
      Second: int option
      Millisecond: int option }

    static member Empty =
        { Year = None; Month = None; Day = None; WeekDay = None
          Hour = None; Minute = None; Second = None; Millisecond = None }

/// The fully-resolved UTC parts of a `DateTime` (month is 1-based, weekDay 0=Sun).
type DateTimePartsUtc =
    { Year: int
      Month: int
      Day: int
      WeekDay: int
      Hour: int
      Minute: int
      Second: int
      Millisecond: int }

/// Signed amounts for calendar arithmetic (`add`/`subtract`).
type DateTimeMath =
    { Years: int
      Months: int
      Weeks: int
      Days: int
      Hours: int
      Minutes: int
      Seconds: int
      Milliseconds: int }

    static member Zero =
        { Years = 0; Months = 0; Weeks = 0; Days = 0
          Hours = 0; Minutes = 0; Seconds = 0; Milliseconds = 0 }

/// Accepted inputs to `makeUnsafe`/`make` (UTC subset of upstream `Input`).
type DateTimeInput =
    | InputString of string
    | InputParts of DateTimeParts
    | InputEpochMillis of int64
    | InputDateTime of DateTime

/// A single calendar unit for `startOf`/`endOf`/`nearest`.
type DateTimeUnit =
    | UnitMillisecond
    | UnitSecond
    | UnitMinute
    | UnitHour
    | UnitDay
    | UnitWeek
    | UnitMonth
    | UnitYear

/// A single part selector for `getPartUtc`.
type DateTimePart =
    | PartMillisecond
    | PartSecond
    | PartMinute
    | PartHour
    | PartDay
    | PartWeekDay
    | PartMonth
    | PartYear

[<RequireQualifiedAccess>]
module DateTime =

    let private inv = CultureInfo.InvariantCulture
    let private msPerDay = 86_400_000L

    // --- ECMAScript date math (mirrors V8's UTC field handling) ---

    let private floorDiv (a: int64) (b: int64) : int64 =
        let q = a / b
        let r = a % b
        if r <> 0L && ((r < 0L) <> (b < 0L)) then q - 1L else q

    let private floorMod (a: int64) (b: int64) : int64 = a - floorDiv a b * b

    /// Length of month `m0` (0-based) in `year`, via the BCL calendar
    /// (`System.DateTime.DaysInMonth` already encodes the leap-year rule).
    let private monthLen (year: int64) (m0: int) : int64 = int64 (System.DateTime.DaysInMonth(int year, m0 + 1))

    /// Day number (days since 1970-01-01) of Jan 1 of the given year.
    let private daysFromYear (y: int64) : int64 =
        365L * (y - 1970L)
        + floorDiv (y - 1969L) 4L
        - floorDiv (y - 1901L) 100L
        + floorDiv (y - 1601L) 400L

    /// Decompose an instant into (year, month0, date).
    let private ymdFromTime (t: int64) : int64 * int * int =
        let dayN = floorDiv t msPerDay
        let mutable y = 1970L + floorDiv dayN 366L
        while daysFromYear (y + 1L) <= dayN do
            y <- y + 1L
        while daysFromYear y > dayN do
            y <- y - 1L
        let doy = dayN - daysFromYear y
        let mutable m = 0
        let mutable rem = doy
        while rem >= monthLen y m do
            rem <- rem - monthLen y m
            m <- m + 1
        y, m, int rem + 1

    let private hoursOf (t: int64) : int = int (floorMod (floorDiv t 3_600_000L) 24L)
    let private minutesOf (t: int64) : int = int (floorMod (floorDiv t 60_000L) 60L)
    let private secondsOf (t: int64) : int = int (floorMod (floorDiv t 1_000L) 60L)
    let private msOf (t: int64) : int = int (floorMod t 1_000L)
    let private weekDayOf (t: int64) : int = int (floorMod (floorDiv t msPerDay + 4L) 7L)

    let private makeDay (year: int64) (month: int64) (date: int64) : int64 =
        let y = year + floorDiv month 12L
        let m = floorMod month 12L
        let mutable dayNum = daysFromYear y
        for i in 0 .. int m - 1 do
            dayNum <- dayNum + monthLen y i
        dayNum + date - 1L

    let private makeTime (h: int64) (m: int64) (s: int64) (ms: int64) : int64 =
        h * 3_600_000L + m * 60_000L + s * 1_000L + ms

    let private makeDate (day: int64) (time: int64) : int64 = day * msPerDay + time

    /// A mutable instant exposing the `getUTC*`/`setUTC*` surface that upstream's
    /// calendar operations are written against. `internal` so the `Cron` slice can
    /// reuse the same UTC field arithmetic for its `next`/`prev` stepping.
    type internal JsDate(initial: int64) =
        let mutable t = initial
        member _.GetTime() : int64 = t
        member _.SetTime(v: int64) = t <- v
        member _.Year = let (y, _, _) = ymdFromTime t in int y
        member _.Month0 = let (_, m, _) = ymdFromTime t in m
        member _.Date = let (_, _, d) = ymdFromTime t in d
        member _.Day = weekDayOf t
        member _.Hours = hoursOf t
        member _.Minutes = minutesOf t
        member _.Seconds = secondsOf t
        member _.Milliseconds = msOf t

        member _.SetUTCFullYear(y: int, ?mo: int, ?d: int) =
            let _, cmo, cd = ymdFromTime t
            let mm = defaultArg mo cmo
            let dd = defaultArg d cd
            t <- makeDate (makeDay (int64 y) (int64 mm) (int64 dd)) (floorMod t msPerDay)

        member _.SetUTCMonth(mo: int, ?d: int) =
            let cy, _, cd = ymdFromTime t
            let dd = defaultArg d cd
            t <- makeDate (makeDay (int64 cy) (int64 mo) (int64 dd)) (floorMod t msPerDay)

        member _.SetUTCDate(d: int) =
            let cy, cmo, _ = ymdFromTime t
            t <- makeDate (makeDay (int64 cy) (int64 cmo) (int64 d)) (floorMod t msPerDay)

        member _.SetUTCHours(h: int, ?mi: int, ?s: int, ?ms: int) =
            let dayN = floorDiv t msPerDay
            let mm = defaultArg mi (minutesOf t)
            let ss = defaultArg s (secondsOf t)
            let mss = defaultArg ms (msOf t)
            t <- makeDate dayN (makeTime (int64 h) (int64 mm) (int64 ss) (int64 mss))

        member _.SetUTCMinutes(mi: int, ?s: int, ?ms: int) =
            let dayN = floorDiv t msPerDay
            let ss = defaultArg s (secondsOf t)
            let mss = defaultArg ms (msOf t)
            t <- makeDate dayN (makeTime (int64 (hoursOf t)) (int64 mi) (int64 ss) (int64 mss))

        member _.SetUTCSeconds(s: int, ?ms: int) =
            let dayN = floorDiv t msPerDay
            let mss = defaultArg ms (msOf t)
            t <- makeDate dayN (makeTime (int64 (hoursOf t)) (int64 (minutesOf t)) (int64 s) (int64 mss))

        member _.SetUTCMilliseconds(ms: int) =
            let dayN = floorDiv t msPerDay
            t <- makeDate dayN (makeTime (int64 (hoursOf t)) (int64 (minutesOf t)) (int64 (secondsOf t)) (int64 ms))

    // --- parsing ---

    let private hasZoneRegex = Regex(@"Z|GMT|[+-]\d{2}$|[+-]\d{2}:?\d{2}$|\]$", RegexOptions.Compiled)

    let private isoRegex =
        Regex(@"^(\d{4})-(\d{2})-(\d{2})(?:[T ](\d{2}):(\d{2})(?::(\d{2}))?(?:\.(\d{1,3}))?)?\s*(Z|GMT|[+-]\d{2}:?\d{2})?$",
              RegexOptions.Compiled)

    let private parseZoneOffset (z: string) : int64 =
        if z = "" || z = "Z" || z = "GMT" then 0L
        else
            let sign = if z.[0] = '-' then -1L else 1L
            let digits = z.Substring(1).Replace(":", "")
            let hh = int64 (digits.Substring(0, 2))
            let mm = int64 (digits.Substring(2, 2))
            sign * (hh * 60L + mm) * 60_000L

    /// Parse a date string to epoch millis. Zoneless strings are treated as UTC
    /// (upstream appends "Z"); offsets and "Z"/"GMT" are honoured.
    let private parseInstant (input: string) : int64 =
        let s = input.Trim()
        let m = isoRegex.Match s
        if m.Success then
            let group (i: int) = m.Groups.[i]
            let geti (i: int) dflt =
                let g = group i
                if g.Success && g.Value <> "" then int64 g.Value else dflt
            let y = int64 (group 1).Value
            let mo = int64 (group 2).Value - 1L
            let d = int64 (group 3).Value
            let h = geti 4 0L
            let mi = geti 5 0L
            let sec = geti 6 0L
            let msStr = (group 7).Value
            let ms = if msStr = "" then 0L else int64 ((msStr + "000").Substring(0, 3))
            let baseEpoch = makeDate (makeDay y mo d) (makeTime h mi sec ms)
            baseEpoch - parseZoneOffset (group 8).Value
        else
            match System.DateTimeOffset.TryParse(s, inv, DateTimeStyles.AssumeUniversal) with
            | true, dto -> dto.ToUnixTimeMilliseconds()
            | _ -> raise (System.Exception(sprintf "Invalid date: %s" input))

    // --- constructors ---

    let private makeUtc (epochMillis: int64) : DateTime = { EpochMillis = epochMillis }

    let private setPartsDate (date: JsDate) (parts: DateTimeParts) : unit =
        match parts.Year with Some y -> date.SetUTCFullYear y | None -> ()
        match parts.Month with Some mo -> date.SetUTCMonth(mo - 1) | None -> ()
        match parts.Day with Some d -> date.SetUTCDate d | None -> ()
        match parts.WeekDay with
        | Some wd -> date.SetUTCDate(date.Date + (wd - date.Day))
        | None -> ()
        match parts.Hour with Some h -> date.SetUTCHours h | None -> ()
        match parts.Minute with Some mi -> date.SetUTCMinutes mi | None -> ()
        match parts.Second with Some s -> date.SetUTCSeconds s | None -> ()
        match parts.Millisecond with Some ms -> date.SetUTCMilliseconds ms | None -> ()

    /// Decode an input to a UTC `DateTime`, throwing on invalid input.
    let makeUnsafe (input: DateTimeInput) : DateTime =
        match input with
        | InputDateTime dt -> dt
        | InputEpochMillis ms -> makeUtc ms
        | InputString s -> makeUtc (parseInstant s)
        | InputParts parts ->
            let date = JsDate(0L)
            setPartsDate date parts
            makeUtc (date.GetTime())

    /// Decode an input to a UTC `DateTime`, returning `None` on invalid input.
    let make (input: DateTimeInput) : DateTime option =
        try Some(makeUnsafe input) with _ -> None

    /// Whether a date string already carries zone information.
    let hasZone (input: string) : bool = hasZoneRegex.IsMatch input

    // --- conversions ---

    let toEpochMillis (self: DateTime) : int64 = self.EpochMillis

    /// Identity in this UTC-only port (upstream collapses a `Zoned` to its instant).
    let toUtc (self: DateTime) : DateTime = self

    let toPartsUtc (self: DateTime) : DateTimePartsUtc =
        let dt = System.DateTimeOffset.FromUnixTimeMilliseconds(self.EpochMillis).UtcDateTime
        { Year = dt.Year
          Month = dt.Month
          Day = dt.Day
          WeekDay = int dt.DayOfWeek
          Hour = dt.Hour
          Minute = dt.Minute
          Second = dt.Second
          Millisecond = dt.Millisecond }

    let getPartUtc (part: DateTimePart) (self: DateTime) : int =
        let p = toPartsUtc self
        match part with
        | PartMillisecond -> p.Millisecond
        | PartSecond -> p.Second
        | PartMinute -> p.Minute
        | PartHour -> p.Hour
        | PartDay -> p.Day
        | PartWeekDay -> p.WeekDay
        | PartMonth -> p.Month
        | PartYear -> p.Year

    /// ISO 8601 rendering, e.g. "2024-03-15T12:00:00.000Z" (matches `toJSON`).
    let formatIso (self: DateTime) : string =
        System.DateTimeOffset
            .FromUnixTimeMilliseconds(self.EpochMillis)
            .UtcDateTime.ToString("yyyy-MM-dd'T'HH:mm:ss.fff'Z'", inv)

    /// Alias of `formatIso` (upstream `Date.toJSON`).
    let toJSON (self: DateTime) : string = formatIso self

    let formatIsoDateUtc (self: DateTime) : string = (formatIso self).Substring(0, 10)

    // --- mapping ---

    /// Apply a mutation expressed against the `getUTC*`/`setUTC*` surface.
    /// Private because `JsDate` is an internal re-implementation detail (upstream
    /// exposes the JS `Date`, which has no clean F# public equivalent here).
    let private mutate (f: JsDate -> unit) (self: DateTime) : DateTime =
        let date = JsDate(self.EpochMillis)
        f date
        makeUtc (date.GetTime())

    let private mutateUtc (f: JsDate -> unit) (self: DateTime) : DateTime = mutate f self

    let mapEpochMillis (f: int64 -> int64) (self: DateTime) : DateTime = makeUtc (f self.EpochMillis)

    let setPartsUtc (parts: DateTimeParts) (self: DateTime) : DateTime =
        mutateUtc (fun date -> setPartsDate date parts) self

    /// In this UTC-only port `setParts` coincides with `setPartsUtc`.
    let setParts (parts: DateTimeParts) (self: DateTime) : DateTime = setPartsUtc parts self

    let removeTime (self: DateTime) : DateTime =
        mutate (fun date -> date.SetUTCHours(0, 0, 0, 0)) self

    // --- comparisons ---

    let equivalence (a: DateTime) (b: DateTime) : bool = a = b

    let order (self: DateTime) (that: DateTime) : int = sign (compare self.EpochMillis that.EpochMillis)

    let min (self: DateTime) (that: DateTime) : DateTime = if order self that <= 0 then self else that
    let max (self: DateTime) (that: DateTime) : DateTime = if order self that >= 0 then self else that

    let clamp (minimum: DateTime) (maximum: DateTime) (self: DateTime) : DateTime =
        if order self minimum < 0 then minimum
        elif order self maximum > 0 then maximum
        else self

    let isLessThan (self: DateTime) (that: DateTime) : bool = order self that < 0
    let isLessThanOrEqualTo (self: DateTime) (that: DateTime) : bool = order self that <= 0
    let isGreaterThan (self: DateTime) (that: DateTime) : bool = order self that > 0
    let isGreaterThanOrEqualTo (self: DateTime) (that: DateTime) : bool = order self that >= 0

    let between (minimum: DateTime) (maximum: DateTime) (self: DateTime) : bool =
        order self minimum >= 0 && order self maximum <= 0

    /// The `Duration` from `self` to `other` (positive when `other` is later).
    let distance (self: DateTime) (other: DateTime) : Duration =
        Duration.millis (float (other.EpochMillis - self.EpochMillis))

    // --- math ---

    let addDuration (duration: Input) (self: DateTime) : DateTime =
        mapEpochMillis (fun millis -> millis + int64 (Duration.toMillis (Duration.fromInputUnsafe duration))) self

    let subtractDuration (duration: Input) (self: DateTime) : DateTime =
        mapEpochMillis (fun millis -> millis - int64 (Duration.toMillis (Duration.fromInputUnsafe duration))) self

    let add (parts: DateTimeMath) (self: DateTime) : DateTime =
        mutate
            (fun date ->
                if parts.Milliseconds <> 0 then date.SetTime(date.GetTime() + int64 parts.Milliseconds)
                if parts.Seconds <> 0 then date.SetTime(date.GetTime() + int64 parts.Seconds * 1_000L)
                if parts.Minutes <> 0 then date.SetTime(date.GetTime() + int64 parts.Minutes * 60_000L)
                if parts.Hours <> 0 then date.SetTime(date.GetTime() + int64 parts.Hours * 3_600_000L)
                if parts.Days <> 0 then date.SetUTCDate(date.Date + parts.Days)
                if parts.Weeks <> 0 then date.SetUTCDate(date.Date + parts.Weeks * 7)
                if parts.Months <> 0 then
                    let day = date.Date
                    date.SetUTCMonth(date.Month0 + parts.Months + 1, 0)
                    if day < date.Date then date.SetUTCDate day
                if parts.Years <> 0 then
                    let day = date.Date
                    let month = date.Month0
                    date.SetUTCFullYear(date.Year + parts.Years, month + 1, 0)
                    if day < date.Date then date.SetUTCDate day)
            self

    let subtract (parts: DateTimeMath) (self: DateTime) : DateTime =
        add
            { Years = -parts.Years
              Months = -parts.Months
              Weeks = -parts.Weeks
              Days = -parts.Days
              Hours = -parts.Hours
              Minutes = -parts.Minutes
              Seconds = -parts.Seconds
              Milliseconds = -parts.Milliseconds }
            self

    let private startOfDate (date: JsDate) (part: DateTimeUnit) (weekStartsOn: int) : unit =
        match part with
        | UnitMillisecond -> ()
        | UnitSecond -> date.SetUTCMilliseconds 0
        | UnitMinute -> date.SetUTCSeconds(0, 0)
        | UnitHour -> date.SetUTCMinutes(0, 0, 0)
        | UnitDay -> date.SetUTCHours(0, 0, 0, 0)
        | UnitWeek ->
            let diff = (date.Day - weekStartsOn + 7) % 7
            date.SetUTCDate(date.Date - diff)
            date.SetUTCHours(0, 0, 0, 0)
        | UnitMonth ->
            date.SetUTCDate 1
            date.SetUTCHours(0, 0, 0, 0)
        | UnitYear ->
            date.SetUTCMonth(0, 1)
            date.SetUTCHours(0, 0, 0, 0)

    let private endOfDate (date: JsDate) (part: DateTimeUnit) (weekStartsOn: int) : unit =
        match part with
        | UnitMillisecond -> ()
        | UnitSecond -> date.SetUTCMilliseconds 999
        | UnitMinute -> date.SetUTCSeconds(59, 999)
        | UnitHour -> date.SetUTCMinutes(59, 59, 999)
        | UnitDay -> date.SetUTCHours(23, 59, 59, 999)
        | UnitWeek ->
            let diff = (date.Day - weekStartsOn + 7) % 7
            date.SetUTCDate(date.Date - diff + 6)
            date.SetUTCHours(23, 59, 59, 999)
        | UnitMonth ->
            date.SetUTCMonth(date.Month0 + 1, 0)
            date.SetUTCHours(23, 59, 59, 999)
        | UnitYear ->
            date.SetUTCMonth(11, 31)
            date.SetUTCHours(23, 59, 59, 999)

    let startOf (part: DateTimeUnit) (weekStartsOn: int) (self: DateTime) : DateTime =
        mutate (fun date -> startOfDate date part weekStartsOn) self

    let endOf (part: DateTimeUnit) (weekStartsOn: int) (self: DateTime) : DateTime =
        mutate (fun date -> endOfDate date part weekStartsOn) self

    let nearest (part: DateTimeUnit) (weekStartsOn: int) (self: DateTime) : DateTime =
        mutate
            (fun date ->
                match part with
                | UnitMillisecond -> ()
                | _ ->
                    let millis = date.GetTime()
                    let start = JsDate(millis)
                    startOfDate start part weekStartsOn
                    let startMillis = start.GetTime()
                    let endD = JsDate(millis)
                    endOfDate endD part weekStartsOn
                    let endMillis = endD.GetTime() + 1L
                    let diffStart = millis - startMillis
                    let diffEnd = endMillis - millis
                    if diffStart < diffEnd then date.SetTime startMillis else date.SetTime endMillis)
            self
