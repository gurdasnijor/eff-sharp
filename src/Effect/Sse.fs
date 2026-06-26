namespace Effect

/// Server-Sent Events text framing.
type SseEvent =
    { Id: string option
      Event: string
      Data: string }

type SseRetry =
    { DurationMillis: int
      LastEventId: string option }

type SseParsed =
    | SseEvent of SseEvent
    | SseRetry of SseRetry

type SseParser =
    { Feed: string -> SseParsed list
      Reset: unit -> unit }

[<RequireQualifiedAccess>]
module Sse =

    let private utf8Bom = "\uFEFF"
    let private byteOrderMarkAsText = string (char 239) + string (char 187) + string (char 191)

    let event (event: string) (data: string) : SseEvent =
        { Id = None
          Event = event
          Data = data }

    let message (data: string) : SseEvent =
        event "message" data

    let retry (durationMillis: int) : SseRetry =
        { DurationMillis = durationMillis
          LastEventId = None }

    let private appendLine (prefix: string) (value: string) =
        prefix + value.Replace("\n", "\n" + prefix) + "\n"

    let encodeEvent (event: SseEvent) : string =
        let mutable output = ""

        match event.Id with
        | Some id -> output <- output + "id: " + id + "\n"
        | None -> ()

        if event.Event <> "message" then
            output <- output + "event: " + event.Event + "\n"

        if event.Data <> "" then
            output <- output + appendLine "data: " event.Data

        output + "\n"

    let encodeRetry (retry: SseRetry) : string =
        "retry: " + string retry.DurationMillis + "\n\n"

    let encode =
        function
        | SseEvent event -> encodeEvent event
        | SseRetry retry -> encodeRetry retry

    let makeParser () : SseParser =
        let mutable firstChunk = true
        let mutable buffer = ""
        let mutable skipLeadingLf = false
        let mutable eventId: string option = None
        let mutable lastEventId: string option = None
        let mutable eventName: string option = None
        let mutable data = ""

        let reset () =
            firstChunk <- true
            buffer <- ""
            skipLeadingLf <- false
            eventId <- None
            lastEventId <- None
            eventName <- None
            data <- ""

        let parseLine (line: string) : SseParsed list =
            if line = "" then
                if data.Length = 0 then
                    eventName <- None
                    []
                else
                    let parsed =
                        { Id = eventId
                          Event = eventName |> Option.defaultValue "message"
                          Data = data.Substring(0, data.Length - 1) }

                    data <- ""
                    eventId <- None
                    eventName <- None
                    [ SseEvent parsed ]
            elif line.StartsWith(":") then
                []
            else
                let field, value =
                    let index = line.IndexOf(':')

                    if index < 0 then
                        line, ""
                    else
                        let rawValue = line.Substring(index + 1)
                        let value =
                            if rawValue.StartsWith(" ") then
                                rawValue.Substring(1)
                            else
                                rawValue

                        line.Substring(0, index), value

                match field with
                | "data" ->
                    data <- data + value + "\n"
                    []
                | "event" ->
                    eventName <- Some value
                    []
                | "id" when not (value.Contains("\u0000")) ->
                    eventId <- Some value
                    lastEventId <- Some value
                    []
                | "retry" ->
                    match System.Int32.TryParse value with
                    | true, millis -> [ SseRetry { DurationMillis = millis; LastEventId = lastEventId } ]
                    | _ -> []
                | _ -> []

        let stripBom (text: string) =
            if text.StartsWith(utf8Bom) then
                text.Substring(utf8Bom.Length)
            elif text.StartsWith(byteOrderMarkAsText) then
                text.Substring(byteOrderMarkAsText.Length)
            else
                text

        let feed (chunk: string) : SseParsed list =
            let chunk =
                if firstChunk then
                    firstChunk <- false
                    stripBom chunk
                else
                    chunk

            let chunk =
                if skipLeadingLf && chunk.StartsWith("\n") then
                    skipLeadingLf <- false
                    chunk.Substring(1)
                else
                    skipLeadingLf <- false
                    chunk

            buffer <- buffer + chunk

            let rec loop position parsed =
                if position >= buffer.Length then
                    buffer <- ""
                    List.rev parsed
                else
                    let rec findLineEnd index =
                        if index >= buffer.Length then
                            None
                        else
                            match buffer.[index] with
                            | '\n' -> Some(index, 1, false)
                            | '\r' ->
                                if index + 1 < buffer.Length && buffer.[index + 1] = '\n' then
                                    Some(index, 2, false)
                                else
                                    Some(index, 1, index + 1 = buffer.Length)
                            | _ -> findLineEnd (index + 1)

                    match findLineEnd position with
                    | None ->
                        buffer <- buffer.Substring(position)
                        List.rev parsed
                    | Some(lineEnd, terminatorLength, pendingLf) ->
                        let line = buffer.Substring(position, lineEnd - position)
                        skipLeadingLf <- pendingLf
                        loop (lineEnd + terminatorLength) (List.rev (parseLine line) @ parsed)

            loop 0 []

        reset ()

        { Feed = feed
          Reset = reset }
