module ReferencesSpec

open Effect
open Effect.Vitest

describe "References" (fun () ->
    test "reference stores a stable key and default value" (fun () ->
        let reference = Reference.make "test/reference" 42

        toBe reference.Key "test/reference"
        toBe (Reference.defaultValue reference) 42
        toBe (Context.getReference reference Context.empty) 42)

    test "well-known references expose expected defaults" (fun () ->
        toEqual (Reference.defaultValue References.MinimumLogLevel) LogLevel.Info
        toEqual (Reference.defaultValue References.CurrentLogLevel) LogLevel.Info
        toEqual (Reference.defaultValue References.CurrentConcurrency) (None: int option)
        toBe (Reference.defaultValue References.CurrentLogAnnotations |> Map.isEmpty) true
        toEqual (Reference.defaultValue References.CurrentLogSpans) []
        toBe (Reference.defaultValue References.TracerEnabled) true
        toEqual (Reference.defaultValue References.UnhandledLogLevel) (Some LogLevel.Debug)))
