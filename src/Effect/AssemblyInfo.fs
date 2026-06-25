module internal Effect.AssemblyInfo

open System.Runtime.CompilerServices

// TestClock (and other test infra) live in the separate Effect.Testing assembly —
// out of the shipped core — but need the core's internal primitives (`Cell`, the
// `Effect` constructor). Grant that one companion assembly access to internals.
// .NET only; under Fable there is no assembly boundary (sources compile together),
// which is why this file is referenced by Effect.fsproj rather than the shared
// Effect.Sources.props the Fable mirrors import.
[<assembly: InternalsVisibleTo("EffSharp.Testing")>]
do ()
