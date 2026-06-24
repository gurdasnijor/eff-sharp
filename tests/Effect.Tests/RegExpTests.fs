module Effect.Tests.RegExpTests

open System.Text.RegularExpressions
open Xunit
open Effect

// Ported from the doc examples in
// repos/effect-smol/packages/effect/src/RegExp.ts (no upstream RegExp.test.ts).

[<Fact>]
let ``isRegExp narrows regular expressions`` () =
    Assert.True(RegExp.isRegExp (box (Regex "a")))
    Assert.False(RegExp.isRegExp (box "a"))

[<Fact>]
let ``escape escapes special characters`` () =
    Assert.Equal(@"a\*b", RegExp.escape "a*b")
    Assert.Equal(@"\^\$\.\*\+\?\(\)\[\]\{\}\|\\\/", RegExp.escape @"^$.*+?()[]{}|\/")

[<Fact>]
let ``make builds a usable pattern`` () =
    let pattern = RegExp.makeWith RegexOptions.IgnoreCase "hello"
    Assert.True(pattern.IsMatch "Hello World")
    Assert.False(pattern.IsMatch "goodbye")
