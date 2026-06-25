module RegExpSpec

open System.Text.RegularExpressions
open Effect
open Effect.Vitest

describe "RegExp" (fun () ->
    test "isRegExp narrows regular expressions" (fun () ->
        toBe (RegExp.isRegExp (box (Regex "a"))) true
        toBe (RegExp.isRegExp (box "a")) false)

    test "escape escapes special characters" (fun () ->
        toBe (RegExp.escape "a*b") @"a\*b"
        toBe (RegExp.escape @"^$.*+?()[]{}|\/") @"\^\$\.\*\+\?\(\)\[\]\{\}\|\\\/")

    test "make builds a usable pattern" (fun () ->
        let pattern = RegExp.makeWith RegexOptions.IgnoreCase "hello"
        toBe (pattern.IsMatch "Hello World") true
        toBe (pattern.IsMatch "goodbye") false))

