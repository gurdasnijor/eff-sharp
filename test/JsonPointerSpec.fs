module JsonPointerSpec

open Effect
open Effect.Vitest

describe "JsonPointer" (fun () ->
    test "escapeToken escapes tilde to ~0" (fun () ->
        toBe (JsonPointer.escapeToken "a~b") "a~0b"
        toBe (JsonPointer.escapeToken "~") "~0"
        toBe (JsonPointer.escapeToken "~~") "~0~0")

    test "escapeToken escapes slash to ~1" (fun () ->
        toBe (JsonPointer.escapeToken "a/b") "a~1b"
        toBe (JsonPointer.escapeToken "/") "~1"
        toBe (JsonPointer.escapeToken "//") "~1~1")

    test "escapeToken escapes both tilde and slash" (fun () ->
        toBe (JsonPointer.escapeToken "a~b/c") "a~0b~1c"
        toBe (JsonPointer.escapeToken "path/to~key") "path~1to~0key"
        toBe (JsonPointer.escapeToken "~a/b~c/d~") "~0a~1b~0c~1d~0")

    test "escapeToken replaces tilde before slash" (fun () ->
        toBe (JsonPointer.escapeToken "a~/b") "a~0~1b"
        toBe (JsonPointer.escapeToken "a/~b") "a~1~0b"
        toBe (JsonPointer.escapeToken "~/") "~0~1"
        toBe (JsonPointer.escapeToken "/~") "~1~0")

    test "escapeToken handles confusable sequences" (fun () ->
        toBe (JsonPointer.escapeToken "~01") "~001"
        toBe (JsonPointer.escapeToken "~10") "~010"
        toBe (JsonPointer.escapeToken "~00") "~000"
        toBe (JsonPointer.escapeToken "~11") "~011")

    test "escapeToken edge cases and passthrough" (fun () ->
        toBe (JsonPointer.escapeToken "") ""
        toBe (JsonPointer.escapeToken "abc") "abc"
        toBe (JsonPointer.escapeToken "héllo") "héllo"
        toBe (JsonPointer.escapeToken "世界") "世界"
        toBe (JsonPointer.escapeToken "a.b") "a.b")

    test "unescapeToken unescapes ~0 to tilde" (fun () ->
        toBe (JsonPointer.unescapeToken "a~0b") "a~b"
        toBe (JsonPointer.unescapeToken "~0") "~"
        toBe (JsonPointer.unescapeToken "~0~0") "~~")

    test "unescapeToken unescapes ~1 to slash" (fun () ->
        toBe (JsonPointer.unescapeToken "a~1b") "a/b"
        toBe (JsonPointer.unescapeToken "~1") "/"
        toBe (JsonPointer.unescapeToken "~1~1") "//")

    test "unescapeToken replaces ~1 before ~0" (fun () ->
        toBe (JsonPointer.unescapeToken "a~1b~0c") "a/b~c"
        toBe (JsonPointer.unescapeToken "~01") "~1"
        toBe (JsonPointer.unescapeToken "~10") "/0"
        toBe (JsonPointer.unescapeToken "~00") "~0"
        toBe (JsonPointer.unescapeToken "~11") "/1")

    test "unescapeToken edge cases and passthrough" (fun () ->
        toBe (JsonPointer.unescapeToken "") ""
        toBe (JsonPointer.unescapeToken "abc") "abc"
        toBe (JsonPointer.unescapeToken "héllo~0world~1test") "héllo~world/test")

    test "round-trip unescape(escape(token)) = token" (fun () ->
        let cases =
            [ "abc"; "a~b"; "a/b"; "a~b/c"; "path/to~key"; "name/alias"; "~"; "/"; "~~"; "//"; "~/"; "/~"; ""
              "~01"; "~10"; "~00"; "~11"; "a~01b"; "a~10b"; "héllo"; "世界"; "🚀"; "héllo~world/test"
              "世界/🌍~key"; "a~b/c~d/e"; "~a/b~c/d~" ]

        for token in cases do
            let escaped = JsonPointer.escapeToken token
            toBe (JsonPointer.unescapeToken escaped) token))

