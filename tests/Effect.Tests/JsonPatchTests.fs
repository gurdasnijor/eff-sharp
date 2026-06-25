module Effect.Tests.JsonPatchTests

open Xunit
open Effect

// Ported from repos/effect-smol/packages/effect/test/JsonPatch.test.ts.
// The untyped JSON tree is the locally-defined `Json` DU; helper constructors
// below keep the cases as readable as the JS object literals upstream.

let private n (i: int) = JNumber(float i)
let private s = JString
let private arr xs = JArray xs
let private obj kvs = JObject(Map.ofList kvs)

let private get o n = JsonPatch.get o n
let private apply p d = JsonPatch.apply p d

let private appliesTo patch oldV newV =
    Assert.Equal<Json>(newV, apply patch oldV)

let private throwsContaining (sub: string) (f: unit -> 'a) =
    let ex = Assert.Throws<exn>(fun () -> f () |> ignore)
    Assert.Contains(sub, ex.Message)

// ---------------------------------------------------------------------------
// get
// ---------------------------------------------------------------------------

[<Fact>]
let ``get: returns [] for identical values`` () =
    for v in [ n 1; s "hello"; JBool true; JNull; arr [ n 1; n 2; n 3 ]; obj [ "a", n 1 ] ] do
        Assert.Equal<Operation list>([], get v v)

[<Fact>]
let ``get: emits a root replace for primitive changes`` () =
    Assert.Equal<Operation list>([ Replace("", n 2) ], get (n 1) (n 2))
    Assert.Equal<Operation list>([ Replace("", s "world") ], get (s "hello") (s "world"))
    Assert.Equal<Operation list>([ Replace("", JBool false) ], get (JBool true) (JBool false))
    Assert.Equal<Operation list>([ Replace("", n 42) ], get JNull (n 42))

[<Fact>]
let ``get: emits a root replace for type changes`` () =
    Assert.Equal<Operation list>([ Replace("", s "string") ], get (n 1) (s "string"))
    Assert.Equal<Operation list>([ Replace("", obj [ "a", n 1 ]) ], get (arr [ n 1; n 2 ]) (obj [ "a", n 1 ]))
    Assert.Equal<Operation list>([ Replace("", arr [ n 1; n 2 ]) ], get (obj [ "a", n 1 ]) (arr [ n 1; n 2 ]))

[<Fact>]
let ``get: arrays - append, replace, remove descending`` () =
    Assert.Equal<Operation list>([ Add("/2", n 3) ], get (arr [ n 1; n 2 ]) (arr [ n 1; n 2; n 3 ]))
    Assert.Equal<Operation list>([ Replace("/1", n 4) ], get (arr [ n 1; n 2; n 3 ]) (arr [ n 1; n 4; n 3 ]))
    Assert.Equal<Operation list>([ Remove "/2" ], get (arr [ n 1; n 2; n 3 ]) (arr [ n 1; n 2 ]))

[<Fact>]
let ``get: arrays - mixed operations`` () =
    Assert.Equal<Operation list>(
        [ Replace("/1", n 4); Replace("/2", n 5); Add("/3", n 6) ],
        get (arr [ n 1; n 2; n 3 ]) (arr [ n 1; n 4; n 5; n 6 ])
    )

    Assert.Equal<Operation list>(
        [ Replace("/1", n 5); Remove "/3"; Remove "/2" ],
        get (arr [ n 1; n 2; n 3; n 4 ]) (arr [ n 1; n 5 ])
    )

[<Fact>]
let ``get: nested arrays`` () =
    Assert.Equal<Operation list>(
        [ Replace("/1/1", n 5) ],
        get (arr [ arr [ n 1; n 2 ]; arr [ n 3; n 4 ] ]) (arr [ arr [ n 1; n 2 ]; arr [ n 3; n 5 ] ])
    )

    Assert.Equal<Operation list>(
        [ Add("/1", arr [ n 3; n 4 ]) ],
        get (arr [ arr [ n 1; n 2 ] ]) (arr [ arr [ n 1; n 2 ]; arr [ n 3; n 4 ] ])
    )

[<Fact>]
let ``get: objects - add / remove / replace in sorted key order`` () =
    Assert.Equal<Operation list>([ Add("/b", n 2) ], get (obj [ "a", n 1 ]) (obj [ "a", n 1; "b", n 2 ]))
    Assert.Equal<Operation list>([ Remove "/b" ], get (obj [ "a", n 1; "b", n 2 ]) (obj [ "a", n 1 ]))
    Assert.Equal<Operation list>([ Replace("/b", n 3) ], get (obj [ "a", n 1; "b", n 2 ]) (obj [ "a", n 1; "b", n 3 ]))

    Assert.Equal<Operation list>(
        [ Remove "/a"; Remove "/b"; Remove "/c" ],
        get (obj [ "a", n 1; "b", n 2; "c", n 3 ]) (obj [])
    )

[<Fact>]
let ``get: objects - stable key order across replaces`` () =
    let patch = get (obj [ "b", n 1; "a", n 1 ]) (obj [ "a", n 2; "b", n 2 ])

    let replacePaths =
        patch
        |> List.choose (function
            | Replace(p, _) -> Some p
            | _ -> None)

    Assert.Equal<string list>([ "/a"; "/b" ], replacePaths)
    appliesTo patch (obj [ "b", n 1; "a", n 1 ]) (obj [ "a", n 2; "b", n 2 ])

[<Fact>]
let ``get: objects - mixed operations`` () =
    Assert.Equal<Operation list>(
        [ Remove "/b"; Add("/c", n 3); Add("/d", n 4) ],
        get (obj [ "a", n 1; "b", n 2 ]) (obj [ "a", n 1; "c", n 3; "d", n 4 ])
    )

[<Fact>]
let ``get: nested objects`` () =
    Assert.Equal<Operation list>(
        [ Replace("/a/b", n 2) ],
        get (obj [ "a", obj [ "b", n 1 ] ]) (obj [ "a", obj [ "b", n 2 ] ])
    )

    Assert.Equal<Operation list>(
        [ Add("/a/c", n 2) ],
        get (obj [ "a", obj [ "b", n 1 ] ]) (obj [ "a", obj [ "b", n 1; "c", n 2 ] ])
    )

[<Fact>]
let ``get: JSON Pointer escaping in keys`` () =
    Assert.Equal<Operation list>([ Replace("/a~1b", n 2) ], get (obj [ "a/b", n 1 ]) (obj [ "a/b", n 2 ]))
    Assert.Equal<Operation list>([ Replace("/a~0b", n 2) ], get (obj [ "a~b", n 1 ]) (obj [ "a~b", n 2 ]))
    Assert.Equal<Operation list>([ Add("/~01", n 0) ], get (obj []) (obj [ "~1", n 0 ]))
    Assert.Equal<Operation list>([ Replace("/a~01b", n 2) ], get (obj [ "a~1b", n 1 ]) (obj [ "a~1b", n 2 ]))

[<Fact>]
let ``get: structurally equal but distinct values yield empty patch`` () =
    Assert.Equal<Operation list>(
        [],
        get (obj [ "a", n 1; "b", obj [ "c", n 2 ] ]) (obj [ "a", n 1; "b", obj [ "c", n 2 ] ])
    )

[<Fact>]
let ``get: deep nested structures are applicable`` () =
    let oldV =
        obj
            [ "users",
              arr
                  [ obj [ "id", n 1; "name", s "Alice"; "tags", arr [ s "admin" ] ]
                    obj [ "id", n 2; "name", s "Bob"; "tags", arr [] ] ]
              "metadata", obj [ "version", n 1 ] ]

    let newV =
        obj
            [ "users",
              arr
                  [ obj [ "id", n 1; "name", s "Alice"; "tags", arr [ s "admin"; s "moderator" ] ]
                    obj [ "id", n 2; "name", s "Bob"; "tags", arr [] ]
                    obj [ "id", n 3; "name", s "Charlie"; "tags", arr [] ] ]
              "metadata", obj [ "version", n 2 ] ]

    let patch = get oldV newV

    Assert.Equal<Operation list>(
        [ Replace("/metadata/version", n 2)
          Add("/users/0/tags/1", s "moderator")
          Add("/users/2", obj [ "id", n 3; "name", s "Charlie"; "tags", arr [] ]) ],
        patch
    )

    appliesTo patch oldV newV

// ---------------------------------------------------------------------------
// apply
// ---------------------------------------------------------------------------

[<Fact>]
let ``apply: replace`` () =
    Assert.Equal<Json>(n 42, apply [ Replace("", n 42) ] (n 1))
    Assert.Equal<Json>(obj [ "a", n 2 ], apply [ Replace("/a", n 2) ] (obj [ "a", n 1 ]))
    Assert.Equal<Json>(arr [ n 1; n 20; n 3 ], apply [ Replace("/1", n 20) ] (arr [ n 1; n 2; n 3 ]))
    Assert.Equal<Json>(obj [ "a", obj [ "b", n 2 ] ], apply [ Replace("/a/b", n 2) ] (obj [ "a", obj [ "b", n 1 ] ]))

[<Fact>]
let ``apply: add`` () =
    Assert.Equal<Json>(obj [ "a", n 1; "b", n 2 ], apply [ Add("/b", n 2) ] (obj [ "a", n 1 ]))
    Assert.Equal<Json>(arr [ n 1; n 10; n 2; n 3 ], apply [ Add("/1", n 10) ] (arr [ n 1; n 2; n 3 ]))
    Assert.Equal<Json>(arr [ n 1; n 2; n 3; n 4 ], apply [ Add("/-", n 4) ] (arr [ n 1; n 2; n 3 ]))

    Assert.Equal<Json>(
        obj [ "users", arr [ obj [ "id", n 1; "tags", arr [ s "admin" ] ] ] ],
        apply [ Add("/users/0/tags/-", s "admin") ] (obj [ "users", arr [ obj [ "id", n 1; "tags", arr [] ] ] ])
    )

[<Fact>]
let ``apply: remove`` () =
    Assert.Equal<Json>(obj [ "b", n 2 ], apply [ Remove "/a" ] (obj [ "a", n 1; "b", n 2 ]))
    Assert.Equal<Json>(arr [ n 1; n 3 ], apply [ Remove "/1" ] (arr [ n 1; n 2; n 3 ]))
    Assert.Equal<Json>(obj [ "a", obj [ "c", n 2 ] ], apply [ Remove "/a/b" ] (obj [ "a", obj [ "b", n 1; "c", n 2 ] ]))

[<Fact>]
let ``apply: multiple operations in sequence`` () =
    Assert.Equal<Json>(
        obj [ "a", n 10; "c", n 3 ],
        apply [ Add("/c", n 3); Replace("/a", n 10); Remove "/b" ] (obj [ "a", n 1; "b", n 2 ])
    )

[<Fact>]
let ``apply: operations after a root replace`` () =
    Assert.Equal<Json>(obj [ "a", n 1 ], apply [ Replace("", obj []); Add("/a", n 1) ] (obj [ "old", JBool true ]))

[<Fact>]
let ``apply: JSON Pointer decoding`` () =
    Assert.Equal<Json>(obj [ "a/b", n 2 ], apply [ Replace("/a~1b", n 2) ] (obj [ "a/b", n 1 ]))
    Assert.Equal<Json>(obj [ "a~b", n 1 ], apply [ Add("/a~0b", n 1) ] (obj []))
    Assert.Equal<Json>(obj [ "path/to~key", s "value" ], apply [ Add("/path~1to~0key", s "value") ] (obj []))
    Assert.Equal<Json>(obj [ "a~1b", n 1 ], apply [ Add("/a~01b", n 1) ] (obj []))
    Assert.Equal<Json>(obj [ "~1", n 0 ], apply [ Add("/~01", n 0) ] (obj []))

[<Fact>]
let ``apply: sequential path dependencies`` () =
    Assert.Equal<Json>(
        obj [ "new", obj [ "nested", obj [ "value", n 2 ] ] ],
        apply
            [ Add("/new", obj [])
              Add("/new/nested", obj [ "value", n 1 ])
              Replace("/new/nested/value", n 2) ]
            (obj [])
    )

    Assert.Equal<Json>(
        obj [ "items", arr [ n 10; n 2 ] ],
        apply
            [ Add("/items", arr [])
              Add("/items/-", n 1)
              Add("/items/-", n 2)
              Replace("/items/0", n 10) ]
            (obj [])
    )

[<Fact>]
let ``apply: empty patch returns the original`` () =
    let doc = obj [ "a", n 1; "b", n 2 ]
    Assert.Equal<Json>(doc, apply [] doc)
    Assert.True(System.Object.ReferenceEquals(doc, apply [] doc))

[<Fact>]
let ``apply: root replace to different types`` () =
    Assert.Equal<Json>(arr [], apply [ Replace("", arr []) ] (obj [ "a", n 1 ]))
    Assert.Equal<Json>(s "string", apply [ Replace("", s "string") ] (n 42))
    Assert.Equal<Json>(JNull, apply [ Replace("", JNull) ] (JBool true))

// --- error cases ---

[<Fact>]
let ``apply: rejects non-empty pointers that do not start with slash`` () =
    throwsContaining "must start with \"/\"" (fun () -> apply [ Add("invalid", n 1) ] (obj []))

[<Fact>]
let ``apply: rejects invalid array indices`` () =
    throwsContaining "Invalid array index" (fun () -> apply [ Add("/abc", n 1) ] (arr []))
    throwsContaining "Invalid array index" (fun () -> apply [ Replace("/-1", n 1) ] (arr [ n 1; n 2; n 3 ]))

[<Fact>]
let ``apply: rejects out-of-bounds array access`` () =
    throwsContaining "Array index out of bounds" (fun () -> apply [ Replace("/10", n 1) ] (arr [ n 1; n 2; n 3 ]))
    throwsContaining "Array index out of bounds" (fun () -> apply [ Add("/10", n 1) ] (arr [ n 1; n 2; n 3 ]))
    throwsContaining "Array index out of bounds" (fun () -> apply [ Remove "/10" ] (arr [ n 1; n 2; n 3 ]))

[<Fact>]
let ``apply: rejects '-' for replace and remove`` () =
    throwsContaining "\"-\" is not valid for replace" (fun () -> apply [ Replace("/-", n 1) ] (arr [ n 1; n 2; n 3 ]))
    throwsContaining "\"-\" is not valid for remove" (fun () -> apply [ Remove "/-" ] (arr [ n 1; n 2; n 3 ]))

[<Fact>]
let ``apply: rejects replace/remove of non-existent members`` () =
    throwsContaining "does not exist" (fun () -> apply [ Replace("/nonexistent", n 1) ] (obj [ "a", n 1 ]))
    throwsContaining "does not exist" (fun () -> apply [ Remove "/nonexistent" ] (obj [ "a", n 1 ]))

[<Fact>]
let ``apply: rejects add/replace when parent is missing or not a container`` () =
    throwsContaining "Cannot add at" (fun () -> apply [ Add("/a/b", n 1) ] (obj [ "a", JNull ]))
    throwsContaining "Cannot add at" (fun () -> apply [ Add("/a/b", n 1) ] (obj []))
    throwsContaining "not a container" (fun () -> apply [ Add("/a/b", n 1) ] (obj [ "a", s "string" ]))
    throwsContaining "not a container" (fun () -> apply [ Replace("/a/b", n 1) ] (obj [ "a", n 42 ]))

    throwsContaining "not a container" (fun () ->
        apply [ Add("/a/b/c", n 1) ] (obj [ "a", obj [ "b", s "not-object" ] ]))

[<Fact>]
let ``apply: rejects remove at the root`` () =
    throwsContaining "root" (fun () -> apply [ Remove "" ] (obj [ "a", n 1 ]))

// ---------------------------------------------------------------------------
// round-trip
// ---------------------------------------------------------------------------

[<Fact>]
let ``round-trip: apply(get(old, new), old) = new`` () =
    let cases =
        [ obj
              [ "users",
                arr
                    [ obj [ "id", n 1; "name", s "Alice"; "active", JBool true ]
                      obj [ "id", n 2; "name", s "Bob"; "active", JBool false ] ]
                "metadata", obj [ "version", n 1; "tags", arr [ s "v1" ] ] ],
          obj
              [ "users",
                arr
                    [ obj [ "id", n 1; "name", s "Alice Updated"; "active", JBool true ]
                      obj [ "id", n 3; "name", s "Charlie"; "active", JBool true ] ]
                "metadata", obj [ "version", n 2; "tags", arr [ s "v2"; s "latest" ] ] ]
          arr [ n 1; n 2; n 3; n 4; n 5 ], arr [ n 1; n 20; n 3; n 40; n 50; n 60 ]
          obj [ "a", n 1; "b", n 2; "c", n 3 ], obj [ "a", n 10; "d", n 4; "e", n 5 ]
          arr [ n 0; n 1; n 2; n 3; n 4; n 5; n 6; n 7; n 8; n 9 ],
          arr [ n 10; n 1; n 20; n 3; n 40; n 50; n 6; n 70; n 8 ] ]

    for (oldV, newV) in cases do
        appliesTo (get oldV newV) oldV newV

[<Fact>]
let ``round-trip: empty structures`` () =
    let cases =
        [ arr [], arr [ n 1; n 2; n 3 ]
          arr [ n 1; n 2; n 3 ], arr []
          obj [], obj [ "a", n 1; "b", n 2 ]
          obj [ "a", n 1; "b", n 2 ], obj [] ]

    for (oldV, newV) in cases do
        appliesTo (get oldV newV) oldV newV

[<Fact>]
let ``round-trip: escaped keys`` () =
    let oldV = obj [ "key/with/slash", n 1; "key~with~tilde", n 2; "normal", n 3 ]

    let newV =
        obj
            [ "key/with/slash", n 10
              "key~with~tilde", n 20
              "normal", n 30
              "new/key", n 40 ]

    appliesTo (get oldV newV) oldV newV

[<Fact>]
let ``round-trip: type changes`` () =
    let cases =
        [ n 1, s "string"
          s "string", n 42
          JBool true, JNull
          JNull, arr []
          arr [], obj []
          obj [], arr [] ]

    for (oldV, newV) in cases do
        appliesTo (get oldV newV) oldV newV
