module DataSpec

open Effect
open Effect.Vitest

type private Person = { Name: string; Age: int }

type private Shape =
    | Circle of radius: float
    | Rect of width: float * height: float

type private MyError(code: int, message: string) =
    inherit Data.Error(message)
    member _.Code = code

type private NotFound(resource: string) =
    inherit Data.TaggedError("NotFound")
    member _.Resource = resource

describe "Data" (fun () ->
    test "records assign fields and compare structurally" (fun () ->
        let p = { Name = "Alice"; Age = 30 }
        toBe p.Name "Alice"
        toBe p.Age 30
        toBe (Data.equals { Name = "Bob"; Age = 25 } { Name = "Bob"; Age = 25 }) true
        toBe (Data.equals { Name = "Bob"; Age = 25 } { Name = "Bob"; Age = 26 }) false)

    test "tagged enums carry tags and fields" (fun () ->
        let c = Circle 5.0
        toBe (Data.tagOf c) "Circle"

        match c with
        | Circle r -> toBe r 5.0
        | _ -> failwith "expected Circle"

        let r = Rect(3.0, 4.0)
        toBe (Data.tagOf r) "Rect"

        match r with
        | Rect(w, h) ->
            toBe w 3.0
            toBe h 4.0
        | _ -> failwith "expected Rect")

    test "isTag and native matching" (fun () ->
        toBe (Data.isTag "Circle" (Circle 1.0)) true
        toBe (Data.isTag "Circle" (Rect(1.0, 1.0))) false

        let area =
            function
            | Circle radius -> System.Math.PI * radius ** 2.0
            | Rect(width, height) -> width * height

        toBe (area (Circle 5.0)) (System.Math.PI * 25.0)
        toBe (area (Rect(3.0, 4.0))) 12.0
        toBe (Data.equals (Circle 1.0) (Rect(1.0, 1.0))) false)

    test "Error and TaggedError carry fields" (fun () ->
        let e = MyError(404, "not found")
        toBe e.Code 404
        toBe e.Message "not found"

        let nf = NotFound("/users")
        toBe nf.Tag "NotFound"
        toBe nf.Resource "/users"

        let a = NotFound("x") :> Data.TaggedError
        let b = Data.TaggedError("Forbidden")
        toBe (a.Tag <> b.Tag) true))

