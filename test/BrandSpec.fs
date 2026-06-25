module BrandSpec

open Effect
open Effect.Vitest

let private isInteger (n: float) : Result<unit, string> =
    if n = System.Math.Truncate n then
        Ok()
    else
        Error(sprintf "Expected %g to be an integer" n)

describe "Brand" (fun () ->
    test "creates nominal brands without runtime validation" (fun () ->
        let myNumber = Brand.nominal<float> ()
        toBe (myNumber.Construct 1.0) 1.0
        toBe (myNumber.Construct 1.1) 1.1
        toBe (myNumber.Is 1.0) true
        toBe (myNumber.Is 1.1) true
        toEqual (myNumber.Option 1.0) (Some 1.0)
        toEqual (myNumber.Option 1.1) (Some 1.1)
        toEqual (myNumber.Result 1.0) (Ok 1.0)
        toEqual (myNumber.Result 1.1) (Ok 1.1))

    test "validates custom predicates created with make" (fun () ->
        let int' = Brand.make isInteger
        toBe (int'.Construct 1.0) 1.0
        toThrow (fun () -> int'.Construct 1.1 |> ignore)
        toBe (int'.Construct -1.0) -1.0
        toBe (int'.Is 1.0) true
        toBe (int'.Is 1.1) false
        toBe (int'.Is -1.0) true
        toEqual (int'.Option 1.0) (Some 1.0)
        toEqual (int'.Option 1.1) None
        toEqual (int'.Option -1.0) (Some -1.0)
        toEqual (int'.Result 1.0) (Ok 1.0)
        toEqual (int'.Result -1.0) (Ok -1.0))

    test "make failures carry the predicate message" (fun () ->
        let int' = Brand.make isInteger

        match int'.Result 1.1 with
        | Error e -> toBe e.Message "Expected 1.1 to be an integer"
        | Ok _ -> failwith "expected failure")

    test "BrandError formats as BrandError(message)" (fun () ->
        let int' = Brand.make isInteger

        match int'.Result 1.1 with
        | Error e -> toBe (string e) "BrandError(Expected 1.1 to be an integer)"
        | Ok _ -> failwith "expected failure"))

