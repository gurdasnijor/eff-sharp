module CellSpec

open Effect
open Effect.Vitest

describe "Cell" (fun () ->
    test "trySet is first-writer-wins and stores the completed value" (fun () ->
        let cell: Cell<int> = Cell.make ()

        toBe (Cell.isDone cell) false
        toEqual (Cell.value cell) None
        toBe (Cell.trySet cell 1) true
        toBe (Cell.trySet cell 2) false
        toBe (Cell.isDone cell) true
        toEqual (Cell.value cell) (Some 1))

    itEffect "await resumes waiters once the cell is completed" (fun () ->
        let cell: Cell<int> = Cell.make ()
        let seen = ResizeArray<int>()

        Async.Start(
            async {
                let! value = Cell.await cell
                seen.Add value
            }
        )

        Effect.sync (fun () -> toBe (Cell.trySet cell 42) true)
        |> Effect.zipRight (Effect.sleep 0)
        |> Effect.map (fun () -> toEqual (List.ofSeq seen) [ 42 ])))
