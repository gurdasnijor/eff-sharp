module SchemaASTSpec

open Effect
open Effect.Vitest

describe "SchemaAST compatibility helpers" (fun () ->
    test "resolveAt reads annotations through wrapper nodes" (fun () ->
        let schema = Schema.String |> Schema.isMinLength 1 |> Schema.annotate "primaryKey" true

        toBe (SchemaAST.resolveAt<bool> "primaryKey" schema.Ast) (Some true)
        toBe (SchemaAST.resolveAt<bool> "missing" schema.Ast) None)

    test "toType and primitive predicates strip refinements and annotations" (fun () ->
        let schema = Schema.String |> Schema.isMinLength 1 |> Schema.annotate "x" "y"
        let ast = SchemaAST.toType schema.Ast

        toBe (SchemaAST.isString ast) true
        toBe (SchemaAST.isNumber ast) false
        toBe (SchemaAST.isBoolean Schema.Boolean.Ast) true)

    test "literal and union predicates expose state-inspection primitives" (fun () ->
        let literal = Schema.Literal (JString "journaled")
        let union = Schema.Union [ Schema.Literal (JString "insert"); Schema.Literal (JString "delete") ]

        toBe (SchemaAST.isLiteral literal.Ast) true
        match SchemaAST.literalValue literal.Ast with
        | Some(JString value) -> toBe value "journaled"
        | other -> failwithf "expected literal string, got %A" other
        toBe (SchemaAST.isUnion union.Ast) true))
