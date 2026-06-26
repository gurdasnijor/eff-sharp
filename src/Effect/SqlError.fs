namespace Effect

/// Structured SQL failures used by driver integrations.
type SqlErrorReason =
    | ConnectionError of cause: obj option * message: string option * operation: string option
    | AuthenticationError of cause: obj option * message: string option * operation: string option
    | AuthorizationError of cause: obj option * message: string option * operation: string option
    | SqlSyntaxError of cause: obj option * message: string option * operation: string option
    | UniqueViolation of cause: obj option * message: string option * operation: string option * constraintName: string
    | ConstraintError of cause: obj option * message: string option * operation: string option
    | DeadlockError of cause: obj option * message: string option * operation: string option
    | SerializationError of cause: obj option * message: string option * operation: string option
    | LockTimeoutError of cause: obj option * message: string option * operation: string option
    | StatementTimeoutError of cause: obj option * message: string option * operation: string option
    | UnknownError of cause: obj option * message: string option * operation: string option

type SqlError = { Reason: SqlErrorReason }

type ResultLengthMismatch =
    { Expected: int
      Actual: int }

[<RequireQualifiedAccess>]
module SqlError =

    let private fields reason =
        match reason with
        | ConnectionError(cause, message, operation)
        | AuthenticationError(cause, message, operation)
        | AuthorizationError(cause, message, operation)
        | SqlSyntaxError(cause, message, operation)
        | ConstraintError(cause, message, operation)
        | DeadlockError(cause, message, operation)
        | SerializationError(cause, message, operation)
        | LockTimeoutError(cause, message, operation)
        | StatementTimeoutError(cause, message, operation)
        | UnknownError(cause, message, operation) -> cause, message, operation
        | UniqueViolation(cause, message, operation, _) -> cause, message, operation

    let private tag reason =
        match reason with
        | ConnectionError _ -> "ConnectionError"
        | AuthenticationError _ -> "AuthenticationError"
        | AuthorizationError _ -> "AuthorizationError"
        | SqlSyntaxError _ -> "SqlSyntaxError"
        | UniqueViolation _ -> "UniqueViolation"
        | ConstraintError _ -> "ConstraintError"
        | DeadlockError _ -> "DeadlockError"
        | SerializationError _ -> "SerializationError"
        | LockTimeoutError _ -> "LockTimeoutError"
        | StatementTimeoutError _ -> "StatementTimeoutError"
        | UnknownError _ -> "UnknownError"

    let make reason : SqlError = { Reason = reason }

    let message (error: SqlError) : string =
        let _, msg, _ = fields error.Reason
        defaultArg msg (tag error.Reason)

    let cause (error: SqlError) : obj option =
        let c, _, _ = fields error.Reason
        c

    let operation (error: SqlError) : string option =
        let _, _, op = fields error.Reason
        op

    let isRetryableReason reason =
        match reason with
        | ConnectionError _
        | DeadlockError _
        | SerializationError _
        | LockTimeoutError _
        | StatementTimeoutError _ -> true
        | AuthenticationError _
        | AuthorizationError _
        | SqlSyntaxError _
        | UniqueViolation _
        | ConstraintError _
        | UnknownError _ -> false

    let isRetryable error = isRetryableReason error.Reason

    let connection cause message operation =
        make (ConnectionError(cause, message, operation))

    let authentication cause message operation =
        make (AuthenticationError(cause, message, operation))

    let authorization cause message operation =
        make (AuthorizationError(cause, message, operation))

    let syntax cause message operation =
        make (SqlSyntaxError(cause, message, operation))

    let unique cause message operation constraintName =
        make (UniqueViolation(cause, message, operation, constraintName))

    let constraintError cause message operation =
        make (ConstraintError(cause, message, operation))

    let deadlock cause message operation =
        make (DeadlockError(cause, message, operation))

    let serialization cause message operation =
        make (SerializationError(cause, message, operation))

    let lockTimeout cause message operation =
        make (LockTimeoutError(cause, message, operation))

    let statementTimeout cause message operation =
        make (StatementTimeoutError(cause, message, operation))

    let unknown cause message operation =
        make (UnknownError(cause, message, operation))

    let isSqlError (u: obj) : bool =
        match u with
        | :? SqlError -> true
        | _ -> false

    let isSqlErrorReason (u: obj) : bool =
        match u with
        | :? SqlErrorReason -> true
        | _ -> false

    let resultLengthMismatch expected actual =
        { Expected = expected; Actual = actual }

    let resultLengthMismatchMessage mismatch =
        sprintf "Expected %d results but got %d" mismatch.Expected mismatch.Actual
