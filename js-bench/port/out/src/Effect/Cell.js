
import { Record } from "../../fable_modules/fable-library-js.5.4.0/Types.js";
import { record_type, array_type, lambda_type, unit_type, option_type } from "../../fable_modules/fable-library-js.5.4.0/Reflection.js";
import { fromContinuations, start } from "../../fable_modules/fable-library-js.5.4.0/Async.js";
import { singleton } from "../../fable_modules/fable-library-js.5.4.0/AsyncBuilder.js";
import { Operators_Lock } from "../../fable_modules/fable-library-js.5.4.0/FSharp.Core.js";
import { value, some } from "../../fable_modules/fable-library-js.5.4.0/Option.js";
import { clear } from "../../fable_modules/fable-library-js.5.4.0/Util.js";
import { item } from "../../fable_modules/fable-library-js.5.4.0/Array.js";

/**
 * One-shot completion cell — the Fable-portable replacement for
 * `System.Threading.Tasks.TaskCompletionSource`.
 * 
 * Why this exists: the `Effect` core is already a reader into `Async<Exit>`, so
 * "suspend a fiber until X happens" only needs an `Async<'T>` that resolves
 * later. `TaskCompletionSource` provided that on .NET, but Fable's runtime
 * implements neither `TaskCompletionSource` nor `Async.AwaitTask`. A `Cell`
 * rebuilds the same one-shot, first-writer-wins, multi-waiter semantics on
 * `Async.FromContinuations` (which Fable *does* implement), so a single
 * implementation compiles on both targets with no `#if`.
 * 
 * Thread-safety: `lock` guards the check-and-register (`await`) against the
 * complete-and-drain (`trySet`). On .NET that lock is real; under Fable it is a
 * verified no-op and single-threaded JS makes the critical sections free. This
 * is the keystone primitive behind `Deferred`, `Semaphore`, and `PubSub`.
 */
export class Cell$1 extends Record {
    constructor(Value, Waiters) {
        super();
        this.Value = Value;
        this.Waiters = Waiters;
    }
}

export function Cell$1_$reflection(gen0) {
    return record_type("Effect.Cell`1", [gen0], Cell$1, () => [["Value", option_type(gen0)], ["Waiters", array_type(lambda_type(gen0, unit_type))]]);
}

/**
 * Allocate an empty cell.
 */
export function Cell_make() {
    return new Cell$1(undefined, []);
}

/**
 * Whether the cell has been completed.
 */
export function Cell_isDone(c) {
    return c.Value != null;
}

/**
 * The stored value, if completed. (Backs `Deferred.poll`.)
 */
export function Cell_value(c) {
    return c.Value;
}

function Cell_resume(ok, v) {
    start(singleton.Delay(() => {
        ok(v);
        return singleton.Zero();
    }));
}

/**
 * Attempt to complete the cell. First writer wins: returns `true` iff this
 * call filled it, `false` if it was already completed. Drains all waiters.
 */
export function Cell_trySet(c, v) {
    return Operators_Lock(c, () => {
        const matchValue = c.Value;
        if (matchValue == null) {
            c.Value = some(v);
            const pending = c.Waiters.slice();
            clear(c.Waiters);
            for (let idx = 0; idx <= (pending.length - 1); idx++) {
                Cell_resume(item(idx, pending), v);
            }
            return true;
        }
        else {
            return false;
        }
    });
}

/**
 * Suspend until the cell is completed. If already complete, resolves with the
 * stored value on the caller's own stack (no thread hop needed — there is no
 * completer to protect).
 */
export function Cell_await(c) {
    return fromContinuations((tupledArg) => {
        const ok = tupledArg[0];
        Operators_Lock(c, () => {
            const matchValue = c.Value;
            if (matchValue == null) {
                void (c.Waiters.push(ok));
            }
            else {
                ok(value(matchValue));
            }
        });
    });
}

