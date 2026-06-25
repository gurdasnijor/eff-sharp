
import { Union } from "../../fable_modules/fable-library-js.5.4.0/Types.js";
import { Cause_renderValue, Cause_render, Cause_failures, Cause_interrupt, Cause_die, Cause_fail, Cause$1_$reflection } from "./Cause.js";
import { union_type } from "../../fable_modules/fable-library-js.5.4.0/Reflection.js";
import { tryHead } from "../../fable_modules/fable-library-js.5.4.0/List.js";
import { FSharpResult$2 } from "../../fable_modules/fable-library-js.5.4.0/Result.js";
import { printf, toText } from "../../fable_modules/fable-library-js.5.4.0/String.js";

/**
 * Slice 2: `Exit` — the result of running an effect to completion.
 * 
 * Port of repos/effect-smol/packages/effect/src/Exit.ts. An `Exit<'A,'E>` is
 * either a `Success` carrying the value, or a `Failure` carrying the `Cause`
 * of the failure. It is the bridge between the running world (`Effect`) and a
 * fully-evaluated outcome.
 * The completed outcome of an effect.
 */
export class Exit$2 extends Union {
    constructor(tag, fields) {
        super();
        this.tag = tag;
        this.fields = fields;
    }
    cases() {
        return ["Success", "Failure"];
    }
}

export function Exit$2_$reflection(gen0, gen1) {
    return union_type("Effect.Exit`2", [gen0, gen1], Exit$2, () => [[["value", gen0]], [["cause", Cause$1_$reflection(gen1)]]]);
}

/**
 * A successful exit. (Exit.succeed)
 */
export function Exit_succeed(value) {
    return new Exit$2(0, [value]);
}

/**
 * A failed exit from a typed error. (Exit.fail)
 */
export function Exit_fail(error) {
    return new Exit$2(1, [Cause_fail(error)]);
}

/**
 * A failed exit from a defect. (Exit.die)
 */
export function Exit_die(defect) {
    return new Exit$2(1, [Cause_die(defect)]);
}

/**
 * A failed exit from fiber interruption. (Exit.interrupt)
 */
export function Exit_interrupt(fiberId) {
    return new Exit$2(1, [Cause_interrupt(fiberId)]);
}

/**
 * A failed exit from an arbitrary cause. (Exit.failCause)
 */
export function Exit_failCause(cause) {
    return new Exit$2(1, [cause]);
}

export function Exit_isSuccess(exit) {
    if (exit.tag === 1) {
        return false;
    }
    else {
        return true;
    }
}

export function Exit_isFailure(exit) {
    if (exit.tag === 0) {
        return false;
    }
    else {
        return true;
    }
}

/**
 * Fold an exit into a single value.
 */
export function Exit_matchExit(onFailure, onSuccess, exit) {
    if (exit.tag === 1) {
        return onFailure(exit.fields[0]);
    }
    else {
        return onSuccess(exit.fields[0]);
    }
}

/**
 * Bridge to/from an F# `Result`, collapsing a failure to its first typed
 * error (defects/interrupts have no `Result` representation).
 */
export function Exit_toResult(exit) {
    if (exit.tag === 1) {
        return new FSharpResult$2(1, [tryHead(Cause_failures(exit.fields[0]))]);
    }
    else {
        return new FSharpResult$2(0, [exit.fields[0]]);
    }
}

/**
 * `Success(1)` / `Failure(Cause([Fail("error")]))`
 */
export function Exit_render(exit) {
    if (exit.tag === 1) {
        const arg_1 = Cause_render(exit.fields[0]);
        return toText(printf("Failure(%s)"))(arg_1);
    }
    else {
        const arg = Cause_renderValue(exit.fields[0]);
        return toText(printf("Success(%s)"))(arg);
    }
}

