
import { toString, Record, Union } from "../../fable_modules/fable-library-js.5.4.0/Types.js";
import { record_type, list_type, union_type, option_type, int32_type, obj_type } from "../../fable_modules/fable-library-js.5.4.0/Reflection.js";
import { append, exists, fold, isEmpty, map, choose, singleton, empty } from "../../fable_modules/fable-library-js.5.4.0/List.js";
import { some } from "../../fable_modules/fable-library-js.5.4.0/Option.js";
import { int32ToString, defaultOf, equals } from "../../fable_modules/fable-library-js.5.4.0/Util.js";
import { join, printf, toText } from "../../fable_modules/fable-library-js.5.4.0/String.js";

/**
 * Slice 2: `Cause` — the full reason an effect failed.
 * 
 * Port of repos/effect-smol/packages/effect/src/Cause.ts. A `Cause` is a list
 * of `Reason`s, each being one of:
 * Fail      - a typed, expected error of type 'E
 * Die       - an unexpected defect (Effect models this as `unknown`; here a
 * boxed value / exception)
 * Interrupt - fiber interruption, carrying an optional fiber id
 * 
 * The JS runtime type-guards (`isCause`/`isReason` over arbitrary values) are
 * intentionally omitted: they exist because TS is structurally typed at
 * runtime, whereas F#'s static types make them unnecessary. The reason-level
 * guards (`isFailReason` etc.) are kept since they narrow within a `Cause`.
 * A single reason contributing to a `Cause`.
 */
export class Reason$1 extends Union {
    constructor(tag, fields) {
        super();
        this.tag = tag;
        this.fields = fields;
    }
    cases() {
        return ["Fail", "Die", "Interrupt"];
    }
}

export function Reason$1_$reflection(gen0) {
    return union_type("Effect.Reason`1", [gen0], Reason$1, () => [[["error", gen0]], [["defect", obj_type]], [["fiberId", option_type(int32_type)]]]);
}

/**
 * The aggregate of all reasons an effect failed.
 */
export class Cause$1 extends Record {
    constructor(Reasons) {
        super();
        this.Reasons = Reasons;
    }
}

export function Cause$1_$reflection(gen0) {
    return record_type("Effect.Cause`1", [gen0], Cause$1, () => [["Reasons", list_type(Reason$1_$reflection(gen0))]]);
}

export function Cause_makeFailReason(error) {
    return new Reason$1(0, [error]);
}

export function Cause_makeDieReason(defect) {
    return new Reason$1(1, [defect]);
}

export function Cause_makeInterruptReason(fiberId) {
    return new Reason$1(2, [fiberId]);
}

/**
 * A cause with no reasons.
 */
export function Cause_empty() {
    return new Cause$1(empty());
}

/**
 * A cause from a single typed failure.
 */
export function Cause_fail(error) {
    return new Cause$1(singleton(new Reason$1(0, [error])));
}

/**
 * A cause from a single defect.
 */
export function Cause_die(defect) {
    return new Cause$1(singleton(new Reason$1(1, [defect])));
}

/**
 * A cause from a single fiber interruption.
 */
export function Cause_interrupt(fiberId) {
    return new Cause$1(singleton(new Reason$1(2, [fiberId])));
}

export function Cause_isFailReason(reason) {
    if (reason.tag === 0) {
        return true;
    }
    else {
        return false;
    }
}

export function Cause_isDieReason(reason) {
    if (reason.tag === 1) {
        return true;
    }
    else {
        return false;
    }
}

export function Cause_isInterruptReason(reason) {
    if (reason.tag === 2) {
        return true;
    }
    else {
        return false;
    }
}

/**
 * The typed failures within a cause, in order.
 */
export function Cause_failures(cause) {
    return choose((_arg) => {
        if (_arg.tag === 0) {
            return some(_arg.fields[0]);
        }
        else {
            return undefined;
        }
    }, cause.Reasons);
}

/**
 * The defects within a cause, in order.
 */
export function Cause_defects(cause) {
    return choose((_arg) => {
        if (_arg.tag === 1) {
            return some(_arg.fields[0]);
        }
        else {
            return undefined;
        }
    }, cause.Reasons);
}

/**
 * Transform the typed errors within a cause, leaving Die/Interrupt reasons
 * untouched. A cause with no `Fail` reason is returned with its reasons
 * intact (only the phantom error type changes). (Cause.map — upstream
 * `causeMap`.)
 */
export function Cause_map(f, cause) {
    return new Cause$1(map((reason) => {
        switch (reason.tag) {
            case 1:
                return new Reason$1(1, [reason.fields[0]]);
            case 2:
                return new Reason$1(2, [reason.fields[0]]);
            default:
                return new Reason$1(0, [f(reason.fields[0])]);
        }
    }, cause.Reasons));
}

/**
 * Merge two causes: `first`'s reasons followed by `second`'s, de-duplicated
 * by value equality (upstream `Arr.union`). Combining with `empty` returns
 * the other cause unchanged. (Cause.combine — upstream `causeCombine`.)
 */
export function Cause_combine(first, second) {
    const matchValue = first.Reasons;
    const matchValue_1 = second.Reasons;
    if (isEmpty(matchValue)) {
        return second;
    }
    else if (isEmpty(matchValue_1)) {
        return first;
    }
    else {
        return new Cause$1(fold((acc, reason) => {
            if (exists((seen) => equals(seen, reason), acc)) {
                return acc;
            }
            else {
                return append(acc, singleton(reason));
            }
        }, empty(), append(matchValue, matchValue_1)));
    }
}

/**
 * Render a value the way Effect's toString does: strings quoted, null as
 * `undefined`, everything else via its own string form.
 */
export function Cause_renderValue(value) {
    if (equals(value, defaultOf())) {
        return "undefined";
    }
    else if (typeof value === "string") {
        return ("\"" + value) + "\"";
    }
    else {
        return toString(value);
    }
}

export function Cause_renderReason(reason) {
    switch (reason.tag) {
        case 1: {
            const arg_1 = Cause_renderValue(reason.fields[0]);
            return toText(printf("Die(%s)"))(arg_1);
        }
        case 2: {
            const id = reason.fields[0];
            const inner = (id == null) ? "undefined" : int32ToString(id);
            return toText(printf("Interrupt(%s)"))(inner);
        }
        default: {
            const arg = Cause_renderValue(reason.fields[0]);
            return toText(printf("Fail(%s)"))(arg);
        }
    }
}

/**
 * `Cause([Fail("error"), ...])`
 */
export function Cause_render(cause) {
    const arg = join(", ", map(Cause_renderReason, cause.Reasons));
    return toText(printf("Cause([%s])"))(arg);
}

