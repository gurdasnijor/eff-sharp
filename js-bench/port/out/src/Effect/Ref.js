
import { Record } from "../../fable_modules/fable-library-js.5.4.0/Types.js";
import { MutableRef_make, MutableRef$1_$reflection } from "./MutableRef.js";
import { record_type } from "../../fable_modules/fable-library-js.5.4.0/Reflection.js";
import { Effect_sync } from "./Effect.js";
import { defaultArg, value as value_1 } from "../../fable_modules/fable-library-js.5.4.0/Option.js";

/**
 * Port of repos/effect-smol/packages/effect/src/Ref.ts — fiber-safe mutable
 * state for `Effect` programs.
 * 
 * A `Ref<'A>` holds one value and exposes reads, writes and atomic
 * transformations as effects. Each operation is a single `Effect.sync` thunk,
 * so the whole read-modify-write happens in one synchronous step (atomic with
 * respect to the cooperative async core).
 * 
 * Backing (per the w2-ref decoupling note): the cell is a merged
 * `MutableRef` (an atomic-ish `{ mutable Current }`) rather than a new
 * primitive. Operations never fail and need no environment, so they stay
 * generic in `'E`/`'R`.
 * 
 * Omissions vs upstream (noted per CONVENTIONS):
 * - HKT/variance plumbing (`Ref.Variance`, `Invariant`) — F# has no HKTs.
 * - JS-runtime machinery: `RefProto`, `PipeInspectableProto`, `toJSON`, the
 * `pipe`/`dual` data-last currying. Every function takes `self` first,
 * mirroring `MutableRef`.
 * - `Option` is F#'s native `option`; the `_tag === "Some"` checks lower into
 * `match`.
 */
export class Ref$1 extends Record {
    constructor(Cell) {
        super();
        this.Cell = Cell;
    }
}

export function Ref$1_$reflection(gen0) {
    return record_type("Effect.Ref`1", [gen0], Ref$1, () => [["Cell", MutableRef$1_$reflection(gen0)]]);
}

/**
 * Creates a `Ref` holding `value`, outside the `Effect` context.
 * (Ref.makeUnsafe)
 */
export function Ref_makeUnsafe(value) {
    return new Ref$1(MutableRef_make(value));
}

/**
 * Creates a `Ref` holding `value`, wrapped in an `Effect`. (Ref.make)
 */
export function Ref_make(value) {
    return Effect_sync(() => Ref_makeUnsafe(value));
}

/**
 * Reads the current value, outside the `Effect` context. (Ref.getUnsafe)
 */
export function Ref_getUnsafe(self) {
    return self.Cell.Current;
}

/**
 * Reads the current value. (Ref.get)
 */
export function Ref_get(self) {
    return Effect_sync(() => self.Cell.Current);
}

/**
 * Replaces the current value. (Ref.set)
 */
export function Ref_set(self, value) {
    return Effect_sync(() => {
        self.Cell.Current = value;
    });
}

/**
 * Replaces the value, returning the previous one. (Ref.getAndSet)
 */
export function Ref_getAndSet(self, value) {
    return Effect_sync(() => {
        const current = self.Cell.Current;
        self.Cell.Current = value;
        return current;
    });
}

/**
 * Updates the value with `f`, returning the previous one. (Ref.getAndUpdate)
 */
export function Ref_getAndUpdate(self, f) {
    return Effect_sync(() => {
        const current = self.Cell.Current;
        self.Cell.Current = f(current);
        return current;
    });
}

/**
 * Conditionally updates the value, returning the previous one. `Some` stores
 * the new value; `None` leaves it unchanged. (Ref.getAndUpdateSome)
 */
export function Ref_getAndUpdateSome(self, pf) {
    return Effect_sync(() => {
        const current = self.Cell.Current;
        const matchValue = pf(current);
        if (matchValue == null) {
        }
        else {
            const next = value_1(matchValue);
            self.Cell.Current = next;
        }
        return current;
    });
}

/**
 * Replaces the value, returning the new one. (Ref.setAndGet)
 */
export function Ref_setAndGet(self, value) {
    return Effect_sync(() => {
        self.Cell.Current = value;
        return value;
    });
}

/**
 * Computes `[result, newValue]` from the current value, stores `newValue`
 * and returns `result`. (Ref.modify)
 */
export function Ref_modify(self, f) {
    return Effect_sync(() => {
        const patternInput = f(self.Cell.Current);
        self.Cell.Current = patternInput[1];
        return patternInput[0];
    });
}

/**
 * Computes `[result, option]`; `Some` stores the new value, `None` leaves it
 * unchanged. Always returns `result`. (Ref.modifySome)
 */
export function Ref_modifySome(self, pf) {
    return Ref_modify(self, (value) => {
        const patternInput = pf(value);
        return [patternInput[0], defaultArg(patternInput[1], value)];
    });
}

/**
 * Updates the value with `f`. (Ref.update)
 */
export function Ref_update(self, f) {
    return Effect_sync(() => {
        self.Cell.Current = f(self.Cell.Current);
    });
}

/**
 * Updates the value with `f`, returning the new value. (Ref.updateAndGet)
 */
export function Ref_updateAndGet(self, f) {
    return Effect_sync(() => {
        self.Cell.Current = f(self.Cell.Current);
        return self.Cell.Current;
    });
}

/**
 * Conditionally updates the value. `Some` stores the new value; `None`
 * leaves it unchanged. (Ref.updateSome)
 */
export function Ref_updateSome(self, pf) {
    return Effect_sync(() => {
        const matchValue = pf(self.Cell.Current);
        if (matchValue == null) {
        }
        else {
            const next = value_1(matchValue);
            self.Cell.Current = next;
        }
    });
}

/**
 * Conditionally updates the value, returning the resulting current value.
 * (Ref.updateSomeAndGet)
 */
export function Ref_updateSomeAndGet(self, pf) {
    return Effect_sync(() => {
        const matchValue = pf(self.Cell.Current);
        if (matchValue == null) {
        }
        else {
            const next = value_1(matchValue);
            self.Cell.Current = next;
        }
        return self.Cell.Current;
    });
}

