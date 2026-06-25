
import { Record } from "../../fable_modules/fable-library-js.5.4.0/Types.js";
import { record_type } from "../../fable_modules/fable-library-js.5.4.0/Reflection.js";
import { equals } from "../../fable_modules/fable-library-js.5.4.0/Util.js";

/**
 * Port of repos/effect-smol/packages/effect/src/MutableRef.ts — a synchronous
 * mutable reference cell holding one current value.
 * 
 * Unlike `Ref` (effectful, fiber-safe), a `MutableRef` is plain in-place state:
 * read/write `.Current` directly, or use these helpers for read-modify-write.
 * 
 * `compareAndSet` uses F#'s built-in structural equality (`=`) rather than the
 * `Equal` module (decoupling rule). Omissions (JS-runtime-only): the
 * `toJSON`/inspection hooks and the `pipe`/dual data-last currying — every
 * function takes `self` first.
 */
export class MutableRef$1 extends Record {
    constructor(Current) {
        super();
        this.Current = Current;
    }
}

export function MutableRef$1_$reflection(gen0) {
    return record_type("Effect.MutableRef`1", [gen0], MutableRef$1, () => [["Current", gen0]]);
}

/**
 * Creates a reference holding `value`. (MutableRef.make)
 */
export function MutableRef_make(value) {
    return new MutableRef$1(value);
}

/**
 * Reads the current value. (MutableRef.get)
 */
export function MutableRef_get(self) {
    return self.Current;
}

/**
 * Sets a new value, returning the same reference. (MutableRef.set)
 */
export function MutableRef_set(self, value) {
    self.Current = value;
    return self;
}

/**
 * Sets a new value, returning that value. (MutableRef.setAndGet)
 */
export function MutableRef_setAndGet(self, value) {
    self.Current = value;
    return self.Current;
}

/**
 * Sets a new value, returning the previous one. (MutableRef.getAndSet)
 */
export function MutableRef_getAndSet(self, value) {
    const previous = self.Current;
    self.Current = value;
    return previous;
}

/**
 * Applies `f` to the current value, returning the same reference.
 * (MutableRef.update)
 */
export function MutableRef_update(self, f) {
    return MutableRef_set(self, f(self.Current));
}

/**
 * Applies `f` to the current value, returning the new value.
 * (MutableRef.updateAndGet)
 */
export function MutableRef_updateAndGet(self, f) {
    return MutableRef_setAndGet(self, f(self.Current));
}

/**
 * Applies `f` to the current value, returning the previous one.
 * (MutableRef.getAndUpdate)
 */
export function MutableRef_getAndUpdate(self, f) {
    return MutableRef_getAndSet(self, f(self.Current));
}

/**
 * Sets `newValue` only if the current value structurally equals `oldValue`;
 * returns whether the swap happened. (MutableRef.compareAndSet)
 */
export function MutableRef_compareAndSet(self, oldValue, newValue) {
    if (equals(oldValue, self.Current)) {
        self.Current = newValue;
        return true;
    }
    else {
        return false;
    }
}

/**
 * Increments by 1, returning the same reference. (MutableRef.increment)
 */
export function MutableRef_increment(self) {
    return MutableRef_update(self, (n) => ((n + 1) | 0));
}

/**
 * Decrements by 1, returning the same reference. (MutableRef.decrement)
 */
export function MutableRef_decrement(self) {
    return MutableRef_update(self, (n) => ((n - 1) | 0));
}

/**
 * Increments by 1, returning the new value. (MutableRef.incrementAndGet)
 */
export function MutableRef_incrementAndGet(self) {
    return MutableRef_updateAndGet(self, (n) => ((n + 1) | 0)) | 0;
}

/**
 * Decrements by 1, returning the new value. (MutableRef.decrementAndGet)
 */
export function MutableRef_decrementAndGet(self) {
    return MutableRef_updateAndGet(self, (n) => ((n - 1) | 0)) | 0;
}

/**
 * Increments by 1, returning the previous value. (MutableRef.getAndIncrement)
 */
export function MutableRef_getAndIncrement(self) {
    return MutableRef_getAndUpdate(self, (n) => ((n + 1) | 0)) | 0;
}

/**
 * Decrements by 1, returning the previous value. (MutableRef.getAndDecrement)
 */
export function MutableRef_getAndDecrement(self) {
    return MutableRef_getAndUpdate(self, (n) => ((n - 1) | 0)) | 0;
}

/**
 * Flips a boolean reference, returning the same reference. (MutableRef.toggle)
 */
export function MutableRef_toggle(self) {
    return MutableRef_update(self, (value) => !value);
}

