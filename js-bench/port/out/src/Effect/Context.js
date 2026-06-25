
import { Record } from "../../fable_modules/fable-library-js.5.4.0/Types.js";
import { class_type, obj_type, record_type, string_type } from "../../fable_modules/fable-library-js.5.4.0/Reflection.js";
import { fold, containsKey, find, tryFind, add, empty } from "../../fable_modules/fable-library-js.5.4.0/Map.js";
import { comparePrimitives } from "../../fable_modules/fable-library-js.5.4.0/Util.js";
import { map } from "../../fable_modules/fable-library-js.5.4.0/Option.js";

/**
 * `Context` — the dependency-injection (DI) service map.
 * 
 * Port of repos/effect-smol/packages/effect/src/Context.ts, **decoupled from
 * upstream's internals**. Upstream's `Context` imports `Effectable` (which pulls
 * in `Fiber`); per the slice brief we do NOT follow that. In F# a `Context` is
 * just a typed, immutable map from a service `Tag` to its instance — it needs
 * nothing from `Effect`, so it compiles *before* `Effect.fs` and the async core
 * wires service access on top (`Effect.service` / `Effect.provideService`).
 * 
 * Omissions vs upstream (per CONVENTIONS):
 * * The HKT / `Effectable` / `Pipeable` / `Inspectable` plumbing is dropped.
 * * The JS runtime guard `isKey`/`isContext` over arbitrary values is dropped
 * (F#'s static types make it dead weight).
 * * `Reference` (a key with a built-in default) is not ported in this slice;
 * `Tag` + `Effect.service` cover the required surface.
 * A typed key identifying a service of type `'Service` stored in a `Context`.
 * The `'Service` parameter is phantom (it only tracks the value type at compile
 * time); identity at runtime is the unique `Key` string, mirroring upstream's
 * string `key`.
 */
export class Tag$1 extends Record {
    constructor(Key) {
        super();
        this.Key = Key;
    }
}

export function Tag$1_$reflection(gen0) {
    return record_type("Effect.Tag`1", [gen0], Tag$1, () => [["Key", string_type]]);
}

/**
 * An immutable, typed map from service `Tag`s to their instances. Services are
 * boxed internally and recovered by the typed `Tag` on read.
 */
export class Context extends Record {
    constructor(Services) {
        super();
        this.Services = Services;
    }
}

export function Context_$reflection() {
    return record_type("Effect.Context", [], Context, () => [["Services", class_type("Microsoft.FSharp.Collections.FSharpMap`2", [string_type, obj_type])]]);
}

/**
 * Create a typed service tag from a unique key string. Two tags with the
 * same key address the same slot (last write wins), matching upstream.
 */
export function Tag_make(key) {
    return new Tag$1(key);
}

export const ContextModule_empty = new Context(empty({
    Compare: (x, y) => (comparePrimitives(x, y) | 0),
}));

/**
 * Add (or replace) a service under `tag`. (Context.add)
 */
export function ContextModule_add(tag, service, ctx) {
    return new Context(add(tag.Key, service, ctx.Services));
}

/**
 * A one-service context. (Context.make)
 */
export function ContextModule_make(tag, service) {
    return ContextModule_add(tag, service, ContextModule_empty);
}

/**
 * Read a service, or `None` if absent. (Context.getOption)
 */
export function ContextModule_tryGet(tag, ctx) {
    return map((value) => value, tryFind(tag.Key, ctx.Services));
}

/**
 * Read a service, raising if absent. (Context.get / unsafeGet)
 * `Map.find` already raises `KeyNotFoundException` on a missing key.
 */
export function ContextModule_get(tag, ctx) {
    return find(tag.Key, ctx.Services);
}

/**
 * Alias of `get`, mirroring upstream's `unsafeGet`. (Context.unsafeGet)
 */
export function ContextModule_unsafeGet(tag, ctx) {
    return ContextModule_get(tag, ctx);
}

/**
 * Whether a service is present. (Context.has)
 */
export function ContextModule_contains(tag, ctx) {
    return containsKey(tag.Key, ctx.Services);
}

/**
 * Merge two contexts; on key conflict the services in `b` win. (Context.merge)
 */
export function ContextModule_merge(a, b) {
    return new Context(fold((acc, k, v) => add(k, v, acc), a.Services, b.Services));
}

