
import { Effect_runAsync, Effect_join, Effect_fork, EffectBuilder__ReturnFrom_Z1FBAE0A2, EffectBuilderModule_effect, EffectBuilder__Return_1505, EffectBuilder__For_Z76CAA648, EffectBuilder__Combine_Z6C3CB047, EffectBuilder__Bind_5662ABF3, EffectBuilder__Delay_2CE84734, Effect_map, Effect_flatMap, Effect_succeed } from "./src/Effect/Effect.js";
import { Ref_get, Ref_update, Ref_make } from "./src/Effect/Ref.js";
import { rangeDouble } from "./fable_modules/fable-library-js.5.4.0/Range.js";
import { startAsPromise } from "./fable_modules/fable-library-js.5.4.0/Async.js";

export const bindN = 10000;

export const mapN = 10000;

export const refN = 10000;

export const forkN = 1000;

const bindProgram = (() => {
    let e = Effect_succeed(0);
    for (let forLoopVar = 1; forLoopVar <= bindN; forLoopVar++) {
        e = Effect_flatMap((x) => Effect_succeed(x + 1), e);
    }
    return e;
})();

const mapProgram = (() => {
    let e = Effect_succeed(0);
    for (let forLoopVar = 1; forLoopVar <= mapN; forLoopVar++) {
        e = Effect_map((y) => ((1 + y) | 0), e);
    }
    return e;
})();

const refProgram = EffectBuilder__Delay_2CE84734(EffectBuilderModule_effect, () => EffectBuilder__Bind_5662ABF3(EffectBuilderModule_effect, Ref_make(0), (_arg) => {
    const r = _arg;
    return EffectBuilder__Combine_Z6C3CB047(EffectBuilderModule_effect, EffectBuilder__For_Z76CAA648(EffectBuilderModule_effect, rangeDouble(1, 1, refN), (_arg_1) => EffectBuilder__Bind_5662ABF3(EffectBuilderModule_effect, Ref_update(r, (y) => ((1 + y) | 0)), () => EffectBuilder__Bind_5662ABF3(EffectBuilderModule_effect, Ref_get(r), (_arg_3) => EffectBuilder__Return_1505(EffectBuilderModule_effect, undefined)))), EffectBuilder__Delay_2CE84734(EffectBuilderModule_effect, () => EffectBuilder__ReturnFrom_Z1FBAE0A2(EffectBuilderModule_effect, Ref_get(r))));
}));

const forkProgram = EffectBuilder__Delay_2CE84734(EffectBuilderModule_effect, () => EffectBuilder__For_Z76CAA648(EffectBuilderModule_effect, rangeDouble(1, 1, forkN), (_arg) => EffectBuilder__Bind_5662ABF3(EffectBuilderModule_effect, Effect_fork(Effect_succeed(1)), (_arg_1) => EffectBuilder__Bind_5662ABF3(EffectBuilderModule_effect, Effect_join(_arg_1), (_arg_2) => EffectBuilder__Return_1505(EffectBuilderModule_effect, undefined)))));

/**
 * bind throughput — resolves to the final accumulator (expect bindN).
 */
export function runBindAsync() {
    return startAsPromise(Effect_runAsync(undefined, bindProgram));
}

/**
 * succeed/map — resolves to the final accumulator (expect mapN).
 */
export function runMapAsync() {
    return startAsPromise(Effect_runAsync(undefined, mapProgram));
}

/**
 * Ref update/get — resolves to the final counter (expect refN).
 */
export function runRefAsync() {
    return startAsPromise(Effect_runAsync(undefined, refProgram));
}

/**
 * fork/join — fork + join a trivial fiber forkN times.
 */
export function runForkAsync() {
    return startAsPromise(Effect_runAsync(undefined, forkProgram));
}

