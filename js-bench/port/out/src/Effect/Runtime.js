
import { Record } from "../../fable_modules/fable-library-js.5.4.0/Types.js";
import { ContextModule_empty, Context_$reflection } from "./Context.js";
import { record_type } from "../../fable_modules/fable-library-js.5.4.0/Reflection.js";
import { Effect_fork, Effect_runAsync, EffectFailureException_$ctor_Z6861C5C0, Effect_runSync, Effect_runExit } from "./Effect.js";
import { Cause_render } from "./Cause.js";
import { tail, head, isEmpty } from "../../fable_modules/fable-library-js.5.4.0/List.js";
import { isException } from "../../fable_modules/fable-library-js.5.4.0/Util.js";

/**
 * Port of repos/effect-smol/packages/effect/src/Runtime.ts — the runtime bundle
 * that carries the services an `Effect` runs with, plus the `run*` family that
 * executes an effect against it.
 * 
 * Model: a `Runtime` wraps the `Context` (our DI environment — the bundle of
 * services/refs). `make` builds one from a `Context`; `defaultRuntime` is the
 * empty-context runtime. The `run*` functions provide the runtime's context as
 * the effect's environment and run it — mirroring `Effect.runFork`/`runPromise`/
 * `runSync` "over a Runtime" (in effect-smol this surface lives on
 * `ManagedRuntime`; the classic `Runtime` bundle is reproduced here).
 * 
 * Faithfulness / omissions (per CONVENTIONS):
 * - effect-smol's `Runtime.ts` is, in this beta, the *runMain* layer
 * (`makeRunMain`/`Teardown`/`defaultTeardown`/`errorExitCode`/
 * `errorReported`). Those are a **platform adapter** concern: `makeRunMain`
 * needs fiber *observers* (`fiber.addObserver`) and a host keep-alive timer,
 * and the `errorExitCode`/`errorReported` markers are JS symbol-keyed
 * properties on `Error` objects — JS-runtime machinery dropped per the
 * conventions. They are deferred until a platform layer + fiber supervision
 * land; this module ports the runtime *bundle* the brief specifies.
 * - **Fiber supervision is deferred**: a `Runtime` carries no RuntimeFlags,
 * FiberRefs, scheduler override, or supervisor. `runFork` returns a bare
 * `Fiber` handle (no parent-scope registration); `defaultRuntime` bundles no
 * default services (Clock/Random/… are provided by the caller's `Context`,
 * and in any case compile after this module).
 */
export class Runtime extends Record {
    constructor(Context) {
        super();
        this.Context = Context;
    }
}

export function Runtime_$reflection() {
    return record_type("Effect.Runtime", [], Runtime, () => [["Context", Context_$reflection()]]);
}

/**
 * Build a `Runtime` from a `Context` of services. (Runtime.make)
 */
export function RuntimeModule_make(context) {
    return new Runtime(context);
}

export const RuntimeModule_defaultRuntime = new Runtime(ContextModule_empty);

/**
 * The services this runtime carries.
 */
export function RuntimeModule_context(self) {
    return self.Context;
}

/**
 * Run an effect against the runtime, returning its full `Exit` as an
 * `Async`. The total, non-throwing runner. (Runtime.runExit)
 */
export function RuntimeModule_runExit(self, effect) {
    return Effect_runExit(self.Context, effect);
}

/**
 * Run an effect synchronously, returning its `Exit` (never throws for a typed
 * error). (Runtime.runSyncExit)
 */
export function RuntimeModule_runSyncExit(self, effect) {
    return Effect_runSync(self.Context, effect);
}

/**
 * Run an effect synchronously to its success value, **raising** on failure (a
 * defect that is itself an exception is re-raised; otherwise an
 * `EffectFailureException`). (Runtime.runSync)
 */
export function RuntimeModule_runSync(self, effect) {
    const matchValue = Effect_runSync(self.Context, effect);
    if (matchValue.tag === 1) {
        const cause = matchValue.fields[0];
        const matchValue_1 = cause.Reasons;
        let matchResult, ex;
        if (!isEmpty(matchValue_1)) {
            if (head(matchValue_1).tag === 1) {
                if (isException(head(matchValue_1).fields[0])) {
                    if (isEmpty(tail(matchValue_1))) {
                        matchResult = 0;
                        ex = head(matchValue_1).fields[0];
                    }
                    else {
                        matchResult = 1;
                    }
                }
                else {
                    matchResult = 1;
                }
            }
            else {
                matchResult = 1;
            }
        }
        else {
            matchResult = 1;
        }
        switch (matchResult) {
            case 0:
                throw ex;
            default:
                throw EffectFailureException_$ctor_Z6861C5C0(cause, Cause_render(cause));
        }
    }
    else {
        return matchValue.fields[0];
    }
}

/**
 * Run an effect to its success value, rejecting on failure. On JS the returned
 * Async is bridged to a Promise with Async.StartAsPromise. (Runtime.runPromise)
 */
export function RuntimeModule_runPromise(self, effect) {
    return Effect_runAsync(self.Context, effect);
}

/**
 * Run an effect to its full `Exit` (never rejects for a typed error).
 * (Runtime.runPromiseExit)
 */
export function RuntimeModule_runPromiseExit(self, effect) {
    return Effect_runExit(self.Context, effect);
}

/**
 * Start an effect on a fresh fiber and return its handle immediately. The
 * fiber runs concurrently against the runtime's context. (Runtime.runFork)
 */
export function RuntimeModule_runFork(self, effect) {
    const matchValue = Effect_runSync(self.Context, Effect_fork(effect));
    if (matchValue.tag === 1) {
        const cause = matchValue.fields[0];
        throw EffectFailureException_$ctor_Z6861C5C0(cause, Cause_render(cause));
    }
    else {
        return matchValue.fields[0];
    }
}

