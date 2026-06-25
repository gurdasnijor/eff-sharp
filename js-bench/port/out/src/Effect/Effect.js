
import { Union, FSharpRef, Record } from "../../fable_modules/fable-library-js.5.4.0/Types.js";
import { union_type, lambda_type, option_type, class_type, record_type, int32_type, bool_type } from "../../fable_modules/fable-library-js.5.4.0/Reflection.js";
import { Exit_die, Exit_fail, Exit$2, Exit$2_$reflection } from "./Exit.js";
import { disposeSafe, defaultOf, isException, getEnumerator, Exception } from "../../fable_modules/fable-library-js.5.4.0/Util.js";
import { Cause_render, Cause_combine, Cause_failures, Cause_interrupt, Cause_map } from "./Cause.js";
import { singleton } from "../../fable_modules/fable-library-js.5.4.0/AsyncBuilder.js";
import { tail, head, isEmpty, tryPick, ofSeq, tryHead } from "../../fable_modules/fable-library-js.5.4.0/List.js";
import { value as value_1 } from "../../fable_modules/fable-library-js.5.4.0/Option.js";
import { startImmediate, sleep, startChild } from "../../fable_modules/fable-library-js.5.4.0/Async.js";
import { min } from "../../fable_modules/fable-library-js.5.4.0/Double.js";
import { ContextModule_make, ContextModule_tryGet } from "./Context.js";
import { KeyNotFoundException_$ctor_Z721C83C5 } from "../../fable_modules/fable-library-js.5.4.0/System.Collections.Generic.js";
import { printf, toText } from "../../fable_modules/fable-library-js.5.4.0/String.js";

/**
 * Per-fiber runtime state threaded through every effect. Interruption is
 * **cooperative** (the hybrid chosen by the fiber spike): `Interrupted` is a
 * flag set by `interrupt`, checked at yield points (`flatMap` boundaries,
 * `sleep`, fork/await); `Mask` is the uninterruptible-region depth — while it is
 * > 0 a pending interrupt is deferred and honored when the region exits. This
 * (vs. .NET `CancellationToken`) is what lets finalizers run on interrupt:
 * honoring an interrupt is a *normal* `Exit.Interrupt` return, not an exception
 * unwind that bypasses `try/finally`.
 */
export class FiberRuntime extends Record {
    constructor(Interrupted, Mask) {
        super();
        this.Interrupted = Interrupted;
        this.Mask = (Mask | 0);
    }
}

export function FiberRuntime_$reflection() {
    return record_type("Effect.FiberRuntime", [], FiberRuntime, () => [["Interrupted", bool_type], ["Mask", int32_type]]);
}

export function FiberRuntime_Root() {
    return new FiberRuntime(false, 0);
}

/**
 * A handle to a forked, concurrently-running effect and the `FiberRuntime` whose
 * `Interrupted` flag interrupts it. (Fiber.Fiber)
 * 
 * On .NET the backing computation is a `Task<Exit>` (`Async.StartAsTask`). Fable's
 * runtime library implements neither `StartAsTask` nor `AwaitTask`, so under
 * `FABLE_COMPILER` the handle is an `Async.StartChild` join plus a completion cell
 * (`Result`) that `poll`/`isLive` read in place of `Task.IsCompleted`/`Result`.
 * The .NET representation is unchanged.
 */
export class Fiber$2 extends Record {
    constructor(Join, Result, Runtime) {
        super();
        this.Join = Join;
        this.Result = Result;
        this.Runtime = Runtime;
    }
}

export function Fiber$2_$reflection(gen0, gen1) {
    return record_type("Effect.Fiber`2", [gen0, gen1], Fiber$2, () => [["Join", class_type("Microsoft.FSharp.Control.FSharpAsync`1", [Exit$2_$reflection(gen0, gen1)])], ["Result", record_type("Microsoft.FSharp.Core.FSharpRef`1", [option_type(Exit$2_$reflection(gen0, gen1))], FSharpRef, () => [["contents", option_type(Exit$2_$reflection(gen0, gen1))]])], ["Runtime", FiberRuntime_$reflection()]]);
}

/**
 * Native F# port of Effect's core `Effect<A, E, R>` type.
 * 
 * Representation — a Reader over the fiber runtime AND the environment, into
 * `Async<Exit>`:
 * `Effect of (FiberRuntime -> 'R -> Async<Exit<'A,'E>>)`
 * `Async` gives stack-safety/scheduling; the explicit `FiberRuntime` gives
 * faithful cooperative interruption + uninterruptible masking (see the
 * `spikes/fiber` evidence). The three channels are unchanged: 'A success,
 * 'E typed error, 'R environment. The `FiberRuntime` is internal — it is created
 * by the runners and never appears in the public surface.
 */
export class Effect$3 extends Union {
    constructor(Item) {
        super();
        this.tag = 0;
        this.fields = [Item];
    }
    cases() {
        return ["Effect"];
    }
}

export function Effect$3_$reflection(gen0, gen1, gen2) {
    return union_type("Effect.Effect`3", [gen0, gen1, gen2], Effect$3, () => [[["Item", lambda_type(FiberRuntime_$reflection(), lambda_type(gen2, class_type("Microsoft.FSharp.Control.FSharpAsync`1", [Exit$2_$reflection(gen0, gen1)])))]]]);
}

/**
 * Raised by `runAsync`/`runSync` when an effect completes with a `Failure`
 * whose cause is a typed error (or a non-exception defect).
 */
export class EffectFailureException extends Exception {
    constructor(cause, render) {
        super(render);
        this.cause = cause;
    }
}

export function EffectFailureException_$reflection() {
    return class_type("Effect.EffectFailureException", undefined, EffectFailureException, class_type("System.Exception"));
}

export function EffectFailureException_$ctor_Z6861C5C0(cause, render) {
    return new EffectFailureException(cause, render);
}

export function EffectFailureException__get_Cause(_) {
    return _.cause;
}

function Effect_unwrap(ex) {
    return ex;
}

function Effect_retagDefects(cause) {
    return Cause_map((_arg) => {
        throw new Exception("retagDefects: unexpected Fail reason in a defect-only cause");
    }, cause);
}

function Effect_interruptedCause() {
    return Cause_interrupt(undefined);
}

export function Effect_succeed(value) {
    return new Effect$3((_arg, _arg_1) => singleton.Delay(() => singleton.Return(new Exit$2(0, [value]))));
}

export function Effect_fail(error) {
    return new Effect$3((_arg, _arg_1) => singleton.Delay(() => singleton.Return(Exit_fail(error))));
}

export function Effect_failCause(cause) {
    return new Effect$3((_arg, _arg_1) => singleton.Delay(() => singleton.Return(new Exit$2(1, [cause]))));
}

export function Effect_sync(thunk) {
    return new Effect$3((_arg, _arg_1) => singleton.Delay(() => singleton.TryWith(singleton.Delay(() => singleton.Return(new Exit$2(0, [thunk()]))), (_arg_2) => singleton.Return(Exit_die(_arg_2)))));
}

export function Effect_fromResult(result) {
    return new Effect$3((_arg, _arg_1) => singleton.Delay(() => singleton.Return((result.tag === 1) ? Exit_fail(result.fields[0]) : (new Exit$2(0, [result.fields[0]])))));
}

export function Effect_promise(f) {
    return new Effect$3((_arg, _arg_1) => singleton.Delay(() => singleton.Bind(f(), (_arg_2) => singleton.Return(new Exit$2(0, [_arg_2])))));
}

export function Effect_tryPromise(f) {
    return new Effect$3((_arg, _arg_1) => singleton.Delay(() => singleton.TryWith(singleton.Delay(() => singleton.Bind(f(), (_arg_2) => singleton.Return(new Exit$2(0, [_arg_2])))), (_arg_3) => singleton.Return(Exit_die(Effect_unwrap(_arg_3))))));
}

export function Effect_environment() {
    return new Effect$3((_arg, r) => singleton.Delay(() => singleton.Return(new Exit$2(0, [r]))));
}

export function Effect_environmentWith(f) {
    return new Effect$3((_arg, r) => singleton.Delay(() => singleton.Return(new Exit$2(0, [f(r)]))));
}

export function Effect_map(f, _arg) {
    return new Effect$3((fib, r) => singleton.Delay(() => singleton.Bind(_arg.fields[0](fib, r), (_arg_1) => {
        const exit = _arg_1;
        return singleton.Return((exit.tag === 1) ? (new Exit$2(1, [exit.fields[0]])) : (new Exit$2(0, [f(exit.fields[0])])));
    })));
}

export function Effect_mapError(f, _arg) {
    return new Effect$3((fib, r) => singleton.Delay(() => singleton.Bind(_arg.fields[0](fib, r), (_arg_1) => {
        const exit = _arg_1;
        return singleton.Return((exit.tag === 1) ? (new Exit$2(1, [Cause_map(f, exit.fields[0])])) : (new Exit$2(0, [exit.fields[0]])));
    })));
}

/**
 * Sequence a dependent effect. A yield point: between the two steps a pending
 * interrupt is honored (unless masked), short-circuiting to `Exit.Interrupt`.
 */
export function Effect_flatMap(f, _arg) {
    return new Effect$3((fib, r) => singleton.Delay(() => singleton.Bind(_arg.fields[0](fib, r), (_arg_1) => {
        let fib_1;
        const exit = _arg_1;
        if (exit.tag === 1) {
            return singleton.Return(new Exit$2(1, [exit.fields[0]]));
        }
        else if ((fib_1 = fib, fib_1.Interrupted && (fib_1.Mask === 0))) {
            return singleton.Return(new Exit$2(1, [Effect_interruptedCause()]));
        }
        else {
            const patternInput = f(exit.fields[0]);
            return singleton.ReturnFrom(patternInput.fields[0](fib, r));
        }
    })));
}

export function Effect_catchAll(handler, _arg) {
    return new Effect$3((fib, r) => singleton.Delay(() => singleton.Bind(_arg.fields[0](fib, r), (_arg_1) => {
        const exit = _arg_1;
        if (exit.tag === 1) {
            const cause = exit.fields[0];
            const matchValue = tryHead(Cause_failures(cause));
            if (matchValue == null) {
                return singleton.Return(new Exit$2(1, [Effect_retagDefects(cause)]));
            }
            else {
                const patternInput = handler(value_1(matchValue));
                return singleton.ReturnFrom(patternInput.fields[0](fib, r));
            }
        }
        else {
            return singleton.Return(new Exit$2(0, [exit.fields[0]]));
        }
    })));
}

export function Effect_zipRight(b, a) {
    return Effect_flatMap((_arg) => b, a);
}

export function Effect_zipWith(f, b, a) {
    return Effect_flatMap((av) => Effect_map((bv) => f(av, bv), b), a);
}

/**
 * Run `eff` on a fresh fiber concurrently; returns a `Fiber` handle to
 * await/join/interrupt. The parent is not affected by the child's outcome.
 * (Effect.fork)
 */
export function Effect_fork(_arg) {
    return new Effect$3((_arg_1, r) => singleton.Delay(() => {
        const child = FiberRuntime_Root();
        const cell = new FSharpRef(undefined);
        return singleton.Bind(startChild(singleton.Delay(() => singleton.Bind(_arg.fields[0](child, r), (_arg_2) => {
            const e = _arg_2;
            cell.contents = e;
            return singleton.Return(e);
        }))), (_arg_3) => singleton.Return(new Exit$2(0, [new Fiber$2(_arg_3, cell, child)])));
    }));
}

/**
 * Wait for a fiber to complete and observe its full `Exit`. (Fiber.await)
 */
export function Effect_await(fib) {
    return new Effect$3((_arg, _arg_1) => singleton.Delay(() => singleton.Bind(fib.Join, (_arg_2) => singleton.Return(new Exit$2(0, [_arg_2])))));
}

/**
 * Wait for a fiber and fold its failure back into this effect. (Fiber.join)
 */
export function Effect_join(fib) {
    return new Effect$3((_arg, _arg_1) => singleton.Delay(() => singleton.ReturnFrom(fib.Join)));
}

/**
 * Interrupt a fiber and wait until it has actually stopped (its finalizers
 * run). Completes with `unit`. (Fiber.interrupt)
 */
export function Effect_interrupt(fib) {
    return new Effect$3((_arg, _arg_1) => singleton.Delay(() => {
        fib.Runtime.Interrupted = true;
        return singleton.Bind(fib.Join, (_arg_2) => singleton.Return(new Exit$2(0, [undefined])));
    }));
}

/**
 * Mark a region uninterruptible: a pending interrupt is deferred until the
 * region exits, then honored. Finalizers run under this so they cannot be
 * interrupted. (Effect.uninterruptible)
 */
export function Effect_uninterruptible(_arg) {
    return new Effect$3((fib, r) => singleton.Delay(() => {
        fib.Mask = ((fib.Mask + 1) | 0);
        return singleton.Bind(_arg.fields[0](fib, r), (_arg_1) => {
            let fib_1;
            fib.Mask = ((fib.Mask - 1) | 0);
            return ((fib_1 = fib, fib_1.Interrupted && (fib_1.Mask === 0))) ? singleton.Return(new Exit$2(1, [Effect_interruptedCause()])) : singleton.Return(_arg_1);
        });
    }));
}

/**
 * A cancellable delay — a yield point that honors interruption. (Effect.sleep)
 */
export function Effect_sleep(milliseconds) {
    return new Effect$3((fib, _arg) => singleton.Delay(() => {
        let remaining = milliseconds;
        let interrupted = false;
        return singleton.Combine(singleton.While(() => ((remaining > 0) && !interrupted), singleton.Delay(() => {
            let fib_1;
            if ((fib_1 = fib, fib_1.Interrupted && (fib_1.Mask === 0))) {
                interrupted = true;
                return singleton.Zero();
            }
            else {
                const step = min(5, remaining) | 0;
                return singleton.Bind(sleep(step), () => {
                    remaining = ((remaining - step) | 0);
                    return singleton.Zero();
                });
            }
        })), singleton.Delay(() => singleton.Return(interrupted ? (new Exit$2(1, [Effect_interruptedCause()])) : (new Exit$2(0, [undefined])))));
    }));
}

export function Effect_suspend(f) {
    return new Effect$3((fib, r) => singleton.Delay(() => {
        const patternInput = f();
        return singleton.ReturnFrom(patternInput.fields[0](fib, r));
    }));
}

/**
 * Run `finalizer` after `eff` regardless of outcome. The finalizer runs
 * **under mask** so an in-flight interrupt cannot skip it.
 */
export function Effect_ensuring(finalizer, _arg) {
    return new Effect$3((fib, r) => singleton.Delay(() => singleton.Bind(singleton.Delay(() => singleton.TryWith(singleton.Delay(() => singleton.ReturnFrom(_arg.fields[0](fib, r))), (_arg_1) => singleton.Return(Exit_die(Effect_unwrap(_arg_1))))), (_arg_2) => {
        const exit = _arg_2;
        fib.Mask = ((fib.Mask + 1) | 0);
        return singleton.Bind(singleton.Delay(() => singleton.TryWith(singleton.Delay(() => singleton.ReturnFrom(finalizer.fields[0](fib, r))), (_arg_3) => singleton.Return(Exit_die(Effect_unwrap(_arg_3))))), (_arg_4) => {
            let fc;
            const finExit = _arg_4;
            fib.Mask = ((fib.Mask - 1) | 0);
            return singleton.Return((finExit.tag === 1) ? ((fc = finExit.fields[0], (exit.tag === 0) ? (new Exit$2(1, [fc])) : (new Exit$2(1, [Cause_combine(exit.fields[0], fc)])))) : exit);
        });
    })));
}

export function Effect_forEach(items, f) {
    return new Effect$3((fib, r) => singleton.Delay(() => {
        const results = [];
        return singleton.Using(getEnumerator(items), (_arg) => {
            const en = _arg;
            let failure = undefined;
            return singleton.Combine(singleton.While(() => ((failure == null) && en["System.Collections.IEnumerator.MoveNext"]()), singleton.Delay(() => {
                let fib_1;
                if ((fib_1 = fib, fib_1.Interrupted && (fib_1.Mask === 0))) {
                    failure = Effect_interruptedCause();
                    return singleton.Zero();
                }
                else {
                    const patternInput = f(en["System.Collections.Generic.IEnumerator`1.get_Current"]());
                    return singleton.Bind(patternInput.fields[0](fib, r), (_arg_1) => {
                        const exit = _arg_1;
                        if (exit.tag === 1) {
                            failure = exit.fields[0];
                            return singleton.Zero();
                        }
                        else {
                            void (results.push(exit.fields[0]));
                            return singleton.Zero();
                        }
                    });
                }
            })), singleton.Delay(() => singleton.Return((failure == null) ? (new Exit$2(0, [ofSeq(results)])) : (new Exit$2(1, [failure])))));
        });
    }));
}

export function Effect_whileLoop(guard, body) {
    return new Effect$3((fib, r) => singleton.Delay(() => {
        let result = new Exit$2(0, [undefined]);
        let go = true;
        return singleton.Combine(singleton.While(() => (go && guard()), singleton.Delay(() => {
            let fib_1;
            if ((fib_1 = fib, fib_1.Interrupted && (fib_1.Mask === 0))) {
                result = (new Exit$2(1, [Effect_interruptedCause()]));
                go = false;
                return singleton.Zero();
            }
            else {
                return singleton.Bind(body.fields[0](fib, r), (_arg) => {
                    const exit = _arg;
                    if (exit.tag === 1) {
                        result = (new Exit$2(1, [exit.fields[0]]));
                        go = false;
                        return singleton.Zero();
                    }
                    else {
                        return singleton.Zero();
                    }
                });
            }
        })), singleton.Delay(() => singleton.Return(result)));
    }));
}

export function Effect_catchDefect(handler, _arg) {
    return new Effect$3((fib, r) => singleton.Delay(() => singleton.Bind(_arg.fields[0](fib, r), (_arg_1) => {
        const exit = _arg_1;
        if (exit.tag === 1) {
            const cause = exit.fields[0];
            const matchValue = tryPick((_arg_2) => {
                let matchResult, ex;
                if (_arg_2.tag === 1) {
                    if (isException(_arg_2.fields[0])) {
                        matchResult = 0;
                        ex = _arg_2.fields[0];
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
                        return ex;
                    default:
                        return undefined;
                }
            }, cause.Reasons);
            if (matchValue == null) {
                return singleton.Return(new Exit$2(1, [cause]));
            }
            else {
                const patternInput = handler(matchValue);
                return singleton.ReturnFrom(patternInput.fields[0](fib, r));
            }
        }
        else {
            return singleton.Return(new Exit$2(0, [exit.fields[0]]));
        }
    })));
}

export function Effect_acquireUseRelease(acquire, useResource, release) {
    return new Effect$3((fib, r) => singleton.Delay(() => singleton.Bind(acquire.fields[0](fib, r), (_arg) => {
        const acqExit = _arg;
        if (acqExit.tag === 0) {
            const a = acqExit.fields[0];
            return singleton.Bind(singleton.Delay(() => singleton.TryWith(singleton.Delay(() => {
                const patternInput = useResource(a);
                return singleton.ReturnFrom(patternInput.fields[0](fib, r));
            }), (_arg_1) => singleton.Return(Exit_die(Effect_unwrap(_arg_1))))), (_arg_2) => {
                const useExit = _arg_2;
                fib.Mask = ((fib.Mask + 1) | 0);
                const patternInput_1 = release(a, useExit);
                return singleton.Bind(singleton.Delay(() => singleton.TryWith(singleton.Delay(() => singleton.ReturnFrom(patternInput_1.fields[0](fib, r))), (_arg_3) => singleton.Return(Exit_die(Effect_unwrap(_arg_3))))), (_arg_4) => {
                    let fc;
                    const relExit = _arg_4;
                    fib.Mask = ((fib.Mask - 1) | 0);
                    return singleton.Return((relExit.tag === 1) ? ((fc = relExit.fields[0], (useExit.tag === 0) ? (new Exit$2(1, [fc])) : (new Exit$2(1, [Cause_combine(useExit.fields[0], fc)])))) : useExit);
                });
            });
        }
        else {
            return singleton.Return(new Exit$2(1, [acqExit.fields[0]]));
        }
    })));
}

export function Effect_service(tag) {
    return new Effect$3((_arg, ctx) => singleton.Delay(() => {
        const matchValue = ContextModule_tryGet(tag, ctx);
        if (matchValue == null) {
            return singleton.Return(Exit_die(KeyNotFoundException_$ctor_Z721C83C5(toText(printf("Service not found: %s"))(tag.Key))));
        }
        else {
            const s = value_1(matchValue);
            return singleton.Return(new Exit$2(0, [s]));
        }
    }));
}

export function Effect_provideContext(ctx, eff) {
    return new Effect$3((fib, _arg) => eff.fields[0](fib, ctx));
}

export function Effect_provideService(tag, service, eff) {
    return Effect_provideContext(ContextModule_make(tag, service), eff);
}

export function Effect_runExit(env, _arg) {
    return singleton.Delay(() => singleton.TryWith(singleton.Delay(() => singleton.ReturnFrom(_arg.fields[0](FiberRuntime_Root(), env))), (_arg_1) => singleton.Return(Exit_die(Effect_unwrap(_arg_1)))));
}

export function Effect_runAsync(env, eff) {
    return singleton.Delay(() => singleton.Bind(Effect_runExit(env, eff), (_arg) => {
        const exit = _arg;
        if (exit.tag === 1) {
            const cause = exit.fields[0];
            const matchValue = cause.Reasons;
            let matchResult, ex;
            if (!isEmpty(matchValue)) {
                if (head(matchValue).tag === 1) {
                    if (isException(head(matchValue).fields[0])) {
                        if (isEmpty(tail(matchValue))) {
                            matchResult = 0;
                            ex = head(matchValue).fields[0];
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
                    return singleton.Return((() => {
                        throw ex;
                    })());
                default:
                    return singleton.Return((() => {
                        throw EffectFailureException_$ctor_Z6861C5C0(cause, Cause_render(cause));
                    })());
            }
        }
        else {
            return singleton.Return(exit.fields[0]);
        }
    }));
}

export function Effect_runSync(env, eff) {
    let result = undefined;
    startImmediate(singleton.Delay(() => singleton.Bind(Effect_runExit(env, eff), (_arg) => {
        result = _arg;
        return singleton.Zero();
    })));
    if (result == null) {
        throw new Exception("runSync: effect suspended asynchronously; use runPromiseExit on JS");
    }
    else {
        return result;
    }
}

/**
 * Computation-expression builder: the `effect { ... }` do-notation.
 */
export class EffectBuilder {
    constructor() {
    }
}

export function EffectBuilder_$reflection() {
    return class_type("Effect.EffectBuilder", undefined, EffectBuilder);
}

export function EffectBuilder_$ctor() {
    return new EffectBuilder();
}

export function EffectBuilder__Return_1505(_, value) {
    return Effect_succeed(value);
}

export function EffectBuilder__ReturnFrom_Z1FBAE0A2(_, eff) {
    return eff;
}

export function EffectBuilder__Bind_5662ABF3(_, eff, f) {
    return Effect_flatMap(f, eff);
}

export function EffectBuilder__Zero(_) {
    return Effect_succeed(undefined);
}

export function EffectBuilder__Delay_2CE84734(_, f) {
    return Effect_suspend(f);
}

export function EffectBuilder__Combine_Z6C3CB047(_, a, b) {
    return Effect_zipRight(b, a);
}

export function EffectBuilder__For_Z76CAA648(_, items, f) {
    return Effect_map((value) => {
    }, Effect_forEach(items, f));
}

export function EffectBuilder__While_7979D147(_, guard, body) {
    return Effect_whileLoop(guard, body);
}

export function EffectBuilder__TryWith_Z7E082A31(_, body, handler) {
    return Effect_catchDefect(handler, body);
}

export function EffectBuilder__TryFinally_Z2D4D90B8(_, body, compensation) {
    return Effect_ensuring(Effect_sync(compensation), body);
}

export function EffectBuilder__Using_Z564D1F78(_, resource, body) {
    return Effect_ensuring(Effect_sync(() => {
        if (!(resource === defaultOf())) {
            let copyOfStruct = resource;
            disposeSafe(copyOfStruct);
        }
    }), body(resource));
}

export const EffectBuilderModule_effect = EffectBuilder_$ctor();

