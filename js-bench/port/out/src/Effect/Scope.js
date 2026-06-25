
import { Record, Union } from "../../fable_modules/fable-library-js.5.4.0/Types.js";
import { record_type, union_type, list_type, lambda_type, unit_type, obj_type } from "../../fable_modules/fable-library-js.5.4.0/Reflection.js";
import { Exit$2, Exit$2_$reflection } from "./Exit.js";
import { Effect_succeed, Effect_acquireUseRelease, Effect_suspend, Effect_map, Effect_flatMap, Effect$3, Effect_sync, Effect$3_$reflection } from "./Effect.js";
import { map, cons, empty } from "../../fable_modules/fable-library-js.5.4.0/List.js";
import { Operators_Lock } from "../../fable_modules/fable-library-js.5.4.0/FSharp.Core.js";
import { singleton } from "../../fable_modules/fable-library-js.5.4.0/AsyncBuilder.js";
import { Cause$1, Reason$1 } from "./Cause.js";

export class ScopeState$2 extends Union {
    constructor(tag, fields) {
        super();
        this.tag = tag;
        this.fields = fields;
    }
    cases() {
        return ["Open", "Closed"];
    }
}

export function ScopeState$2_$reflection(gen0, gen1) {
    return union_type("Effect.ScopeState`2", [gen0, gen1], ScopeState$2, () => [[["Item", list_type(lambda_type(Exit$2_$reflection(obj_type, obj_type), Effect$3_$reflection(unit_type, gen0, gen1)))]], []]);
}

/**
 * A lifetime boundary that runs registered finalizers on close.
 */
export class Scope$2 extends Record {
    constructor(State, Gate) {
        super();
        this.State = State;
        this.Gate = Gate;
    }
}

export function Scope$2_$reflection(gen0, gen1) {
    return record_type("Effect.Scope`2", [gen0, gen1], Scope$2, () => [["State", ScopeState$2_$reflection(gen0, gen1)], ["Gate", obj_type]]);
}

/**
 * A fresh, open scope with no finalizers. (Scope.make)
 */
export function Scope_make() {
    return new Scope$2(new ScopeState$2(0, [empty()]), {});
}

/**
 * Register a finalizer that receives the closing `Exit`. Runs immediately as
 * a no-op effect; the work happens at `close`. If the scope is already
 * closed the finalizer is dropped (best-effort). (Scope.addFinalizerExit)
 */
export function Scope_addFinalizerExit(scope, finalizer) {
    return Effect_sync(() => {
        Operators_Lock(scope.Gate, () => {
            const matchValue = scope.State;
            if (matchValue.tag === 1) {
            }
            else {
                scope.State = (new ScopeState$2(0, [cons(finalizer, matchValue.fields[0])]));
            }
        });
    });
}

/**
 * Register a finalizer that ignores the closing `Exit`. (Scope.addFinalizer)
 */
export function Scope_addFinalizer(scope, finalizer) {
    return Scope_addFinalizerExit(scope, (_arg) => finalizer);
}

/**
 * Close the scope, running all finalizers in LIFO order with `exit`. Each
 * finalizer runs even if a previous one failed (failures are swallowed).
 * (Scope.close)
 */
export function Scope_close(scope, exit) {
    return new Effect$3((fib, r) => singleton.Delay(() => {
        const finalizers = Operators_Lock(scope.Gate, () => {
            const matchValue = scope.State;
            if (matchValue.tag === 1) {
                return empty();
            }
            else {
                scope.State = (new ScopeState$2(1, []));
                return matchValue.fields[0];
            }
        });
        return singleton.Combine(singleton.For(finalizers, (_arg) => {
            const patternInput = _arg(exit);
            return singleton.TryWith(singleton.Delay(() => singleton.Bind(patternInput.fields[0](fib, r), (_arg_1) => {
                return singleton.Zero();
            })), (_arg_2) => {
                return singleton.Zero();
            });
        }), singleton.Delay(() => singleton.Return(new Exit$2(0, [undefined]))));
    }));
}

/**
 * Erase an `Exit<'A,'E>` to the boxed `Exit<obj,obj>` finalizers receive.
 */
export function Scope_eraseExit(exit) {
    if (exit.tag === 1) {
        return new Exit$2(1, [new Cause$1(map((_arg) => {
            switch (_arg.tag) {
                case 1:
                    return new Reason$1(1, [_arg.fields[0]]);
                case 2:
                    return new Reason$1(2, [_arg.fields[0]]);
                default:
                    return new Reason$1(0, [_arg.fields[0]]);
            }
        }, exit.fields[0].Reasons))]);
    }
    else {
        return new Exit$2(0, [exit.fields[0]]);
    }
}

/**
 * Acquire a resource and register its release on `scope`; the resource value
 * is returned and stays alive until the scope closes. (Effect.acquireRelease
 * — lives here because it is `Scope`-typed.)
 */
export function Scope_acquireRelease(scope, acquire, release) {
    return Effect_flatMap((a) => Effect_map(() => a, Scope_addFinalizerExit(scope, (exit) => release(a, exit))), acquire);
}

/**
 * Run `f` with a fresh scope that is closed (finalizers run) when `f`
 * completes — on success, typed failure or defect. (Effect.scoped)
 */
export function Scope_scoped(f) {
    return Effect_suspend(() => Effect_acquireUseRelease(Effect_succeed(Scope_make()), f, (s, exit) => Scope_close(s, Scope_eraseExit(exit))));
}

