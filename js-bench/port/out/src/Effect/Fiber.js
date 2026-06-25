
import { Effect_sync, Effect_map, Effect_forEach, Effect_interrupt, Effect_join, Effect_await } from "./Effect.js";

/**
 * Wait for a fiber and observe its `Exit`. (Fiber.await)
 */
export function await$(self) {
    return Effect_await(self);
}

/**
 * Wait for a fiber and fold its failure into this effect. (Fiber.join)
 */
export function join(self) {
    return Effect_join(self);
}

/**
 * Interrupt a fiber and wait until it has stopped. (Fiber.interrupt)
 */
export function interrupt(self) {
    return Effect_interrupt(self);
}

/**
 * Await every fiber, collecting their `Exit`s in order. (Fiber.awaitAll)
 */
export function awaitAll(fibers) {
    return Effect_forEach(fibers, Effect_await);
}

/**
 * Join every fiber, collecting their values; the first failure
 * short-circuits. (Fiber.joinAll)
 */
export function joinAll(fibers) {
    return Effect_forEach(fibers, Effect_join);
}

/**
 * Interrupt every fiber, waiting for all to stop. (Fiber.interruptAll)
 */
export function interruptAll(fibers) {
    return Effect_map((value) => {
    }, Effect_forEach(fibers, Effect_interrupt));
}

/**
 * Non-blocking peek: `Some exit` if the fiber has completed, else `None`.
 * (Fiber.pollUnsafe, lifted into an effect.)
 */
export function poll(self) {
    return Effect_sync(() => self.Result.contents);
}

