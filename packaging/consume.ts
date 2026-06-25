// Proof-of-cutover: import the Fable-emitted eff-sharp TypeScript and run a real
// effect on Node. This is exactly how fluent-firegrid would consume the package.
import {
  Effect_succeed,
  Effect_map,
  Effect_flatMap,
  Effect_runSync,
} from "./dist/src/Effect/Effect.ts";

// (succeed 1 |> map (+1) |> flatMap (x => succeed x*10))  ==> 20
const prog = Effect_flatMap(
  (x: number) => Effect_succeed(x * 10),
  Effect_map((x: number) => x + 1, Effect_succeed(1)),
);

const exit: any = Effect_runSync(undefined, prog);
// Exit<'A,'E> = Success of 'A | Failure of Cause<'E>  ->  Fable union: tag 0 = Success
const value = exit?.fields?.[0];
console.log("eff-sharp on Node — exit tag:", exit?.tag, "value:", value);

if (exit?.tag === 0 && value === 20) {
  console.log("PASS: eff-sharp (F#->Fable->TS) runs on Node -> 20");
  process.exit(0);
} else {
  console.log("FAIL:", JSON.stringify(exit));
  process.exit(1);
}
