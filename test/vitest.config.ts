import { defineConfig } from "vitest/config";

// The specs are F# compiled by Fable to out/*Spec.js; they self-register via
// Effect.Vitest, which imports Vitest explicitly.
export default defineConfig({
  test: {
    include: ["out/**/*Spec.js"],
  },
});
