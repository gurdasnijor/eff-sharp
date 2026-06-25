module TxReentrantLockSpec

// TODO(tx-vitest): Port the 18 old TxReentrantLock xUnit facts once the
// Fable/Vitest harness can compile STM sources. `test/Harness.fsproj` currently
// removes `$(EffectSrcDir)Tx*.fs`, so direct references to `TxReentrantLock`
// fail at compile time.
