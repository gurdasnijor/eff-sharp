# Git hooks

## `pre-commit` — Fantomas format check (fast, staged files only)

Runs `dotnet fantomas --check` against just the `.fs`/`.fsi` files you're
committing (~0.7s in practice). It does **not** touch the rest of the tree — the
repo's historical style predates Fantomas, so only new/edited code is gated (the
same scope CI uses; see [`tools/ci/check.sh`](../ci/check.sh)).

### Install (opt-in, one command from the repo root)

```sh
git config core.hooksPath tools/hooks
```

That points Git at this version-controlled hooks directory, so the hook stays in
sync for everyone who opts in. To stop using it: `git config --unset core.hooksPath`.

### Use

- A commit with an unformatted staged file is blocked, with the fix command shown.
- Format and re-stage: `dotnet fantomas <files> && git add <files>`.
- Bypass once: `git commit --no-verify`.

The full gate (build, **tests**, format, lint) lives in `tools/ci/check.sh` and
runs in GitHub Actions; this hook is only the fast formatting pre-check.
