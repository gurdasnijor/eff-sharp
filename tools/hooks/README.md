# Git hooks

## `pre-commit` — Fantomas format check (fast, staged files only)

Runs `dotnet fantomas --check` against just the `.fs`/`.fsi` files you're
committing (~0.7s in practice) — a fast pre-check so you catch formatting before
pushing. The `quality` job in [`tools/ci/check.sh`](../ci/check.sh) then enforces
it in CI: changed files vs the PR base on pull requests, the whole tree on push
to `main`. The tree is fully Fantomas-formatted.

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
