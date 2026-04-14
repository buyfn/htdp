## Repo Shape

- This repo is a small HtDP exercise collection, not a packaged app: there is no README, CI, build, lint, or workspace config to consult.
- The real structure is chapter-based under `I Fixed-Size Data/`, with mostly standalone exercise files in `1 Arithmetic/`, `2 Functions and programs/`, `3 How to design programs/`, and `4 Intervals, Enumerations, and Itemizations/`.

## Verification

- Run focused checks from the repo root with `raco test "path/to/file.rkt"`.
- Quote paths: chapter directories contain spaces.
- Prefer testing only the file you changed. Several files have top-level `main`/`big-bang` or batch-IO calls, so broad sweeps can execute programs, print output, or create files as a side effect.

## File Format Gotchas

- Most source files use DrRacket's HtDP beginner language via a `#reader(lib "htdp-beginner-reader.ss" "lang")...` header, not `#lang racket`. Preserve that reader line and its embedded teachpack/settings metadata.
- `prologue.rkt` is a DrRacket `wxme` editor file, not normal text. Treat it as DrRacket-managed unless the task explicitly requires touching that format.
- Ignore `*.rkt~` files during searches and edits unless the user explicitly asks about backups.
