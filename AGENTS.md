## Repo Shape

- This repo is a small HtDP exercise collection, not a packaged app: there is no README, CI, build, lint, or workspace config to consult.
- The structure mirrors the book's parts and chapters: `I Fixed-Size Data/`, `II Arbitrarily large data/`, `III Abstraction/`, each holding chapter directories with mostly standalone exercise files. New part directories appear as the user progresses through the book.

## Book Reference

- The full text of HtDP 2e is mirrored locally in `htdp-book/` (gitignored): plain text in `text/`, original HTML in `html/`, file→part mapping in its README. Grep it instead of fetching htdp.org, e.g. `grep -n "Exercise 238" htdp-book/text/part_three.txt`.
- When reviewing an exercise, read its statement and surrounding section there first, and only suggest techniques the book has introduced by that point (e.g. higher-order signature notation arrives in §15.2, `local` in §16.2).

## Verification

- Run focused checks from the repo root with `raco test "path/to/file.rkt"`.
- Quote paths: chapter directories contain spaces.
- Prefer testing only the file you changed. Several files have top-level `main`/`big-bang` or batch-IO calls, so broad sweeps can execute programs, print output, or create files as a side effect.

## File Format Gotchas

- Source files use DrRacket's HtDP teaching languages via a `#reader(lib "htdp-<level>-reader.ss" "lang")...` header, not `#lang racket` — the level (beginner, beginner-abbr, intermediate, …) advances with the book. Preserve that reader line and its embedded teachpack/settings metadata.
- `prologue.rkt` is a DrRacket `wxme` editor file, not normal text. Treat it as DrRacket-managed unless the task explicitly requires touching that format.
- Ignore `*.rkt~` files during searches and edits unless the user explicitly asks about backups.
