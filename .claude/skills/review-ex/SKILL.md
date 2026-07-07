---
name: review-ex
description: Review a solved HtDP exercise against the book's statement and constraints, then run its tests. Use when the user asks to review, check, or critique an exercise solution (e.g. "/review-ex 258" or "review my solution to exercise 258").
---

Review the user's solution to exercise $ARGUMENTS. Never rewrite the solution or provide an alternative one — this is a review, not a fix.

1. **Locate the file.** Find `ex-$ARGUMENTS.rkt` in the part/chapter directories (e.g. `III Abstraction/14/ex-150.rkt`). If it doesn't exist or is empty, stop and say so — there is nothing to review yet.

2. **Read the exercise statement.** Grep the local book mirror: `grep -n "Exercise $ARGUMENTS" htdp-book/text/*.txt`, then read the statement and enough of the surrounding section to know what the exercise actually asks for and which techniques the book has introduced by that point.

3. **Review the solution** against:
   - Does it do what the exercise asks (all parts, including "also do X" tails)?
   - Design recipe: signature, purpose statement, examples/tests, template-consistent structure for the data involved.
   - Only techniques introduced so far in the book — flag anachronisms (e.g. higher-order signature notation before §15.2, `local` before §16.2).
   - Edge cases the tests miss.

4. **Run the tests**: `raco test "path/to/ex-$ARGUMENTS.rkt"` (quote the path — chapter directories contain spaces). Test only this file; broad sweeps execute programs as side effects.

5. **Report**: test results, what's solid, and concrete issues ordered by importance. Point at specific lines. Suggest what to reconsider rather than writing the corrected code.
