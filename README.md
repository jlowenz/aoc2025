# Advent of Code 2025 — My Haskell Solutions

Learning Haskell by solving Advent of Code problems.

Each day lives in `dayNN/` as a self-contained [Stack](https://docs.haskellstack.org/)
project — `cd day01 && stack run` prints both answers. Inputs sit alongside the code as
`input.txt`.

## Index

Day numbers link to the puzzle; descriptions are summarized from my solutions, not the
official text.

| Day | Problem | Approach |
| --- | --- | --- |
| [01](https://adventofcode.com/2025/day/1) | Turn a 100-position dial from 50 by `L`/`R` amounts; count landings on 0. | Parsec, then three counters — `mod`, step-by-step, and a closed-form `div`. |
| [02](https://adventofcode.com/2025/day/2) | Sum the numbers in the given ranges whose digits are a repeated pattern. | Part 1 checks a doubled half; part 2, any repeating block size. |
| [03](https://adventofcode.com/2025/day/3) | Pick digits in order from each row to form the largest joltage. | Part 1 is a left/right max split; part 2 keeps a 12-digit running best. |
| [04](https://adventofcode.com/2025/day/4) | Clear `@` rolls with few enough neighbors, repeating until stable. | 3×3 stencil convolution over a [Repa](https://hackage.haskell.org/package/repa) array, subtracting each cleared layer. |
| [05](https://adventofcode.com/2025/day/5) | Count ingredient IDs falling in the fresh ranges, then those ranges' total size. | Allen's interval relations to merge, then a balanced interval tree. |
| [06](https://adventofcode.com/2025/day/6) | Sum stacked arithmetic problems sharing a `+`/`*` operator row. | One parser on a `Part` tag: whitespace-split for part 1, fixed-width columns for part 2. |
| [07](https://adventofcode.com/2025/day/7) | A beam splits down rows of `^`; count splitters, then distinct beam worlds. | Level-by-level propagation; part 2 accumulates `Integer` counts rather than enumerating. |
| [08](https://adventofcode.com/2025/day/8) | Connect 1000 points in 3D by increasing distance and watch components form. | Distances in a min-priority queue, components in `IORef`s. [Two strategies](#day-08-two-aggregation-strategies). |
| [09](https://adventofcode.com/2025/day/9) | Largest axis-aligned rectangle with two of the given points as opposite corners. | Part 2 also confines it to the polygon: segments in interval maps, plus crossing tests. |
| [10](https://adventofcode.com/2025/day/10) | Cheapest set of button presses reaching a target light pattern. | Lights as bitmasks; part 2 becomes an integer program solved via GLPK (`src/Lp.hs`). |
| [11](https://adventofcode.com/2025/day/11) | Count routes from `you` to `out` through a graph of three-letter nodes. | Names packed into `Int`s; part 2 counts by in-degree bookkeeping instead of enumerating. |
| [12](https://adventofcode.com/2025/day/12) | Which of 1000 regions can be packed exactly by six polyominoes. | Precomputed rotations; area and utilization checks short-circuit the placement search. |

## Completeness

All 24 stars. Every `main` prints both answers (somewhere) and all twelve build under `stack build`. 

Code is not even close to a production state; often exploratory and definitely messy. 
