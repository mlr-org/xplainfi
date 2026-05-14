# Benchmarks

Runtime comparisons of the current branch against a historical reference
commit. These scripts are **not** unit tests — they are local dev-time
sanity checks for performance-relevant changes.

## How to run

From an R session at the package root:

```r
source("bench/cfi-arf-scaling.R")
source("bench/sage-arf-scaling.R")
```

Each script uses `cross::bench_branches()` to install the package from
each branch / commit into a temporary library, run the benchmark
expression, and aggregate timings into a tibble.

Expect a few minutes per script (two installs + multiple benchmark
iterations).

## Layout

Each script targets one importance method + sampler combination. Adding
a benchmark means a new script in this directory; results are not
persisted to disk by default — `print()` the returned tibble in your
session, or save with `saveRDS()` if you want to compare runs later.

## Reference commits

| Short SHA   | What it is                                                  |
|-------------|-------------------------------------------------------------|
| `aa88c624`  | Last commit before the `samples_per_row` refactor began. CFI and ConditionalSAGE both pass replicated evidence to `arf::forge(n_synth = 1)`. |

To pick a different reference, edit `REF_PRE` at the top of each script.

## Excluded from package build

The whole `bench/` directory is in `.Rbuildignore` so it never ships in
the tarball or makes it onto CRAN.
