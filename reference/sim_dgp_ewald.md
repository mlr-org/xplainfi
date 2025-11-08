# Simulate data as in Ewald et al. (2024)

Reproduces the data generating process from Ewald et al. (2024) for
benchmarking feature importance methods. Includes correlated features
and interaction effects.

## Usage

``` r
sim_dgp_ewald(n = 500)
```

## Arguments

- n:

  (`integer(1)`) Number of samples to create.

## Value

A regression task
([mlr3::TaskRegr](https://mlr3.mlr-org.com/reference/TaskRegr.html))
with
[data.table](https://rdatatable.gitlab.io/data.table/reference/data.table.html)
backend.

## Details

**Mathematical Model:** \$\$X_1, X_3, X_5 \sim \text{Uniform}(0,1)\$\$
\$\$X_2 = X_1 + \varepsilon_2, \quad \varepsilon_2 \sim N(0, 0.001)\$\$
\$\$X_4 = X_3 + \varepsilon_4, \quad \varepsilon_4 \sim N(0, 0.1)\$\$
\$\$Y = X_4 + X_5 + X_4 \cdot X_5 + \varepsilon, \quad \varepsilon \sim
N(0, 0.1)\$\$

**Feature Properties:**

- X1, X3, X5: Independent uniform(0,1) distributions

- X2: Nearly perfect copy of X1 (correlation ≈ 0.99)

- X4: Noisy copy of X3 (correlation ≈ 0.67)

- Y depends on X4, X5, and their interaction

## References

Ewald F, Bothmann L, Wright M, Bischl B, Casalicchio G, König G (2024).
“A Guide to Feature Importance Methods for Scientific Inference.” In
Longo L, Lapuschkin S, Seifert C (eds.), *Explainable Artificial
Intelligence*, 440–464. ISBN 978-3-031-63797-1,
[doi:10.1007/978-3-031-63797-1_22](https://doi.org/10.1007/978-3-031-63797-1_22)
.

## Examples

``` r
sim_dgp_ewald(100)
#> 
#> ── <TaskRegr> (100x6) ──────────────────────────────────────────────────────────
#> • Target: y
#> • Properties: -
#> • Features (5):
#>   • dbl (5): x1, x2, x3, x4, x5
```
