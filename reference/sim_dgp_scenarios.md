# Simulation DGPs for Feature Importance Method Comparison

These data generating processes (DGPs) are designed to illustrate
specific strengths and weaknesses of different feature importance
methods like PFI, CFI, and RFI. Each DGP focuses on one primary
challenge to make the differences between methods clear.

## Usage

``` r
sim_dgp_correlated(n = 500L, r = 0.9)

sim_dgp_mediated(n = 500L)

sim_dgp_confounded(n = 500L, hidden = TRUE)

sim_dgp_interactions(n = 500L)

sim_dgp_independent(n = 500L)
```

## Arguments

- n:

  (`integer(1)`: `500L`) Number of observations to generate.

- r:

  (`numeric(1)`: `0.9`) Correlation between x1 and x2. Must be between
  -1 and 1.

- hidden:

  (`logical(1)`: `TRUE`) Whether to hide the confounder from the
  returned task. If `FALSE`, the confounder is included as a feature,
  allowing direct adjustment. If `TRUE` (default), only the proxy is
  available, simulating unmeasured confounding.

## Value

A regression task
([mlr3::TaskRegr](https://mlr3.mlr-org.com/reference/TaskRegr.html))
with
[data.table](https://rdatatable.gitlab.io/data.table/reference/data.table.html)
backend.

## Details

**Correlated Features DGP:** This DGP creates highly correlated
predictors where PFI will show artificially low importance due to
redundancy, while CFI will correctly identify each feature's conditional
contribution.

**Mathematical Model:** \$\$(X_1, X_2)^T \sim \text{MVN}(0, \Sigma)\$\$
where \\\Sigma\\ is a 2×2 covariance matrix with 1 on the diagonal and
correlation \\r\\ on the off-diagonal. \$\$X_3 \sim N(0,1), \quad X_4
\sim N(0,1)\$\$ \$\$Y = 2 \cdot X_1 + X_3 + \varepsilon\$\$ where
\\\varepsilon \sim N(0, 0.2^2)\\.

**Feature Properties:**

- `x1`: Standard normal from MVN, direct causal effect on y (β=2.0)

- `x2`: Correlated with `x1` (correlation = `r`), NO causal effect on y
  (β=0)

- `x3`: Independent standard normal, direct causal effect on y (β=1.0)

- `x4`: Independent standard normal, no effect on y (β=0)

**Expected Behavior:**

- **Marginal methods** (PFI, Marginal SAGE): Will falsely assign
  importance to x2 due to correlation with x1

- **Conditional methods** (CFI, Conditional SAGE): Should correctly
  assign near-zero importance to x2

- **Key insight**: x2 is a "spurious predictor" - correlated with causal
  feature but not causal itself

**Mediated Effects DGP:** This DGP demonstrates the difference between
total and direct causal effects. Some features affect the outcome only
through mediators.

**Mathematical Model:** \$\$\text{exposure} \sim N(0,1), \quad
\text{direct} \sim N(0,1)\$\$ \$\$\text{mediator} = 0.8 \cdot
\text{exposure} + 0.6 \cdot \text{direct} + \varepsilon_m\$\$ \$\$Y =
1.5 \cdot \text{mediator} + 0.5 \cdot \text{direct} + \varepsilon\$\$
where \\\varepsilon_m \sim N(0, 0.3^2)\\ and \\\varepsilon \sim N(0,
0.2^2)\\.

**Feature Properties:**

- `exposure`: Has no direct effect on y, only through mediator (total
  effect = 1.2)

- `mediator`: Mediates the effect of exposure on y

- `direct`: Has both direct effect on y and effect on mediator

- `noise`: No causal relationship to y

**Causal Structure:** exposure → mediator → y ← direct → mediator

**Expected Behavior:**

- **PFI**: Shows total effects (exposure appears important)

- **CFI**: Shows direct effects (exposure appears less important when
  conditioning on mediator)

- **RFI with mediator**: Should show direct effects similar to CFI

**Confounding DGP:** This DGP includes a confounder that affects both a
feature and the outcome. Uses simple coefficients for easy
interpretation.

**Mathematical Model:** \$\$H \sim N(0,1)\$\$ \$\$X_1 = H +
\varepsilon_1\$\$ \$\$\text{proxy} = H + \varepsilon_p, \quad
\text{independent} \sim N(0,1)\$\$ \$\$Y = H + X_1 +
\text{independent} + \varepsilon\$\$ where all \\\varepsilon \sim N(0,
0.5^2)\\ independently.

**Model Structure:**

- Confounder H ~ N(0,1) (potentially unobserved)

- x1 = H + noise (affected by confounder)

- proxy = H + noise (noisy measurement of confounder)

- independent ~ N(0,1) (truly independent)

- y = H + x1 + independent + noise

**Expected Behavior:**

- **PFI**: Will show inflated importance for x1 due to confounding

- **CFI**: Should partially account for confounding through conditional
  sampling

- **RFI conditioning on proxy**: Should reduce confounding bias by
  conditioning on proxy

**Interaction Effects DGP:** This DGP demonstrates a pure interaction
effect where features have no main effects.

**Mathematical Model:** \$\$Y = 2 \cdot X_1 \cdot X_2 + X_3 +
\varepsilon\$\$ where \\X_j \sim N(0,1)\\ independently and
\\\varepsilon \sim N(0, 0.5^2)\\.

**Feature Properties:**

- `x1`, `x2`: Independent features with ONLY interaction effect (no main
  effects)

- `x3`: Independent feature with main effect only

- `noise1`, `noise2`: No causal effects

**Expected Behavior:**

- **PFI**: Should assign near-zero importance to x1 and x2 (no marginal
  effect)

- **CFI**: Should capture the interaction and assign high importance to
  x1 and x2

- **Ground truth**: x1 and x2 are important ONLY through their
  interaction

**Independent Features DGP:** This is a baseline scenario where all
features are independent and their effects are additive. All importance
methods should give similar results.

**Mathematical Model:** \$\$Y = 2.0 \cdot X_1 + 1.0 \cdot X_2 + 0.5
\cdot X_3 + \varepsilon\$\$ where \\X_j \sim N(0,1)\\ independently and
\\\varepsilon \sim N(0, 0.2^2)\\.

**Feature Properties:**

- `important1-3`: Independent features with different effect sizes

- `unimportant1-2`: Independent noise features with no effect

**Expected Behavior:**

- **All methods**: Should rank features consistently by their true
  effect sizes

- **Ground truth**: important1 \> important2 \> important3 \>
  unimportant1,2 ≈ 0

## Functions

- `sim_dgp_correlated()`: Correlated features demonstrating PFI's
  limitations

- `sim_dgp_mediated()`: Mediated effects showing direct vs total
  importance

- `sim_dgp_confounded()`: Confounding scenario for conditional sampling

- `sim_dgp_interactions()`: Interaction effects between features

- `sim_dgp_independent()`: Independent features baseline scenario

## References

Ewald F, Bothmann L, Wright M, Bischl B, Casalicchio G, König G (2024).
“A Guide to Feature Importance Methods for Scientific Inference.” In
Longo L, Lapuschkin S, Seifert C (eds.), *Explainable Artificial
Intelligence*, 440–464. ISBN 978-3-031-63797-1,
[doi:10.1007/978-3-031-63797-1_22](https://doi.org/10.1007/978-3-031-63797-1_22)
.

## Examples

``` r
task = sim_dgp_correlated(200)
task$data()
#>               y         x1          x2          x3          x4
#>           <num>      <num>       <num>       <num>       <num>
#>   1:  0.1988233  0.3558112  0.09592091 -0.44089261 -0.37359214
#>   2:  4.4439808  1.8857623  2.14396112  0.59190115  0.71325297
#>   3:  0.4949982  0.3295766  0.33435184 -0.43382809 -0.53279407
#>   4:  3.0150855  1.4450772  1.24603720 -0.23279077 -0.80212052
#>   5:  0.7940927  0.5816946  0.60540732 -0.35435092 -0.53253128
#>  ---                                                          
#> 196:  1.0118572  0.5375648  0.16650867  0.07029008  0.66791682
#> 197:  0.3621165 -0.2405438  0.74760445  1.15751848  0.11745412
#> 198: -0.3662537 -0.9787081 -0.52679987  1.36567461  0.06779393
#> 199: -2.9233425 -1.0790281  0.20895486 -0.62572339 -0.64384364
#> 200: -1.1414891 -0.2139570  0.48422793 -1.02957413 -0.55394547

# With different correlation
task_high_cor = sim_dgp_correlated(200, r = 0.95)
cor(task_high_cor$data()$x1, task_high_cor$data()$x2)
#> [1] 0.9554293
task = sim_dgp_mediated(200)
task$data()
#>                y     direct   exposure   mediator       noise
#>            <num>      <num>      <num>      <num>       <num>
#>   1: -2.09067586 -0.8288459 -0.4669271 -0.9013657  0.11568888
#>   2: -1.68849594  0.6657015 -2.2635859 -1.5524571 -0.93656998
#>   3: -2.10068698 -1.8963496  0.9379329 -1.0085500  1.25897454
#>   4:  2.32856052  1.2555192  0.2178414  1.2438239  0.74098349
#>   5: -1.18349872 -0.4411634 -0.4603331 -0.5645074 -1.22235621
#>  ---                                                         
#> 196: -0.97182172 -0.7197189 -0.3181762 -0.3295917  1.54623207
#> 197:  0.08257363  0.7000345 -0.5468015 -0.3545658  0.79929111
#> 198:  1.06452798  1.3535456  0.1636022  0.6712042 -0.06495242
#> 199: -0.11020049  1.1333105 -0.9864391 -0.4701934 -0.29454006
#> 200:  1.80364189  1.1107433  0.6686527  1.1459565 -1.72833124
# Hidden confounder scenario (traditional)
task_hidden = sim_dgp_confounded(200, hidden = TRUE)
task_hidden$feature_names  # proxy available but not confounder
#> [1] "independent" "proxy"       "x1"         

# Observable confounder scenario
task_observed = sim_dgp_confounded(200, hidden = FALSE)
task_observed$feature_names  # both confounder and proxy available
#> [1] "confounder"  "independent" "proxy"       "x1"         
task = sim_dgp_interactions(200)
task$data()
#>               y      noise1      noise2         x1          x2          x3
#>           <num>       <num>       <num>      <num>       <num>       <num>
#>   1:  0.1052359 -0.65909883  0.21143414  1.0731843  0.02065633 -0.48773366
#>   2: -0.3101191 -1.06133274  0.46526820  1.2174555 -0.11573776  0.43572042
#>   3: -1.9750370  1.05451614 -0.85174449 -0.3272769  1.16952895 -0.77746083
#>   4:  1.9205948  1.01159373  0.05395737 -1.8729982 -0.18134721  0.88867049
#>   5: -0.9264540  0.19451762 -1.75853410  0.6436678  0.43372926 -0.89042561
#>  ---                                                                      
#> 196: -0.5861379 -0.08919244 -0.75033289 -0.5104026  0.06227604  0.04127531
#> 197: -1.1227129  1.27261174  0.60500856 -0.9114021  0.27495308 -0.48437807
#> 198: -0.2070160 -0.41302836  1.58817180 -0.4819472  0.31199214 -0.08846544
#> 199: -2.0405295 -1.20418123 -0.28535013  0.9466497 -0.33544568 -0.85098146
#> 200: -1.3674926 -1.08940994 -0.08000474 -0.2999553  0.47475332 -1.07168946
task = sim_dgp_independent(200)
task$data()
#>              y important1 important2  important3 unimportant1 unimportant2
#>          <num>      <num>      <num>       <num>        <num>        <num>
#>   1: -1.062434 -0.1482168 -0.9681594  0.41001493    0.5564233   -0.7875845
#>   2: -3.195441 -1.0490663 -0.4015526 -1.37946409    0.3293214    0.8204012
#>   3:  3.783562  1.4225055  0.9398335 -0.23626963    0.7050078   -1.9056293
#>   4:  1.185540 -0.4122975  0.8727734  2.50302744   -1.4828280   -0.4903590
#>   5: -2.494444 -0.1941624 -1.4186204 -1.07771653    0.4035265    0.8896244
#>  ---                                                                      
#> 196: -1.544499 -0.6445021 -0.2543392  0.06484215   -0.5464436   -0.2989333
#> 197:  3.441020  1.8564388  0.4101827 -1.15787063    1.5452606    0.7546928
#> 198: -3.365942 -1.1628908 -1.0771115  0.30775885    0.7747423    0.3765775
#> 199:  3.197456  1.1644645  0.9089074  0.22025260    0.4048971   -0.6765147
#> 200: -2.724263 -1.5712366 -0.2093543  1.68417425    1.0620308    0.6310499
```
