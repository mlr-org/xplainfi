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
where \\\Sigma\\ is a \\2 \times 2\\ covariance matrix with 1 on the
diagonal and correlation \\r\\ on the off-diagonal. \$\$X_3 \sim N(0,1),
\quad X_4 \sim N(0,1)\$\$ \$\$Y = 2 \cdot X_1 + X_3 + \varepsilon\$\$
where \\\varepsilon \sim N(0, 0.2^2)\\.

**Feature Properties:**

- `x1`: Standard normal from MVN, direct causal effect on y (\\\beta =
  2.0\\)

- `x2`: Correlated with `x1` (correlation = `r`), NO causal effect on y
  (\\\beta = 0\\)

- `x3`: Independent standard normal, direct causal effect on y (\\\beta
  = 1.0\\)

- `x4`: Independent standard normal, no effect on y (\\\beta = 0\\)

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

**Causal Structure:** exposure -\> mediator -\> y \<- direct -\>
mediator

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
  unimportant1,2 (approximately 0)

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

## See also

Other simulation:
[`sim_dgp_ewald()`](https://mlr-org.github.io/xplainfi/reference/sim_dgp_ewald.md)

Other simulation:
[`sim_dgp_ewald()`](https://mlr-org.github.io/xplainfi/reference/sim_dgp_ewald.md)

Other simulation:
[`sim_dgp_ewald()`](https://mlr-org.github.io/xplainfi/reference/sim_dgp_ewald.md)

Other simulation:
[`sim_dgp_ewald()`](https://mlr-org.github.io/xplainfi/reference/sim_dgp_ewald.md)

Other simulation:
[`sim_dgp_ewald()`](https://mlr-org.github.io/xplainfi/reference/sim_dgp_ewald.md)

## Examples

``` r
task = sim_dgp_correlated(200)
task$data()
#>                y         x1         x2         x3         x4
#>            <num>      <num>      <num>      <num>      <num>
#>   1:  1.07443429  0.7247021  1.0783212 -0.3498109 -0.1542626
#>   2: -1.27369019 -0.3239298 -0.5479473 -0.6918682  0.2757990
#>   3: -0.04787933 -1.0108349 -0.7911916  1.7350980  1.7380799
#>   4: -0.85568201 -0.3637147 -0.7517807  0.1091285  0.5658416
#>   5:  0.17234091  0.5313798  0.5984704 -0.9451395  0.1120640
#>  ---                                                        
#> 196: -0.98074901 -0.5895649 -0.8533400  0.1900975 -0.5441447
#> 197: -1.98241970 -0.8072049 -1.0033702 -0.4173807 -1.3467181
#> 198: -3.51755244 -1.1105988 -1.5555767 -1.1201463  1.5892296
#> 199: -1.24662654 -0.3263952 -0.6487019 -0.6639614  1.2676205
#> 200: -6.46855687 -3.1410565 -2.5888999 -0.2086050 -0.5805086

# With different correlation
task_high_cor = sim_dgp_correlated(200, r = 0.95)
cor(task_high_cor$data()$x1, task_high_cor$data()$x2)
#> [1] 0.961228
task = sim_dgp_mediated(200)
task$data()
#>               y     direct   exposure   mediator      noise
#>           <num>      <num>      <num>      <num>      <num>
#>   1: -0.8438386 -0.3079838 -0.4155640 -0.4350984  0.7859485
#>   2:  3.1192554  2.2420571  0.3476729  1.2353687 -0.5332259
#>   3:  1.1699368  0.2974630  0.9004481  0.5590843 -1.2822132
#>   4: -3.7600653 -2.0167605 -0.1622083 -1.7575890 -0.4667710
#>   5:  0.7373922 -0.7233280  0.9474198  0.5905845  0.8294418
#>  ---                                                       
#> 196:  3.3011092  1.6443451  1.0046298  1.6582872  0.3863334
#> 197:  4.3597768  2.4073720  1.2136482  2.3242832 -1.5519238
#> 198:  3.7570818  0.9817321  1.7604758  2.0992219 -0.4019963
#> 199:  0.1044258 -1.1970151  1.0468488  0.3571339  0.4254793
#> 200: -0.7351683 -0.2020410 -0.5285364 -0.5249404  0.5069934
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
#>                y     noise1     noise2         x1          x2          x3
#>            <num>      <num>      <num>      <num>       <num>       <num>
#>   1: -2.90003713  0.5979755 -0.6225577  0.9209392 -1.87533029 -0.84707514
#>   2: -1.99208921 -0.1427276  0.4920769 -0.7823324  1.65722956  0.97297323
#>   3: -0.86584049  0.8901172  0.2334693  0.8567056 -0.58219091  0.08741571
#>   4:  1.97605464 -1.6218752  1.0560187 -0.5373473 -2.18733388 -0.45433366
#>   5:  0.33167936  1.8299619  0.8761618 -0.6850280 -0.42942821 -0.65782034
#>  ---                                                                     
#> 196: -2.77536645 -0.9627287 -0.1749047  0.8350220 -1.57658947 -0.21964425
#> 197: -1.48914352 -0.7370495 -0.6748140  1.3146498 -0.71894867  0.41027371
#> 198: -0.29906381 -1.1894481  1.5431048  0.5730079 -0.61607165 -0.38002873
#> 199: -2.03579173  0.3972096  0.2876543  1.5243544 -0.06684301 -1.54391707
#> 200: -0.04526492  0.5840553  0.2795661 -0.9266456  0.21247125  0.58162852
task = sim_dgp_independent(200)
task$data()
#>               y  important1 important2  important3 unimportant1 unimportant2
#>           <num>       <num>      <num>       <num>        <num>        <num>
#>   1:  0.1924801 -0.36346551  0.9065750 -0.58655808    0.3826055    0.1936410
#>   2: -3.7098452 -1.87531181  0.7427155 -0.51200539   -1.2711770    1.8488801
#>   3: -3.1023949 -1.52125813  0.1954028  0.06967424    0.3353996   -0.5771160
#>   4: -0.2788211  0.22649727 -0.9349219 -0.23603879    0.2773690   -2.7288938
#>   5: -0.3638238 -0.48377696  0.3312750  0.16452001    0.5725728   -0.2302252
#>  ---                                                                        
#> 196: -1.6446188 -0.31815032 -1.4213899  0.89409581   -0.4676632    0.2700802
#> 197: -0.6819125 -0.69709913  1.0343990 -0.26676454   -1.2590153   -0.8168216
#> 198:  1.4878788  0.75885108  0.8882495 -1.57741333    0.6044820   -0.4715877
#> 199: -1.4000559 -1.05085009  0.6023162 -0.35733052   -2.1182633   -0.1554175
#> 200: -0.4686897 -0.08432597 -0.4227289  0.18816441   -0.6938619    1.4428712
```
