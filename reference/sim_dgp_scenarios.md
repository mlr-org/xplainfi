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
with [data.table](https://rdrr.io/pkg/data.table/man/data.table.html)
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

- Will depend on the used learner and the strength of correlation (`r`)

- **Marginal methods** (PFI, Marginal SAGE): Should falsely assign
  importance to x2 due to correlation with x1

- **CFI** Should correctly assign near-zero importance to x2

- x2 is a "spurious predictor" - correlated with causal feature but not
  causal itself

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
  sampling and reduce its importance

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

- Will depend on the used learner and its ability to model interactions

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
#>               y          x1           x2         x3         x4
#>           <num>       <num>        <num>      <num>      <num>
#>   1:  1.3389513 -0.06408294  0.009893616  1.5235490 -0.1713826
#>   2:  0.7728366  0.80322894  0.198380549 -0.6065398  0.1472664
#>   3:  3.4537552  1.02546212  0.921886541  1.0341708 -1.5696164
#>   4:  1.0751190 -0.01977654  0.413854450  0.9904485 -0.6196518
#>   5: -2.5670872 -1.28195890 -0.911735351 -0.2509076  0.8246445
#>  ---                                                          
#> 196:  1.8869212  0.56626013  1.431988915  0.8586208 -1.1149540
#> 197:  0.8752049  0.23406480  0.119261654  0.2147391 -1.4768831
#> 198: -0.8062437 -0.92637225 -0.433328494  0.8242756  0.6048966
#> 199: -1.4936130 -0.21404806 -0.479104304 -0.8260781 -1.5950459
#> 200: -1.6572472 -1.03395280 -1.140383929  0.3558275  0.8902006

# With different correlation
task_high_cor = sim_dgp_correlated(200, r = 0.95)
cor(task_high_cor$data()$x1, task_high_cor$data()$x2)
#> [1] 0.9419921
task = sim_dgp_mediated(200)
task$data()
#>               y      direct    exposure    mediator       noise
#>           <num>       <num>       <num>       <num>       <num>
#>   1:  0.4222490  0.21634573  0.25821131  0.11611504 -0.45577432
#>   2:  1.2399533  0.42953577  0.61806941  0.59291185 -1.64615567
#>   3:  1.5196285  0.60221043 -0.13609997  0.60290373 -0.48893633
#>   4: -2.6979498 -0.66210996 -1.39030720 -1.54408990 -1.06598833
#>   5:  2.5148065 -0.07494283  2.16869909  1.58242258 -0.93798953
#>  ---                                                           
#> 196: -1.1064275 -0.55140745 -0.18642121 -0.69881309 -0.19525172
#> 197:  2.1086648  0.32238904  1.13449913  1.33303602  0.29103945
#> 198:  1.5570108  0.11826230  0.76223107  1.08641454  0.09851852
#> 199:  3.7329951  1.61282503  0.70320018  1.83383083 -0.54292323
#> 200:  0.5971528  1.27108424 -0.06160606  0.09705634  0.43449972
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
#>               y     noise1     noise2          x1         x2         x3
#>           <num>      <num>      <num>       <num>      <num>      <num>
#>   1: -2.2059629 -0.6647187  0.1299030  1.35181656 -0.8989332  0.2237425
#>   2:  1.6570777 -0.4886612 -1.3432050 -0.07467575 -0.4675924  0.7764740
#>   3: -0.7119888  0.5700763  1.2946658  0.64907460 -0.1496162 -0.2154697
#>   4: -0.3309567  0.3827243 -0.9424296  0.36507920  0.1757651 -0.6256711
#>   5: -0.8174818  0.4055048  1.2228314 -0.85742864  0.2787350  0.3618385
#>  ---                                                                   
#> 196:  2.6369930 -0.4655933  0.1941572 -2.07609335 -0.7462815 -0.8093674
#> 197: -0.4104256 -1.3313404  0.7827425  0.06201667  1.6186738 -0.6865771
#> 198: -0.5439796  0.1443296  0.2867504 -1.38986409  0.4579920  0.4841929
#> 199:  1.1222575 -0.1340498 -0.6471952 -1.15010855 -0.2526693  0.4622059
#> 200: -3.0727106  0.3286037 -0.5843386 -1.32105862  0.6410236 -1.4866170
task = sim_dgp_independent(200)
task$data()
#>               y important1  important2  important3 unimportant1 unimportant2
#>           <num>      <num>       <num>       <num>        <num>        <num>
#>   1:  2.4800950  1.1233448 -0.03561928  0.49390915  -0.44473155   -0.9239520
#>   2: -0.4367573 -0.5207896  0.59066220  0.48647786   1.03980658   -0.9378596
#>   3:  2.5543158  1.7579333 -0.60911922 -0.40272261   0.76303054   -1.0687419
#>   4: -5.0525861 -1.1367780 -1.92722790 -1.54281274   2.83611190    0.3352571
#>   5: -2.6676693 -1.6059468 -0.24233627  0.64193462   0.59803875    1.4131479
#>  ---                                                                        
#> 196: -1.6574589 -0.9666351 -0.51159117  1.89441247   0.95685947    0.4854584
#> 197:  1.4560721  0.1190601  0.76303492  0.95115667   1.04314109    0.5507955
#> 198: -4.8375467 -1.4305259 -0.36090723 -2.78742134   0.55211812    0.3392400
#> 199: -0.6687938 -0.3419020  0.20018809  0.07613768  -1.43737146    1.2259787
#> 200:  2.3770031  0.2672968  1.51130826  1.09289399   0.01896733   -1.1843001
```
