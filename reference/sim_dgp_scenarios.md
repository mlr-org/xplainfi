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
#>                y           x1         x2         x3         x4
#>            <num>        <num>      <num>      <num>      <num>
#>   1:  0.73300912  0.783315212  1.1698596 -0.6065398  0.1472664
#>   2:  1.32211554 -0.040357698  0.4223380  1.0341708 -1.5696164
#>   3:  1.45777085  0.171549391 -0.1716507  0.9904485 -0.6196518
#>   4: -0.06601915 -0.031424884 -0.7301900 -0.2509076  0.8246445
#>   5:  0.04721760  0.317679656  0.6794906 -0.6348972  0.6662048
#>  ---                                                          
#> 196:  4.28403122  1.938477961  1.3743296  0.2147391 -1.4768831
#> 197: -0.39583992 -0.721170374 -1.1067884  0.8242756  0.6048966
#> 198: -0.40216736  0.331674750  0.2941393 -0.8260781 -1.5950459
#> 199: -1.38464181 -0.897650075 -0.8818099  0.3558275  0.8902006
#> 200:  0.31836598 -0.001739906  0.7826777 -0.1713826 -0.2821594

# With different correlation
task_high_cor = sim_dgp_correlated(200, r = 0.95)
cor(task_high_cor$data()$x1, task_high_cor$data()$x2)
#> [1] 0.9454645
task = sim_dgp_mediated(200)
task$data()
#>               y      direct    exposure    mediator       noise
#>           <num>       <num>       <num>       <num>       <num>
#>   1:  1.2399533  0.42953577  0.61806941  0.59291185 -1.64615567
#>   2:  1.5196285  0.60221043 -0.13609997  0.60290373 -0.48893633
#>   3: -2.6979498 -0.66210996 -1.39030720 -1.54408990 -1.06598833
#>   4:  2.5148065 -0.07494283  2.16869909  1.58242258 -0.93798953
#>   5:  3.1800877  1.48541680  0.49346678  1.64397191  0.70801829
#>  ---                                                           
#> 196:  2.1086648  0.32238904  1.13449913  1.33303602  0.29103945
#> 197:  1.5570108  0.11826230  0.76223107  1.08641454  0.09851852
#> 198:  3.7329951  1.61282503  0.70320018  1.83383083 -0.54292323
#> 199:  0.5971528  1.27108424 -0.06160606  0.09705634  0.43449972
#> 200: -0.9393176 -0.73420483  0.21634573 -0.40417861  0.69951769
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
#>               y     noise1       noise2          x1         x2         x3
#>           <num>      <num>        <num>       <num>      <num>      <num>
#>   1:  1.6570777 -0.4886612 -1.343204963 -0.07467575 -0.4675924  0.7764740
#>   2: -0.7119888  0.5700763  1.294665817  0.64907460 -0.1496162 -0.2154697
#>   3: -0.3309567  0.3827243 -0.942429584  0.36507920  0.1757651 -0.6256711
#>   4: -0.8174818  0.4055048  1.222831384 -0.85742864  0.2787350  0.3618385
#>   5:  3.9761422 -0.7315579 -0.627786478 -0.66082866 -1.1932515  1.5982249
#>  ---                                                                     
#> 196: -0.4104256 -1.3313404  0.782742497  0.06201667  1.6186738 -0.6865771
#> 197: -0.5439796  0.1443296  0.286750405 -1.38986409  0.4579920  0.4841929
#> 198:  1.1222575 -0.1340498 -0.647195186 -1.15010855 -0.2526693  0.4622059
#> 199: -3.0727106  0.3286037 -0.584338556 -1.32105862  0.6410236 -1.4866170
#> 200: -0.5053054  0.1299030  0.001360365 -0.89893320  0.2237425 -0.6647187
task = sim_dgp_independent(200)
task$data()
#>                y  important1 important2  important3 unimportant1 unimportant2
#>            <num>       <num>      <num>       <num>        <num>        <num>
#>   1: -0.43675731 -0.52078958  0.5906622  0.48647786   1.03980658   -0.9378596
#>   2:  2.55431577  1.75793333 -0.6091192 -0.40272261   0.76303054   -1.0687419
#>   3: -5.05258611 -1.13677801 -1.9272279 -1.54281274   2.83611190    0.3352571
#>   4: -2.66766925 -1.60594680 -0.2423363  0.64193462   0.59803875    1.4131479
#>   5:  0.07874565 -0.83047173  1.9059784 -0.47022501   0.62371531    1.9633393
#>  ---                                                                         
#> 196:  1.45607208  0.11906006  0.7630349  0.95115667   1.04314109    0.5507955
#> 197: -4.83754675 -1.43052590 -0.3609072 -2.78742134   0.55211812    0.3392400
#> 198: -0.66879384 -0.34190200  0.2001881  0.07613768  -1.43737146    1.2259787
#> 199:  2.37700310  0.26729680  1.5113083  1.09289399   0.01896733   -1.1843001
#> 200:  0.08348134 -0.03561928  0.4939091 -0.44473155  -0.92395201    0.1103508
```
