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
#>               y         x1          x2         x3         x4
#>           <num>      <num>       <num>      <num>      <num>
#>   1:  3.5086228  2.3739115  2.27854973 -1.3047030  0.6802114
#>   2:  0.3160273 -0.1202408  0.06809580  0.6088383 -0.2426855
#>   3: -0.4240733  0.2912600  0.70182318 -0.8120685 -1.7451540
#>   4: -2.3854714 -1.2964999 -1.21193492  0.1129467 -1.2339886
#>   5: -3.8124078 -1.5808545 -1.69392605 -0.7195614 -1.5326436
#>  ---                                                        
#> 196: -0.1507502  0.3032338  0.07070621 -0.5115658 -0.5943514
#> 197: -3.5240129 -1.1180362 -0.79817788 -1.7354896  0.2559298
#> 198: -0.6150132 -0.4483046 -0.21201158  0.6259426  1.0133033
#> 199: -0.8411900 -1.2288842 -0.82844550  1.5638294  0.7094097
#> 200:  2.8645297  1.1586435  0.80932928  0.3835062 -0.4330287

# With different correlation
task_high_cor = sim_dgp_correlated(200, r = 0.95)
cor(task_high_cor$data()$x1, task_high_cor$data()$x2)
#> [1] 0.9564631
task = sim_dgp_mediated(200)
task$data()
#>               y     direct    exposure   mediator       noise
#>           <num>      <num>       <num>      <num>       <num>
#>   1: -0.2558845 -0.5057433 -0.08422492 -0.2397263  0.09434266
#>   2:  1.0393519  0.6875956 -0.26377055  0.2807641  1.32043610
#>   3:  0.5308421  0.2445455 -0.09941249  0.2227038 -1.26496285
#>   4: -0.9744397  0.3688921 -0.95921452 -0.7233904 -1.38435569
#>   5:  2.1191797  1.6281251 -0.70571995  0.7087117  0.36034382
#>  ---                                                         
#> 196: -1.3883831 -0.8353297 -0.75175012 -0.8588457  0.84785152
#> 197:  1.0555357  0.1279330  0.36881596  0.7659646 -1.93111685
#> 198: -1.5379470 -0.8819137  0.49303526 -0.6153501 -0.30007875
#> 199:  0.6150672 -0.2541678  0.28897104  0.3669280 -1.99027409
#> 200: -3.4979095 -2.5336223 -0.11831229 -1.5071141 -0.78867698
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
#>               y     noise1      noise2          x1         x2          x3
#>           <num>      <num>       <num>       <num>      <num>       <num>
#>   1: -0.3971196 -1.8192357  0.79114256 -0.44424984  0.7581484 -0.43514143
#>   2: -0.6789936 -1.6299329 -0.52576664 -0.27084066  1.1168832 -0.99744643
#>   3:  0.5754177  0.1806157  0.83344835  0.04275333 -0.2912638 -0.02597943
#>   4: -1.4122238  1.1225485 -0.08880987 -0.61932878  0.3976816 -0.38121805
#>   5:  1.9419322 -0.1630032  3.28775219 -1.19920201 -0.5820011  0.32708470
#>  ---                                                                     
#> 196:  1.1639690  0.3378486 -0.40120306 -0.05358271  1.1390647  0.62218729
#> 197:  1.0178608  0.5210962 -0.53933062 -0.54136614 -0.4919935  1.47962775
#> 198: -1.0102450 -1.1650768 -0.33386391 -0.76410271  0.9499500  0.31168673
#> 199: -2.3841177  1.2184696 -1.40755729  1.26971537 -0.7294755 -0.59292000
#> 200:  0.3209181  0.8152151 -0.20269233  0.05039309 -1.0028597  0.59789395
task = sim_dgp_independent(200)
task$data()
#>                y   important1  important2  important3 unimportant1 unimportant2
#>            <num>        <num>       <num>       <num>        <num>        <num>
#>   1:  3.13523909  1.666734751 -0.38765171  0.99263220    0.2093885  -0.34725051
#>   2: -2.08894306 -1.127438412  0.56408760 -0.57866520    1.0783999   0.36043175
#>   3:  1.76698647  0.935045868  0.23149435 -0.92350142    1.2106659   0.95039878
#>   4: -0.03669799  0.002857508 -0.05402223 -0.28340443   -1.2764969  -0.40547383
#>   5: -0.87739922 -0.350128886 -1.27703343  1.95848462    0.4922211  -0.44497006
#>  ---                                                                           
#> 196: -1.94600678 -0.184542802 -1.54249380 -0.30439462   -1.3168109  -0.08652575
#> 197: -1.78772004 -0.500364562 -0.17941744 -1.18814761    0.3482896  -0.76049254
#> 198: -0.14197559 -0.116753185 -0.21794212  0.81566808    1.8662782   1.38713285
#> 199: -0.64029723 -0.075530836 -0.28996242 -0.08903692    0.6890199   1.53675260
#> 200: -0.41772275 -0.270928522  0.44214285 -1.00297048   -1.4577478  -0.11013045
```
