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
#>               y         x1         x2          x3         x4
#>           <num>      <num>      <num>       <num>      <num>
#>   1: -1.8904090 -0.3864652 -0.1425856 -1.29704038 -2.3201308
#>   2: -0.5911444 -0.7394614 -1.1075541  0.50791219 -0.7852117
#>   3:  0.6039825 -0.1257942  0.1581399  0.74771048 -0.1778089
#>   4:  3.9573345  1.9376045  1.5429887  0.01652144  0.3807222
#>   5:  5.3962697  2.0486566  2.1855852  1.10583078 -2.3184899
#>  ---                                                        
#> 196: -1.8960696 -0.6641624 -0.4129588 -0.53349737  0.9604060
#> 197:  0.1905454  0.1763217  0.2870971 -0.50943707  0.1589040
#> 198:  2.0986304  0.9185246  0.9850249  0.16231912 -1.7682471
#> 199:  3.4044954  1.2145425  1.0112862  1.02668349  0.8666155
#> 200:  3.0779629  0.5403404 -0.5187116  1.96242316  0.9320713

# With different correlation
task_high_cor = sim_dgp_correlated(200, r = 0.95)
cor(task_high_cor$data()$x1, task_high_cor$data()$x2)
#> [1] 0.955145
task = sim_dgp_mediated(200)
task$data()
#>               y       direct   exposure    mediator      noise
#>           <num>        <num>      <num>       <num>      <num>
#>   1: -2.5526662 -1.565597988 -0.6841665 -1.13609196 -1.5788513
#>   2:  0.6116764  0.152680814  0.4691867  0.40803804  1.2003368
#>   3: -1.7941981 -1.528708179  0.6860528 -0.64642429 -0.2535383
#>   4:  0.9336957  1.665957853 -0.7258828 -0.06714066 -0.6029782
#>   5:  1.7667532  0.863997757  0.3757058  0.92276033  0.4937929
#>  ---                                                          
#> 196: -0.0939475  0.007276463  0.2178308 -0.15679515  1.3067330
#> 197:  1.6920557  0.183227274  0.3211813  1.07562363 -1.2697792
#> 198: -1.6554197 -0.183207966 -1.0799452 -1.07410718  0.3762697
#> 199:  2.6260317  1.805703007  0.1353080  1.03376061 -0.4188767
#> 200:  3.4424034  1.933360740  0.9587412  1.58628463  0.2513132
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
#>               y       noise1      noise2         x1         x2           x3
#>           <num>        <num>       <num>      <num>      <num>        <num>
#>   1: -0.0816020  0.294527804  2.15887761  0.5100882  0.7348541  0.009447535
#>   2: -0.8047106  0.056717753  0.04799380 -0.3325053 -0.9734194 -0.576301105
#>   3:  0.4371675 -0.269835481 -1.85261555  0.2289291 -0.2505210  0.125164276
#>   4:  0.5038127 -0.728453293  0.28315915  0.5982485 -0.1061740  0.436060115
#>   5:  0.7480113 -2.019369654  0.31141572 -0.3653197 -1.1472010 -0.257846099
#>  ---                                                                       
#> 196:  1.7797414  0.870710922  1.54154570  0.1995873  0.7968572  2.061944953
#> 197:  0.2317634  0.486750598  1.55559523 -0.9578879  0.6290096  1.037370042
#> 198:  1.2409268 -0.008958779  0.89330517  0.7489906  1.4241339 -0.968743495
#> 199: -1.8727184  0.312222142  1.02852456 -0.5749409  1.1568031  0.294573816
#> 200:  0.8786011 -0.435829162 -0.08011678 -0.6220276 -1.8167891 -0.988481839
task = sim_dgp_independent(200)
task$data()
#>               y  important1 important2 important3 unimportant1 unimportant2
#>           <num>       <num>      <num>      <num>        <num>        <num>
#>   1: -0.7572342  0.49888218 -1.7345150  0.5778449    0.2338352   -1.0846151
#>   2: -1.0003985  0.04246577 -1.0671204  0.2718555    0.3599515    1.0565585
#>   3:  1.2381639  0.80753117 -0.3292688  0.1636560   -0.5480557    0.4106251
#>   4: -1.7240877 -0.16061306 -0.5536807 -1.5981256    0.1415069   -0.1204161
#>   5:  0.9278418  0.03663703  0.5499684  0.3290837    0.7331272    1.5772022
#>  ---                                                                       
#> 196: -5.5515741 -3.44348420  0.9044160  0.4457775    0.7910591    1.2009669
#> 197: -1.1351502 -0.99430711  1.1153089 -0.1304496    1.5116321   -0.1326604
#> 198:  0.1414280  0.57033222 -0.5789209 -0.5946380    0.1142632   -0.8850365
#> 199: -0.3416438 -0.19358979  0.3909529 -1.0562461   -0.1990446   -1.0650966
#> 200:  0.3450803  0.10800732  0.5254375 -0.7329457    0.8865615   -1.8265938
```
