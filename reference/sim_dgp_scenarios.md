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
Intelligence*, 440–464. ISBN 978-3-031-63797-1.
[doi:10.1007/978-3-031-63797-1_22](https://doi.org/10.1007/978-3-031-63797-1_22)
.

## See also

Other simulation:
[`sim_dgp_ewald()`](https://mlr-org.github.io/xplainfi/reference/sim_dgp_ewald.md)

## Examples

``` r
task = sim_dgp_correlated(200)
task$data()
#>                y          x1         x2          x3          x4
#>            <num>       <num>      <num>       <num>       <num>
#>   1: -1.56233426 -0.85567321 -0.1757790  0.18957560  0.77182207
#>   2:  1.23647900  0.54453536  0.8394943  0.04786178  0.43704889
#>   3: -3.99879999 -2.15751113 -1.4954627  0.20720456  0.86302855
#>   4: -2.31793390 -0.99100964 -1.2985687 -0.48162445  0.07317525
#>   5:  2.53127947  1.28627049  1.7663006 -0.09473848  1.92332771
#>  ---                                                           
#> 196:  0.41770097 -0.45171814 -0.3139804  1.01584015  0.43527293
#> 197:  3.28024715  2.30678380  2.7388082 -1.39650759 -0.93680513
#> 198: -0.11842641 -0.10266824  0.1665224  0.27081046  1.52522295
#> 199:  0.51280696  0.05398648 -0.3422510  0.63380940 -0.31275210
#> 200: -0.02711591  0.08923512  1.0886609 -0.08767144  0.69128718

# With different correlation
task_high_cor = sim_dgp_correlated(200, r = 0.95)
cor(task_high_cor$data()$x1, task_high_cor$data()$x2)
#> [1] 0.964518
task = sim_dgp_mediated(200)
task$data()
#>               y      direct   exposure   mediator      noise
#>           <num>       <num>      <num>      <num>      <num>
#>   1: -1.0448636 -0.76607654 -0.5352188 -0.6599801 -1.0521494
#>   2: -2.4120830 -0.98190147 -0.4501933 -1.2973925 -0.2740551
#>   3:  0.6139283 -0.09596803  1.1751283  0.4068364 -0.9682156
#>   4: -2.1850252 -0.28467939 -1.2914221 -1.2184397 -1.3575901
#>   5: -2.3724818 -1.83607697  0.2103732 -0.8521622 -1.5788991
#>  ---                                                        
#> 196:  1.5185311 -0.19308273  0.9864197  0.9029298 -1.4603482
#> 197: -2.2348339 -0.25843237 -1.4854236 -1.3951212 -0.6644109
#> 198:  1.7457270  0.84582010  0.6012399  0.9727775  1.0546399
#> 199: -1.2615716  0.40867555 -1.7458815 -1.1440169 -1.4500075
#> 200: -1.3589001  0.65942972 -1.6016322 -0.8775154  0.1290953
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
#>               y       noise1      noise2          x1            x2         x3
#>           <num>        <num>       <num>       <num>         <num>      <num>
#>   1:  0.8686765 -0.291132100  0.16702772  0.20243874  0.0000257905  0.7925762
#>   2: -0.8679986 -0.778925285 -0.07458649  0.59974145 -0.9576897634 -0.2870565
#>   3:  0.5320281 -0.001448312  0.14668057 -0.01848349  0.5093433928  0.8152208
#>   4:  2.7880091 -1.017502333  0.77731199 -1.79237960 -1.0043858601 -0.7157472
#>   5:  0.9336115  0.900157209  0.32587057  0.24215465  0.5004082458  0.6072789
#>  ---                                                                         
#> 196:  3.6778120 -1.284077485 -0.29768397  1.14243270  0.8031648632  1.4797409
#> 197:  1.8207533 -1.100763635 -0.74336950  0.42588107  0.2323358291  0.9844194
#> 198: -1.0888766  1.110759395 -0.32870170  1.57198764  0.3081857058 -2.3818430
#> 199: -1.2233233  1.528626532 -0.69904635  0.85141100  0.3592611455 -0.7903082
#> 200:  5.1734777 -1.402559134 -2.38358794  1.41411527  1.6405085478  0.8870459
task = sim_dgp_independent(200)
task$data()
#>                y important1  important2 important3 unimportant1 unimportant2
#>            <num>      <num>       <num>      <num>        <num>        <num>
#>   1:  0.13421702 -0.9064696  0.13433523  3.4652397   1.97363558   2.44134111
#>   2: -3.21097506 -1.3603639 -0.06563275 -1.1044188   1.23246455   1.23082025
#>   3:  1.33766836  0.4801532  0.83980356 -1.6431651   0.05048308  -0.18534467
#>   4: -0.62345939 -0.2035125 -0.48522613  0.7663791   1.10142104  -0.11344231
#>   5: -0.81628158 -0.2950791 -0.45641723  0.4614419  -0.61678307   0.06559093
#>  ---                                                                        
#> 196: -0.41468902 -1.3211565  1.26943955  1.3842612  -1.07433210   0.58967835
#> 197: -0.76953179 -0.6869133  1.06530945 -0.5050095  -0.04788688  -0.09224151
#> 198: -1.63393932 -1.0509718  0.23332692  0.5449418   2.00154001   0.13547813
#> 199: -0.04886783 -0.2342709  0.16168739  0.3662759   1.02960212   0.37632348
#> 200:  1.37590756  0.3256438  0.42697763  1.2312201   0.41004199   2.68038828
```
