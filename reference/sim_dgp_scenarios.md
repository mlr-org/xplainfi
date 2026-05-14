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
#>               y          x1         x2         x3         x4
#>           <num>       <num>      <num>      <num>      <num>
#>   1:  1.2898134  1.01418477  1.8696403 -0.6387840 -0.5387450
#>   2: -3.2870038 -1.30728040 -0.9699627 -0.8593734  1.2803274
#>   3:  0.4512088  0.09154201 -0.2069516  0.2586489  0.1959273
#>   4: -2.9407059 -2.19533059 -1.8194394  1.8837486  0.2050102
#>   5: -1.1604817 -0.84538760 -0.8353758  0.5196751 -2.9226312
#>  ---                                                        
#> 196:  2.5617286  0.76856663  0.7868515  0.8255663 -1.5860158
#> 197: -0.8195873 -1.19593233 -0.9794198  1.4081430 -1.9273215
#> 198:  3.9400320  2.04349825  1.7414704 -0.1837432 -0.2673565
#> 199:  4.2215532  1.34313221  0.4949508  1.2892657  1.6359560
#> 200:  1.7244960  1.10151247  1.0007979 -0.3120159  0.6929400

# With different correlation
task_high_cor = sim_dgp_correlated(200, r = 0.95)
cor(task_high_cor$data()$x1, task_high_cor$data()$x2)
#> [1] 0.9570561
task = sim_dgp_mediated(200)
task$data()
#>               y       direct    exposure   mediator      noise
#>           <num>        <num>       <num>      <num>      <num>
#>   1:  2.3106735  0.780065386  0.57265449  1.3194546  0.2651142
#>   2:  0.5806412 -0.224095578  0.85234931  0.3759976 -0.7911857
#>   3:  2.2124654  0.953966356  0.39878179  1.1374999  0.4505129
#>   4: -0.5391019  0.323698740 -1.10947097 -0.4765170  1.6537148
#>   5: -1.3729415  0.594836318 -1.45069793 -0.9942501 -0.1496287
#>  ---                                                          
#> 196:  0.3886044 -0.472183643  1.28687396  0.3632020  0.5679895
#> 197: -1.0877414 -1.131080324  0.45916930 -0.5495702 -0.1226481
#> 198: -0.9302773  0.008839338  0.06612991 -0.6749322  1.2272764
#> 199:  1.6862846  2.132741622 -0.09845431  0.5610206 -0.8890324
#> 200:  2.0071143  1.404708357  0.41375380  0.8777991  0.4032402
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
#>                y     noise1     noise2          x1         x2          x3
#>            <num>      <num>      <num>       <num>      <num>       <num>
#>   1:  1.20001757 -1.5095572  0.8746698 -0.10613817 -1.8311432  0.54993163
#>   2:  0.00625846 -0.5468273  0.3982702 -0.09454975 -0.3341683 -0.46682848
#>   3: -1.14211162 -1.7728744 -0.6242617  0.21173247 -0.2548234 -0.29499250
#>   4: -2.69642000 -0.6934789 -0.9417615 -1.55246894  0.8337952 -0.19877241
#>   5: -1.53726185  0.9578770 -0.8282233 -1.11847539  0.5617133 -0.27070303
#>  ---                                                                     
#> 196:  0.65535777  0.1259212  0.5336791  0.78584104  0.8692593 -1.14493955
#> 197: -1.21247954 -0.9810424  0.1924748 -0.07378546  0.3084454 -1.38889804
#> 198:  0.30198009 -2.2118711 -0.4401220  0.99664147  0.2738161 -0.07028474
#> 199:  0.31078187  0.6615493 -2.1177985  0.33358713  0.1590624  0.79339237
#> 200: -1.51419364 -1.2576111 -1.3522914  0.78095537 -0.2558259 -1.83441550
task = sim_dgp_independent(200)
task$data()
#>               y important1 important2 important3 unimportant1 unimportant2
#>           <num>      <num>      <num>      <num>        <num>        <num>
#>   1:  2.6783324  1.3245288 -1.0453796  2.6762177  -1.68109893  -1.48016009
#>   2: -3.0620489 -0.9695081 -0.9166506 -1.1795127   0.07156409  -0.05161362
#>   3: -0.4394855 -0.5181026 -0.5771855  2.1076962  -0.69335863  -0.03985836
#>   4: -2.6821668 -0.7380600 -0.3642767 -1.1634460  -1.60500035  -0.13396369
#>   5:  0.7819126  0.3365451  0.6258418 -0.5187832  -0.81005806  -0.11275910
#>  ---                                                                      
#> 196:  0.7691852 -0.3779592  0.5679376  1.6268057   0.48069947   1.85194013
#> 197: -3.4838360 -1.7355054  0.5330948 -1.3647075   0.74021995  -0.46993764
#> 198:  1.8653367  0.3713316  1.2426933 -0.8499973  -0.65049308   0.34958136
#> 199: -0.7521878  0.3123702 -1.1227090  0.1450825   1.47368631   0.56705354
#> 200:  2.2016526  1.3972556 -0.3170222 -0.1619323   1.27806503  -0.47851973
```
