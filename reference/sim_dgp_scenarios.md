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
#>               y         x1          x2         x3           x4
#>           <num>      <num>       <num>      <num>        <num>
#>   1:  0.1383268  0.6969730 -0.11385149 -0.9629154 -0.546009749
#>   2:  3.0302616  0.9008572 -0.02699007  1.4362699 -0.147678736
#>   3: -1.4004301  0.5076176  0.86639418 -2.5034315  1.641666628
#>   4: -0.2942892 -0.3343458 -0.29434882  0.4940121  1.761673848
#>   5: -2.2616514 -0.2183426 -0.15378743 -2.0555398  0.005574552
#>  ---                                                          
#> 196: -1.3712826 -1.2601536 -1.25562092  1.1554728  0.525960329
#> 197:  1.5269383  0.2140719 -0.17396993  1.0617389 -0.122891644
#> 198:  4.9221695  2.0887526  1.71095122  0.5668733  0.664002303
#> 199: -5.4030067 -1.6821232 -1.46266413 -2.0395946 -0.237642638
#> 200:  2.4066942  1.4235040  1.42942756 -0.3785214 -1.421530093

# With different correlation
task_high_cor = sim_dgp_correlated(200, r = 0.95)
cor(task_high_cor$data()$x1, task_high_cor$data()$x2)
#> [1] 0.9491007
task = sim_dgp_mediated(200)
task$data()
#>               y     direct    exposure   mediator        noise
#>           <num>      <num>       <num>      <num>        <num>
#>   1:  1.4071722 -0.2806152  1.33624184  1.0380912  0.848877910
#>   2: -0.3625845 -0.9545186  1.12415253 -0.0855780 -0.223475933
#>   3:  3.8523517  1.6706700  1.24281106  2.1504153  2.254703355
#>   4:  0.4282910  0.7647828 -0.04148880  0.2032904 -1.037693496
#>   5: -1.3136631 -0.8757969  0.05405341 -0.5388717 -0.003356046
#>  ---                                                          
#> 196: -2.9787140 -1.7305924 -0.21478083 -1.2919342  0.139784764
#> 197: -2.7128868 -1.1049460 -1.19800021 -1.4461564 -1.569480510
#> 198:  2.5934361  2.0460682 -0.45287834  0.9959856  0.531995462
#> 199:  1.6074271  0.8228680  0.15014672  0.4817324 -0.350016691
#> 200: -1.8534502 -0.3710650 -0.87943070 -0.9565766 -0.189251403
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
#>               y     noise1      noise2         x1          x2          x3
#>           <num>      <num>       <num>      <num>       <num>       <num>
#>   1: -1.1458636  1.1499033  1.49604411 -0.1258770 -0.34034918 -0.89897340
#>   2:  1.3751500  1.3149744 -0.26301444  0.8940792  0.26719078  0.41701946
#>   3:  1.6630308 -0.6770490 -0.32045603  0.1510828  0.88144198  0.39943944
#>   4: -4.6394129  2.1731733  0.04629612 -1.2611181  1.64119399  0.03889935
#>   5:  4.3078767 -0.4850941  0.49812545  1.7575264  1.13678450  0.33618672
#>  ---                                                                     
#> 196:  3.6623939 -0.2194908  0.87170527 -0.9617671 -2.40225186 -1.17061549
#> 197: -2.4485967 -0.9750422  1.31582618  0.3809640 -0.95828118 -1.21816239
#> 198:  1.1733677 -0.7568525  0.07542252  0.8775227  0.64208487  0.11832230
#> 199: -1.1958247 -1.2438673  1.90583805 -0.4286321 -0.25082085 -0.46955017
#> 200: -0.5088222  0.2961222  0.41419182 -2.2902154  0.01523804 -0.90827009
task = sim_dgp_independent(200)
task$data()
#>                y important1  important2  important3 unimportant1 unimportant2
#>            <num>      <num>       <num>       <num>        <num>        <num>
#>   1:  3.75596250  1.2271885  1.10851377  0.70627024   0.19262031  -1.08117811
#>   2: -0.96473878 -0.4750816 -0.29737206  0.90980343  -1.13645475   0.85212732
#>   3: -0.75507236  0.1023374 -0.78056787  0.22401537  -0.14249875   0.07908144
#>   4: -0.28102254  0.8051748 -0.91307884 -2.63306618  -0.93671741   0.06201690
#>   5:  3.29736867  1.0819608  0.92297006 -0.08650601   1.01894768  -0.10297187
#>  ---                                                                         
#> 196:  1.68925201  0.4012678  0.27007884  0.67105528   2.49205628  -1.36276074
#> 197: -0.47105420 -1.0805452  1.10834965  1.15019225   0.07091781   0.44205244
#> 198:  0.09482886  0.4153693 -0.02962345 -1.01584936  -0.14475226  -2.92527535
#> 199: -1.50465701 -0.8679688  0.04753101  0.37964860  -0.10761898   2.14917029
#> 200: -2.38407933 -1.0426442 -0.86980914  1.52548088   0.17461458   0.46551835
```
