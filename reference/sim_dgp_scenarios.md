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
#>              y           x1          x2          x3          x4
#>          <num>        <num>       <num>       <num>       <num>
#>   1: -2.680531 -0.723932447 -0.73327886 -1.55686399 -0.80772113
#>   2:  0.472482 -0.639641622 -0.84084012  1.39128300 -0.24773083
#>   3: -1.533193 -0.233309060 -0.65011512 -0.87970849 -0.91576373
#>   4: -1.615782  0.096707182  0.39697223 -2.04808894 -0.09839084
#>   5: -1.158779 -0.131215530 -0.15133679 -0.62689111  0.72011907
#>  ---                                                           
#> 196:  1.737595  0.703563537  0.12111339  0.43432712  0.46452373
#> 197: -1.432937 -0.813647044 -0.61059252  0.14198544  1.37392245
#> 198: -1.806332 -1.138141425 -1.31216848  0.09883898 -0.23707583
#> 199: -1.724123 -0.514710853 -1.12810036 -0.54571074 -1.21945260
#> 200:  1.886137 -0.004931503  0.08403097  2.10418858 -0.94433564

# With different correlation
task_high_cor = sim_dgp_correlated(200, r = 0.95)
cor(task_high_cor$data()$x1, task_high_cor$data()$x2)
#> [1] 0.9450242
task = sim_dgp_mediated(200)
task$data()
#>               y     direct    exposure   mediator      noise
#>           <num>      <num>       <num>      <num>      <num>
#>   1:  0.5380659  1.6507206 -1.58090268 -0.2417487  0.2971638
#>   2:  0.4045822  0.6251543 -0.02811489  0.1216162 -0.2342132
#>   3: -2.8774267 -1.7318873 -0.69871524 -1.3390988  1.5953201
#>   4:  0.6201106 -0.1109693  0.73608609  0.4900853  1.6469481
#>   5:  0.7101382  1.2637051 -0.76932461  0.1980253 -2.1710623
#>  ---                                                        
#> 196: -0.9288650  0.6448375 -1.25465925 -0.8085357 -0.1884695
#> 197: -1.1899284 -0.5083321 -0.19001679 -0.6847026  1.5004908
#> 198:  3.5527220  1.8147419  1.14422131  1.7273925 -0.1976017
#> 199:  0.8897990  0.6214995  0.38844420  0.4251144 -1.1441463
#> 200: -1.7166783 -1.9477716  0.92220208 -0.4736343 -0.4612006
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
#>               y      noise1      noise2         x1          x2         x3
#>           <num>       <num>       <num>      <num>       <num>      <num>
#>   1: -0.3521564 -0.56416026  1.85154390 -1.3126392  0.08984765  0.1751419
#>   2:  3.1099716 -0.06354237 -0.09227622 -0.6289231 -1.09094959  1.7199377
#>   3:  3.0650735 -1.13311989  0.95026491 -0.6980733 -0.35521564  1.7327267
#>   4: -1.3603035  1.05903612 -0.19473241 -0.9321643  0.05394393 -0.6851730
#>   5:  1.3345090  0.46439162  0.58617045  1.2797814  0.02826156  1.0475276
#>  ---                                                                     
#> 196:  1.3111309  0.26860208 -0.44885285 -0.7454773 -0.45849620  0.6480072
#> 197:  1.6136951  0.65334216  0.73020209  0.2212917  0.87771525  1.1426400
#> 198:  1.2116892  1.25088983  0.27920369  0.6193738 -0.46661829  2.2653028
#> 199: -0.8670141  1.72818821  0.81900619  0.8734467 -0.81061463  0.4286260
#> 200:  3.5371571  0.06010483 -0.02047754  0.3999491  2.76541502  1.6795143
task = sim_dgp_independent(200)
task$data()
#>               y important1  important2  important3 unimportant1 unimportant2
#>           <num>      <num>       <num>       <num>        <num>        <num>
#>   1: -3.9755407 -2.3887282  0.82076645 -0.33870112   0.02741222    0.2612288
#>   2:  5.4693090  2.6533199 -0.42264919  1.59612901  -0.94986004   -0.4688073
#>   3:  1.0629267  0.1858970  0.55051950  0.57902433   0.61100139    0.6401850
#>   4:  0.5859039  0.2925336 -0.16537831  0.46230276   0.29150292    0.5608116
#>   5: -5.4113327 -1.0959153 -2.20507645 -1.63976288   0.54801071    0.9221985
#>  ---                                                                        
#> 196: -4.9310377 -1.1524870 -2.37271821 -0.10810292  -0.03585948   -1.9058852
#> 197: -0.1356868 -0.8208112  1.81235372 -0.98841988   0.26992514    0.7337208
#> 198: -2.7226092 -1.4814388  1.13483054 -1.96985465   1.46277220    1.0544528
#> 199:  4.0899725  1.3669662  1.06195754  0.99426830  -0.32599883    0.2140141
#> 200:  1.8714903  0.9308251  0.01512051  0.03653186   0.72861874   -0.6971588
```
