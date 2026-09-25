# Simulation DGPs for Feature Importance Method Comparison

These data generating processes (DGPs) are designed to illustrate
specific strengths and weaknesses of different feature importance
methods like PFI, CFI, and RFI. Each DGP focuses on one primary
challenge to make the differences between methods clear.

The `_nonlinear` variants change only the link functions of a base DGP
and keep everything else (features, roles, noise) identical, so each
base/variant pair differs in exactly one respect. They exist where
nonlinearity is methodologically interesting on its own, not for
completeness: the interaction case is already covered by
[mlr3::mlr_task_generators_friedman1](https://mlr3.mlr-org.com/reference/mlr_task_generators_friedman1.html),
and the confounded and mediated scenarios gain nothing from a nonlinear
link.

## Usage

``` r
sim_dgp_correlated(n = 500L, r = 0.9)

sim_dgp_correlated_nonlinear(n = 500L, r = 0.9)

sim_dgp_toeplitz(n = 500L, p = 4L, r = 0.5, beta = seq(0, 1, length.out = p))

sim_dgp_toeplitz_nonlinear(
  n = 500L,
  p = 4L,
  r = 0.5,
  beta = seq(0, 1, length.out = p)
)

sim_dgp_mediated(n = 500L)

sim_dgp_confounded(n = 500L, hidden = TRUE)

sim_dgp_interactions(n = 500L)

sim_dgp_independent(n = 500L)

sim_dgp_independent_nonlinear(n = 500L)
```

## Arguments

- n:

  (`integer(1)`: `500L`) Number of observations to generate.

- r:

  (`numeric(1)`: `0.9`) Correlation between x1 and x2. Must be between
  -1 and 1.

- p:

  (`integer(1)`: `4L`) Number of features.

- beta:

  (`numeric(p)`: `seq(0, 1, length.out = p)`) Linear coefficients of the
  features.

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

**Correlated Nonlinear DGP:** Identical to the correlated DGP except
that the causal effect of `x1` is a sine. The spurious feature `x2` now
proxies a *non-monotone* effect, and a linear learner recovers little of
the signal from either.

**Mathematical Model:** \$\$(X_1, X_2)^T \sim \text{MVN}(0, \Sigma),
\quad X_3 \sim N(0,1), \quad X_4 \sim N(0,1)\$\$ \$\$Y = 2 \cdot \sin(2
X_1) + X_3 + \varepsilon\$\$ where \\\Sigma\\ has correlation \\r\\ off
the diagonal and \\\varepsilon \sim N(0, 0.2^2)\\.

**Feature Properties:**

- `x1`: Non-monotone causal effect, correlated with `x2`

- `x2`: Correlated with `x1` (correlation = `r`), NO causal effect on y

- `x3`: Independent, linear causal effect (\\\beta = 1.0\\), kept as a
  within-DGP reference

- `x4`: Independent, no effect on y

**Expected Behavior:**

- As for the correlated DGP, but the size of the spurious `x2`
  importance now depends on how well the learner captures the sine, so
  it separates learner flexibility from the marginal-vs-conditional
  question

- A linear learner assigns low importance to `x1` and `x2` alike

**Toeplitz DGP:** A flexible generalization of the correlated DGP to `p`
features with an AR(1) correlation structure, following the
proof-of-concept simulation of the cARFi paper. Correlation decays with
distance in the feature index, so how far spurious importance "leaks"
from a causal feature to its neighbors becomes a function of `r`.

**Mathematical Model:** \$\$X \sim \text{MVN}(0, \Sigma), \quad
\Sigma\_{ij} = r^{\|i - j\|}\$\$ \$\$Y = X \beta + \varepsilon\$\$ where
\\\varepsilon \sim N(0, 1)\\.

**Feature Properties:**

- `x1`, ..., `xp`: Standard normal marginals, \\\text{cor}(X_i, X_j) =
  r^{\|i-j\|}\\

- With the default `beta = seq(0, 1, length.out = p)`, `x1` is a null
  feature correlated `r` with the weakest causal feature `x2`, and
  effects grow with the feature index

**Expected Behavior:**

- **Marginal methods** (PFI, marginal SAGE): Assign importance to `x1`
  through its correlation with `x2`

- **CFI**: Should assign near-zero importance to `x1`

- Importance ranking follows `beta` for both, but the gap between
  neighbors shrinks with `r`

**Toeplitz Nonlinear DGP:** Identical to the toeplitz DGP except that
each feature enters through a symmetric step function (the nonlinear
variant of the cARFi proof-of-concept simulation). The step has zero
linear correlation with the feature, so a linear learner sees no signal
from any feature.

**Mathematical Model:** \$\$Y = g(X) \beta + \varepsilon, \quad g(x) =
\begin{cases} 1 & \|x\| \< \Phi^{-1}(0.75) \\ -1 & \text{otherwise}
\end{cases}\$\$ applied elementwise, with \\X\\ and \\\varepsilon\\ as
in the toeplitz DGP.

**Expected Behavior:**

- As for the toeplitz DGP for flexible learners

- A linear learner assigns near-zero importance to every feature, so
  this separates learner flexibility from the marginal-vs-conditional
  question

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

**Independent Nonlinear DGP:** Identical to the independent DGP except
that two of the three causal effects are nonlinear. The effects stay
additive, so this isolates nonlinearity from interactions (for which see
[mlr3::mlr_task_generators_friedman1](https://mlr3.mlr-org.com/reference/mlr_task_generators_friedman1.html)).

**Mathematical Model:** \$\$Y = 2 \cdot \sin(2 X_1) + X_2^2 + 0.5 \cdot
X_3 + \varepsilon\$\$ where \\X_j \sim N(0,1)\\ independently and
\\\varepsilon \sim N(0, 0.2^2)\\.

**Feature Properties:**

- `important1`: Non-monotone (sine) effect

- `important2`: Symmetric (quadratic) effect, so its linear correlation
  with y is zero

- `important3`: Linear effect (\\\beta = 0.5\\), kept as a within-DGP
  reference

- `unimportant1-2`: Independent noise features with no effect

**Expected Behavior:**

- **Flexible learners** (e.g. random forests): Rank important1 and
  important2 above important3

- **Linear learners**: Assign important2 essentially zero importance,
  since a symmetric effect has no linear signal; the importance ranking
  then depends on the learner rather than on the DGP

## Functions

- `sim_dgp_correlated()`: Correlated features demonstrating PFI's
  limitations

- `sim_dgp_correlated_nonlinear()`: Correlated features with a
  non-monotone causal effect

- `sim_dgp_toeplitz()`: Toeplitz-correlated features with graded linear
  effects

- `sim_dgp_toeplitz_nonlinear()`: Toeplitz-correlated features with
  symmetric step effects

- `sim_dgp_mediated()`: Mediated effects showing direct vs total
  importance

- `sim_dgp_confounded()`: Confounding scenario for conditional sampling

- `sim_dgp_interactions()`: Interaction effects between features

- `sim_dgp_independent()`: Independent features baseline scenario

- `sim_dgp_independent_nonlinear()`: Independent features with nonlinear
  additive effects

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
#>                y           x1          x2         x3         x4
#>            <num>        <num>       <num>      <num>      <num>
#>   1: -0.69512027  0.441209199  0.73488577 -1.5396345 -0.2576389
#>   2:  0.41438040 -0.329014684 -0.43588682  1.1037414 -0.3957107
#>   3:  3.45586046  1.598960756  1.90185921  0.3939166  0.7163383
#>   4:  0.90832323  0.399281143 -0.06144084  0.5209461  0.4693036
#>   5: -0.85744561 -0.004664788  0.71517420 -0.7463366  1.2624788
#>  ---                                                           
#> 196:  1.25010617 -0.250474234 -0.75758423  1.8031539  0.2381913
#> 197: -1.07338869  0.521692135  0.99519360 -1.3645334  0.2529281
#> 198: -0.08262187 -0.175956513 -0.65315791  0.4924728  0.5455155
#> 199: -3.53362813 -1.351689236 -1.56053945 -0.7764810 -0.2681907
#> 200:  2.59484847  0.690567796  0.29282623  1.1055259  0.6489999

# With different correlation
task_high_cor = sim_dgp_correlated(200, r = 0.95)
cor(task_high_cor$data()$x1, task_high_cor$data()$x2)
#> [1] 0.9463994
task = sim_dgp_correlated_nonlinear(200)
task$data()
#>               y         x1         x2           x3         x4
#>           <num>      <num>      <num>        <num>      <num>
#>   1:  0.4961038  1.5851012  1.4648778  0.237236755 -1.6644938
#>   2: -2.2756789 -1.1152857 -1.1120164 -0.824513192  0.3050108
#>   3: -0.8750906 -1.9123050 -0.7775266 -2.239446514  0.3278606
#>   4:  0.9168585 -1.6218701 -0.7329853  0.614737592  0.4279078
#>   5: -3.6349644 -0.9465093 -0.9766947 -1.956682213 -0.4678800
#>  ---                                                         
#> 196: -2.6614660 -0.9164623 -1.0544004 -0.760478040 -0.7448569
#> 197: -0.8283320 -0.1753449  0.5066665  0.007574549  1.2166657
#> 198: -1.1857102 -0.4862010 -1.4739515  0.743954855 -0.6805050
#> 199: -0.9752292 -0.7679415 -0.8053726  0.947579195  2.1886161
#> 200: -2.9460016 -0.3426943 -0.1839082 -1.634903231  1.3004621
task = sim_dgp_toeplitz(200)
cor(task$data(cols = task$feature_names))
#>           x1        x2        x3        x4
#> x1 1.0000000 0.6236740 0.2827082 0.1748879
#> x2 0.6236740 1.0000000 0.5138550 0.2394468
#> x3 0.2827082 0.5138550 1.0000000 0.4870580
#> x4 0.1748879 0.2394468 0.4870580 1.0000000

# More features, stronger correlation, custom effects
sim_dgp_toeplitz(200, p = 6, r = 0.8, beta = c(1, 0, 1, 0, 1, 0))
#> 
#> ── <TaskRegr> (200x7) ──────────────────────────────────────────────────────────
#> • Target: y
#> • Properties: -
#> • Features (6):
#>   • dbl (6): x1, x2, x3, x4, x5, x6
task = sim_dgp_toeplitz_nonlinear(200)
task$data()
#>               y         x1         x2         x3          x4
#>           <num>      <num>      <num>      <num>       <num>
#>   1:  1.5250489  1.1889275 -0.2378317 -0.2614287  0.08711774
#>   2:  0.5259566 -0.2494107  0.6458553  0.4864824  0.25518463
#>   3: -3.4812441 -1.0428893 -1.5456650 -0.7760716  0.81699281
#>   4: -0.7036240  0.2623998  0.4663405  0.5472899  2.52346729
#>   5:  1.3801288 -0.6144641  1.0058142  0.2177039 -0.43267975
#>  ---                                                        
#> 196: -3.6007544 -1.0548660 -1.1957696 -1.2202287  1.15117480
#> 197: -0.2251890  1.1302570  0.1700516 -1.2244396 -1.20978905
#> 198: -3.1606651 -2.5493381 -2.2941308 -1.6170476 -1.83606650
#> 199:  1.9937373  1.2493068  0.2709170  0.6527745  0.45672904
#> 200: -3.1239387 -0.8857850  1.4471825  1.6594654  0.97078225
task = sim_dgp_mediated(200)
task$data()
#>               y      direct   exposure    mediator       noise
#>           <num>       <num>      <num>       <num>       <num>
#>   1: -1.1796583 -0.71195504 -0.6779076 -0.76615423 -0.01321456
#>   2: -0.9710426 -1.38627717  0.4222896 -0.27848843 -0.02288968
#>   3:  4.1126720  2.77493239  1.0976347  2.10066756  0.69356355
#>   4: -2.2214626 -1.01184999 -0.6737389 -1.20383403  0.62821541
#>   5: -0.2909111  0.05450388 -0.3331111  0.06712379 -0.08392873
#>  ---                                                          
#> 196:  0.8935244  0.19564674  0.5745072  0.59399986 -1.36363614
#> 197:  0.3436400 -0.10651090  0.4599143  0.22740824 -0.56890400
#> 198:  2.7746070  1.07101082  1.2873432  1.47367572  0.51287023
#> 199: -1.2402624 -0.08376636 -1.2023887 -0.74372175 -0.61291474
#> 200:  2.5355585  1.40126805  0.3355006  1.33120532  2.65868354
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
#>   1: -1.5439375  0.52219200  0.10989106  0.4906468 -1.88165484  0.5482099
#>   2: -3.1612460  0.23975703 -0.32000222  0.5740744 -1.95884323 -0.3521060
#>   3: -1.3280519 -0.10333405 -0.01910836 -1.5817675 -0.03902160 -1.4169861
#>   4:  1.3373449  1.25131305  1.18095964  0.1750233 -1.61941474  1.5489000
#>   5: -1.4263162 -0.14037836 -1.09360592  0.2158507  0.17279594 -1.8664387
#>  ---                                                                     
#> 196: -2.2262963  0.29227854 -1.27130473 -1.6236259  0.60995089 -0.1044286
#> 197:  0.8169652 -0.71946801  0.27754102 -1.3952714 -0.16273608  0.2801599
#> 198: -3.7662704  0.09769261  0.69212990 -0.2506034  1.18074696 -2.3611302
#> 199: -0.6795057 -0.08193602 -0.07223138  0.7279842 -0.04004922 -0.7180056
#> 200:  2.7353964  0.79632026  0.99273827  0.5628840  2.46419946  0.9610558
task = sim_dgp_independent(200)
task$data()
#>               y important1 important2 important3 unimportant1 unimportant2
#>           <num>      <num>      <num>      <num>        <num>        <num>
#>   1: -2.4531311 -0.2279515 -1.3359966 -1.1715982   0.31468810  -0.78204179
#>   2:  1.1185174  0.7239033 -0.1231053 -0.4376686   0.33447301   0.01202664
#>   3:  0.2780179 -0.6736073  2.0790014 -0.3757064  -1.27868527  -0.25301748
#>   4: -5.1997680 -2.8806478  0.3410198  0.6015936   0.40073541   0.02034307
#>   5:  0.2563846 -0.3325194  1.1113820 -1.0445729  -1.40015245   0.21524370
#>  ---                                                                      
#> 196: -2.4371298 -0.5189712 -0.7491821 -0.6915806   0.07415213  -0.99489375
#> 197: -1.1772859 -1.0542121  1.5433709 -0.6211722  -0.65881190  -1.42677974
#> 198:  1.0585382 -0.2596338  1.2803382  0.3026558   0.20082666  -0.75596250
#> 199:  1.0623913  0.6566141 -0.8091335  0.4426784  -0.04370768  -0.42034398
#> 200: -0.7575620 -0.2137047  0.2593516 -1.2967536  -1.19122195  -0.01170494
task = sim_dgp_independent_nonlinear(200)
task$data()
#>               y important1 important2 important3 unimportant1 unimportant2
#>           <num>      <num>      <num>      <num>        <num>        <num>
#>   1:  2.9971665  0.8621441  1.2316394 -1.0908212    1.4107361   0.09809842
#>   2:  2.9077651  0.6111636  0.6301187  1.4202657    0.1249999  -0.50294692
#>   3: -0.5091473 -0.9702907 -0.5737746  1.7339262    0.6446350  -0.71674599
#>   4:  1.9207413  0.2755722  0.8550414  0.1248591   -1.0520248   2.11669570
#>   5:  1.8646099 -0.3504555  1.6295710  1.2288607   -0.1738976   2.32542584
#>  ---                                                                      
#> 196:  3.7240492  1.2511724 -1.4086005  0.9333534    0.7740457  -1.17422053
#> 197:  1.1201282  0.9408131  0.1007795 -1.4969108    0.1035337  -0.24945974
#> 198: -1.8638798 -0.5599226  0.3373157 -0.5660515   -0.7608566  -0.27486652
#> 199:  1.8835745 -0.7697672 -1.7035110  1.2753898    0.1218285  -0.59123891
#> 200:  0.9982108 -0.5545633 -1.9831886 -2.1700018    2.5138215   0.86741489
```
