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
task = sim_dgp_correlated_nonlinear(200)
task$data()
#>                y         x1         x2          x3         x4
#>            <num>      <num>      <num>       <num>      <num>
#>   1: -0.87803248 -0.6925921 -0.6657047  0.75946967 -1.0521494
#>   2:  0.02620165  0.3098459 -0.4701458 -1.16032347 -0.2740551
#>   3: -1.93773607 -0.1016425 -0.3348272 -1.58561824 -0.9682156
#>   4:  0.55349314  1.3604368  0.4236518 -0.04831462 -1.3575901
#>   5:  1.80166446  0.5114509  1.2489826  0.27061812 -1.5788991
#>  ---                                                         
#> 196:  3.02376619  0.7609505  0.6211370  0.76547865 -1.4603482
#> 197:  0.77496611  0.2504162  0.5086884 -0.17240946 -0.6644109
#> 198: -1.29256138 -0.2923376 -0.2825627 -0.05235494  1.0546399
#> 199:  1.16378474  0.2302314  0.5794267  0.02494329 -1.4500075
#> 200:  1.62323330  0.6964930  0.7757885  0.02710842  0.1290953
task = sim_dgp_mediated(200)
task$data()
#>                y      direct   exposure    mediator      noise
#>            <num>       <num>      <num>       <num>      <num>
#>   1:  3.09752998  1.69529551  0.3385341  1.49081838  1.4041188
#>   2: -0.43399287 -0.02600798 -0.1484359 -0.27872418 -0.4991568
#>   3:  2.07362968  1.55755864 -0.4141184  0.71583810 -0.2234116
#>   4: -2.10801236 -0.60778862 -0.4844542 -1.19312395  1.2979962
#>   5: -0.63094754 -0.11408312 -0.4201045 -0.41008161 -1.1219520
#>  ---                                                          
#> 196: -0.75566146 -0.19407000 -0.4015631 -0.51552579 -0.8633473
#> 197:  0.78614614  1.56617704 -1.2630736  0.18135115  0.1235631
#> 198: -2.38545069 -1.28453917 -0.2970349 -1.14598543  1.1177951
#> 199: -0.97989218 -1.54145586  1.3472004 -0.12497120 -0.4143468
#> 200: -0.03394123 -0.05492028  0.8387910  0.03525295 -0.8190621
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
#>               y     noise1      noise2         x1         x2          x3
#>           <num>      <num>       <num>      <num>      <num>       <num>
#>   1:  1.0791130  3.4652397  1.97363558  0.1521799 -0.9064696  0.13433523
#>   2: -2.5398290 -1.1044188  1.23246455  1.1355808 -1.3603639 -0.06563275
#>   3:  0.2393906 -1.6431651  0.05048308 -0.5287277  0.4801532  0.83980356
#>   4: -0.4632082  0.7663791  1.10142104 -0.1934503 -0.2035125 -0.48522613
#>   5: -0.5227449  0.4614419 -0.61678307  0.1679603 -0.2950791 -0.45641723
#>  ---                                                                    
#> 196: -0.3537627  1.3842612 -1.07433210  0.7258949 -1.3211565  1.26943955
#> 197: -0.7350205 -0.5050095 -0.04788688  1.2768782 -0.6869133  1.06530945
#> 198: -1.0611540  0.5449418  2.00154001  0.6480764 -1.0509718  0.23332692
#> 199:  1.3288886  0.3662759  1.02960212 -2.0895458 -0.2342709  0.16168739
#> 200:  1.3069660  1.2312201  0.41004199 -0.7066091  0.3256438  0.42697763
task = sim_dgp_independent(200)
task$data()
#>               y  important1  important2  important3 unimportant1 unimportant2
#>           <num>       <num>       <num>       <num>        <num>        <num>
#>   1:  0.5740412  0.40100598 -0.22168533  0.09085489  -0.37246735   1.15280440
#>   2:  0.3635208  0.63797455 -0.93832925  0.40104716  -2.59920209   0.48763246
#>   3:  3.0904612  1.79570485  0.05297207 -0.81226925   1.91767742  -0.06283736
#>   4:  0.3676102 -0.57198941  1.52898349 -0.53849078   0.68306642  -0.77580631
#>   5: -0.1380609 -0.00213578  0.72508337 -1.28929129   0.37063754  -0.79205159
#>  ---                                                                         
#> 196:  2.0793382  1.33026933 -0.64223716  0.71297423   2.78908660   0.52347976
#> 197: -4.1038912 -1.04254939 -2.27910689  0.80420673   0.08894155  -1.04152862
#> 198:  0.5905209 -0.18896784  0.94110071 -0.71581930  -0.23543045   0.27416676
#> 199:  2.7732414  0.37424303  1.14943920  1.48344748   0.85067466  -1.65713984
#> 200: -2.6737294 -1.58983858  0.01112003  0.75395568   1.08489785   0.95864203
task = sim_dgp_independent_nonlinear(200)
task$data()
#>                 y  important1  important2 important3 unimportant1 unimportant2
#>             <num>       <num>       <num>      <num>        <num>        <num>
#>   1: -2.820136928 -0.71732372 -0.07173837 -1.1574362   2.28047482   0.07364662
#>   2:  1.037347498  0.45133666  0.51109675 -1.8204776  -0.26469776  -0.65852185
#>   3:  0.741869581  0.31758361 -0.64359030 -1.5354851  -0.87978151   2.14710637
#>   4:  2.988278184  0.45871427  1.23900146 -0.3152977  -0.41623025  -0.13690496
#>   5: -0.005113735 -1.61997203  0.07348610 -0.8005071   0.81582689   0.38940049
#>  ---                                                                          
#> 196:  3.126693283  1.62683549 -1.64535335  1.7097201  -2.68423569  -0.58997190
#> 197: -0.011815692 -0.03852791 -0.04247643  0.4603375  -0.05427792   0.24761553
#> 198:  1.746531975  0.14102406 -0.80628084  0.2762334   0.07066434   1.04795177
#> 199:  1.216654392  0.24183741 -0.41375831  0.4422180   0.60481235   2.23892669
#> 200:  1.806224588  1.28983652  0.50373544  0.9630879  -1.30015735   0.51258705
```
