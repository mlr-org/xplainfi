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
task = sim_dgp_toeplitz(200)
cor(task$data(cols = task$feature_names))
#>           x1        x2        x3        x4
#> x1 1.0000000 0.5470922 0.2781326 0.1566639
#> x2 0.5470922 1.0000000 0.5781502 0.2487359
#> x3 0.2781326 0.5781502 1.0000000 0.4729196
#> x4 0.1566639 0.2487359 0.4729196 1.0000000

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
#>                y          x1          x2          x3          x4
#>            <num>       <num>       <num>       <num>       <num>
#>   1:  1.09353037  0.73969128  0.06032193  0.58691374 -0.48078926
#>   2:  0.63963609  0.65105537  0.37442783  0.31564825  0.08264968
#>   3: -0.18651347 -1.91492835 -0.86575385  0.13469520  0.99133794
#>   4: -0.20351247 -0.06121846  0.70299821 -0.79310507 -0.48138622
#>   5: -0.96174573  1.17753313  1.50959128  0.29525064  0.88368516
#>  ---                                                            
#> 196:  0.01217682 -0.62269805  1.22918566  0.02424388  0.15838168
#> 197: -1.35357997  0.94239484 -1.12289453  0.13278703  0.82792842
#> 198: -1.05097180 -0.03524031  0.55331983  0.65874087  0.89938577
#> 199: -2.23427089  1.11642481  1.71867250  1.29238050  1.04788123
#> 200: -1.00768953  1.16503372  0.34914272 -1.84707890 -1.08915383
task = sim_dgp_mediated(200)
task$data()
#>               y     direct    exposure   mediator       noise
#>           <num>      <num>       <num>      <num>       <num>
#>   1:  5.9808751  3.4652397  0.13433523  2.7787027  2.44134111
#>   2: -0.9427417 -1.1044188 -0.06563275 -0.3454181  1.23082025
#>   3: -0.9108085 -1.6431651  0.83980356 -0.2989113 -0.18534467
#>   4:  0.8719010  0.7663791 -0.48522613  0.4020729 -0.11344231
#>   5: -0.1796616  0.4614419 -0.45641723 -0.2733036  0.06559093
#>  ---                                                         
#> 196:  3.2438975  1.3842612  1.26943955  1.5238087  0.58967835
#> 197:  0.3412991 -0.5050095  1.06530945  0.5348758 -0.09224151
#> 198:  1.9058103  0.5449418  0.23332692  1.1140886  0.13547813
#> 199:  1.2449807  0.3662759  0.16168739  0.6579961  0.37632348
#> 200:  2.1026325  1.2312201  0.42697763  1.2033268  2.68038828
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
#>                y      noise1      noise2          x1         x2          x3
#>            <num>       <num>       <num>       <num>      <num>       <num>
#>   1: -3.72921521  0.20814972 -0.21834619 -1.32536558  1.3744236 -0.06885733
#>   2: -0.85823471 -2.09080512  0.95755105  0.58197341  1.0683860 -2.61408490
#>   3: -1.34970264 -0.08045929 -0.30047971 -0.45610315  1.0236598  0.90892583
#>   4: -0.34543848 -0.18365219 -0.62504837  0.11360056  0.4320849 -0.31244056
#>   5: -0.27276556  0.21346435  0.80727759  0.96676896 -0.3785328 -0.16582104
#>  ---                                                                       
#> 196: -0.12425705  1.08010302  0.59164782 -1.05833391  0.4788745  0.51513090
#> 197: -0.03028426 -0.15986507 -0.12968416 -0.44914750  0.9530924  1.81130532
#> 198: -1.89421169  0.40486528 -0.05831509  2.00839802 -0.7838062  0.57038316
#> 199: -0.59275293  0.76733061  0.14643123 -0.52860870  0.8147413  0.47035845
#> 200:  0.25059703 -0.47540843  0.73640529  0.02654014 -1.7987990  1.21765255
task = sim_dgp_independent(200)
task$data()
#>               y important1 important2  important3 unimportant1 unimportant2
#>           <num>      <num>      <num>       <num>        <num>        <num>
#>   1: -1.7563318 -0.7609300 -0.1409542 -0.02407361   -1.1913470   0.49826277
#>   2:  1.3964069  0.4004003  0.7705867 -0.98799851    0.5440747  -0.10064842
#>   3: -1.0457406 -0.7080542 -0.1916401  1.09952225    0.7021644   0.07496503
#>   4: -0.2303490 -0.2392158  0.1323551  0.73204791   -0.7661789  -0.51848826
#>   5: -1.0109592 -0.8071869  0.9058805 -0.05714621   -0.6971344  -0.78834649
#>  ---                                                                       
#> 196:  0.4444248  0.5481035 -0.7804927 -0.26528548    0.2721668   0.44424922
#> 197: -0.8627508 -1.0628346  0.8762248  0.39411773   -0.7025471   0.90381086
#> 198:  0.9788672  0.3167526  0.2016382 -0.01955431   -1.1107414   0.43313547
#> 199: -2.2099029 -0.7944637  0.1697192 -0.71677886    0.5047822  -1.26086039
#> 200: -0.7011601 -0.4189291  0.2483522 -0.03760057    0.9840809   1.22931239
task = sim_dgp_independent_nonlinear(200)
task$data()
#>               y important1 important2 important3 unimportant1 unimportant2
#>           <num>      <num>      <num>      <num>        <num>        <num>
#>   1: -1.8534857 -0.9061283 -0.3841070 -0.2255978    0.8683586   -0.5413803
#>   2:  1.4810671  0.3450477  0.2036089 -0.2337387    0.1512786   -0.4773509
#>   3:  3.1790400  0.3679360  1.4341169 -0.6669039   -0.9073540    0.8798601
#>   4:  0.5052139  0.1701021 -0.2658672 -0.4428782   -1.0471584    0.7315262
#>   5:  1.8369101  0.5621031 -0.3548726 -0.1333351    1.0241685    0.5627033
#>  ---                                                                      
#> 196: -2.1740476 -0.9073040 -0.6147340 -0.3944953    0.7861573    2.0663539
#> 197:  1.2636415  0.2447812 -1.0753905 -1.5325120    1.3573083    0.3956258
#> 198:  0.9060698 -1.3761730 -1.3328937  0.4600538    1.9206247   -1.6543321
#> 199:  2.1474193  2.4285510 -2.1771455 -1.1425815    0.1213183   -0.3711244
#> 200: -0.8275322  1.7343286 -0.6727899 -1.6804147    2.5836481    0.8010559
```
