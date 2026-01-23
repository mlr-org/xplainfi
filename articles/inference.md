# Inference for Feature Importance

``` r
library(xplainfi)
library(mlr3learners)
#> Loading required package: mlr3
library(data.table)
library(ggplot2)
```

There are multiple (work in progress) inference methods available with
the underlying implementation, but the API around them is still being
worked out.

## Setup

We use a simple linear DGP for demonstration purposes where

- \\X_1\\ and \\X_2\\ are strongly correlated (r = 0.7)
- \\X_1\\ and \\X_3\\ have an effect on Y
- \\X_2\\ and \\X_4\\ don’t have an effect

``` r
task = sim_dgp_correlated(n = 2000, r = 0.7)
learner = lrn("regr.ranger", num.trees = 500)
measure = msr("regr.mse")
```

DAG for correlated features DGP

## Variance-correction

When we calculate PFI using an appropriate resampling, such as
subsampling with 15 repeats, we can use the approach recommended by
Molnar et al. (2023) based on the proposed correction by Nadeau & Bengio
(2003).

By default, any importance measures’ `$importance()` method will not
output any variances or confidence intervals, it will merely compute
averages over resampling iterations and repeats within resamplings
(`iter_repeat` here).

``` r
pfi = PFI$new(
    task = task,
    learner = learner,
    resampling = rsmp("subsampling", repeats = 15),
    measure = measure,
    n_repeats = 10 # for stability within resampling iters
)

pfi$compute()
pfi$importance()
#> Key: <feature>
#>    feature   importance
#>     <char>        <num>
#> 1:      x1  6.451498301
#> 2:      x2  0.094961175
#> 3:      x3  1.801051193
#> 4:      x4 -0.001016452
```

If we want **unadjusted** confidence intervals based on a
t-distribution, we can ask for them, but note these are too narrow /
optimistic and hence invalid for inference:

``` r
pfi_ci_raw = pfi$importance(ci_method = "raw")
pfi_ci_raw
#> Key: <feature>
#>    feature   importance           se   conf_lower    conf_upper
#>     <char>        <num>        <num>        <num>         <num>
#> 1:      x1  6.451498301 0.0777539664  6.284732629  6.6182639734
#> 2:      x2  0.094961175 0.0041677716  0.086022194  0.1039001566
#> 3:      x3  1.801051193 0.0157425383  1.767286806  1.8348155792
#> 4:      x4 -0.001016452 0.0003145202 -0.001691031 -0.0003418737
```

Analogously we can retrieve the **Nadeau & Bengio**-adjusted standard
errors and derived confidence intervals which were demonstrated to have
better (but still imperfect) coverage:

``` r
pfi_ci_corrected = pfi$importance(ci_method = "nadeau_bengio")
pfi_ci_corrected
#> Key: <feature>
#>    feature   importance           se   conf_lower   conf_upper
#>     <char>        <num>        <num>        <num>        <num>
#> 1:      x1  6.451498301 0.2267648327  5.965136107 6.9378604959
#> 2:      x2  0.094961175 0.0121550589  0.068891167 0.1210311840
#> 3:      x3  1.801051193 0.0459121794  1.702579361 1.8995230240
#> 4:      x4 -0.001016452 0.0009172796 -0.002983821 0.0009509165
```

## Empirical quantiles

Both `"raw"` and `"nadeau_bengio"` methods assume normally distributed
importance scores and use parametric confidence intervals based on the
t-distribution. As a alternative, we can use empirical quantiles to
construct confidence-like intervals without any coverage guarantees.

``` r
pfi_ci_quantile = pfi$importance(ci_method = "quantile")
pfi_ci_quantile
#> Key: <feature>
#>    feature   importance   conf_lower   conf_upper
#>     <char>        <num>        <num>        <num>
#> 1:      x1  6.451498301  5.953713874 6.9244524536
#> 2:      x2  0.094961175  0.074956969 0.1216748325
#> 3:      x3  1.801051193  1.695615492 1.9012580196
#> 4:      x4 -0.001016452 -0.003636112 0.0005228665
```

To highlight the differences between parametric and empirical
approaches, we visualize all methods:

``` r
pfi_cis = rbindlist(
    list(
        pfi_ci_raw[, type := "raw"],
        pfi_ci_corrected[, type := "nadeau_bengio"],
        pfi_ci_quantile[, type := "quantile"]
    ),
    fill = TRUE
)

ggplot(pfi_cis, aes(y = feature, color = type)) +
    geom_errorbar(
        aes(xmin = conf_lower, xmax = conf_upper),
        position = position_dodge(width = 0.6),
        width = .5
    ) +
    geom_point(aes(x = importance), position = position_dodge(width = 0.6)) +
    scale_color_brewer(palette = "Set2") +
    labs(
        title = "Parametric & non-parametric CI methods",
        subtitle = "RF with 15 subsampling iterations",
        color = NULL
    ) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "bottom")
```

![Point-and-whisker plot with features on y-axis and importance on
x-axis. Three colored points for raw, nadeau_bengio, and quantile CI
methods, each showing point estimates with horizontal error
bars.](inference_files/figure-html/pfi-ci-comparison-1.png)

The results highlight just how optimistic the unadjusted, raw confidence
intervals are.

## Conditional predictive impact (CPI)

CPI is implemented by [the cpi
package](https://bips-hb.github.io/cpi/articles/intro.html), and
provides conditional variable importance using knockoffs. It works with
`mlr3` and its output on our data looks like this:

``` r
library(cpi)

resampling = rsmp("cv", folds = 5)
resampling$instantiate(task)
```

``` r
cpi_res = cpi(
    task = task,
    learner = learner,
    resampling = resampling,
    measure = measure,
    test = "t"
)
setDT(cpi_res)
setnames(cpi_res, "Variable", "feature")
cpi_res[, method := "CPI"]

cpi_res
#>    feature          CPI          SE   test   statistic     estimate
#>     <char>        <num>       <num> <char>       <num>        <num>
#> 1:      x1 4.554770e+00 0.142063012      t 32.06161590 4.554770e+00
#> 2:      x2 2.479253e-03 0.003012508      t  0.82298643 2.479253e-03
#> 3:      x3 1.848159e+00 0.058189043      t 31.76129690 1.848159e+00
#> 4:      x4 4.283886e-05 0.000696889      t  0.06147158 4.283886e-05
#>          p.value        ci.lo method
#>            <num>        <num> <char>
#> 1: 1.209450e-182  4.320988517    CPI
#> 2:  2.053069e-01 -0.002478179    CPI
#> 3: 6.961317e-180  1.752402643    CPI
#> 4:  4.754949e-01 -0.001103973    CPI
```

### CPI with knockoffs

Since `xplainfi` also includes knockoffs via the `KnockoffSampler` and
the `KnockoffGaussianSampler`, the latter implementing the second order
Gaussian knockoffs also used by default in
[cpi](https://github.com/bips-hb/cpi), we can recreate its results using
`CFI` with the corresponding `sampler`.

`CFI` with a knockoff sampler supports CPI inference directly via
`ci_method = "cpi"`:

``` r
knockoff_gaussian = KnockoffGaussianSampler$new(task)

cfi = CFI$new(
    task = task,
    learner = learner,
    resampling = resampling,
    measure = measure,
    sampler = knockoff_gaussian
)

cfi$compute()

# CPI uses observation-wise losses with one-sided t-test by default
cfi_cpi_res = cfi$importance(ci_method = "cpi")
cfi_cpi_res
#> Key: <feature>
#>    feature    importance           se  statistic       p.value   conf_lower
#>     <char>         <num>        <num>      <num>         <num>        <num>
#> 1:      x1  4.6427559476 0.1478836872 31.3946456 1.587955e-176  4.399396149
#> 2:      x2  0.0026423506 0.0033612948  0.7861109  2.159478e-01 -0.002889051
#> 3:      x3  1.7620712330 0.0582790374 30.2350779 5.393726e-166  1.666166302
#> 4:      x4 -0.0002589976 0.0007047195 -0.3675188  6.433645e-01 -0.001418696
#>    conf_upper
#>         <num>
#> 1:        Inf
#> 2:        Inf
#> 3:        Inf
#> 4:        Inf

# Rename columns to match cpi package output for comparison
setnames(cfi_cpi_res, c("importance", "conf_lower"), c("CPI", "ci.lo"))
cfi_cpi_res[, method := "CFI+Knockoffs"]
```

The results should be very similar to those computed by
[`cpi()`](https://bips-hb.github.io/cpi/reference/cpi.html), so let’s
compare them:

``` r
rbindlist(list(cpi_res, cfi_cpi_res), fill = TRUE) |>
    ggplot(aes(y = feature, x = CPI, color = method)) +
    geom_point(position = position_dodge(width = 0.3)) +
    geom_errorbar(
        aes(xmin = CPI, xmax = ci.lo),
        position = position_dodge(width = 0.3),
        width = 0.5
    ) +
    scale_color_brewer(palette = "Dark2") +
    labs(
        title = "CPI and CFI with Knockoff sampler",
        subtitle = "RF with 5-fold CV",
        color = NULL
    ) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "top")
```

![Point-and-whisker plot with features on y-axis and CPI values on
x-axis. Two colored points for CPI and CFI+Knockoffs methods with
horizontal error bars.](inference_files/figure-html/cpi-cfi-plot-1.png)

A noteable caveat of the knockoff approach is that they are not readily
available for mixed data (with categorical features).

### CPI with ARF

An alternative is available using ARF as conditional sampler rather than
knockoffs (see [Blesch et
al. (2025)](https://doi.org/10.1609/aaai.v39i15.33712)), which we can
perform analogously:

``` r
arf_sampler = ConditionalARFSampler$new(
    task = task,
    finite_bounds = "local",
    min_node_size = 20,
    epsilon = 1e-15
)

cfi_arf = CFI$new(
    task = task,
    learner = learner,
    resampling = resampling,
    measure = measure,
    sampler = arf_sampler
)

cfi_arf$compute()

# CPI uses observation-wise losses with one-sided t-test
cfi_arf_res = cfi_arf$importance(ci_method = "cpi")
cfi_arf_res
#> Key: <feature>
#>    feature   importance           se statistic       p.value    conf_lower
#>     <char>        <num>        <num>     <num>         <num>         <num>
#> 1:      x1  4.030582334 0.1301510177 30.968504 1.222410e-172  3.8164037048
#> 2:      x2  0.004954503 0.0032955819  1.503377  6.644987e-02 -0.0004687601
#> 3:      x3  1.709224166 0.0535822827 31.899055 3.781474e-181  1.6210482908
#> 4:      x4 -0.001110782 0.0007197487 -1.543292  9.385409e-01 -0.0022952124
#>    conf_upper
#>         <num>
#> 1:        Inf
#> 2:        Inf
#> 3:        Inf
#> 4:        Inf

# Rename columns to match cpi package output for comparison
setnames(cfi_arf_res, c("importance", "conf_lower"), c("CPI", "ci.lo"))
cfi_arf_res[, method := "CFI+ARF"]
```

We can now compare all three methods:

``` r
rbindlist(list(cpi_res, cfi_cpi_res, cfi_arf_res), fill = TRUE) |>
    ggplot(aes(y = feature, x = CPI, color = method)) +
    geom_point(position = position_dodge(width = 0.3)) +
    geom_errorbar(
        aes(xmin = CPI, xmax = ci.lo),
        position = position_dodge(width = 0.3),
        width = 0.5
    ) +
    scale_color_brewer(palette = "Dark2") +
    labs(
        title = "CPI and CFI with Knockoffs and ARF",
        subtitle = "RF with 5-fold CV",
        color = NULL
    ) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "top")
```

![Point-and-whisker plot with features on y-axis and CPI values on
x-axis. Three colored points for CPI, CFI+Knockoffs, and CFI+ARF methods
with horizontal error
bars.](inference_files/figure-html/cpi-cfi-arf-plot-1.png)

As expected, the ARF-based approach differs more from both
knockoff-based approaches, but they are all roughly in agreement.

### Statistical tests with CPI

CPI can also perform additional tests besides the default t-test,
specifically the Wilcoxon-, Fisher-, or binomial test:

``` r
(cpi_res_wilcoxon = cfi_arf$importance(ci_method = "cpi", test = "wilcoxon"))
#> Key: <feature>
#>    feature   importance           se statistic       p.value    conf_lower
#>     <char>        <num>        <num>     <num>         <num>         <num>
#> 1:      x1  4.030582334 0.1301510177   1967386 3.716827e-309  2.5659825422
#> 2:      x2  0.004954503 0.0032955819    992983  5.390444e-01 -0.0008211084
#> 3:      x3  1.709224166 0.0535822827   1929081 1.697037e-288  1.1465162882
#> 4:      x4 -0.001110782 0.0007197487    964259  8.802801e-01 -0.0005923333
#>    conf_upper
#>         <num>
#> 1:        Inf
#> 2:        Inf
#> 3:        Inf
#> 4:        Inf
# Fisher test with same default for B as in cpi()
(cpi_res_fisher = cfi_arf$importance(ci_method = "cpi", test = "fisher", B = 1999))
#> Key: <feature>
#>    feature   importance           se p.value    conf_lower conf_upper
#>     <char>        <num>        <num>   <num>         <num>      <num>
#> 1:      x1  4.030582334 0.1301510177  0.0005  3.7836382934        Inf
#> 2:      x2  0.004954503 0.0032955819  0.0750 -0.0006203853        Inf
#> 3:      x3  1.709224166 0.0535822827  0.0005  1.5992602629        Inf
#> 4:      x4 -0.001110782 0.0007197487  0.9320 -0.0023060964        Inf
(cpi_res_binom = cfi_arf$importance(ci_method = "cpi", test = "binomial"))
#> Key: <feature>
#>    feature   importance           se statistic       p.value conf_lower
#>     <char>        <num>        <num>     <num>         <num>      <num>
#> 1:      x1  4.030582334 0.1301510177      1861  0.000000e+00  0.9204192
#> 2:      x2  0.004954503 0.0032955819       991  6.645228e-01  0.4768744
#> 3:      x3  1.709224166 0.0535822827      1781 3.333550e-304  0.8783373
#> 4:      x4 -0.001110782 0.0007197487       962  9.574574e-01  0.4624027
#>    conf_upper
#>         <num>
#> 1:        Inf
#> 2:        Inf
#> 3:        Inf
#> 4:        Inf

rbindlist(
    list(
        cfi_arf$importance(ci_method = "cpi")[, test := "t"],
        cpi_res_wilcoxon[, test := "Wilcoxon"],
        cpi_res_fisher[, test := "Fisher"],
        cpi_res_binom[, test := "Binomial"]
    ),
    fill = TRUE
) |>
    ggplot(aes(y = feature, x = importance, color = test)) +
    geom_point(position = position_dodge(width = 0.3)) +
    geom_errorbar(
        aes(xmin = importance, xmax = conf_lower),
        position = position_dodge(width = 0.3),
        width = 0.5
    ) +
    scale_color_brewer(palette = "Dark2") +
    labs(
        title = "CPI test with CFI/ARF",
        subtitle = "RF with 5-fold CV",
        color = "Test"
    ) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "top")
```

![Point-and-whisker plot with features on y-axis and importance on
x-axis. Four colored series for t, Wilcoxon, Fisher, and Binomial tests
with horizontal error
bars.](inference_files/figure-html/cpi-tests-1.png)

Given the width of the resulting confidence intervals, the Fisher- or
t-test are generally recommended.

## Custom inference with LOCO

[Lei et al. (2018)](https://doi.org/10.1080/01621459.2017.1307116)
proposed inference for LOCO using the median absolute differences of the
baseline- and post-refit loss differences

\\ \theta_j = \mathrm{med}\left( \|Y - \hat{f}\_{n_1}^{-j}(X)\| - \|Y -
\hat{f}\_{n_1}(X)\| \big\| D_1 \right) \\

If we apply `LOCO` as implemented in `xplainfi` using the median
absolute error (MAE) as our measure including the median as the
aggregation function, we unfortunately get something else, though:

``` r
measure_mae = msr("regr.mae")
measure_mae$aggregator = median

loco = LOCO$new(
    task = task,
    learner = learner,
    resampling = rsmp("holdout"),
    measure = measure_mae
)

loco$compute()
loco$importance()
#> Key: <feature>
#>    feature importance
#>     <char>      <num>
#> 1:      x1 0.98148070
#> 2:      x2 0.05038684
#> 3:      x3 0.62258117
#> 4:      x4 0.05334160
```

This is **not** exactly what the authors propose, because `$score()`
calculates the aggregation function (`median`) for each resampling
iteration first, and takes the difference afterwards, i.e.

\\ \theta_j = \mathrm{med}\left(\|Y - \hat{f}\_{n_1}^{-j}(X)\|\right) -
\mathrm{med}\left(\|Y - \hat{f}\_{n_1}(X)\| \big\| D_1 \right) \\

In the default case where the arithemtic mean is used, it does not
matter whether we calculate the difference of the means or the mean of
the differences, but using the median it does.

We can, however, reconstruct it by using the observation-wise losses (in
this case, the absolute error):

``` r
loco_obsloss = loco$obs_loss()
head(loco_obsloss)
#>    feature iter_rsmp iter_repeat row_ids loss_baseline loss_post obs_importance
#>     <char>     <int>       <num>   <int>         <num>     <num>          <num>
#> 1:      x1         1           1       4    0.11147320 1.0227631     0.91128990
#> 2:      x1         1           1       7    0.30561825 0.8431147     0.53749646
#> 3:      x1         1           1      15    0.23994174 2.5361659     2.29622412
#> 4:      x1         1           1      20    0.21063594 0.1680135    -0.04262239
#> 5:      x1         1           1      22    0.07570713 2.9775583     2.90185118
#> 6:      x1         1           1      23    0.02199827 2.5518444     2.52984614
```

`obs_importance` here refers to the difference
`loss_post - loss_baseline`, so

- \\\texttt{loss_baseline} = \|Y - \hat{f}\_{n_1}(X)\|\\
- \\\texttt{loss_post} = \|Y - \hat{f}\_{n_1}^{-j}(X)\|\\
- \\\texttt{obs_importance} = \texttt{loss_post} -
  \texttt{loss_baseline}\\

Which means by taking the median for each feature \\j\\ within each
resampling iteration, we can construct \\\theta_j(D_1)\\ as proposed,
for each set \\D_k\\ where \\k\\ is the resampling iteration:

``` r
loco_thetas = loco_obsloss[, list(theta = median(obs_importance)), by = c("feature")]
loco_thetas
#>    feature      theta
#>     <char>      <num>
#> 1:      x1 0.83911185
#> 2:      x2 0.02684860
#> 3:      x3 0.51935341
#> 4:      x4 0.03344017
```

The authors then propose to construct distribution-free confidence
intervals, e.g. using a sign- or Wilcoxon test We can for example use
\[wilcoxon.test\] to compute confidence intervals around the estimated
pseudo-median:

``` r
loco_wilcox_ci = loco_obsloss[,
    {
        tt <- wilcox.test(
            obs_importance,
            conf.int = TRUE,
            conf.level = 0.95
        )
        .(
            statistic = tt$statistic,
            estimate = tt$estimate, # the pseudomedian importance
            p.value = tt$p.value,
            conf_lower = tt$conf.int[1],
            conf_upper = tt$conf.int[2]
        )
    },
    by = feature
]

loco_wilcox_ci
#>    feature statistic   estimate      p.value conf_lower conf_upper
#>     <char>     <num>      <num>        <num>      <num>      <num>
#> 1:      x1    216917 0.91943944 1.012455e-99 0.85057682 0.98969443
#> 2:      x2    144722 0.03419386 2.149646e-11 0.02424473 0.04449679
#> 3:      x3    213532 0.59231575 1.508448e-93 0.54444761 0.64071906
#> 4:      x4    162767 0.04423316 5.712254e-25 0.03565144 0.05305982
```
