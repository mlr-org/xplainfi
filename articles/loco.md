# LOCO and WVIM

``` r
library(xplainfi)
library(mlr3)
library(mlr3learners)
library(data.table)
library(ggplot2)
```

## Example Data: Interaction Effects

To illustrate LOCO feature importance, we’ll use a data generating
process with interaction effects:

\\y = 2 \cdot x_1 \cdot x_2 + x_3 + \epsilon\\

where \\\epsilon \sim N(0, 0.5^2)\\ and all features \\x_1, x_2, x_3,
noise_1, noise_2 \sim N(0,1)\\ are independent.

**Key characteristics:**

- **\\x_1, x_2\\**: Have NO individual effects, only interact with each
  other
- **\\x_3\\**: Has a direct main effect on \\y\\
- **\\noise_1, noise_2\\**: Pure noise variables with no effect on \\y\\

This setup demonstrates how LOCO handles interaction effects.

## Leave-One-Covariate-Out (LOCO)

LOCO measures feature importance by comparing model performance with and
without each feature. For each feature, the learner is retrained without
that feature and the performance difference indicates the feature’s
importance.

For feature \\j\\, LOCO is calculated as the difference in expected loss
of the model fit without the feature and the full model:
\\\text{LOCO}\_j = \mathbb{E}(L(Y, f\_{-j}(X\_{-j}))) - \mathbb{E}(L(Y,
f(X)))\\

Higher values indicate more important features (larger performance drop
when removed).

``` r
task <- sim_dgp_interactions(n = 2000)
learner <- lrn("regr.nnet", trace = FALSE)
measure <- msr("regr.mse")

resampling <- rsmp("holdout")
resampling$instantiate(task)

loco <- LOCO$new(
    task = task,
    learner = learner,
    measure = measure,
    resampling = resampling
)

loco$compute()
loco$importance()
#> Key: <feature>
#>    feature importance
#>     <char>      <num>
#> 1:  noise1 -0.7663935
#> 2:  noise2 -0.5653578
#> 3:      x1  2.8024966
#> 4:      x2  2.8173232
#> 5:      x3  0.7637862
```

The `$importance()` method returns a `data.table` with aggregated
importance scores per feature.

## Understanding the Results

**LOCO results interpretation:**

- \\x_3\\ should show high importance due to its direct main effect
- \\x_1\\ and \\x_2\\ show variable importance depending on the model’s
  ability to capture interactions
- \\noise_1\\ and \\noise_2\\ should show low or negative importance
- This demonstrates LOCO measures each feature’s contribution to model
  performance

## Detailed Scores

The `$scores()` method provides detailed information for each feature,
resampling iteration, and refit:

``` r
loco$scores() |>
    knitr::kable(digits = 4, caption = "LOCO scores with baseline and post-refit score")
```

| feature | iter_rsmp | iter_repeat | regr.mse_baseline | regr.mse_post | importance |
|:--------|----------:|------------:|------------------:|--------------:|-----------:|
| noise1  |         1 |           1 |            1.6412 |        0.5037 |    -1.1375 |
| noise2  |         1 |           1 |            1.6412 |        2.4255 |     0.7843 |
| x1      |         1 |           1 |            1.6412 |        4.4140 |     2.7728 |
| x2      |         1 |           1 |            1.6412 |        4.3570 |     2.7158 |
| x3      |         1 |           1 |            1.6412 |        1.5334 |    -0.1078 |
| noise1  |         1 |           2 |            1.6412 |        0.3395 |    -1.3017 |
| noise2  |         1 |           2 |            1.6412 |        0.6961 |    -0.9451 |
| x1      |         1 |           2 |            1.6412 |        4.3600 |     2.7188 |
| x2      |         1 |           2 |            1.6412 |        4.3539 |     2.7127 |
| x3      |         1 |           2 |            1.6412 |        1.5806 |    -0.0606 |
| noise1  |         1 |           3 |            1.6412 |        0.5525 |    -1.0887 |
| noise2  |         1 |           3 |            1.6412 |        0.4151 |    -1.2261 |
| x1      |         1 |           3 |            1.6412 |        4.3785 |     2.7373 |
| x2      |         1 |           3 |            1.6412 |        4.5405 |     2.8993 |
| x3      |         1 |           3 |            1.6412 |        2.0501 |     0.4090 |
| noise1  |         1 |           4 |            1.6412 |        0.4572 |    -1.1840 |
| noise2  |         1 |           4 |            1.6412 |        0.4825 |    -1.1587 |
| x1      |         1 |           4 |            1.6412 |        4.4059 |     2.7647 |
| x2      |         1 |           4 |            1.6412 |        4.5385 |     2.8973 |
| x3      |         1 |           4 |            1.6412 |        1.5518 |    -0.0894 |
| noise1  |         1 |           5 |            1.6412 |        2.1520 |     0.5108 |
| noise2  |         1 |           5 |            1.6412 |        2.1923 |     0.5511 |
| x1      |         1 |           5 |            1.6412 |        4.4073 |     2.7661 |
| x2      |         1 |           5 |            1.6412 |        4.4150 |     2.7738 |
| x3      |         1 |           5 |            1.6412 |        3.4026 |     1.7614 |
| noise1  |         1 |           6 |            1.6412 |        0.3191 |    -1.3221 |
| noise2  |         1 |           6 |            1.6412 |        0.6673 |    -0.9739 |
| x1      |         1 |           6 |            1.6412 |        4.3718 |     2.7306 |
| x2      |         1 |           6 |            1.6412 |        4.4436 |     2.8024 |
| x3      |         1 |           6 |            1.6412 |        3.1771 |     1.5359 |
| noise1  |         1 |           7 |            1.6412 |        0.3678 |    -1.2734 |
| noise2  |         1 |           7 |            1.6412 |        2.1868 |     0.5456 |
| x1      |         1 |           7 |            1.6412 |        4.3799 |     2.7387 |
| x2      |         1 |           7 |            1.6412 |        4.5199 |     2.8787 |
| x3      |         1 |           7 |            1.6412 |        3.4146 |     1.7734 |
| noise1  |         1 |           8 |            1.6412 |        0.3781 |    -1.2631 |
| noise2  |         1 |           8 |            1.6412 |        0.6634 |    -0.9778 |
| x1      |         1 |           8 |            1.6412 |        4.4323 |     2.7911 |
| x2      |         1 |           8 |            1.6412 |        4.4617 |     2.8206 |
| x3      |         1 |           8 |            1.6412 |        3.0936 |     1.4524 |
| noise1  |         1 |           9 |            1.6412 |        0.4903 |    -1.1509 |
| noise2  |         1 |           9 |            1.6412 |        0.4487 |    -1.1924 |
| x1      |         1 |           9 |            1.6412 |        4.9077 |     3.2665 |
| x2      |         1 |           9 |            1.6412 |        4.4057 |     2.7645 |
| x3      |         1 |           9 |            1.6412 |        2.0469 |     0.4057 |
| noise1  |         1 |          10 |            1.6412 |        2.2065 |     0.5653 |
| noise2  |         1 |          10 |            1.6412 |        2.4424 |     0.8012 |
| x1      |         1 |          10 |            1.6412 |        4.4225 |     2.7813 |
| x2      |         1 |          10 |            1.6412 |        4.5354 |     2.8942 |
| x3      |         1 |          10 |            1.6412 |        3.1510 |     1.5098 |
| noise1  |         1 |          11 |            1.6412 |        0.4136 |    -1.2276 |
| noise2  |         1 |          11 |            1.6412 |        0.4517 |    -1.1895 |
| x1      |         1 |          11 |            1.6412 |        4.3770 |     2.7358 |
| x2      |         1 |          11 |            1.6412 |        4.4683 |     2.8271 |
| x3      |         1 |          11 |            1.6412 |        3.4263 |     1.7851 |
| noise1  |         1 |          12 |            1.6412 |        0.7947 |    -0.8465 |
| noise2  |         1 |          12 |            1.6412 |        0.4695 |    -1.1717 |
| x1      |         1 |          12 |            1.6412 |        4.3769 |     2.7357 |
| x2      |         1 |          12 |            1.6412 |        4.4106 |     2.7694 |
| x3      |         1 |          12 |            1.6412 |        1.2868 |    -0.3544 |
| noise1  |         1 |          13 |            1.6412 |        0.5204 |    -1.1208 |
| noise2  |         1 |          13 |            1.6412 |        0.3796 |    -1.2615 |
| x1      |         1 |          13 |            1.6412 |        4.3671 |     2.7259 |
| x2      |         1 |          13 |            1.6412 |        4.3307 |     2.6895 |
| x3      |         1 |          13 |            1.6412 |        1.4689 |    -0.1723 |
| noise1  |         1 |          14 |            1.6412 |        0.3273 |    -1.3139 |
| noise2  |         1 |          14 |            1.6412 |        0.3281 |    -1.3131 |
| x1      |         1 |          14 |            1.6412 |        4.6119 |     2.9708 |
| x2      |         1 |          14 |            1.6412 |        4.5548 |     2.9136 |
| x3      |         1 |          14 |            1.6412 |        2.1357 |     0.4945 |
| noise1  |         1 |          15 |            1.6412 |        2.4582 |     0.8170 |
| noise2  |         1 |          15 |            1.6412 |        2.1874 |     0.5463 |
| x1      |         1 |          15 |            1.6412 |        4.4017 |     2.7605 |
| x2      |         1 |          15 |            1.6412 |        4.4171 |     2.7759 |
| x3      |         1 |          15 |            1.6412 |        2.0632 |     0.4221 |
| noise1  |         1 |          16 |            1.6412 |        0.3843 |    -1.2569 |
| noise2  |         1 |          16 |            1.6412 |        2.0078 |     0.3666 |
| x1      |         1 |          16 |            1.6412 |        4.5522 |     2.9110 |
| x2      |         1 |          16 |            1.6412 |        4.4199 |     2.7788 |
| x3      |         1 |          16 |            1.6412 |        3.4535 |     1.8123 |
| noise1  |         1 |          17 |            1.6412 |        2.3772 |     0.7360 |
| noise2  |         1 |          17 |            1.6412 |        0.3810 |    -1.2602 |
| x1      |         1 |          17 |            1.6412 |        4.3415 |     2.7003 |
| x2      |         1 |          17 |            1.6412 |        4.4422 |     2.8010 |
| x3      |         1 |          17 |            1.6412 |        3.3226 |     1.6814 |
| noise1  |         1 |          18 |            1.6412 |        0.3992 |    -1.2420 |
| noise2  |         1 |          18 |            1.6412 |        1.5704 |    -0.0708 |
| x1      |         1 |          18 |            1.6412 |        4.5080 |     2.8668 |
| x2      |         1 |          18 |            1.6412 |        4.3562 |     2.7150 |
| x3      |         1 |          18 |            1.6412 |        1.3775 |    -0.2636 |
| noise1  |         1 |          19 |            1.6412 |        0.4326 |    -1.2086 |
| noise2  |         1 |          19 |            1.6412 |        0.8192 |    -0.8220 |
| x1      |         1 |          19 |            1.6412 |        4.4174 |     2.7762 |
| x2      |         1 |          19 |            1.6412 |        4.6142 |     2.9731 |
| x3      |         1 |          19 |            1.6412 |        2.6475 |     1.0063 |
| noise1  |         1 |          20 |            1.6412 |        0.4000 |    -1.2411 |
| noise2  |         1 |          20 |            1.6412 |        0.4064 |    -1.2348 |
| x1      |         1 |          20 |            1.6412 |        4.3610 |     2.7198 |
| x2      |         1 |          20 |            1.6412 |        4.3750 |     2.7338 |
| x3      |         1 |          20 |            1.6412 |        3.4978 |     1.8566 |
| noise1  |         1 |          21 |            1.6412 |        2.1700 |     0.5288 |
| noise2  |         1 |          21 |            1.6412 |        0.3211 |    -1.3201 |
| x1      |         1 |          21 |            1.6412 |        4.3831 |     2.7420 |
| x2      |         1 |          21 |            1.6412 |        4.4739 |     2.8327 |
| x3      |         1 |          21 |            1.6412 |        3.1603 |     1.5191 |
| noise1  |         1 |          22 |            1.6412 |        0.4562 |    -1.1850 |
| noise2  |         1 |          22 |            1.6412 |        0.5797 |    -1.0614 |
| x1      |         1 |          22 |            1.6412 |        4.3896 |     2.7484 |
| x2      |         1 |          22 |            1.6412 |        4.3513 |     2.7101 |
| x3      |         1 |          22 |            1.6412 |        1.7523 |     0.1111 |
| noise1  |         1 |          23 |            1.6412 |        0.3755 |    -1.2656 |
| noise2  |         1 |          23 |            1.6412 |        2.2953 |     0.6542 |
| x1      |         1 |          23 |            1.6412 |        4.3746 |     2.7334 |
| x2      |         1 |          23 |            1.6412 |        4.5952 |     2.9540 |
| x3      |         1 |          23 |            1.6412 |        1.7771 |     0.1360 |
| noise1  |         1 |          24 |            1.6412 |        0.5510 |    -1.0902 |
| noise2  |         1 |          24 |            1.6412 |        0.4084 |    -1.2328 |
| x1      |         1 |          24 |            1.6412 |        4.5573 |     2.9161 |
| x2      |         1 |          24 |            1.6412 |        4.4462 |     2.8050 |
| x3      |         1 |          24 |            1.6412 |        3.1122 |     1.4711 |
| noise1  |         1 |          25 |            1.6412 |        0.9175 |    -0.7236 |
| noise2  |         1 |          25 |            1.6412 |        0.5273 |    -1.1139 |
| x1      |         1 |          25 |            1.6412 |        4.3814 |     2.7402 |
| x2      |         1 |          25 |            1.6412 |        4.5403 |     2.8992 |
| x3      |         1 |          25 |            1.6412 |        3.4504 |     1.8092 |
| noise1  |         1 |          26 |            1.6412 |        0.7085 |    -0.9327 |
| noise2  |         1 |          26 |            1.6412 |        1.1625 |    -0.4787 |
| x1      |         1 |          26 |            1.6412 |        4.3979 |     2.7567 |
| x2      |         1 |          26 |            1.6412 |        4.3710 |     2.7298 |
| x3      |         1 |          26 |            1.6412 |        1.5139 |    -0.1273 |
| noise1  |         1 |          27 |            1.6412 |        0.6355 |    -1.0057 |
| noise2  |         1 |          27 |            1.6412 |        2.4203 |     0.7791 |
| x1      |         1 |          27 |            1.6412 |        4.5544 |     2.9132 |
| x2      |         1 |          27 |            1.6412 |        4.5420 |     2.9008 |
| x3      |         1 |          27 |            1.6412 |        1.4426 |    -0.1985 |
| noise1  |         1 |          28 |            1.6412 |        1.2738 |    -0.3674 |
| noise2  |         1 |          28 |            1.6412 |        1.2608 |    -0.3804 |
| x1      |         1 |          28 |            1.6412 |        4.5174 |     2.8763 |
| x2      |         1 |          28 |            1.6412 |        4.5452 |     2.9040 |
| x3      |         1 |          28 |            1.6412 |        1.2841 |    -0.3571 |
| noise1  |         1 |          29 |            1.6412 |        0.7091 |    -0.9321 |
| noise2  |         1 |          29 |            1.6412 |        0.3951 |    -1.2461 |
| x1      |         1 |          29 |            1.6412 |        4.5503 |     2.9091 |
| x2      |         1 |          29 |            1.6412 |        4.3965 |     2.7553 |
| x3      |         1 |          29 |            1.6412 |        1.6742 |     0.0330 |
| noise1  |         1 |          30 |            1.6412 |        2.1727 |     0.5315 |
| noise2  |         1 |          30 |            1.6412 |        1.2831 |    -0.3581 |
| x1      |         1 |          30 |            1.6412 |        4.4099 |     2.7687 |
| x2      |         1 |          30 |            1.6412 |        4.5334 |     2.8922 |
| x3      |         1 |          30 |            1.6412 |        3.3006 |     1.6594 |

LOCO scores with baseline and post-refit score

## Multiple Refits

LOCO also supports `n_repeats` for multiple refits within each
resampling iteration, which improves stability:

``` r
loco_multi = LOCO$new(
    task = task,
    learner = learner,
    measure = measure,
    resampling = rsmp("cv", folds = 3),
    n_repeats = 10
)

loco_multi$compute()
loco_multi$importance()
#> Key: <feature>
#>    feature importance
#>     <char>      <num>
#> 1:  noise1  0.2379570
#> 2:  noise2  0.3506664
#> 3:      x1  4.0299444
#> 4:      x2  3.9218550
#> 5:      x3  1.3056282

# Check individual scores with multiple refits
loco_multi$scores() |>
    head(10) |>
    knitr::kable(digits = 4, caption = "First 10 LOCO scores per refit and resampling fold")
```

| feature | iter_rsmp | iter_repeat | regr.mse_baseline | regr.mse_post | importance |
|:--------|----------:|------------:|------------------:|--------------:|-----------:|
| noise1  |         1 |           1 |            0.3493 |        0.4164 |     0.0671 |
| noise2  |         1 |           1 |            0.3493 |        0.3919 |     0.0426 |
| x1      |         1 |           1 |            0.3493 |        4.2540 |     3.9047 |
| x2      |         1 |           1 |            0.3493 |        4.0460 |     3.6967 |
| x3      |         1 |           1 |            0.3493 |        2.0329 |     1.6836 |
| noise1  |         1 |           2 |            0.3493 |        1.1548 |     0.8055 |
| noise2  |         1 |           2 |            0.3493 |        0.5136 |     0.1643 |
| x1      |         1 |           2 |            0.3493 |        4.1198 |     3.7705 |
| x2      |         1 |           2 |            0.3493 |        4.0961 |     3.7467 |
| x3      |         1 |           2 |            0.3493 |        1.3335 |     0.9842 |

First 10 LOCO scores per refit and resampling fold

Since each refit requires to retrain the provided learner, this of
course increases the computational load. Suitable values depend on the
resources available, but when in doubt it is usually better to overshoot
and encounter diminishing returns rather than undershooting and getting
unreliable results.

## Using Different Measures

LOCO also works with any mlr3 measure. Different measures can highlight
different aspects of feature importance:

``` r
# Use same resampling for fair comparison
loco_mae <- LOCO$new(
    task = task,
    learner = learner,
    measure = msr("regr.mae"),
    resampling = resampling
)
```

## Comparison with Perturbation Methods

LOCO differs from perturbation-based methods like PFI and CFI:

- **LOCO**: Retrains model without each feature (computationally
  expensive)
- **PFI/CFI**: Perturb feature values using existing model (faster)

``` r
# Compare LOCO with PFI using same resampling
pfi <- PFI$new(task, learner, measure, resampling = resampling)
pfi$compute()

comparison <- merge(
    loco$importance()[, .(feature, loco = importance)],
    pfi$importance()[, .(feature, pfi = importance)],
    by = "feature"
)

comparison
#> Key: <feature>
#>    feature       loco          pfi
#>     <char>      <num>        <num>
#> 1:  noise1 -0.7663935 -0.002685962
#> 2:  noise2 -0.5653578  0.002991822
#> 3:      x1  2.8024966  6.496274359
#> 4:      x2  2.8173232  6.578501614
#> 5:      x3  0.7637862  1.770219866
```

**Note:** Using the same instantiated resampling ensures we evaluate
both methods on identical train/test splits. Even so, stochastic
learners like `ranger` will produce slightly different results between
methods due to the random forest’s sampling.

LOCO measures the value of having a feature available during training,
while PFI measures the value of having informative feature values at
prediction time.

## WVIM: The General Framework

LOCO is a special case of what Ewald et al. refer to as “Williamson’s
Variable Importance Measure” (WVIM), which provides a general
formulation for refit-based feature importance. WVIM allows both one or
more feature of interest at a time, meaning that via the `groups`
argument features can be assigned groups which will always be “left out”
or “left in” together. It also as a `direction` argument, specifying
whether features are left out or in.

### Replicating LOCO with WVIM

We can manually replicate LOCO using WVIM’s `"leave-out"` direction.
Since `features` is specified rather than `groups`, each feature well be
left out one at a time, resulting in the LOCO procedure.

``` r
# Create WVIM instance (LOCO's parent class) using same resampling
wvim_loco <- WVIM$new(
    task = task,
    learner = learner,
    measure = measure,
    resampling = resampling,
    features = task$feature_names,
    direction = "leave-out"
)
wvim_loco$compute()

# Compare with original LOCO results
comparison_wvim <- merge(
    loco$importance()[, .(feature, loco = importance)],
    wvim_loco$importance()[, .(feature, wvim = importance)],
    by = "feature"
)

comparison_wvim
#> Key: <feature>
#>    feature       loco       wvim
#>     <char>      <num>      <num>
#> 1:  noise1 -0.7663935 -1.1171265
#> 2:  noise2 -0.5653578 -1.4339895
#> 3:      x1  2.8024966  2.2075487
#> 4:      x2  2.8173232  2.2197634
#> 5:      x3  0.7637862 -0.1849607
```

**Note:** To get comparable results between methods, we must use the
same instantiated resampling. Even then, stochastic learners like
`ranger` will produce slightly different results due to random forest’s
bootstrapping and split selection, but the overall patterns should be
consistent.

### LOCI: Leave-One-Covariate-In

WVIM allows us to compute LOCI (Leave-One-Covariate-In) by changing the
direction to “leave-in”. LOCI trains models with only single features
and compares them to a featureless baseline.

**Note:** LOCI has questionable utility in practice because it is
essentially just a measure for the bivariate association between the
target and each feature separately. However, WVIM makes it trivial to
compute if desired:

``` r
# LOCI: train with only one feature at a time
wvim_loci <- WVIM$new(
    task = task,
    learner = learner,
    measure = measure,
    features = task$feature_names,
    direction = "leave-in",
    resampling = rsmp("cv", folds = 3),
    n_repeats = 10
)

wvim_loci$compute()
wvim_loci$importance()
#> Key: <feature>
#>    feature   importance
#>     <char>        <num>
#> 1:  noise1 -0.005507611
#> 2:  noise2 -0.030116216
#> 3:      x1 -6.172977924
#> 4:      x2 -0.276415375
#> 5:      x3  0.986802222
```

LOCI interprets importance differently from LOCO:

- **LOCO**: “How much does performance degrade when this feature is
  removed from the *full model*?”
- **LOCI**: “How much does this feature alone improve over a
  *featureless baseline*?”

For our interaction data where \\y = 2 \cdot x_1 \cdot x_2 + x_3 +
\epsilon\\:

- LOCI would be expected to
  - show low importance for \\x_1\\ and \\x_2\\ individually (no main
    effects)
  - show high importance for \\x_3\\ (strong main effect)
- LOCO better captures the value of features that participate in
  interactions

However, since we used a neural network learner, it cannot be expected
to learn any meaningful relations based on training with only one
feature. This hopefully showcases that LOCI is primarily “useful” as a
teaching exercise, rather than a meaningful importance measure.

## WVIM with Feature Groups

While LOCO measures the importance of individual features, WVIM
generalizes this to arbitrary feature groups. This allows us to measure
the collective importance of sets of features.

### Understanding Feature Groups

Feature groups are useful when:

- Features are naturally related (e.g., dummy-encoded categorical
  variables)
- You want to measure the importance of feature subsets
- Features have known functional relationships

Let’s create groups from our interaction data:

``` r
# Define feature groups
groups <- list(
    interaction_pair = c("x1", "x2"), # Features that interact
    main_effect = "x3", # Feature with direct effect
    noise_features = c("noise1", "noise2") # Pure noise
)

groups
#> $interaction_pair
#> [1] "x1" "x2"
#> 
#> $main_effect
#> [1] "x3"
#> 
#> $noise_features
#> [1] "noise1" "noise2"
```

### WVIM with Leave-Out Direction

Using `direction = "leave-out"`, WVIM computes the importance of each
group by measuring performance when that entire group is removed:

\\\text{WVIM}\_{\text{group}} = \mathbb{E}(L(Y,
f\_{-\text{group}}(X\_{-\text{group}}))) - \mathbb{E}(L(Y, f(X)))\\

This compares the model without the group to the full model.

``` r
wvim_groups_out <- WVIM$new(
    task = task,
    learner = learner,
    measure = measure,
    groups = groups,
    direction = "leave-out",
    resampling = rsmp("cv", folds = 3),
    n_repeats = 10
)

wvim_groups_out$compute()
wvim_groups_out$importance()
#> Key: <feature>
#>             feature importance
#>              <char>      <num>
#> 1: interaction_pair  3.3780282
#> 2:      main_effect  1.3852866
#> 3:   noise_features -0.3314483
```

**Interpretation:**

- `interaction_pair` (x1, x2) shows the joint contribution of the
  interacting features
- `main_effect` (x3) shows the contribution of the direct effect
- `noise_features` should show near-zero or negative importance

### WVIM with Leave-In Direction

Using `direction = "leave-in"`, WVIM trains models with only each group
and compares to a featureless baseline:

\\\text{WVIM}\_{\text{group}} = \mathbb{E}(L(Y, f\_{\emptyset})) -
\mathbb{E}(L(Y, f\_{\text{group}}(X\_{\text{group}})))\\

This measures how much each group alone improves over having no
features.

``` r
wvim_groups_in <- WVIM$new(
    task = task,
    learner = learner,
    measure = measure,
    groups = groups,
    direction = "leave-in",
    resampling = rsmp("cv", folds = 3)
)

wvim_groups_in$compute()
wvim_groups_in$importance()
#> Key: <feature>
#>             feature importance
#>              <char>      <num>
#> 1: interaction_pair  3.6348866
#> 2:      main_effect  0.9813703
#> 3:   noise_features -0.2612117
```

### Comparing Leave-Out vs Leave-In for Groups

The key difference between the two directions is the baseline model used
for comparison:

- **Leave-out**: Baseline is the **full model** with all features
  - Measures: “What do we lose by removing this group?”
  - Higher values → group is more important to overall model performance
- **Leave-in**: Baseline is the **empty model** without any features
  - Measures: “What do we gain by using only this group?”
  - Higher values → group alone provides better prediction than baseline

``` r
comparison_directions <- merge(
    wvim_groups_out$importance()[, .(feature, leave_out = importance)],
    wvim_groups_in$importance()[, .(feature, leave_in = importance)],
    by = "feature"
)

comparison_directions
#> Key: <feature>
#>             feature  leave_out   leave_in
#>              <char>      <num>      <num>
#> 1: interaction_pair  3.3780282  3.6348866
#> 2:      main_effect  1.3852866  0.9813703
#> 3:   noise_features -0.3314483 -0.2612117
```

For our interaction data where \\y = 2 \cdot x_1 \cdot x_2 + x_3 +
\epsilon\\:

- **Leave-out** captures how much each group contributes to the full
  model
- **Leave-in** shows that the `main_effect` group (x3) provides
  substantial prediction alone, while the `interaction_pair` (x1, x2)
  has limited predictive power in isolation

Features that interact strongly may show high importance in leave-out
(they matter for the full model) but low importance in leave-in (they
don’t work well alone).

### Practical Considerations

**When to use feature groups:**

- Analyzing categorical variables (group all dummy columns together)
- Testing domain-specific feature sets (e.g., “demographic features”,
  “behavioral features”)
- Measuring importance of feature engineering transformations
  collectively

**Computational cost:**

- WVIM requires retraining the model for each group in each resampling
  fold
- With groups, this is more efficient than analyzing features
  individually
- Use `n_repeats` parameter to control the number of refits per fold for
  variance estimation
