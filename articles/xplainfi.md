# Getting Started with xplainfi

``` r
library(xplainfi)
library(mlr3)
library(mlr3learners)
library(data.table)
library(ggplot2)
```

The **xplainfi** package provides feature importance methods for machine
learning models. It implements several approaches for measuring how much
each feature contributes to model predictions, with a focus on
model-agnostic methods that work with any learner.

## Core Concepts

Feature importance methods in xplainfi address different but related
questions:

- **How much does each feature contribute to model performance?**
  (Permutation Feature Importance)
- **What happens when we remove features and retrain?**
  (Leave-One-Covariate-Out)  
- **How do features depend on each other?** (Conditional and Relative
  methods)

All methods share a common interface built on
[mlr3](https://mlr3.mlr-org.com/), making them easy to use with any
task, learner, measure, and resampling strategy.

The general pattern is to call `$compute()` to calculate importance
(which *always re-computes*), then `$importance()` to retrieve the
aggregated results, with intermediate results available in `$scores`.

## Basic Example

Let’s use the Friedman1 task, which provides an ideal setup for
demonstrating feature importance methods with known ground truth:

``` r
task <- tgen("friedman1")$generate(n = 300)
learner <- lrn("regr.ranger", num.trees = 100)
measure <- msr("regr.mse")
resampling <- rsmp("cv", folds = 3)
```

The task has 300 observations with 10 features. Features `important1`
through `important5` truly affect the target, while `unimportant1`
through `unimportant5` are pure noise. We’ll use a random forest learner
with cross-validation for more stable estimates.

The target function is: \\y = 10 \* \operatorname{sin}(\pi \* x_1 \*
x_2) + 20 \* (x_3 - 0.5)^2 + 10 \* x_4 + 5 \* x_5 + \epsilon\\

## Permutation Feature Importance (PFI)

PFI is the most straightforward method: for each feature, we permute
(shuffle) its values and measure how much model performance
deteriorates. More important features cause larger performance drops
when shuffled.

``` r
pfi <- PFI$new(
    task = task,
    learner = learner,
    measure = measure,
    resampling = resampling
)

pfi$compute()
pfi$importance()
#> Key: <feature>
#>          feature   importance
#>           <char>        <num>
#>  1:   important1  4.858724892
#>  2:   important2  8.155693005
#>  3:   important3  1.109254345
#>  4:   important4 10.784727349
#>  5:   important5  2.395793708
#>  6: unimportant1  0.009618005
#>  7: unimportant2  0.080903445
#>  8: unimportant3  0.044057887
#>  9: unimportant4 -0.082032243
#> 10: unimportant5 -0.137666350
```

The `importance` column shows the performance difference when each
feature is permuted. Higher values indicate more important features.

For more stable estimates, we can use multiple permutation iterations
per resampling fold:

``` r
pfi_stable <- PFI$new(
    task = task,
    learner = learner,
    measure = measure,
    resampling = resampling,
    n_repeats = 5
)

pfi_stable$compute()
pfi_stable$importance()
#> Key: <feature>
#>          feature  importance
#>           <char>       <num>
#>  1:   important1  5.49350314
#>  2:   important2  8.27127962
#>  3:   important3  1.03112448
#>  4:   important4 13.70049092
#>  5:   important5  1.89032663
#>  6: unimportant1 -0.01291604
#>  7: unimportant2 -0.01881439
#>  8: unimportant3  0.05705810
#>  9: unimportant4  0.05613447
#> 10: unimportant5 -0.04811418
```

We can also use ratio instead of difference for the importance
calculation, meaning that an unimportant feature is now expected to get
an importance score of 1 rather than 0:

``` r
pfi_stable$importance(relation = "ratio")
#> Key: <feature>
#>          feature importance
#>           <char>      <num>
#>  1:   important1  1.8332042
#>  2:   important2  2.2611905
#>  3:   important3  1.1565258
#>  4:   important4  3.0682785
#>  5:   important5  1.2869951
#>  6: unimportant1  0.9979419
#>  7: unimportant2  0.9974654
#>  8: unimportant3  1.0073128
#>  9: unimportant4  1.0086097
#> 10: unimportant5  0.9923926
```

## Leave-One-Covariate-Out (LOCO)

LOCO measures importance by retraining the model without each feature
and comparing performance to the full model. This shows the contribution
of each feature when all other features are present.

``` r
loco <- LOCO$new(
    task = task,
    learner = learner,
    measure = measure,
    resampling = resampling
)

loco$compute()
loco$importance()
#> Key: <feature>
#>          feature importance
#>           <char>      <num>
#>  1:   important1  3.1047926
#>  2:   important2  5.1216809
#>  3:   important3  0.4987575
#>  4:   important4  7.7615133
#>  5:   important5  0.6788951
#>  6: unimportant1 -0.7586142
#>  7: unimportant2 -0.1510810
#>  8: unimportant3 -0.5204418
#>  9: unimportant4 -0.4949344
#> 10: unimportant5 -0.4696825
```

LOCO is computationally expensive (requires retraining for each feature)
but provides clear interpretation: higher values mean larger performance
drop when the feature is removed. **Important limitation**: LOCO cannot
distinguish between direct effects and indirect effects through
correlated features.

## Feature Samplers

For advanced methods that account for feature dependencies, xplainfi
provides different sampling strategies. While PFI uses simple
permutation (marginal sampling), conditional samplers can preserve
feature relationships.

Let’s demonstrate conditional sampling using Adversarial Random Forests,
which preserves relationships between features when sampling:

``` r
arf_sampler <- ConditionalARFSampler$new(task)

sample_data <- task$data(rows = 1:5)
sample_data[, .(important1, important2)]
#>    important1  important2
#>         <num>       <num>
#> 1:  0.2875775 0.784575267
#> 2:  0.7883051 0.009429905
#> 3:  0.4089769 0.779065883
#> 4:  0.8830174 0.729390652
#> 5:  0.9404673 0.630131853
```

Now we’ll conditionally sample the `important1` feature given the values
of `important2` and `important3`:

``` r
sampled_conditional <- arf_sampler$sample_newdata(
    feature = "important1",
    newdata = sample_data,
    conditioning_set = c("important2", "important3")
)

sample_data[, .(important1, important2, important3)]
#>    important1  important2 important3
#>         <num>       <num>      <num>
#> 1:  0.2875775 0.784575267  0.2372297
#> 2:  0.7883051 0.009429905  0.6864904
#> 3:  0.4089769 0.779065883  0.2258184
#> 4:  0.8830174 0.729390652  0.3184946
#> 5:  0.9404673 0.630131853  0.1739838
sampled_conditional[, .(important1, important2, important3)]
#>       important1  important2 important3
#>            <num>       <num>      <num>
#> 1:  0.3341964978 0.784575267  0.2372297
#> 2:  0.1788524898 0.009429905  0.6864904
#> 3:  0.6858777217 0.779065883  0.2258184
#> 4:  0.9572636532 0.729390652  0.3184946
#> 5: -0.0008153241 0.630131853  0.1739838
```

This conditional sampling is essential for methods like CFI and RFI that
need to preserve feature dependencies. See
`vignette("perturbation-importance")` for detailed comparisons.

## Advanced Features

xplainfi supports many advanced features for robust importance
estimation:

- **Multiple resampling strategies**: Cross-validation, bootstrap,
  custom splits
- **Multiple permutation/refit iterations**: For more stable estimates
- **Feature grouping**: Compute importance for groups of related
  features
- **Different relation types**: Difference vs. ratio scoring
- **Conditional sampling**: Account for feature dependencies (see
  `vignette("perturbation-importance")`)
- **SAGE methods**: Shapley-based approaches (see
  `vignette("sage-methods")`)

## Detailed Scoring Information

All methods store detailed scoring information from each resampling
iteration for further analysis. Let’s examine the structure of PFI’s
detailed scores:

``` r
pfi$scores() |>
    head(10) |>
    knitr::kable(digits = 4, caption = "Detailed PFI scores (first 10 rows)")
```

| feature      | iter_rsmp | iter_repeat | regr.mse_baseline | regr.mse_post | importance |
|:-------------|----------:|------------:|------------------:|--------------:|-----------:|
| important1   |         1 |           1 |            4.3358 |        8.4459 |     4.1102 |
| important2   |         1 |           1 |            4.3358 |       10.9357 |     6.6000 |
| important3   |         1 |           1 |            4.3358 |        5.2284 |     0.8926 |
| important4   |         1 |           1 |            4.3358 |       15.4558 |    11.1200 |
| important5   |         1 |           1 |            4.3358 |        6.5032 |     2.1674 |
| unimportant1 |         1 |           1 |            4.3358 |        4.3324 |    -0.0033 |
| unimportant2 |         1 |           1 |            4.3358 |        4.3681 |     0.0323 |
| unimportant3 |         1 |           1 |            4.3358 |        4.4284 |     0.0927 |
| unimportant4 |         1 |           1 |            4.3358 |        4.3111 |    -0.0247 |
| unimportant5 |         1 |           1 |            4.3358 |        4.1194 |    -0.2163 |

Detailed PFI scores (first 10 rows)

We can also summarize the scoring structure:

``` r
pfi$scores()[, .(
    features = uniqueN(feature),
    resampling_folds = uniqueN(iter_rsmp),
    permutation_iters = uniqueN(iter_repeat),
    total_scores = .N
)]
#>    features resampling_folds permutation_iters total_scores
#>       <int>            <int>             <int>        <int>
#> 1:       10                3                 1           30
```

So `$importance()` always gives us the aggregated importances across
multiple resampling- and permutation-/refitting iterations, whereas
`$scores()` gives you the individual scores as calculated by the
supplied `measures` and the corresponding importance calculated from the
difference of these scores by default.

Analogously to `$importance()`, you can also use `relation = "ratio"`
here:

``` r
pfi$scores(relation = "ratio") |>
    head(10) |>
    knitr::kable(digits = 4, caption = "PFI scores using the ratio (first 10 rows)")
```

| feature      | iter_rsmp | iter_repeat | regr.mse_baseline | regr.mse_post | importance |
|:-------------|----------:|------------:|------------------:|--------------:|-----------:|
| important1   |         1 |           1 |            4.3358 |        8.4459 |     1.9480 |
| important2   |         1 |           1 |            4.3358 |       10.9357 |     2.5222 |
| important3   |         1 |           1 |            4.3358 |        5.2284 |     1.2059 |
| important4   |         1 |           1 |            4.3358 |       15.4558 |     3.5647 |
| important5   |         1 |           1 |            4.3358 |        6.5032 |     1.4999 |
| unimportant1 |         1 |           1 |            4.3358 |        4.3324 |     0.9992 |
| unimportant2 |         1 |           1 |            4.3358 |        4.3681 |     1.0075 |
| unimportant3 |         1 |           1 |            4.3358 |        4.4284 |     1.0214 |
| unimportant4 |         1 |           1 |            4.3358 |        4.3111 |     0.9943 |
| unimportant5 |         1 |           1 |            4.3358 |        4.1194 |     0.9501 |

PFI scores using the ratio (first 10 rows)

## Observation-wise losses and importances

For methods where importances are calculated based on observation-level
comparisons and with decomposable measures, we can also retrieve
observation-level information with `$obs_loss()`, which works
analogously to `$scores()` and `$importances()` but even more detailed:

``` r
pfi$obs_loss()
#>            feature iter_rsmp iter_repeat row_ids loss_baseline   loss_post
#>             <char>     <int>       <int>   <int>         <num>       <num>
#>    1:   important1         1           1       1     3.3403244  0.26184209
#>    2:   important1         1           1       9     0.4640003  0.00316609
#>    3:   important1         1           1      11     1.0938319 10.11218211
#>    4:   important1         1           1      12     2.0091331  2.28764800
#>    5:   important1         1           1      15    11.4484770 38.11092543
#>   ---                                                                     
#> 2996: unimportant5         3           1     290    16.8041217 16.80412169
#> 2997: unimportant5         3           1     294     0.4212832  0.45933049
#> 2998: unimportant5         3           1     295     8.0016602  7.86721528
#> 2999: unimportant5         3           1     296     0.2308082  0.26544478
#> 3000: unimportant5         3           1     298    18.8129904 18.81299041
#>       obs_importance
#>                <num>
#>    1:    -3.07848231
#>    2:    -0.46083425
#>    3:     9.01835017
#>    4:     0.27851489
#>    5:    26.66244838
#>   ---               
#> 2996:     0.00000000
#> 2997:     0.03804724
#> 2998:    -0.13444495
#> 2999:     0.03463658
#> 3000:     0.00000000
```

Since we computed PFI using the mean squared error (`msr("regr.mse")`),
we can use the associated `Measure$obs_loss()`, the squared error.  
In the resulting table we see

- `loss_baseline`: The loss (squared error) for the baseline model
  before permutation
- `loss_post`: The loss for this observation after permutation (or in
  the case of `LOCO`, after refit)
- `obs_importance`: The difference (or ratio if `relation = "ratio"`) of
  the the two losses

Note that not all measures have a `Measure$obs_loss()`: Some measures
like `msr("classif.auc")` are not decomposable, so observation-wise loss
values are not available.  
In other cases, the corresponding `obs_loss()` is just not yet
implemented in
[`mlr3measures`](https://cran.r-project.org/web/packages/mlr3measures/index.html),
but will likely be in the future.

## Parallelization

Both PFI/CFI/RFI and LOCO/WVIM support parallel execution to speed up
computation when working with multiple features or expensive learners.
The parallelization follows mlr3’s approach, allowing users to choose
between `mirai` and `future` backends.

### Example with future

The `future` package provides a simple interface for parallel and
distributed computing:

``` r
library(future)
plan("multisession", workers = 2)

# PFI with parallelization across features
pfi_parallel = PFI$new(
  task,
  learner = lrn("regr.ranger"),
  measure = msr("regr.mse"),
  n_repeats = 10
)
pfi_parallel$compute()
pfi_parallel$importance()

# LOCO with parallelization (uses mlr3fselect internally)
loco_parallel = LOCO$new(
  task,
  learner = lrn("regr.ranger"),
  measure = msr("regr.mse")
)
loco_parallel$compute()
loco_parallel$importance()
```

### Example with mirai

The `mirai` package offers a modern alternative for parallel computing:

``` r
library(mirai)
daemons(n = 2)

# Same PFI/LOCO code works with mirai backend
pfi_parallel = PFI$new(
  task,
  learner = lrn("regr.ranger"),
  measure = msr("regr.mse"),
  n_repeats = 10
)
pfi_parallel$compute()
pfi_parallel$importance()

# Clean up daemons when done
daemons(0)
```

### Notes

- **SAGE**: Currently does not support parallelization and will always
  run sequentially
- **Performance**: Parallelization is most beneficial with multiple
  features, large datasets, or expensive learners
- **Backend choice**: Use either
  [`future::plan()`](https://future.futureverse.org/reference/plan.html)
  or
  [`mirai::daemons()`](https://mirai.r-lib.org/reference/daemons.html) -
  both work with the same code
- **Consistency**: Both PerturbationImportance (PFI/CFI/RFI) and WVIM
  (LOCO) use the same mlr3 parallelization infrastructure
