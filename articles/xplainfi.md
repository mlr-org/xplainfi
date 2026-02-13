# Getting Started with xplainfi

``` r
library(xplainfi)
library(mlr3)
library(mlr3learners)
library(data.table)
library(ggplot2)
```

**xplainfi** provides feature importance methods for machine learning
models. It implements several approaches for measuring how much each
feature contributes to model performance, with a focus on model-agnostic
methods that work with any learner.

## Core Concepts

Feature importance methods in `xplainfi` address different but related
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
aggregated results, with intermediate results available in `$scores()`
and, if the chosen measures supports it, `$obs_loss()`.

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
#>          feature  importance
#>           <char>       <num>
#>  1:   important1  5.65832510
#>  2:   important2  9.27071901
#>  3:   important3  1.23080752
#>  4:   important4 12.58085212
#>  5:   important5  2.10532639
#>  6: unimportant1 -0.01161106
#>  7: unimportant2  0.11012382
#>  8: unimportant3  0.07585563
#>  9: unimportant4 -0.04562983
#> 10: unimportant5 -0.08762484
```

The `importance` column shows the performance difference when each
feature is permuted. Higher values indicate more important features.

For more stable estimates, we can use multiple permutation iterations
per resampling fold with `n_repeats`. Note that in this case “more is
more”, and while there is no clear “good enough” value, setting
`n_repeats` to a small value like 1 will most definitely yield
unreliable results.

``` r
pfi_stable <- PFI$new(
    task = task,
    learner = learner,
    measure = measure,
    resampling = resampling,
    n_repeats = 50
)

pfi_stable$compute()
pfi_stable$importance()
#> Key: <feature>
#>          feature  importance
#>           <char>       <num>
#>  1:   important1  5.69009142
#>  2:   important2  8.51851192
#>  3:   important3  1.35925926
#>  4:   important4 11.99230271
#>  5:   important5  1.81738150
#>  6: unimportant1 -0.04982420
#>  7: unimportant2  0.05955861
#>  8: unimportant3 -0.06882424
#>  9: unimportant4 -0.02720205
#> 10: unimportant5 -0.00145781
```

We can also use ratio instead of difference for the importance
calculation, meaning that an unimportant feature is now expected to get
an importance score of 1 rather than 0:

``` r
pfi_stable$importance(relation = "ratio")
#> Key: <feature>
#>          feature importance
#>           <char>      <num>
#>  1:   important1  1.8066687
#>  2:   important2  2.2139272
#>  3:   important3  1.1928610
#>  4:   important4  2.6727517
#>  5:   important5  1.2618911
#>  6: unimportant1  0.9915111
#>  7: unimportant2  1.0086204
#>  8: unimportant3  0.9893331
#>  9: unimportant4  0.9972700
#> 10: unimportant5  0.9998240
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
#>  1:   important1  3.5085806
#>  2:   important2  5.3550364
#>  3:   important3  0.8200906
#>  4:   important4  7.5367223
#>  5:   important5  0.5861361
#>  6: unimportant1 -0.2583327
#>  7: unimportant2 -0.1602426
#>  8: unimportant3 -0.2885216
#>  9: unimportant4 -0.3449341
#> 10: unimportant5 -0.3606014
```

LOCO is computationally expensive as it requires retraining for each
feature, but provides clear interpretation: higher values mean larger
performance drop when the feature is removed. However, it cannot
distinguish between direct effects and indirect effects through
correlated features.

## Feature Samplers

For advanced methods that account for feature dependencies, xplainfi
provides different sampling strategies. While PFI uses simple
permutation (marginal sampling), conditional samplers can preserve
feature relationships.

Let’s demonstrate conditional sampling using adversarial random rorests
(ARF), which preserves relationships between features when sampling:

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
#>    important1  important2 important3
#>         <num>       <num>      <num>
#> 1:  0.4239519 0.784575267  0.2372297
#> 2:  0.1837310 0.009429905  0.6864904
#> 3:  0.5781874 0.779065883  0.2258184
#> 4:  0.3806457 0.729390652  0.3184946
#> 5:  0.4622495 0.630131853  0.1739838
```

This conditional sampling is essential for methods like CFI and RFI that
need to preserve feature dependencies. See the [perturbation-importance
article](https://mlr-org.github.io/xplainfi/articles/perturbation-importance.html)
for detailed comparisons and
[`vignette("feature-samplers")`](https://mlr-org.github.io/xplainfi/articles/feature-samplers.md)
for more details on implemented samplers.

## Detailed Scoring Information

All methods store detailed scoring information from each resampling
iteration for further analysis. Let’s examine the structure of PFI’s
detailed scores:

``` r
pfi$scores() |>
    head(10) |>
    knitr::kable(digits = 4, caption = "Detailed PFI scores (first 10 rows)")
```

| feature    | iter_rsmp | iter_repeat | regr.mse_baseline | regr.mse_post | importance |
|:-----------|----------:|------------:|------------------:|--------------:|-----------:|
| important1 |         1 |           1 |            4.3358 |        9.5208 |     5.1850 |
| important1 |         1 |           2 |            4.3358 |       10.6373 |     6.3015 |
| important1 |         1 |           3 |            4.3358 |        9.2000 |     4.8643 |
| important1 |         1 |           4 |            4.3358 |       10.5931 |     6.2573 |
| important1 |         1 |           5 |            4.3358 |        9.7336 |     5.3979 |
| important1 |         1 |           6 |            4.3358 |        9.6753 |     5.3396 |
| important1 |         1 |           7 |            4.3358 |        8.4808 |     4.1451 |
| important1 |         1 |           8 |            4.3358 |        8.1783 |     3.8425 |
| important1 |         1 |           9 |            4.3358 |        9.4762 |     5.1404 |
| important1 |         1 |          10 |            4.3358 |        9.9178 |     5.5820 |

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
#> 1:       10                3                30          900
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

| feature    | iter_rsmp | iter_repeat | regr.mse_baseline | regr.mse_post | importance |
|:-----------|----------:|------------:|------------------:|--------------:|-----------:|
| important1 |         1 |           1 |            4.3358 |        9.5208 |     2.1959 |
| important1 |         1 |           2 |            4.3358 |       10.6373 |     2.4534 |
| important1 |         1 |           3 |            4.3358 |        9.2000 |     2.1219 |
| important1 |         1 |           4 |            4.3358 |       10.5931 |     2.4432 |
| important1 |         1 |           5 |            4.3358 |        9.7336 |     2.2450 |
| important1 |         1 |           6 |            4.3358 |        9.6753 |     2.2315 |
| important1 |         1 |           7 |            4.3358 |        8.4808 |     1.9560 |
| important1 |         1 |           8 |            4.3358 |        8.1783 |     1.8862 |
| important1 |         1 |           9 |            4.3358 |        9.4762 |     2.1856 |
| important1 |         1 |          10 |            4.3358 |        9.9178 |     2.2874 |

PFI scores using the ratio (first 10 rows)

## Observation-wise losses and importances

For methods where importances are calculated based on observation-level
comparisons and with decomposable measures, we can also retrieve
observation-level information with `$obs_loss()`, which works
analogously to `$scores()` and `$importances()` but even more detailed:

``` r
pfi$obs_loss()
#>             feature iter_rsmp iter_repeat row_ids loss_baseline  loss_post
#>              <char>     <int>       <int>   <int>         <num>      <num>
#>     1:   important1         1           1       1     3.3403244  0.4756261
#>     2:   important1         1           1       9     0.4640003 13.8393080
#>     3:   important1         1           1      11     1.0938319  9.5233606
#>     4:   important1         1           1      12     2.0091331  1.2088093
#>     5:   important1         1           1      15    11.4484770 20.9659162
#>    ---                                                                    
#> 89996: unimportant5         3          30     290    16.8041217 16.8041217
#> 89997: unimportant5         3          30     294     0.4212832  0.4715050
#> 89998: unimportant5         3          30     295     8.0016602  8.9656018
#> 89999: unimportant5         3          30     296     0.2308082  0.1863293
#> 90000: unimportant5         3          30     298    18.8129904 18.7329169
#>        obs_importance
#>                 <num>
#>     1:    -2.86469831
#>     2:    13.37530770
#>     3:     8.42952866
#>     4:    -0.80032386
#>     5:     9.51743920
#>    ---               
#> 89996:     0.00000000
#> 89997:     0.05022177
#> 89998:     0.96394160
#> 89999:    -0.04447890
#> 90000:    -0.08007353
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
[`mlr3measures`](https://CRAN.R-project.org/package=mlr3measures), but
will likely be in the future.

## Using Pre-trained Learners

By default, `xplainfi` trains the learner internally via
[`mlr3::resample()`](https://mlr3.mlr-org.com/reference/resample.html).
However, if you have already trained a learner (for example because
training is expensive or you want to explain a specific model) you can
pass it directly to perturbation-based methods (`PFI`, `CFI`, `RFI`) and
`SAGE` methods. Refit-based methods (`LOCO` / `WVIM`) require retraining
by design and will warn if given a pretrained learner. The only
requirement is that the `resampling` must be instantiated and have
exactly one iteration (i.e., a single test set). This is necessary
because a pre-trained learner corresponds to a single fitted model, and
there is no meaningful way to associate it with multiple resampling
folds.

A holdout resampling is the natural choice here. We first train the
learner on the train set and `PFI` will calculate importance using the
trained learner and the corresponding test set defined by the
`resampling`:

``` r
resampling_holdout <- rsmp("holdout")$instantiate(task)
learner_trained <- lrn("regr.ranger", num.trees = 100)
learner_trained$train(task, row_ids = resampling_holdout$train_set(1))

pfi_pretrained <- PFI$new(
    task = task,
    learner = learner_trained,
    measure = measure,
    resampling = resampling_holdout,
    n_repeats = 10
)

pfi_pretrained$compute()
pfi_pretrained$importance()
#> Key: <feature>
#>          feature  importance
#>           <char>       <num>
#>  1:   important1  5.40113138
#>  2:   important2  7.41302690
#>  3:   important3  1.53601205
#>  4:   important4 14.36245887
#>  5:   important5  2.07014998
#>  6: unimportant1 -0.01573699
#>  7: unimportant2  0.07325885
#>  8: unimportant3  0.16189938
#>  9: unimportant4  0.01378138
#> 10: unimportant5  0.01746879
```

A common real-world scenario is that the learner was trained on some
dataset and you want to explain the model on entirely new, unseen data.
In that case, create a task from the new data (via
[`as_task_regr()`](https://mlr3.mlr-org.com/reference/as_task_regr.html)
for example) and use `rsmp("custom")` to designate all rows as the test
set. The resampling here is purely a technicality used for internal
consistency, and the train set is irrelevant since the learner is
already trained. A utility function
[`rsmp_all_test()`](https://mlr-org.github.io/xplainfi/reference/rsmp_all_test.md)
can be used as a shortcute do achieve the same goal.

``` r
# Simulate: learner was trained elsewhere, we have new data to use
new_data <- tgen("friedman1")$generate(n = 100)

# Same as rsmp_all_test(task)
resampling_custom <- rsmp("custom")$instantiate(
    new_data,
    train_sets = list(integer(0)),
    test_sets = list(new_data$row_ids)
)

pfi_newdata <- PFI$new(
    task = new_data,
    learner = learner_trained,
    measure = measure,
    resampling = resampling_custom,
    n_repeats = 10
)

pfi_newdata$compute()
pfi_newdata$importance()
#> Key: <feature>
#>          feature  importance
#>           <char>       <num>
#>  1:   important1  4.34309511
#>  2:   important2  9.49786181
#>  3:   important3  1.03711078
#>  4:   important4 13.13643335
#>  5:   important5  2.25419544
#>  6: unimportant1  0.12679511
#>  7: unimportant2 -0.16363612
#>  8: unimportant3 -0.22700424
#>  9: unimportant4 -0.12026119
#> 10: unimportant5  0.09306718
```

If you pass a trained learner with a multi-fold or non-instantiated
resampling, you will get an informative error at construction time:

``` r
PFI$new(
    task = task,
    learner = learner_trained,
    measure = measure,
    resampling = rsmp("cv", folds = 3)
)
#> Error in `super$initialize()`:
#> ! A pre-trained <Learner> requires an instantiated <Resampling>
#> ℹ Instantiate the <Resampling> before passing it, e.g.
#>   `rsmp("holdout")$instantiate(task)`
```

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
