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
#> 1:  noise1 -1.1374749
#> 2:  noise2  0.7842797
#> 3:      x1  2.7728001
#> 4:      x2  2.7157750
#> 5:      x3 -0.1077573
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
    n_repeats = 20
)

loco_multi$compute()
loco_multi$importance()
#> Key: <feature>
#>    feature importance
#>     <char>      <num>
#> 1:  noise1 0.09783824
#> 2:  noise2 0.11159219
#> 3:      x1 3.52161054
#> 4:      x2 3.54195239
#> 5:      x3 1.19103822

# Check individual scores with multiple refits
loco_multi$scores() |>
    head(10) |>
    knitr::kable(digits = 4, caption = "First 10 LOCO scores per refit and resampling fold")
```

| feature | iter_rsmp | iter_repeat | regr.mse_baseline | regr.mse_post | importance |
|:--------|----------:|------------:|------------------:|--------------:|-----------:|
| noise1  |         1 |           1 |            0.7395 |        0.5781 |    -0.1614 |
| noise2  |         1 |           1 |            0.7395 |        0.4073 |    -0.3322 |
| x1      |         1 |           1 |            0.7395 |        4.2616 |     3.5222 |
| x2      |         1 |           1 |            0.7395 |        4.4070 |     3.6675 |
| x3      |         1 |           1 |            0.7395 |        3.2243 |     2.4848 |
| noise1  |         1 |           2 |            0.7395 |        0.3963 |    -0.3431 |
| noise2  |         1 |           2 |            0.7395 |        0.4867 |    -0.2528 |
| x1      |         1 |           2 |            0.7395 |        4.3106 |     3.5711 |
| x2      |         1 |           2 |            0.7395 |        4.3204 |     3.5809 |
| x3      |         1 |           2 |            0.7395 |        3.3890 |     2.6495 |

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
#> 1:  noise1 -1.1374749  0.002236380
#> 2:  noise2  0.7842797 -0.001500455
#> 3:      x1  2.7728001  8.487047597
#> 4:      x2  2.7157750  7.320215120
#> 5:      x3 -0.1077573  1.968836648
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
#>    feature       loco        wvim
#>     <char>      <num>       <num>
#> 1:  noise1 -1.1374749  1.73855081
#> 2:  noise2  0.7842797 -0.09143021
#> 3:      x1  2.7728001  3.87166836
#> 4:      x2  2.7157750  3.97399402
#> 5:      x3 -0.1077573  1.11061754
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
#>    feature  importance
#>     <char>       <num>
#> 1:  noise1 -0.03103653
#> 2:  noise2 -0.03631880
#> 3:      x1 -0.14861338
#> 4:      x2 -0.19616698
#> 5:      x3  0.98570545
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

However, since we used a random forest learner (ranger), it can not be
expected to learn any meaningful relations based on one training with
onle one feature. This hopefully showcases that LOCI is primarily
“useful” as a teaching exercise, rather than a meaningful importance
measure.

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
#> 1: interaction_pair  3.7911190
#> 2:      main_effect  1.6739042
#> 3:   noise_features  0.1539716
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
#>             feature  importance
#>              <char>       <num>
#> 1: interaction_pair  2.17798793
#> 2:      main_effect  0.99212768
#> 3:   noise_features -0.07272751
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
#>             feature leave_out    leave_in
#>              <char>     <num>       <num>
#> 1: interaction_pair 3.7911190  2.17798793
#> 2:      main_effect 1.6739042  0.99212768
#> 3:   noise_features 0.1539716 -0.07272751
```

For our interaction data where \\y = 2 \cdot x_1 \cdot x_2 + x_3 +
\epsilon\\:

- **Leave-out** captures how much each group contributes to the full
  model
- **Leave-in** shows that the `main_effect` group (x3) provides
  substantial prediction alone, while the `interaction_pair` (x1, x2)
  has limited predictive power in isolation

This highlights a key insight: features that interact strongly may show
high importance in leave-out (they matter for the full model) but low
importance in leave-in (they don’t work well alone).

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
