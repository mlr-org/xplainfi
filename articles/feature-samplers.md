# Feature Samplers

Feature samplers are a core component of perturbation-based feature
importance methods (PFI, CFI, RFI) and any other methods based on some
form of marginilization (SAGE). They determine how features are replaced
or perturbed when evaluating feature importance. This vignette
introduces the different types of feature samplers available in xplainfi
and demonstrates their use.

## Setup

We create two tasks: One with mixed features (the seminal penguins
data), and one with all-numeric features.

``` r
library(xplainfi)
library(mlr3)
library(mlr3learners)
library(data.table)

# Create a task for demonstration
task_mixed = tsk("penguins")
task_numeric = sim_dgp_correlated(n = 200)
```

## Base Class: FeatureSampler

All feature samplers inherit from the `FeatureSampler` base class, which
provides a common interface for sampling features.

### Key Properties

**Feature Type Support**: Each sampler declares which feature types it
supports:

``` r
# Check supported feature types for different samplers
task_mixed$feature_types
#> Key: <id>
#>                id    type
#>            <char>  <char>
#> 1:     bill_depth numeric
#> 2:    bill_length numeric
#> 3:      body_mass integer
#> 4: flipper_length integer
#> 5:         island  factor
#> 6:            sex  factor
#> 7:           year integer
permutation = MarginalPermutationSampler$new(task_mixed)
permutation$feature_types
#> [1] "numeric"   "factor"    "ordered"   "integer"   "logical"   "Date"     
#> [7] "POSIXct"   "character"
```

**Two Sampling Methods**:

1.  `$sample(feature, row_ids)` - Sample from the stored task
2.  `$sample_newdata(feature, newdata)` - Sample using external data

Let’s demonstrate both with the permutation sampler:

``` r
# Sample from stored task (using row_ids)
sampled_task = permutation$sample(
    feature = "bill_length",
    row_ids = 40:45
)
sampled_task
#>    species bill_depth bill_length body_mass flipper_length island    sex  year
#>     <fctr>      <num>       <num>     <int>          <int> <fctr> <fctr> <int>
#> 1:  Adelie       19.1        39.8      4650            184  Dream   male  2007
#> 2:  Adelie       18.0        44.1      3150            182  Dream female  2007
#> 3:  Adelie       18.4        37.0      3900            195  Dream   male  2007
#> 4:  Adelie       18.5        36.5      3100            186  Dream female  2007
#> 5:  Adelie       19.7        40.8      4400            196  Dream   male  2007
#> 6:  Adelie       16.9        36.0      3000            185  Dream female  2007

# Sample from "external" data
test_data = task_mixed$data(rows = 40:45)
sampled_external = permutation$sample_newdata(
    feature = "bill_length",
    newdata = test_data
)
sampled_external
#>    species bill_depth bill_length body_mass flipper_length island    sex  year
#>     <fctr>      <num>       <num>     <int>          <int> <fctr> <fctr> <int>
#> 1:  Adelie       19.1        36.5      4650            184  Dream   male  2007
#> 2:  Adelie       18.0        37.0      3150            182  Dream female  2007
#> 3:  Adelie       18.4        39.8      3900            195  Dream   male  2007
#> 4:  Adelie       18.5        44.1      3100            186  Dream female  2007
#> 5:  Adelie       19.7        36.0      4400            196  Dream   male  2007
#> 6:  Adelie       16.9        40.8      3000            185  Dream female  2007
```

Notice that:

- The sampled feature values change (they are permuted)
- Other features and the target remain unchanged
- The data structure is preserved

## Permutation Sampler

The `MarginalPermutationSampler` performs simple random permutation of
features, breaking their relationship with the target and other
features. This is the classic approach used in Permutation Feature
Importance (PFI).

**How it works:**

- Each feature is randomly shuffled (permuted) independently
- The association between feature values and target values is broken
- The association between feature values **across rows** is broken
- The marginal distribution of each feature is preserved

``` r
# Create permutation sampler
permutation = MarginalPermutationSampler$new(task_mixed)

# Sample a continuous feature
original = task_mixed$data(rows = 1:10)
sampled = permutation$sample("bill_length", row_ids = 1:10)

# Compare original and sampled values
data.table(
    original_bill = original$bill_length,
    sampled_bill = sampled$bill_length,
    sex = original$sex # Unchanged
)
#>     original_bill sampled_bill    sex
#>             <num>        <num> <fctr>
#>  1:          39.1         42.0   male
#>  2:          39.5         39.1 female
#>  3:          40.3           NA female
#>  4:            NA         34.1   <NA>
#>  5:          36.7         39.5 female
#>  6:          39.3         36.7   male
#>  7:          38.9         38.9 female
#>  8:          39.2         39.2   male
#>  9:          34.1         40.3   <NA>
#> 10:          42.0         39.3   <NA>
```

Note that the permutation is only performed *within* the requested
`row_ids`.

**Use in PFI**: The permutation sampler is used by default in
Permutation Feature Importance

## Marginal Reference Sampler

The `MarginalReferenceSampler` is another type of marginal sampler that
samples complete rows from reference data. Unlike
`MarginalPermutationSampler` which shuffles feature values
independently, this sampler preserves within-row dependencies by
sampling intact observations.

**Key differences from MarginalPermutationSampler:**

- **MarginalPermutationSampler**: Shuffles each feature independently →
  breaks dependencies between sampled features
- **MarginalReferenceSampler**: Samples complete reference rows →
  preserves dependencies of the sampled features

This is the approach used in [SAGE (Shapley Additive Global
importancE)](https://github.com/iancovert/sage) for marginal feature
importance. In this SAGE implementation, the
[`MarginalImputer`](https://github.com/iancovert/sage/blob/master/sage/imputers.py#L44-L78),
uses this approach. The “imputer” name comes from the “imputation” of
model predictions using out-of-coalition features by sampling from their
marginal distribution. In `xplainfi`, we separate the “feature sampling”
and “model prediction” steps (for now), which is why we keep the
sampling infrastructure independent.

**How it works:**

``` r
# Create marginal reference sampler with n_samples reference pool
marginal_ref = MarginalReferenceSampler$new(task_mixed, n_samples = 30L)

# Sample a feature - each row gets values from a randomly sampled reference row
original = task_mixed$data(rows = 1:5)
sampled = marginal_ref$sample("bill_length", row_ids = 1:5)

# Compare
data.table(
    original_bill = original$bill_length,
    sampled_bill = sampled$bill_length,
    sex = original$sex # Unchanged
)
#>    original_bill sampled_bill    sex
#>            <num>        <num> <fctr>
#> 1:          39.1         51.1   male
#> 2:          39.5         41.1 female
#> 3:          40.3         38.3 female
#> 4:            NA         46.4   <NA>
#> 5:          36.7         41.1 female
```

**Parameters:**

- `n_samples`: Controls the size of the reference data pool
  - If `NULL`: uses all task data
  - If specified: subsamples that many rows from task

**Preserving within-row correlations:**

To demonstrate the difference, consider features that are correlated as
in `task_numeric`. Here, `x1` and `x2` are correlated, and if we sample
them jointly, only `MarginalReferenceSampler` retains their original
correlation (approximately).

``` r
# Sample with MarginalPermutationSampler (breaks correlations)
perm = MarginalPermutationSampler$new(task_numeric)
sampled_perm = perm$sample(c("x1", "x2"), row_ids = 1:10)

# Sample with MarginalReferenceSampler (preserves within-row correlations)
ref = MarginalReferenceSampler$new(task_numeric, n_samples = 50L)
sampled_ref = ref$sample(c("x1", "x2"), row_ids = 1:10)

# Check correlations
cor_original = cor(task_numeric$data()$x1, task_numeric$data()$x2)
cor_perm = cor(sampled_perm$x1, sampled_perm$x2)
cor_ref = cor(sampled_ref$x1, sampled_ref$x2)

data.table(
    method = c("Original", "Permutation", "Reference"),
    correlation = c(cor_original, cor_perm, cor_ref)
)
#>         method correlation
#>         <char>       <num>
#> 1:    Original   0.8794067
#> 2: Permutation   0.2353592
#> 3:   Reference   0.8579748
```

The reference sampler better preserves the correlation structure because
it samples complete rows, while permutation completely breaks the
dependency.

## Conditional Samplers

Conditional samplers account for dependencies between features by
sampling from \\P(X_j \| X\_{-j})\\ rather than the marginal \\P(X_j)\\.
This is relevant when features are correlated.

All conditional samplers inherit from `ConditionalSampler` and support:

- Specifying which features to condition on via `conditioning_set`
- Both `$sample()` and `$sample_newdata()` methods
- Mixed feature types (depending on the specific sampler)

### Gaussian Conditional Sampler

The `ConditionalGaussianSampler` assumes features follow a multivariate
Gaussian distribution and uses closed-form conditional distributions.

**Advantages**:

- Very fast (no model fitting during sampling)
- Deterministic given a seed
- No hyperparameters

**Limitations**:

- Assumes multivariate normality
- Only supports continuous features
- May produce out-of-range values

``` r
# Create Gaussian conditional sampler
gaussian = ConditionalGaussianSampler$new(task_numeric)

# Sample x1 conditioned on other features
sampled = gaussian$sample(
    feature = "x1",
    row_ids = 1:10,
    conditioning_set = c("x2", "x3", "x4")
)

# Compare original and conditionally sampled values
original = task_numeric$data(rows = 1:10)
data.table(
    original = original$x1,
    sampled = sampled$x1,
    x2 = original$x2 # Conditioning feature (unchanged)
)
#>        original       sampled         x2
#>           <num>         <num>      <num>
#>  1: -1.05068376  0.1621207720 -0.5272128
#>  2: -2.06809208 -1.0256534013 -1.2991235
#>  3:  1.13656002  0.3037750530  1.3031674
#>  4: -1.67500747 -1.3601136679 -1.1771093
#>  5: -0.35705594 -0.4412497414 -0.3692326
#>  6: -0.13511336 -0.0008602544  0.2388833
#>  7:  0.88352949 -0.3288406905 -0.2852529
#>  8: -0.55523636 -1.6344225147 -1.3064160
#>  9: -0.47024600 -1.1859281874 -0.3218053
#> 10: -0.02536502 -0.5130306461 -0.4861314
```

Notice that the sampled values respect the conditional distribution -
they’re different from the original but plausible given the conditioning
features.

### ARF Sampler

The `ConditionalARFSampler` uses Adversarial Random Forests to model
complex conditional distributions. It’s the most flexible conditional
sampler.

**Advantages**:

- Handles mixed feature types (continuous, categorical, ordered)
- No distributional assumptions
- Captures non-linear relationships

**Limitations**:

- Requires fitting ARF model (slower initialization and sampling for
  large data)
- More hyperparameters to tune
- Stochastic sampling

``` r
# Create ARF sampler (works with full task including categorical features)
arf = ConditionalARFSampler$new(task_mixed, num_trees = 20, verbose = FALSE)

# Sample island conditioned on body measurements
sampled = arf$sample(
    feature = "island",
    row_ids = 1:10,
    conditioning_set = c("bill_length", "body_mass")
)

# Compare original and sampled island
original = task_mixed$data(rows = 1:10)
data.table(
    original_island = original$island,
    sampled_island = sampled$island,
    bill_length = original$bill_length, # Conditioning feature
    body_mass = original$body_mass # Conditioning feature
)
#>     original_island sampled_island bill_length body_mass
#>              <fctr>         <fctr>       <num>     <int>
#>  1:       Torgersen         Biscoe        39.1      3750
#>  2:       Torgersen         Biscoe        39.5      3800
#>  3:       Torgersen      Torgersen        40.3      3250
#>  4:       Torgersen      Torgersen          NA        NA
#>  5:       Torgersen          Dream        36.7      3450
#>  6:       Torgersen      Torgersen        39.3      3650
#>  7:       Torgersen      Torgersen        38.9      3625
#>  8:       Torgersen         Biscoe        39.2      4675
#>  9:       Torgersen      Torgersen        34.1      3475
#> 10:       Torgersen      Torgersen        42.0      4250
```

**Use in CFI**: ConditionalARFSampler is the default for Conditional
Feature Importance since it can be used with any task, unlike other
samplers.

### Ctree Conditional Sampler

The `ConditionalCtreeSampler` uses conditional inference trees to
partition the feature space and sample from local neighborhoods.

**Advantages**:

- Handles mixed feature types
- Interpretable tree structure
- Automatic feature selection (only splits on informative features)

**Limitations**:

- Requires tree building (slower than kNN)
- May produce duplicates if terminal nodes are small

``` r
# Create ctree sampler
ctree = ConditionalCtreeSampler$new(task_mixed)

# Sample with default parameters
sampled = ctree$sample(
    feature = "bill_length",
    row_ids = 1:10,
    conditioning_set = "island"
)

original = task_mixed$data(rows = 1:10)
data.table(
    island = original$island, # Conditioning feature
    original = original$bill_length,
    sampled = sampled$bill_length
)
#>        island original sampled
#>        <fctr>    <num>   <num>
#>  1: Torgersen     39.1    42.5
#>  2: Torgersen     39.5    46.0
#>  3: Torgersen     40.3    39.5
#>  4: Torgersen       NA    35.2
#>  5: Torgersen     36.7    38.5
#>  6: Torgersen     39.3    41.4
#>  7: Torgersen     38.9    42.0
#>  8: Torgersen     39.2    34.6
#>  9: Torgersen     34.1    42.1
#> 10: Torgersen     42.0    42.5
```

The ctree sampler partitions observations based on the conditioning
features and samples from within the same partition (terminal node).

### kNN Conditional Sampler

The `ConditionalKNNSampler` finds k nearest neighbors based on
conditioning features and samples from them.

**Advantages**:

- Very simple and intuitive
- Fast (no model fitting)
- Single hyperparameter (k)
- Automatic distance metric selection
- Supports mixed feature types (numeric, categorical, ordered, logical)

**Limitations**:

- Sensitive to choice of k
- May produce duplicates if k is small

**Distance metric**:

The sampler automatically selects the appropriate distance metric based
on conditioning features:

- **Euclidean distance** when all conditioning features are
  numeric/integer (standardized)
- **Gower distance** when any conditioning features are categorical
  (factor, ordered, logical)

#### Example 1: All-numeric conditioning (Euclidean distance)

``` r
# Create kNN sampler with k=5 neighbors
knn_numeric = ConditionalKNNSampler$new(task_numeric, k = 5)

# Sample x1 based on nearest neighbors in (x2, x3) space
sampled_numeric = knn_numeric$sample(
    feature = "x1",
    row_ids = 1:5,
    conditioning_set = c("x2", "x3")
)

original_numeric = task_numeric$data(rows = 1:5)
data.table(
    x2 = original_numeric$x2,
    x3 = original_numeric$x3,
    original_x1 = original_numeric$x1,
    sampled_x1 = sampled_numeric$x1
)
#>            x2         x3 original_x1  sampled_x1
#>         <num>      <num>       <num>       <num>
#> 1: -0.5272128  1.7361110  -1.0506838 -0.04454979
#> 2: -1.2991235 -0.8452478  -2.0680921 -1.85485761
#> 3:  1.3031674 -0.9615715   1.1365600  1.54017968
#> 4: -1.1771093  1.0174911  -1.6750075 -1.67500747
#> 5: -0.3692326 -1.4960537  -0.3570559 -0.35705594
```

#### Example 2: Mixed-type conditioning (Gower distance)

``` r
# Use task with categorical features
knn_mixed = ConditionalKNNSampler$new(task_mixed, k = 5)

# Sample bill_length conditioning on island (categorical) and body_mass (numeric)
sampled_mixed = knn_mixed$sample(
    feature = "bill_length",
    row_ids = 1:5,
    conditioning_set = c("island", "body_mass")
)

original_mixed = task_mixed$data(rows = 1:5)
data.table(
    island = original_mixed$island,
    body_mass = original_mixed$body_mass,
    original_bill = original_mixed$bill_length,
    sampled_bill = sampled_mixed$bill_length
)
#>       island body_mass original_bill sampled_bill
#>       <fctr>     <int>         <num>        <num>
#> 1: Torgersen      3750          39.1           NA
#> 2: Torgersen      3800          39.5         36.7
#> 3: Torgersen      3250          40.3         37.8
#> 4: Torgersen        NA            NA         37.8
#> 5: Torgersen      3450          36.7         40.2
```

The kNN sampler finds the k most similar observations (based on
conditioning features) and samples from their feature values. The
distance metric is chosen automatically based on feature types.

## Knockoff Samplers

Now that we’ve seen conditional samplers, we can understand an important
limitation of knockoff samplers: unlike the conditional samplers above,
knockoffs **don’t support arbitrary conditioning sets**.

Knockoff samplers create synthetic features (knockoffs) that satisfy
specific statistical properties. They must fulfill the **knockoff swap
property**: swapping a feature with its knockoff should not change the
joint distribution.

Knockoffs are a separate category because:

1.  They require special construction to satisfy theoretical guarantees,
    and implementations are limited
2.  They don’t support conditional sampling with arbitrary conditioning
    sets
3.  They’re used primarily for feature selection with FDR control, not
    general importance

### Gaussian Knockoffs

For multivariate Gaussian data, we can construct exact knockoffs:

``` r
# Create Gaussian knockoff sampler (using task_numeric from earlier)
knockoff = KnockoffGaussianSampler$new(task_numeric)

# Generate knockoffs
original = task_numeric$data(rows = 1:5)
knockoffs = knockoff$sample(
    feature = task_numeric$feature_names,
    row_ids = 1:5
)

# Original vs knockoff values
data.table(
    x1_original = original$x1,
    x1_knockoff = knockoffs$x1,
    x2_original = original$x2,
    x2_knockoff = knockoffs$x2
)
#>    x1_original x1_knockoff x2_original x2_knockoff
#>          <num>       <num>       <num>       <num>
#> 1:  -1.0506838  -0.7513172  -0.5272128   -1.433181
#> 2:  -2.0680921  -0.6129308  -1.2991235   -1.183605
#> 3:   1.1365600   1.6247546   1.3031674    1.642699
#> 4:  -1.6750075  -0.8129150  -1.1771093   -1.450024
#> 5:  -0.3570559  -1.2109496  -0.3692326   -1.074235
```

Key properties of knockoffs:

- Different values but similar statistical properties
- Pairwise exchangeability with originals
- Preserve correlation structure
- Cannot specify which features to condition on (determined by the
  knockoff construction)

**Conditional Independence Testing**: Knockoffs are particularly
relevant for conditional independence testing as implemented in the [cpi
package](https://cran.r-project.org/package=cpi). You can combine
knockoff samplers with CFI and perform inference:

``` r
# CFI with knockoff sampler for conditional independence testing
cfi_knockoff = CFI$new(
    task = task_numeric,
    learner = lrn("regr.ranger"),
    measure = msr("regr.mse"),
    sampler = knockoff
)

# Compute importance with CPI-based inference
cfi_knockoff$compute()
cfi_knockoff$importance(ci_method = "cpi")
```

See `vignette("inference")` for more details on statistical inference
with feature importance.

**Key takeaways**:

- **Permutation sampling** produces any value from the marginal
  distribution
- **Conditional samplers** produce values consistent with conditioning
  features
- Choice of sampler affects feature importance estimates, especially
  when features are correlated

## Summary

| Sampler                      | Feature Types | Assumptions                  | Speed     | Use Case                     |
|------------------------------|---------------|------------------------------|-----------|------------------------------|
| `MarginalPermutationSampler` | All           | None                         | Very fast | PFI, uncorrelated features   |
| `KnockoffGaussianSampler`    | Continuous    | Multivariate normal          | Fast      | Model-X knockoffs            |
| `ConditionalGaussianSampler` | Continuous    | Multivariate normal          | Very fast | CFI with continuous features |
| `ConditionalARFSampler`      | All           | None                         | Moderate  | CFI, complex dependencies    |
| `ConditionalCtreeSampler`    | All           | None                         | Moderate  | CFI, interpretable sampling  |
| `ConditionalKNNSampler`      | All           | None (auto-selects distance) | Fast      | CFI, simple local structure  |

**General guidelines**:

1.  Use **permutation sampling** when features are independent or for
    baseline PFI
2.  Use **Gaussian conditional** for fast conditional sampling with
    continuous features
3.  Use **ARF** for the most flexible conditional sampling with mixed
    types
4.  Use **kNN** for simple, fast conditional sampling
5.  Use **ctree** when you want interpretable conditional sampling
6.  Use **knockoffs** when you need theoretical guarantees for feature
    selection and inference
