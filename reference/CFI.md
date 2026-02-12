# Conditional Feature Importance

Implementation of CFI using modular sampling approach

## Details

CFI replaces feature values with conditional samples from the
distribution of the feature given the other features. Any
[ConditionalSampler](https://mlr-org.github.io/xplainfi/reference/ConditionalSampler.md)
or
[KnockoffSampler](https://mlr-org.github.io/xplainfi/reference/KnockoffSampler.md)
can be used.

### Statistical Inference

Two approaches for statistical inference are primarily supported via
`$importance(ci_method = "cpi")`:

- **CPI** (Watson & Wright, 2021): The original Conditional Predictive
  Impact method, designed for use with knockoff samplers
  ([KnockoffGaussianSampler](https://mlr-org.github.io/xplainfi/reference/KnockoffGaussianSampler.md)).

- **cARFi** (Blesch et al., 2025): CFI with ARF-based conditional
  sampling
  ([ConditionalARFSampler](https://mlr-org.github.io/xplainfi/reference/ConditionalARFSampler.md)),
  using the same CPI inference framework.

Both require a decomposable measure (e.g., MSE) and holdout resampling
so each observation appears at most once in the test set.

Available tests: `"t"` (t-test), `"wilcoxon"` (signed-rank), `"fisher"`
(permutation), `"binomial"` (sign test). The Fisher test is recommended.

Method-agnostic inference methods (`"raw"`, `"nadeau_bengio"`,
`"quantile"`) are also available; see
[FeatureImportanceMethod](https://mlr-org.github.io/xplainfi/reference/FeatureImportanceMethod.md)
for details.

## References

Watson D, Wright M (2021). “Testing Conditional Independence in
Supervised Learning Algorithms.” *Machine Learning*, **110**(8),
2107–2129.
[doi:10.1007/s10994-021-06030-6](https://doi.org/10.1007/s10994-021-06030-6)
.

Blesch K, Koenen N, Kapar J, Golchian P, Burk L, Loecher M, Wright M
(2025). “Conditional Feature Importance with Generative Modeling Using
Adversarial Random Forests.” *Proceedings of the AAAI Conference on
Artificial Intelligence*, **39**(15), 15596–15604.
[doi:10.1609/aaai.v39i15.33712](https://doi.org/10.1609/aaai.v39i15.33712)
.

## Super classes

[`xplainfi::FeatureImportanceMethod`](https://mlr-org.github.io/xplainfi/reference/FeatureImportanceMethod.md)
-\>
[`xplainfi::PerturbationImportance`](https://mlr-org.github.io/xplainfi/reference/PerturbationImportance.md)
-\> `CFI`

## Methods

### Public methods

- [`CFI$new()`](#method-CFI-new)

- [`CFI$compute()`](#method-CFI-compute)

- [`CFI$clone()`](#method-CFI-clone)

Inherited methods

- [`xplainfi::FeatureImportanceMethod$obs_loss()`](https://mlr-org.github.io/xplainfi/reference/FeatureImportanceMethod.html#method-obs_loss)
- [`xplainfi::FeatureImportanceMethod$print()`](https://mlr-org.github.io/xplainfi/reference/FeatureImportanceMethod.html#method-print)
- [`xplainfi::FeatureImportanceMethod$reset()`](https://mlr-org.github.io/xplainfi/reference/FeatureImportanceMethod.html#method-reset)
- [`xplainfi::FeatureImportanceMethod$scores()`](https://mlr-org.github.io/xplainfi/reference/FeatureImportanceMethod.html#method-scores)
- [`xplainfi::PerturbationImportance$importance()`](https://mlr-org.github.io/xplainfi/reference/PerturbationImportance.html#method-importance)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of the CFI class

#### Usage

    CFI$new(
      task,
      learner,
      measure = NULL,
      resampling = NULL,
      features = NULL,
      groups = NULL,
      relation = "difference",
      n_repeats = 30L,
      batch_size = NULL,
      sampler = NULL
    )

#### Arguments

- `task, learner, measure, resampling, features, groups, relation, n_repeats, batch_size`:

  Passed to
  [PerturbationImportance](https://mlr-org.github.io/xplainfi/reference/PerturbationImportance.md).

- `sampler`:

  ([ConditionalSampler](https://mlr-org.github.io/xplainfi/reference/ConditionalSampler.md))
  Optional custom sampler. Defaults to instantiating
  `ConditionalARFSampler` internally with default parameters.

------------------------------------------------------------------------

### Method `compute()`

Compute CFI scores

#### Usage

    CFI$compute(
      n_repeats = NULL,
      batch_size = NULL,
      store_models = TRUE,
      store_backends = TRUE
    )

#### Arguments

- `n_repeats`:

  (`integer(1)`) Number of permutation iterations. If `NULL`, uses
  stored value.

- `batch_size`:

  (`integer(1)` \| `NULL`: `NULL`) Maximum number of rows to predict at
  once. If `NULL`, uses stored value.

- `store_models, store_backends`:

  (`logical(1)`: `TRUE`) Whether to store fitted models / data backends,
  passed to
  [mlr3::resample](https://mlr3.mlr-org.com/reference/resample.html)
  internally for the initial fit of the learner. This may be required
  for certain measures and is recommended to leave enabled unless really
  necessary.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    CFI$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
library(mlr3)
library(mlr3learners)

task <- sim_dgp_correlated(n = 500)

# Using default ConditionalARFSampler
cfi <- CFI$new(
  task = task,
  learner = lrn("regr.ranger", num.trees = 10),
  measure = msr("regr.mse")
)
#> ℹ No `sampler` provided, using <ConditionalARFSampler> with default settings.
#> ℹ No <Resampling> provided, using `resampling = rsmp("holdout", ratio = 2/3)`
#>   (test set size: 167)
cfi$compute()
cfi$importance()
#> Key: <feature>
#>    feature  importance
#>     <char>       <num>
#> 1:      x1  2.30619753
#> 2:      x2  0.13534473
#> 3:      x3  1.55134940
#> 4:      x4 -0.01195141
```
