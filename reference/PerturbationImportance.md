# Perturbation Feature Importance Base Class

Abstract base class for perturbation-based importance methods PFI, CFI,
and RFI

## Super class

[`xplainfi::FeatureImportanceMethod`](https://jemus42.github.io/xplainfi/reference/FeatureImportanceMethod.md)
-\> `PerturbationImportance`

## Public fields

- `sampler`:

  ([FeatureSampler](https://jemus42.github.io/xplainfi/reference/FeatureSampler.md))
  Sampler object for feature perturbation

## Methods

### Public methods

- [`PerturbationImportance$new()`](#method-PerturbationImportance-new)

- [`PerturbationImportance$clone()`](#method-PerturbationImportance-clone)

Inherited methods

- [`xplainfi::FeatureImportanceMethod$compute()`](https://jemus42.github.io/xplainfi/reference/FeatureImportanceMethod.html#method-compute)
- [`xplainfi::FeatureImportanceMethod$importance()`](https://jemus42.github.io/xplainfi/reference/FeatureImportanceMethod.html#method-importance)
- [`xplainfi::FeatureImportanceMethod$obs_loss()`](https://jemus42.github.io/xplainfi/reference/FeatureImportanceMethod.html#method-obs_loss)
- [`xplainfi::FeatureImportanceMethod$print()`](https://jemus42.github.io/xplainfi/reference/FeatureImportanceMethod.html#method-print)
- [`xplainfi::FeatureImportanceMethod$reset()`](https://jemus42.github.io/xplainfi/reference/FeatureImportanceMethod.html#method-reset)
- [`xplainfi::FeatureImportanceMethod$scores()`](https://jemus42.github.io/xplainfi/reference/FeatureImportanceMethod.html#method-scores)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of the PerturbationImportance class

#### Usage

    PerturbationImportance$new(
      task,
      learner,
      measure,
      resampling = NULL,
      features = NULL,
      groups = NULL,
      sampler = NULL,
      relation = "difference",
      n_repeats = 1L,
      batch_size = NULL
    )

#### Arguments

- `task, learner, measure, resampling, features, groups`:

  Passed to
  [FeatureImportanceMethod](https://jemus42.github.io/xplainfi/reference/FeatureImportanceMethod.md).

- `sampler`:

  ([FeatureSampler](https://jemus42.github.io/xplainfi/reference/FeatureSampler.md))
  Sampler to use for feature perturbation.

- `relation`:

  (`character(1)`: `"difference"`) How to relate perturbed and baseline
  scores. Can also be `"ratio"`.

- `n_repeats`:

  (`integer(1)`: `1L`) Number of permutation/conditional sampling
  iterations. Can be overridden in `$compute()`.

- `batch_size`:

  (`integer(1)` \| `NULL`: `NULL`) Maximum number of rows to predict at
  once. When `NULL`, predicts all `test_size * n_repeats` rows in one
  call. Use smaller values to reduce memory usage at the cost of more
  prediction calls. Can be overridden in `$compute()`.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    PerturbationImportance$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
