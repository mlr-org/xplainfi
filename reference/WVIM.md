# Williamson's Variable Importance Measure (WVIM)

Base class generalizazing refit-based variable importance measures.
Default corresponds to leaving out each feature `n_repeats` times, which
corresponds to LOCO (Leave One Covariate Out).

## Super class

[`xplainfi::FeatureImportanceMethod`](https://jemus42.github.io/xplainfi/reference/FeatureImportanceMethod.md)
-\> `WVIM`

## Public fields

- `direction`:

  (`character(1)`) Either "leave-out" or "leave-in".

- `design`:

  ([`logical()`](https://rdrr.io/r/base/logical.html)) Feature selection
  design matrix where `TRUE` equals "left in" and `FALSE` "left out".
  Columns correspond to `task$feature_names` and the number of rows
  corresponds to `length(features) * n_repeats`. The base matrix is
  created by
  [wvim_design_matrix](https://jemus42.github.io/xplainfi/reference/wvim_design_matrix.md)
  and then replicated `n_repeats` times before.

- `instance`:

  (`FSelectInstanceBatchSingleCrit`) The `mlr3fselect` feature selection
  instance containing also the archive of all evaluations, possible
  useful for future use. Only stored if `store_instance` is `TRUE`.

## Methods

### Public methods

- [`WVIM$new()`](#method-WVIM-new)

- [`WVIM$compute()`](#method-WVIM-compute)

- [`WVIM$clone()`](#method-WVIM-clone)

Inherited methods

- [`xplainfi::FeatureImportanceMethod$importance()`](https://jemus42.github.io/xplainfi/reference/FeatureImportanceMethod.html#method-importance)
- [`xplainfi::FeatureImportanceMethod$obs_loss()`](https://jemus42.github.io/xplainfi/reference/FeatureImportanceMethod.html#method-obs_loss)
- [`xplainfi::FeatureImportanceMethod$print()`](https://jemus42.github.io/xplainfi/reference/FeatureImportanceMethod.html#method-print)
- [`xplainfi::FeatureImportanceMethod$reset()`](https://jemus42.github.io/xplainfi/reference/FeatureImportanceMethod.html#method-reset)
- [`xplainfi::FeatureImportanceMethod$scores()`](https://jemus42.github.io/xplainfi/reference/FeatureImportanceMethod.html#method-scores)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    WVIM$new(
      task,
      learner,
      measure = NULL,
      resampling = NULL,
      features = NULL,
      groups = NULL,
      direction = c("leave-out", "leave-in"),
      label = "Williamson's Variable Importance Measure (WVIM)",
      n_repeats = 1L
    )

#### Arguments

- `task, learner, measure, resampling, features, groups`:

  Passed to `FeatureImportanceMethod` for construction.

- `direction`:

  (`character(1)`) Either "leave-out" or "leave-in".

- `label`:

  (`character(1)`) Method label.

- `n_repeats`:

  (`integer(1)`) Number of refit iterations per resampling iteration.

------------------------------------------------------------------------

### Method `compute()`

Computes leave-out or leave-in feature importance.
`wvim_design_matrix(task$feature_names, "leave-out")` corresponds to
LOCO.

#### Usage

    WVIM$compute(
      store_models = TRUE,
      store_backends = TRUE,
      store_instance = FALSE
    )

#### Arguments

- `store_models, store_backends`:

  (`logical(1)`: `TRUE`) Whether to store fitted models / data backends,
  passed to
  [mlr3::resample](https://mlr3.mlr-org.com/reference/resample.html)
  internally backends in resample result. Required for some measures,
  but may increase memory footprint.

- `store_instance`:

  (`logical(1)`: `FALSE`) Whether to store the
  [mlr3fselect::mlr3fselect](https://mlr3fselect.mlr-org.com/reference/mlr3fselect-package.html)
  instance in `$instance`.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    WVIM$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
