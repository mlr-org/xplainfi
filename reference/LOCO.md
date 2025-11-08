# Leave-One-Covariate-Out (LOCO)

Calculates Leave-One-Covariate-Out (LOCO) scores.

## Details

LOCO measures feature importance by comparing model performance with and
without each feature. For each feature, the model is retrained without
that feature and the performance difference (reduced_model_loss -
full_model_loss) indicates the feature's importance. Higher values
indicate more important features.

## References

Lei J, G'Sell M, Rinaldo A, Tibshirani R, Wasserman L (2018).
“Distribution-Free Predictive Inference for Regression.” *Journal of the
American Statistical Association*, **113**(523), 1094–1111.
[doi:10.1080/01621459.2017.1307116](https://doi.org/10.1080/01621459.2017.1307116)
.

## Super classes

[`xplainfi::FeatureImportanceMethod`](https://jemus42.github.io/xplainfi/reference/FeatureImportanceMethod.md)
-\>
[`xplainfi::WVIM`](https://jemus42.github.io/xplainfi/reference/WVIM.md)
-\> `LOCO`

## Methods

### Public methods

- [`LOCO$new()`](#method-LOCO-new)

- [`LOCO$compute()`](#method-LOCO-compute)

- [`LOCO$clone()`](#method-LOCO-clone)

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

    LOCO$new(
      task,
      learner,
      measure,
      resampling = NULL,
      features = NULL,
      n_repeats = 1L
    )

#### Arguments

- `task`:

  ([mlr3::Task](https://mlr3.mlr-org.com/reference/Task.html)) Task to
  compute importance for.

- `learner`:

  ([mlr3::Learner](https://mlr3.mlr-org.com/reference/Learner.html))
  Learner to use for prediction.

- `measure`:

  ([mlr3::Measure](https://mlr3.mlr-org.com/reference/Measure.html))
  Measure to use for scoring.

- `resampling`:

  ([mlr3::Resampling](https://mlr3.mlr-org.com/reference/Resampling.html))
  Resampling strategy. Defaults to holdout.

- `features`:

  ([`character()`](https://rdrr.io/r/base/character.html)) Features to
  compute importance for. Defaults to all features.

- `n_repeats`:

  (`integer(1)`: `1L`) Number of refit iterations per resampling
  iteration.

------------------------------------------------------------------------

### Method `compute()`

Compute LOCO importances.

#### Usage

    LOCO$compute(store_models = TRUE, store_backends = TRUE)

#### Arguments

- `store_models, store_backends`:

  (`logical(1)`: `TRUE`) Whether to store fitted models / data backends,
  passed to
  [mlr3::resample](https://mlr3.mlr-org.com/reference/resample.html)
  internally

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    LOCO$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
```
