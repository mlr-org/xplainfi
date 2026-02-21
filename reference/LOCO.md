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

[`xplainfi::FeatureImportanceMethod`](https://mlr-org.github.io/xplainfi/reference/FeatureImportanceMethod.md)
-\>
[`xplainfi::WVIM`](https://mlr-org.github.io/xplainfi/reference/WVIM.md)
-\> `LOCO`

## Methods

### Public methods

- [`LOCO$new()`](#method-LOCO-new)

- [`LOCO$compute()`](#method-LOCO-compute)

- [`LOCO$clone()`](#method-LOCO-clone)

Inherited methods

- [`xplainfi::FeatureImportanceMethod$obs_loss()`](https://mlr-org.github.io/xplainfi/reference/FeatureImportanceMethod.html#method-obs_loss)
- [`xplainfi::FeatureImportanceMethod$print()`](https://mlr-org.github.io/xplainfi/reference/FeatureImportanceMethod.html#method-print)
- [`xplainfi::FeatureImportanceMethod$reset()`](https://mlr-org.github.io/xplainfi/reference/FeatureImportanceMethod.html#method-reset)
- [`xplainfi::FeatureImportanceMethod$scores()`](https://mlr-org.github.io/xplainfi/reference/FeatureImportanceMethod.html#method-scores)
- [`xplainfi::WVIM$importance()`](https://mlr-org.github.io/xplainfi/reference/WVIM.html#method-importance)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    LOCO$new(
      task,
      learner,
      measure = NULL,
      resampling = NULL,
      features = NULL,
      n_repeats = 30L
    )

#### Arguments

- `task`:

  ([mlr3::Task](https://mlr3.mlr-org.com/reference/Task.html)) Task to
  compute importance for.

- `learner`:

  ([mlr3::Learner](https://mlr3.mlr-org.com/reference/Learner.html))
  Learner to use for prediction.

- `measure`:

  ([mlr3::Measure](https://mlr3.mlr-org.com/reference/Measure.html):
  `NULL`) Measure to use for scoring. Defaults to `classif.ce` for
  classification and `regr.mse` for regression.

- `resampling`:

  ([mlr3::Resampling](https://mlr3.mlr-org.com/reference/Resampling.html))
  Resampling strategy. Defaults to holdout.

- `features`:

  ([`character()`](https://rdrr.io/r/base/character.html)) Features to
  compute importance for. Defaults to all features.

- `n_repeats`:

  (`integer(1)`: `30L`) Number of refit iterations per resampling
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
library(mlr3)
library(mlr3learners)

task <- sim_dgp_correlated(n = 500)

loco <- LOCO$new(
  task = task,
  learner = lrn("regr.ranger", num.trees = 10),
  measure = msr("regr.mse")
)
#> ℹ No <Resampling> provided, using `resampling = rsmp("holdout", ratio = 2/3)`
#>   (test set size: 167)
loco$compute()
loco$importance()
#> Key: <feature>
#>    feature importance
#>     <char>      <num>
#> 1:      x1  0.9657516
#> 2:      x2  0.2995086
#> 3:      x3  1.4123144
#> 4:      x4  0.0883162
```
