# Williamson's Variable Importance Measure (WVIM)

Base class generalizing refit-based variable importance measures.
Default corresponds to leaving out each feature `n_repeats` times, which
corresponds to LOCO (Leave One Covariate Out).

## References

Lei J, G'Sell M, Rinaldo A, Tibshirani R, Wasserman L (2018).
“Distribution-Free Predictive Inference for Regression.” *Journal of the
American Statistical Association*, **113**(523), 1094–1111.
[doi:10.1080/01621459.2017.1307116](https://doi.org/10.1080/01621459.2017.1307116)
.

## Super class

[`xplainfi::FeatureImportanceMethod`](https://mlr-org.github.io/xplainfi/reference/FeatureImportanceMethod.md)
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
  [wvim_design_matrix](https://mlr-org.github.io/xplainfi/reference/wvim_design_matrix.md)
  and then replicated `n_repeats` times before.

- `instance`:

  (`FSelectInstanceBatchSingleCrit`) The `mlr3fselect` feature selection
  instance containing also the archive of all evaluations, possible
  useful for future use. Only stored if `store_instance` is `TRUE`.

## Methods

### Public methods

- [`WVIM$new()`](#method-WVIM-new)

- [`WVIM$importance()`](#method-WVIM-importance)

- [`WVIM$compute()`](#method-WVIM-compute)

- [`WVIM$clone()`](#method-WVIM-clone)

Inherited methods

- [`xplainfi::FeatureImportanceMethod$obs_loss()`](https://mlr-org.github.io/xplainfi/reference/FeatureImportanceMethod.html#method-obs_loss)
- [`xplainfi::FeatureImportanceMethod$print()`](https://mlr-org.github.io/xplainfi/reference/FeatureImportanceMethod.html#method-print)
- [`xplainfi::FeatureImportanceMethod$reset()`](https://mlr-org.github.io/xplainfi/reference/FeatureImportanceMethod.html#method-reset)
- [`xplainfi::FeatureImportanceMethod$scores()`](https://mlr-org.github.io/xplainfi/reference/FeatureImportanceMethod.html#method-scores)

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
      n_repeats = 30L
    )

#### Arguments

- `task, learner, measure, resampling, features, groups`:

  Passed to `FeatureImportanceMethod` for construction.

- `direction`:

  (`character(1)`) Either "leave-out" or "leave-in".

- `label`:

  (`character(1)`) Method label.

- `n_repeats`:

  (`integer(1)`: `30L`) Number of refit iterations per resampling
  iteration.

------------------------------------------------------------------------

### Method `importance()`

Get aggregated importance scores. Extends the base `$importance()`
method to support `ci_method = "lei"`.

This implements distribution-free inference based on Lei et al. (2018),
testing observation-wise loss differences using the Wilcoxon signed-rank
test by default.

Lei et al. (2018) proposed this method specifically for LOCO with L1
(absolute) loss, median aggregation, and a single train/test split
(holdout). The inference is conditional on the training data, requiring
i.i.d. test observations from a single split. While the aggregation
function, statistical test, and resampling strategy are parameterizable,
deviating from these defaults may invalidate the theoretical guarantees.

For a comprehensive overview of inference methods, see
`vignette("inference", package = "xplainfi")`.

#### Usage

    WVIM$importance(
      relation = NULL,
      standardize = FALSE,
      ci_method = c("none", "raw", "nadeau_bengio", "quantile", "lei"),
      conf_level = 0.95,
      alternative = c("greater", "two.sided"),
      test = c("wilcoxon", "t", "fisher", "binomial"),
      B = 1999,
      aggregator = NULL,
      p_adjust = "none",
      ...
    )

#### Arguments

- `relation`:

  (`character(1)`) How to relate perturbed scores to originals
  ("difference" or "ratio"). If `NULL`, uses stored parameter value.

- `standardize`:

  (`logical(1)`: `FALSE`) If `TRUE`, importances are standardized by the
  highest score so all scores fall in `[-1, 1]`.

- `ci_method`:

  (`character(1)`: `"none"`) Variance estimation method. In addition to
  base methods (`"none"`, `"raw"`, `"nadeau_bengio"`, `"quantile"`),
  WVIM methods support `"lei"` for distribution-free inference (Lei et
  al., 2018).

- `conf_level`:

  (`numeric(1)`: `0.95`) Confidence level to use for confidence interval
  construction when `ci_method != "none"`.

- `alternative`:

  (`character(1)`: `"greater"`) Type of alternative hypothesis for
  statistical tests. `"greater"` tests H0: importance \<= 0 vs H1:
  importance \> 0 (one-sided). `"two.sided"` tests H0: importance = 0 vs
  H1: importance != 0.

- `test`:

  (`character(1)`: `"wilcoxon"`) Test to use for Lei et al. inference.
  One of `"wilcoxon"`, `"t"`, `"fisher"`, or `"binomial"`. Only used
  when `ci_method = "lei"`.

- `B`:

  (`integer(1)`: `1999`) Number of replications for Fisher test. Only
  used when `ci_method = "lei"` and `test = "fisher"`.

- `aggregator`:

  (`function`: [`stats::median`](https://rdrr.io/r/stats/median.html))
  Aggregation function for computing the point estimate from
  observation-wise importance values. Defaults to
  [`stats::median`](https://rdrr.io/r/stats/median.html) as proposed by
  Lei et al. (2018). Only used when `ci_method = "lei"`.

- `p_adjust`:

  (`character(1)`: `"none"`) Method for p-value adjustment for multiple
  comparisons. Accepts any method supported by
  [stats::p.adjust.methods](https://rdrr.io/r/stats/p.adjust.html), e.g.
  `"holm"`, `"bonferroni"`, `"BH"`, `"none"`. When `"bonferroni"`,
  confidence intervals are also adjusted (alpha/k). For other correction
  methods (e.g. `"holm"`, `"BH"`), only p-values are adjusted;
  confidence intervals remain at the nominal `conf_level` because these
  sequential/adaptive procedures do not have a clean per-comparison
  alpha for CI construction.

- `...`:

  Additional arguments passed to the base method.

#### Returns

([data.table](https://rdrr.io/pkg/data.table/man/data.table.html))
Aggregated importance scores.

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

## Examples

``` r
library(mlr3)
library(mlr3learners)

task <- sim_dgp_correlated(n = 500)

# Group correlated features together, independent features separately
groups <- list(
  correlated = c("x1", "x2"),
  independent = c("x3", "x4")
)

wvim <- WVIM$new(
  task = task,
  learner = lrn("regr.ranger", num.trees = 10),
  groups = groups
)
#> ℹ No <Measure> provided, using `measure = msr("regr.mse")`
#> ℹ No <Resampling> provided, using `resampling = rsmp("holdout", ratio = 2/3)`
#>   (test set size: 167)
wvim$compute()
wvim$importance()
#> Key: <feature>
#>        feature importance
#>         <char>      <num>
#> 1:  correlated   4.722423
#> 2: independent   1.206976
```
