# Perturbation Feature Importance Base Class

Abstract base class for perturbation-based importance methods PFI, CFI,
and RFI

## Super class

[`xplainfi::FeatureImportanceMethod`](https://mlr-org.github.io/xplainfi/reference/FeatureImportanceMethod.md)
-\> `PerturbationImportance`

## Public fields

- `sampler`:

  ([FeatureSampler](https://mlr-org.github.io/xplainfi/reference/FeatureSampler.md))
  Sampler object for feature perturbation

## Methods

### Public methods

- [`PerturbationImportance$new()`](#method-PerturbationImportance-new)

- [`PerturbationImportance$importance()`](#method-PerturbationImportance-importance)

- [`PerturbationImportance$clone()`](#method-PerturbationImportance-clone)

Inherited methods

- [`xplainfi::FeatureImportanceMethod$compute()`](https://mlr-org.github.io/xplainfi/reference/FeatureImportanceMethod.html#method-compute)
- [`xplainfi::FeatureImportanceMethod$obs_loss()`](https://mlr-org.github.io/xplainfi/reference/FeatureImportanceMethod.html#method-obs_loss)
- [`xplainfi::FeatureImportanceMethod$print()`](https://mlr-org.github.io/xplainfi/reference/FeatureImportanceMethod.html#method-print)
- [`xplainfi::FeatureImportanceMethod$reset()`](https://mlr-org.github.io/xplainfi/reference/FeatureImportanceMethod.html#method-reset)
- [`xplainfi::FeatureImportanceMethod$scores()`](https://mlr-org.github.io/xplainfi/reference/FeatureImportanceMethod.html#method-scores)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of the PerturbationImportance class

#### Usage

    PerturbationImportance$new(
      task,
      learner,
      measure = NULL,
      resampling = NULL,
      features = NULL,
      groups = NULL,
      sampler = NULL,
      relation = "difference",
      n_repeats = 30L,
      batch_size = NULL
    )

#### Arguments

- `task, learner, measure, resampling, features, groups`:

  Passed to
  [FeatureImportanceMethod](https://mlr-org.github.io/xplainfi/reference/FeatureImportanceMethod.md).

- `sampler`:

  ([FeatureSampler](https://mlr-org.github.io/xplainfi/reference/FeatureSampler.md))
  Sampler to use for feature perturbation.

- `relation`:

  (`character(1)`: `"difference"`) How to relate perturbed and baseline
  scores. Can also be `"ratio"`.

- `n_repeats`:

  (`integer(1)`: `30L`) Number of permutation/conditional sampling
  iterations. Can also be overridden in `$compute()`.

- `batch_size`:

  (`integer(1)` \| `NULL`: `NULL`) Maximum number of rows to predict at
  once. When `NULL`, predicts all `test_size * n_repeats` rows in one
  call. Use smaller values to reduce memory usage at the cost of more
  prediction calls. Can be overridden in `$compute()`.

------------------------------------------------------------------------

### Method `importance()`

Get aggregated importance scores. Extends the base `$importance()`
method to support `ci_method = "cpi"`. For details, see
[CFI](https://mlr-org.github.io/xplainfi/reference/CFI.md), which is the
only sub-method for which it is known to be valid.

#### Usage

    PerturbationImportance$importance(
      relation = NULL,
      standardize = FALSE,
      ci_method = c("none", "raw", "nadeau_bengio", "quantile", "cpi"),
      conf_level = 0.95,
      alternative = c("greater", "two.sided"),
      test = c("t", "wilcoxon", "fisher", "binomial"),
      B = 1999,
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
  perturbation methods support `"cpi"` (Conditional Predictive Impact).
  CPI is specifically designed for
  [CFI](https://mlr-org.github.io/xplainfi/reference/CFI.md) with
  knockoff samplers and uses one-sided hypothesis tests.

- `conf_level`:

  (`numeric(1)`: `0.95`) Confidence level for confidence intervals when
  `ci_method != "none"`.

- `alternative`:

  (`character(1)`: `"greater"`) Type of alternative hypothesis for
  statistical tests. `"greater"` tests H0: importance \<= 0 vs H1:
  importance \> 0 (one-sided). `"two.sided"` tests H0: importance = 0 vs
  H1: importance != 0.

- `test`:

  (`character(1)`: `"t"`) Test to use for CPI. One of `"t"`,
  `"wilcoxon"`, `"fisher"`, or `"binomial"`. Only used when
  `ci_method = "cpi"`.

- `B`:

  (`integer(1)`: `1999`) Number of replications for Fisher test. Only
  used when `ci_method = "cpi"` and `test = "fisher"`.

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

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    PerturbationImportance$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
