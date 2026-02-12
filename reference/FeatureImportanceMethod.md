# Feature Importance Method Class

Feature Importance Method Class

Feature Importance Method Class

## References

Nadeau C, Bengio Y (2003). “Inference for the Generalization Error.”
*Machine Learning*, **52**(3), 239–281.
[doi:10.1023/A:1024068626366](https://doi.org/10.1023/A%3A1024068626366)
. Molnar C, Freiesleben T, König G, Herbinger J, Reisinger T,
Casalicchio G, Wright M, Bischl B (2023). “Relating the Partial
Dependence Plot and Permutation Feature Importance to the Data
Generating Process.” In Longo L (ed.), *Explainable Artificial
Intelligence*, 456–479. ISBN 978-3-031-44064-9,
[doi:10.1007/978-3-031-44064-9_24](https://doi.org/10.1007/978-3-031-44064-9_24)
.

## Public fields

- `label`:

  (`character(1)`) Method label.

- `task`:

  ([mlr3::Task](https://mlr3.mlr-org.com/reference/Task.html))

- `learner`:

  ([mlr3::Learner](https://mlr3.mlr-org.com/reference/Learner.html))

- `measure`:

  ([mlr3::Measure](https://mlr3.mlr-org.com/reference/Measure.html))

- `resampling`:

  ([mlr3::Resampling](https://mlr3.mlr-org.com/reference/Resampling.html)),
  instantiated upon construction.

- `resample_result`:

  ([mlr3::ResampleResult](https://mlr3.mlr-org.com/reference/ResampleResult.html))
  of the original `learner` and `task`, used for baseline scores.

- `features`:

  (`character`: `NULL`) Features of interest. By default, importances
  will be computed for each feature in `task`, but optionally this can
  be restricted to at least one feature. Ignored if `groups` is
  specified.

- `groups`:

  (`list`: `NULL`) A (named) list of features (names or indices as in
  `task`). If `groups` is specified, `features` is ignored. Importances
  will be calculated for group of features at a time, e.g., in
  [PFI](https://mlr-org.github.io/xplainfi/reference/PFI.md) not one but
  the group of features will be permuted at each step. Analogously in
  [WVIM](https://mlr-org.github.io/xplainfi/reference/WVIM.md), each
  group of features will be left out (or in) for each model refit. Not
  all methods support groups (e.g.,
  [SAGE](https://mlr-org.github.io/xplainfi/reference/SAGE.md)).

- `param_set`:

  ([`paradox::ps()`](https://paradox.mlr-org.com/reference/ps.html))

- `predictions`:

  ([data.table](https://rdrr.io/pkg/data.table/man/data.table.html))
  Feature-specific prediction objects provided for some methods
  ([PFI](https://mlr-org.github.io/xplainfi/reference/PFI.md),
  [WVIM](https://mlr-org.github.io/xplainfi/reference/WVIM.md)).
  Contains columns for feature of interest, resampling iteration, refit
  or perturbation iteration, and
  [mlr3::Prediction](https://mlr3.mlr-org.com/reference/Prediction.html)
  objects.

## Methods

### Public methods

- [`FeatureImportanceMethod$new()`](#method-FeatureImportanceMethod-new)

- [`FeatureImportanceMethod$compute()`](#method-FeatureImportanceMethod-compute)

- [`FeatureImportanceMethod$importance()`](#method-FeatureImportanceMethod-importance)

- [`FeatureImportanceMethod$obs_loss()`](#method-FeatureImportanceMethod-obs_loss)

- [`FeatureImportanceMethod$reset()`](#method-FeatureImportanceMethod-reset)

- [`FeatureImportanceMethod$print()`](#method-FeatureImportanceMethod-print)

- [`FeatureImportanceMethod$scores()`](#method-FeatureImportanceMethod-scores)

- [`FeatureImportanceMethod$clone()`](#method-FeatureImportanceMethod-clone)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class. This is
typically intended for use by derived classes.

#### Usage

    FeatureImportanceMethod$new(
      task,
      learner,
      measure = NULL,
      resampling = NULL,
      features = NULL,
      groups = NULL,
      param_set = paradox::ps(),
      label
    )

#### Arguments

- `task, learner, measure, resampling, features, groups, param_set, label`:

  Used to set fields

------------------------------------------------------------------------

### Method `compute()`

Compute feature importance scores

#### Usage

    FeatureImportanceMethod$compute(store_backends = TRUE)

#### Arguments

- `store_backends`:

  (`logical(1): TRUE`) Whether to store backends.

------------------------------------------------------------------------

### Method `importance()`

Get aggregated importance scores. The stored
[`measure`](https://mlr3.mlr-org.com/reference/Measure.html) object's
`aggregator` (default: `mean`) will be used to aggregated importance
scores across resampling iterations and, depending on the method use,
permutations
([PerturbationImportance](https://mlr-org.github.io/xplainfi/reference/PerturbationImportance.md)
or refits [LOCO](https://mlr-org.github.io/xplainfi/reference/LOCO.md)).

#### Usage

    FeatureImportanceMethod$importance(
      relation = NULL,
      standardize = FALSE,
      ci_method = c("none", "raw", "nadeau_bengio", "quantile"),
      conf_level = 0.95,
      alternative = c("greater", "two.sided"),
      ...
    )

#### Arguments

- `relation`:

  (character(1)) How to relate perturbed scores to originals
  ("difference" or "ratio"). If `NULL`, uses stored parameter value.
  This is only applicable for methods where importance is based on some
  relation between baseline and post-modification loss, i.e.
  [PerturbationImportance](https://mlr-org.github.io/xplainfi/reference/PerturbationImportance.md)
  methods such as
  [PFI](https://mlr-org.github.io/xplainfi/reference/PFI.md) or
  [WVIM](https://mlr-org.github.io/xplainfi/reference/WVIM.md) /
  [LOCO](https://mlr-org.github.io/xplainfi/reference/LOCO.md). Not
  available for
  [SAGE](https://mlr-org.github.io/xplainfi/reference/SAGE.md) methods.

- `standardize`:

  (`logical(1)`: `FALSE`) If `TRUE`, importances are standardized by the
  highest score so all scores fall in `[-1, 1]`.

- `ci_method`:

  (`character(1)`: `"none"`) Which confidence interval estimation method
  to use, defaulting to omitting variance estimation (`"none"`). If
  `"raw"`, uncorrected (too narrow) CIs are provided purely for
  informative purposes. If `"nadeau_bengio"`, variance correction is
  performed according to Nadeau & Bengio (2003) as suggested by Molnar
  et al. (2023). If `"quantile"`, empirical quantiles are used to
  construct confidence-like intervals. These methods are model-agnostic
  and rely on suitable `resampling`s, e.g. subsampling with 15 repeats
  for `"nadeau_bengio"`. See details.

- `conf_level`:

  (`numeric(1)`: `0.95`) Confidence level to use for confidence interval
  construction when `ci_method != "none"`.

- `alternative`:

  (`character(1)`: `"greater"`) Type of alternative hypothesis for
  statistical tests. `"greater"` tests H0: importance \<= 0 vs H1:
  importance \> 0 (one-sided). `"two.sided"` tests H0: importance = 0 vs
  H1: importance != 0. Only used when `ci_method != "none"`.

- `...`:

  Additional arguments passed to specialized methods, if any.

#### Details

##### Confidence Interval Methods

The parametric methods (`"raw"`, `"nadeau_bengio"`) return standard
error (`se`), test statistic (`statistic`), p-value (`p.value`), and
confidence bounds (`conf_lower`, `conf_upper`). The `"quantile"` method
returns only lower and upper bounds.

###### `"raw"`: Uncorrected (!) t-test

Uses a standard t-test assuming independence of resampling iterations.

- SE = sd(resampling scores) / sqrt(n_iters)

- Test statistic: t = importance / SE with df = n_iters - 1

- P-value: From t-distribution (one-sided or two-sided depending on
  `alternative`)

- CIs: importance +/- qt(1 - alpha, df) \* SE

**Warning**: These CIs are too narrow because resampling iterations
share training data and are not independent. This method is included
only for demonstration purposes.

###### `"nadeau_bengio"`: Corrected t-test

Applies the Nadeau & Bengio (2003) correction to account for correlation
between resampling iterations due to overlapping training sets.

- Correction factor: (1/n_iters + n_test/n_train)

- SE = sqrt(correction_factor \* var(resampling scores))

- Test statistic and p-value: As in `"raw"`, but with corrected SE

Recommended with bootstrap or subsampling (\>= 10 iterations).

###### `"quantile"`: Non-parametric empirical method

Uses the resampling distribution directly without parametric
assumptions.

- CIs: Empirical quantiles of the resampling distribution

This method does not provide `se`, `statistic`, or `p.value`.

##### Method-Specific CI Methods

Some importance methods provide additional CI methods tailored to their
approach:

- **[CFI](https://mlr-org.github.io/xplainfi/reference/CFI.md)**: Adds
  `"cpi"` (Conditional Predictive Impact), which uses observation-wise
  loss differences with holdout resampling. Supports t-test, Wilcoxon,
  Fisher permutation, and binomial tests. See Watson & Wright (2021).

##### Practical Recommendations

Variance estimates for importance scores are biased due to the
resampling procedure. Molnar et al. (2023) suggest using the Nadeau &
Bengio correction with approximately 15 iterations of subsampling.

Bootstrapping can cause information leakage with learners that bootstrap
internally (e.g., Random Forests), as observations may appear in both
train and test sets. Prefer subsampling in such cases:

    PFI$new(
      task = sim_dgp_interactions(n = 1000),
      learner = lrn("regr.ranger", num.trees = 100),
      measure = msr("regr.mse"),
      resampling = rsmp("subsampling", repeats = 15),
      n_repeats = 20
    )

The `"nadeau_bengio"` correction was validated for PFI; its use with
other methods like LOCO or SAGE is experimental.

#### Returns

([data.table](https://rdrr.io/pkg/data.table/man/data.table.html))
Aggregated importance scores with columns `"feature"`, `"importance"`,
and depending on `ci_method` also `"se"`, `"statistic"`, `"p.value"`,
`"conf_lower"`, `"conf_upper"`.

------------------------------------------------------------------------

### Method `obs_loss()`

Calculate observation-wise importance scores.

Requires that `$compute()` was run and that `measure` is decomposable
and has an observation-wise loss (`Measure$obs_loss()`) associated with
it. This is not the case for measure like `classif.auc`, which is not
decomposable.

#### Usage

    FeatureImportanceMethod$obs_loss(relation = NULL)

#### Arguments

- `relation`:

  (character(1)) How to relate perturbed scores to originals
  ("difference" or "ratio"). If `NULL`, uses stored parameter value.
  This is only applicable for methods where importance is based on some
  relation between baseline and post-modification loss, i.e.
  [PerturbationImportance](https://mlr-org.github.io/xplainfi/reference/PerturbationImportance.md)
  methods such as
  [PFI](https://mlr-org.github.io/xplainfi/reference/PFI.md) or
  [WVIM](https://mlr-org.github.io/xplainfi/reference/WVIM.md) /
  [LOCO](https://mlr-org.github.io/xplainfi/reference/LOCO.md). Not
  available for
  [SAGE](https://mlr-org.github.io/xplainfi/reference/SAGE.md) methods.

#### Returns

([data.table](https://rdrr.io/pkg/data.table/man/data.table.html))
Observation-wise losses and importance scores with columns `"feature"`,
`"iter_rsmp"`, `"iter_repeat"` (if applicable), `"row_ids"`,
`"loss_baseline"`, `"loss_post"`, and `"obs_importance"`.

------------------------------------------------------------------------

### Method `reset()`

Resets all stored fields populated by `$compute`: `$resample_result`,
`$scores`, `$obs_losses`, and `$predictions`.

#### Usage

    FeatureImportanceMethod$reset()

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print importance scores

#### Usage

    FeatureImportanceMethod$print(...)

#### Arguments

- `...`:

  Passed to [`print()`](https://rdrr.io/r/base/print.html)

------------------------------------------------------------------------

### Method `scores()`

Calculate importance scores for each resampling iteration and
sub-iterations (`iter_rsmp` in
[PFI](https://mlr-org.github.io/xplainfi/reference/PFI.md) for example).

Iteration-wise importance are computed on the fly depending on the
chosen relation (`difference` or `ratio`) to avoid re-computation if
only a different relation is needed.

#### Usage

    FeatureImportanceMethod$scores(relation = NULL)

#### Arguments

- `relation`:

  (character(1)) How to relate perturbed scores to originals
  ("difference" or "ratio"). If `NULL`, uses stored parameter value.
  This is only applicable for methods where importance is based on some
  relation between baseline and post-modification loss, i.e.
  [PerturbationImportance](https://mlr-org.github.io/xplainfi/reference/PerturbationImportance.md)
  methods such as
  [PFI](https://mlr-org.github.io/xplainfi/reference/PFI.md) or
  [WVIM](https://mlr-org.github.io/xplainfi/reference/WVIM.md) /
  [LOCO](https://mlr-org.github.io/xplainfi/reference/LOCO.md). Not
  available for
  [SAGE](https://mlr-org.github.io/xplainfi/reference/SAGE.md) methods.

#### Returns

([data.table](https://rdrr.io/pkg/data.table/man/data.table.html))
Iteration-wise importance scores with columns for `"feature"`, iteration
indices, baseline and post-modification scores, and `"importance"`.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    FeatureImportanceMethod$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
