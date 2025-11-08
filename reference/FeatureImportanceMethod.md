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
  [PFI](https://jemus42.github.io/xplainfi/reference/PFI.md) not one but
  the group of features will be permuted at each step. Analogusly in
  [WVIM](https://jemus42.github.io/xplainfi/reference/WVIM.md), each
  group of features will be left out (or in) for each model refit. Not
  all methods support groups (e.g.,
  [SAGE](https://jemus42.github.io/xplainfi/reference/SAGE.md)). See
  FIXME: vignette or examples.

- `param_set`:

  ([`paradox::ps()`](https://paradox.mlr-org.com/reference/ps.html))

- `predictions`:

  ([data.table](https://rdatatable.gitlab.io/data.table/reference/data.table.html))
  Feature-specific prediction objects provided for some methods
  ([PFI](https://jemus42.github.io/xplainfi/reference/PFI.md),
  [WVIM](https://jemus42.github.io/xplainfi/reference/WVIM.md)).
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
      measure,
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
([PerturbationImportance](https://jemus42.github.io/xplainfi/reference/PerturbationImportance.md)
or refits [LOCO](https://jemus42.github.io/xplainfi/reference/LOCO.md)).

#### Usage

    FeatureImportanceMethod$importance(
      relation = NULL,
      standardize = FALSE,
      ci_method = c("none", "raw", "nadeau_bengio", "quantile"),
      conf_level = 0.95,
      ...
    )

#### Arguments

- `relation`:

  (character(1)) How to relate perturbed scores to originals
  ("difference" or "ratio"). If `NULL`, uses stored parameter value.
  This is only applicable for methods where importance is based on some
  relation between baseline and post-modifcation loss, i.e.
  [PerturbationImportance](https://jemus42.github.io/xplainfi/reference/PerturbationImportance.md)
  methods such as
  [PFI](https://jemus42.github.io/xplainfi/reference/PFI.md) or
  [WVIM](https://jemus42.github.io/xplainfi/reference/WVIM.md) /
  [LOCO](https://jemus42.github.io/xplainfi/reference/LOCO.md). Not
  available for
  [SAGE](https://jemus42.github.io/xplainfi/reference/SAGE.md) methods.

- `standardize`:

  (`logical(1)`: `FALSE`) If `TRUE`, importances are standardized by the
  highest score so all scores fall in `[-1, 1]`.

- `ci_method`:

  (`character(1)`: `"none"`) Variance estimation method to use,
  defaulting to omitting variance estimation (`"none"`). If `"raw"`,
  uncorrected variance estimates are provided purely for informative
  purposes with **invalid** (too narrow) confidence intervals. If
  `"nadeau_bengio"`, variance correction is performed according to
  Nadeau & Bengio (2003) as suggested by Molnar et al. (2023). If
  `"quantile"`, empirical quantiles are used to construct
  confidence-like intervals. These methods are model-agnostic and rely
  on suitable `resampling`s, e.g. subsampling with 15 repeats for
  `"nadeau_bengio"`. See details.

- `conf_level`:

  (`numeric(1): 0.95`): Conficence level to use for confidence interval
  construction when `ci_method != "none"`.

- `...`:

  Additional arguments passen to specialized methods, if any.

#### Details

Variance estimates for importance scores are biased due to the
resampling procedure. Molnar et al. (2023) suggest to use the variance
correction factor proposed by Nadeau & Bengio (2003) of n2/n1, where n2
and n1 are the sizes of the test- and train set, respectively. This
should then be combined with approx. 15 iterations of either
bootstrapping or subsampling.

The use of bootstrapping in this context can lead to problematic
information leakage when combined with learners that perform
bootstrapping themselves, e.g., Random Forest learners. In such cases,
observations may be used as train- and test instances simultaneously,
leading to erroneous performance estimates.

An approach leading to still imperfect, but improved variance estimates
could be:

    PFI$new(
      task = sim_dgp_interactions(n = 1000),
      learner = lrn("regr.ranger", num.trees = 100),
      measure = msr("regr.mse"),
      # Subsampling instead of bootstrapping due to RF
      resampling = rsmp("subsampling", repeats = 15),
      n_repeats = 5
    )

`n_repeats = 5` in this context only improves the stability of the PFI
estimate within the resampling iteration, whereas
`rsmp("subsampling", repeats = 15)` is used to accounter for learner
variance and neccessitates variance correction factor.

This appraoch can in principle also be applied to `CFI` and `RFI`, but
beware that a conditional sample such as
[ConditionalARFSampler](https://jemus42.github.io/xplainfi/reference/ConditionalARFSampler.md)
also needs to be trained on data, which would need to be taken account
by the variance estimation method. Analogously, the `"nadeau_bengio"`
correction was recommended for the use with
[PFI](https://jemus42.github.io/xplainfi/reference/PFI.md) by Molnar et
al., so its use with other methods like
[LOCO](https://jemus42.github.io/xplainfi/reference/LOCO.md) or
[SAGE](https://jemus42.github.io/xplainfi/reference/SAGE.md) is
experimental.

Note that even if `measure` uses an `aggregator` function that is not
the mean, variance estimation currently will always use
[`mean()`](https://rdrr.io/r/base/mean.html) and
[`var()`](https://rdrr.io/r/stats/cor.html).

#### Returns

([data.table](https://rdatatable.gitlab.io/data.table/reference/data.table.html))
Aggregated importance scores. with variables `"feature", "importance"`
and depending in `ci_method` also `"se", "conf_lower", "conf_upper"`.

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
  relation between baseline and post-modifcation loss, i.e.
  [PerturbationImportance](https://jemus42.github.io/xplainfi/reference/PerturbationImportance.md)
  methods such as
  [PFI](https://jemus42.github.io/xplainfi/reference/PFI.md) or
  [WVIM](https://jemus42.github.io/xplainfi/reference/WVIM.md) /
  [LOCO](https://jemus42.github.io/xplainfi/reference/LOCO.md). Not
  available for
  [SAGE](https://jemus42.github.io/xplainfi/reference/SAGE.md) methods.

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
[PFI](https://jemus42.github.io/xplainfi/reference/PFI.md) for example).

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
  relation between baseline and post-modifcation loss, i.e.
  [PerturbationImportance](https://jemus42.github.io/xplainfi/reference/PerturbationImportance.md)
  methods such as
  [PFI](https://jemus42.github.io/xplainfi/reference/PFI.md) or
  [WVIM](https://jemus42.github.io/xplainfi/reference/WVIM.md) /
  [LOCO](https://jemus42.github.io/xplainfi/reference/LOCO.md). Not
  available for
  [SAGE](https://jemus42.github.io/xplainfi/reference/SAGE.md) methods.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    FeatureImportanceMethod$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
