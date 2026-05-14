# xplainfi — agent context

R package for global feature importance measures via mlr3.

- GitHub: https://github.com/mlr-org/xplainfi
- GitHub repo for paper introducing the package: https://github.com/slds-lmu/paper_2025_xplainfi

## Working conventions

- **Git operations are the maintainer's responsibility.** Edit the
  working tree, describe the diff in chat, let the user stage and
  commit. Do not run `git add` / `git commit` / `git push` unless
  explicitly authorized.
- **Tabs for indentation.** Match existing file style.
- **`xplain_opt()`** is the package option mechanism (`R/utils-opt.R`).
  New options need a default + a row in the doc table.
- **`require_package()`** (`R/utils-pkg.R`) is the standard guard for
  Suggests packages — use it instead of bare `requireNamespace`.

## Code style

- `data.table` for tabular work; use `.SD` / `.SDcols` idioms.
- Avoid dplyr/tidyr in package code and vignettes to avoid additional
  soft dependencies via Suggests.
- Messaging via `cli::cli_inform()` / `cli::cli_abort()`. Never
  `cat()` for user-facing output.
- Prefer `glue::glue()` over `paste()`.
- `pak::pak()` for installs, `devtools::load_all()` for dev.
- mlr3 ecosystem for learners and tasks. The `default` in
  `paradox::ps()` is documentation-only.
- Max line width 120.
- Param docs: `` #' @param n_repeats (`integer(1)`: `1L`) Description. ``
- Files end with a newline.
- Comments explain *why*, not what. Preserve existing comments in
  files you edit unless they refer to code that no longer exists.

## Testing

- Fast learners like `rpart`, or `ranger` with 10 trees when a "good"
  model is required for plausibility.
- Tasks from `sim_dgp_*`, as simple / compact as the task allows
  (speed, memory).
- Classifier learners should use `predict_type = "prob"` by default.
- Cover regression + binary classification + multiclass.
- Set a seed before stochastic methods. Don't always use 1 or 42;
  seeds should be arbitrary.
- Use `expect_importance_dt()` helper for importance data.tables.
- Use specific `testthat` / `checkmate` expectations
  (`expect_gt()`, `checkmate::expect_numeric()` etc.) not generic
  `expect_true()`.
- After modifying code, may need `make install` before running tests.

## Theory / research context

xplainfi implements perturbation-based and retraining-based global
importance methods. The theoretical basis for the package's
terminology and method coverage is:

> Ewald, F. K., Bothmann, L., Wright, M. N., Bischl, B., Casalicchio,
> G., & König, G. (2024). *A Guide to Feature Importance Methods for
> Scientific Inference.* arXiv:2404.12862.

Statistical correctness and methodological soundness are highly valued
in this codebase.

Conceptual axes worth keeping in mind when touching method code:

- **Perturbation vs retraining.** PFI / CFI / RFI / SAGE perturb data
  for a fixed fitted model; LOCO / WVIM retrain a model with one or
  more features removed. The two families answer related but distinct
  questions.
- **Marginal vs conditional sampling.** PFI and the marginal SAGE
  variants sample features marginally; CFI, RFI, and conditional SAGE
  variants sample features conditional on the remaining feature set
  (or a user-specified subset).
- **PFI is perturbation-based, not just permutation-based.** The
  "permutation" label is historical; the family of methods is more
  general.

See `R/bibentries.R` (resolved via `print_bib()` in roxygen
`@references` blocks) for the full citation list.
