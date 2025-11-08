# Create Feature Selection Design Matrix

Creates a logical design matrix for leave-in or leave-out feature
evaluation. Used internally with mlr3fselect to evaluate feature
subsets.

## Usage

``` r
wvim_design_matrix(
  all_features,
  feature_names = all_features,
  direction = c("leave-out", "leave-in")
)
```

## Arguments

- all_features:

  (character()) All available feature names from the task.

- feature_names:

  (character() \| list of character()) Features or feature groups to
  evaluate. Can be a vector for individual features or a named list for
  grouped features. Defaults to `all_features` if unspecified.

- direction:

  (character(1)) Either `"leave-in"` or `"leave-out"` (default).
  Controls which features are selected in the design matrix.
  `"leave-out"` sets features of interest to `FALSE`, and `"leave-in"`
  analogously sets them to `TRUE`.

## Value

data.table with logical columns for each feature in `all_features` and
`length(feature_names)` rows, one for each entry in `feature_names`

## Examples

``` r
task = mlr3::tsk("mtcars")

# Individual features
feature_names = task$feature_names[1:3]
wvim_design_matrix(task$feature_names, feature_names, "leave-in")
#>        am   carb    cyl   disp   drat   gear     hp   qsec     vs     wt
#>    <lgcl> <lgcl> <lgcl> <lgcl> <lgcl> <lgcl> <lgcl> <lgcl> <lgcl> <lgcl>
#> 1:   TRUE  FALSE  FALSE  FALSE  FALSE  FALSE  FALSE  FALSE  FALSE  FALSE
#> 2:  FALSE   TRUE  FALSE  FALSE  FALSE  FALSE  FALSE  FALSE  FALSE  FALSE
#> 3:  FALSE  FALSE   TRUE  FALSE  FALSE  FALSE  FALSE  FALSE  FALSE  FALSE
wvim_design_matrix(task$feature_names, feature_names, "leave-out")
#>        am   carb    cyl   disp   drat   gear     hp   qsec     vs     wt
#>    <lgcl> <lgcl> <lgcl> <lgcl> <lgcl> <lgcl> <lgcl> <lgcl> <lgcl> <lgcl>
#> 1:  FALSE   TRUE   TRUE   TRUE   TRUE   TRUE   TRUE   TRUE   TRUE   TRUE
#> 2:   TRUE  FALSE   TRUE   TRUE   TRUE   TRUE   TRUE   TRUE   TRUE   TRUE
#> 3:   TRUE   TRUE  FALSE   TRUE   TRUE   TRUE   TRUE   TRUE   TRUE   TRUE

# Feature groups
feature_groups = list(
  A = task$feature_names[1:2],
  B = task$feature_names[3:5]
)
wvim_design_matrix(task$feature_names, feature_groups, "leave-out")
#>        am   carb    cyl   disp   drat   gear     hp   qsec     vs     wt
#>    <lgcl> <lgcl> <lgcl> <lgcl> <lgcl> <lgcl> <lgcl> <lgcl> <lgcl> <lgcl>
#> 1:  FALSE  FALSE   TRUE   TRUE   TRUE   TRUE   TRUE   TRUE   TRUE   TRUE
#> 2:   TRUE   TRUE  FALSE  FALSE  FALSE   TRUE   TRUE   TRUE   TRUE   TRUE
```
