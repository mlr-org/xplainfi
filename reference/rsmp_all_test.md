# Create a resampling with all data being test data

Utility for use with a pretrained learner in importance methods which
support it

## Usage

``` r
rsmp_all_test(task)
```

## Arguments

- task:

  ([mlr3::Task](https://mlr3.mlr-org.com/reference/Task.html))

## Value

[mlr3::Resampling](https://mlr3.mlr-org.com/reference/Resampling.html)
with an empty `train_set` and a single `test_set` identical to all of
the given `Task`.

## Details

Note that the resulting Resampling will have an empty train set, making
it useless for any other purpose than the use with a pretrained learner.

## Examples

``` r
library(mlr3)
# Create custom task from some data.frame
custom_task <- as_task_regr(mtcars, target = "mpg")
# Create matching Resampling with all-test data
resampling_custom <- rsmp_all_test(custom_task)
```
