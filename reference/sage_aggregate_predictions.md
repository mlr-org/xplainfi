# Aggregate Predictions by Coalition and Test Instance

Averages predictions across multiple samples (reference data or
conditional samples) for each unique combination of coalition and test
instance.

## Usage

``` r
sage_aggregate_predictions(
  combined_data,
  predictions,
  task_type,
  class_names = NULL
)
```

## Arguments

- combined_data:

  (`data.table`) Data with columns `.coalition_id`, `.test_instance_id`,
  and feature columns.

- predictions:

  (`matrix` or `numeric`) For classification: matrix of class
  probabilities. For regression: numeric vector of predictions.

- task_type:

  (`character(1)`) Task type, either `"classif"` or `"regr"`.

- class_names:

  ([`character()`](https://rdrr.io/r/base/character.html) or `NULL`:
  `NULL`) Character vector of class names. Required for classification,
  ignored for regression.

## Value

`data.table` with columns:

- `.coalition_id`: Coalition identifier (integer)

- `.test_instance_id`: Test instance identifier (integer)

- For classification: One column per class with averaged probabilities
  (numeric)

- For regression: `avg_pred` column with averaged predictions (numeric)
