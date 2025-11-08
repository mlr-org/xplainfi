# Batch Predict for SAGE

Performs batched prediction on combined data to manage memory usage.
Supports both classification (probability predictions) and regression.

## Usage

``` r
sage_batch_predict(learner, combined_data, task, batch_size, task_type)
```

## Arguments

- learner:

  ([`Learner`](https://mlr3.mlr-org.com/reference/Learner.html)) Trained
  mlr3 learner.

- combined_data:

  (`data.table`) Data with feature columns to predict on.

- task:

  ([`Task`](https://mlr3.mlr-org.com/reference/Task.html)) mlr3 task
  object.

- batch_size:

  (`integer(1)` or `NULL`) Batch size for predictions. If `NULL` or if
  `total_rows <= batch_size`, processes all data at once.

- task_type:

  (`character(1)`) Task type, either `"classif"` or `"regr"`.

## Value

For classification: `matrix` of class probabilities (n_rows x
n_classes). For regression: `numeric` vector of predictions (length
n_rows).
