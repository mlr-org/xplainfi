test_that("predict_raw matches predict_newdata for classif.log_reg", {
  skip_if_not_installed("mlr3learners")
  task = tsk("sonar")
  learner = lrn("classif.log_reg", predict_type = "prob")
  # sonar is linearly separable, glm warns about fitted probabilities of 0/1
  suppressWarnings(learner$train(task))
  newdata = task$data(cols = task$feature_names)

  expect_equal(
    predict_raw(learner, newdata, task)$prob,
    learner$predict_newdata(newdata)$prob
  )
})

test_that("PFI with classif.log_reg gives feature-specific importances", {
  skip_if_not_installed("mlr3learners")
  set.seed(1)
  task = tsk("sonar")
  pfi = PFI$new(
    task,
    lrn("classif.log_reg", predict_type = "prob"),
    measure = msr("classif.bbrier"),
    resampling = rsmp("holdout"),
    n_repeats = 1
  )
  suppressWarnings(pfi$compute())

  # with swapped prob columns every feature got the same (huge) importance
  expect_gt(uniqueN(pfi$importance()$importance), 1)
})
