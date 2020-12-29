library(mlr3learners)

test_that("regr.glm", {
  learner = mlr3::lrn("regr.glm")
  fun = stats::glm
  exclude = c(
    "formula", # handled via mlr3
    "data", # handled via mlr3
    "weights", # handled via mlr3
    "na.action", # handled via mlr3
    "method", # handled via mlr3
    "subset", # handled via mlr3
    "contrasts", # handled via mlr3
    "family"
  )

  ParamTest = run_paramtest(learner, fun, exclude)
  expect_true(ParamTest, info = paste0(
    "Missing parameters:",
    paste0("- '", ParamTest$missing, "'", collapse = ",")))
})

test_that("predict regr.glm", {
  learner = lrn("regr.glm")
  fun = stats::predict.glm
  exclude = c(
    "object", # handled via mlr3
    "newdata", # handled via mlr3
    "type", # handled via mlr3
    "na.action", # handled via mlr3
    "terms", # not supported by mlr3 learner
    "weights" # handled via mlr3
  )

  ParamTest = run_paramtest(learner, fun, exclude)
  expect_true(ParamTest, info = paste0(
    "Missing parameters:",
    paste0("- '", ParamTest$missing, "'", collapse = ",")))
})
