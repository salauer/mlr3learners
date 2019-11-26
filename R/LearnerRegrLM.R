#' @title Linear Model Regression Learner
#'
#' @usage NULL
#' @aliases mlr_learners_regr.lm
#' @format [R6::R6Class()] inheriting from [mlr3::LearnerRegr].
#'
#' @section Construction:
#' ```
#' LearnerRegrLM$new()
#' mlr3::mlr_learners$get("regr.lm")
#' mlr3::lrn("regr.lm")
#' ```
#'
#' @description
#' Ordinary linear regression. Calls [stats::lm()].
#'
#' The importance scores are calculated using `-log10(p)` where `p` is the \eqn{p}-value.
#' This transformation is necessary to ensure numerical stability for very small \eqn{p}-values.
#'
#' @export
#' @template seealso_learner
#' @templateVar learner_name regr.lm
#' @examples
#' library(mlr3)
#' learner = lrn("regr.lm")
#' print(learner)
#'
#' # available parameters:
#' learner$param_set$ids()
#'
#' task = tsk("mtcars")
#' learner$train(task)
#' summary(learner$model)
#'
#' pred = learner$predict(task, row_ids = head(task$row_ids))
#' pred
LearnerRegrLM = R6Class("LearnerRegrLM", inherit = LearnerRegr,
  public = list(
    initialize = function() {
      super$initialize(
        id = "regr.lm", ,
        predict_types = c("response", "se"),
        feature_types = c("integer", "numeric", "factor"),
        properties = c("weights", "importance"),
        packages = "stats",
        man = "mlr3learners::mlr_learners_regr.lm"
      )
    },

    train_internal = function(task) {
      pars = self$param_set$get_values(tags = "train")
      if ("weights" %in% task$properties) {
        pars = insert_named(pars, list(weights = task$weights$weight))
      }

      invoke(stats::lm, formula = task$formula(), data = task$data(), .args = pars)
    },

    predict_internal = function(task) {
      newdata = task$data(cols = task$feature_names)

      if (self$predict_type == "response") {
        PredictionRegr$new(task = task, response = predict(self$model, newdata = newdata, se.fit = FALSE))
      } else {
        pred = predict(self$model, newdata = newdata, se.fit = TRUE)
        PredictionRegr$new(task = task, response = pred$fit, se = pred$se.fit)
      }
    },

    importance = function() {
      if (is.null(self$model)) {
        stopf("No model stored")
      }

      p = summary(self$model)$coefficients[, 4L]
      p = p[names(p) != "(Intercept)"]
      sort(-log10(p), decreasing = TRUE)
    }
  )
)
