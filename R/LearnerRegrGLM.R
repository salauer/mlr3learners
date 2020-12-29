#' @title Generalized Linear Model Regression Learner
#'
#' @name mlr_learners_regr.glm
#'
#' @description
#' Generalized linear regression.
#' Calls [stats::glm()].
#'
#' @templateVar id regr.glm
#' @template section_dictionary_learner
#'
#' @template section_contrasts
#'
#' @export
#' @template seealso_learner
#' @template example
LearnerRegrLM = R6Class(
  "LearnerRegrGLM",
  inherit = LearnerRegr,

  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ParamSet$new(list(
        ParamLgl$new("singular.ok", default = TRUE, tags = "train"),
        ParamLgl$new("x", default = FALSE, tags = "train"),
        ParamLgl$new("y", default = TRUE, tags = "train"),
        ParamLgl$new("model", default = TRUE, tags = "train"),
        ParamUty$new("etastart", tags = "train"),
        ParamUty$new("mustart", tags = "train"),
        ParamUty$new("start", default = NULL, tags = "train"),
        ParamUty$new("offset", tags = "train"),
        ParamFct$new("family", default = "poisson", levels = c("poisson", "quasipoisson", "Gamma", "inverse.gaussian"), tags = "train"),
        ParamDbl$new("epsilon", default = 1e-8, tags = c("train", "control")),
        ParamDbl$new("maxit", default = 25, tags = c("train", "control")),
        ParamLgl$new("trace", default = FALSE, tags = c("train", "control")),
        ParamUty$new("dispersion", default = NULL, tags = "predict"),
        ParamFct$new("type", default = "response", levels = c("response", "link", "terms"), tags = "predict")
      ))

      super$initialize(
        id = "regr.glm",
        param_set = ps,
        predict_types = c("response", "se.fit"),
        feature_types = c("logical", "integer", "numeric", "character", "factor", "ordered"),
        properties = "weights",
        packages = "stats",
        man = "mlr3learners::mlr_learners_regr.glm"
      )
    }
  ),

  private = list(
    .train = function(task) {
      pars = self$param_set$get_values(tags = "train")
      if ("weights" %in% task$properties) {
        pars = insert_named(pars, list(weights = task$weights$weight))
      }

      mlr3misc::invoke(stats::glm,
                       formula = task$formula(), data = task$data(),
                       .args = pars, .opts = opts_default_contrasts)
    },

    .predict = function(task) {
      newdata = task$data(cols = task$feature_names)
      pars = self$param_set$get_values(tags = "predict")

      if (self$predict_type == "response") {
        response = predict(self$model, newdata = newdata, se.fit = FALSE, .args = pars)
        list(response = response)
      } else {
        pred = predict(self$model, newdata = newdata, se.fit = TRUE, .args = pars)
        list(response = pred$fit, se = pred$se.fit)
      }
    }
  )
)
