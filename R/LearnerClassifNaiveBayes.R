#' @title Naive Bayes Classification Learner
#'
#' @aliases mlr_learners_classif.naive_bayes
#' @format [R6::R6Class()] inheriting from [mlr3::LearnerClassif].
#'
#' @description
#' Naive Bayes classification.
#' Calls [e1071::naiveBayes()] from package \CRANpkg{e1071}.
#'
#' @export
LearnerClassifNaiveBayes = R6Class("LearnerClassifNaiveBayes", inherit = LearnerClassif,
  public = list(
    initialize = function(id = "classif.naive_bayes") {
      super$initialize(
        id = id,
        param_set = ParamSet$new(
          params = list(
            ParamDbl$new(id = "laplace", default = 0, lower = 0, tags = "train")
          )
        ),
        predict_types = c("response", "prob"),
        properties = c("twoclass", "multiclass"),
        feature_types = c("logical", "integer", "numeric", "factor"),
        packages = "e1071"
      )
    },

    train = function(task) {
      y = task$truth()
      x = task$data(cols = task$feature_names)
      self$model = invoke(e1071::naiveBayes, x = x, y = y, .args = self$params("train"))
      self
    },

    predict = function(task) {
      response = prob = NULL
      newdata = task$data(cols = task$feature_names)

      if (self$predict_type == "response")
        response = predict(self$model, newdata = newdata, type = "class")
      if (self$predict_type == "prob")
        prob = predict(self$model, newdata = newdata, type = "raw")
      PredictionClassif$new(task, response = response, prob = prob)
    }
  )
)