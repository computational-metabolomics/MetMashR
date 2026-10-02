#' Apply method
#'
#' Applies method to the input DatasetExperiment. Some models also provide
#' separate `model_train` and `model_predict` methods.
#' @usage
#' model_apply(M, D, ...)
#' model_train(M, D, ...)
#' model_predict(M, D, ...)
#' @param M a method object
#' @param D another object used by the first
#' @param ... additional inputs (not used)
#' @return Returns a modified method object
#' @examples
#' M <- example_model()
#' M <- model_apply(M, iris_DatasetExperiment())
#' @aliases model_train model_predict
#' @name model_apply
NULL
