#' metrics
#'
#' This class implements methods to evaluate model performances
#' @importFrom R6 R6Class
#'
#' @export
metrics <- R6Class("metrics")
metrics$confusion_matrix <- function(ytrue, ypred) {
  return(table(ytrue, ypred))
}
metrics$accuracy_score <- function(ytrue, ypred) {
  conf_mat <- table(ytrue, ypred)
  return(sum(diag(conf_mat)) / sum(conf_mat))
}
metrics$recall_score <- function(ytrue, ypred) {
  conf_mat <- table(ytrue, ypred)
  return(diag(conf_mat / rowSums(conf_mat)))
}
metrics$precision_score <-  function(ytrue, ypred) {
  conf_mat <- table(ytrue, ypred)
  return(diag(conf_mat / colSums(conf_mat)))
}
