#' train_test_split
#'
#' @param data a data.frame
#' @param train_size a number between 0 and 1 portion of number to include in train set
#' @param stratify a character the name of column to be use to strafied data
#' @param seed a number to fixed sample generator
#'
#' @return
#' @export
#'
#' @examples
#' train_test_split(iris, train_size = 0.7, stratify = "Species", seed = 123)
train_test_split = function(data, train_size = 0.7, stratify = NULL, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)

  # check if train_size in ]0;1[
  if(train_size > 1 || train_size < 0){
    stop("The train size must be a float between 0.0 and 1.0")
  }

  # check if stratity options is set and exist
  if (!is.null(stratify)) {
    if(is.null(data[[stratify]])){
      stop("The stratyfy column doesn't exist")
    }
    stratified_index = split_indices_stratified(data[[stratify]], train_size)
    train_index = unlist(stratified_index$train)
  } else {
    train_index = sample(1:nrow(data), size = floor(train_size * nrow(data)))
  }

  # split data
  train_set = data[train_index, ]
  test_set = data[-train_index, ]

  return(list(train_set = train_set, test_set = test_set))
}
