#' train_test_split
#'
#' This function splits the input data into train and test sets.
#'
#' @param data A data frame or matrix containing the dataset to be split.
#'
#' @param train_size The proportion of the dataset to include in the
#'                    train set. Should be a value between 0 and 1.
#'
#' @param stratify A character indicating the name of class labels for
#'                 stratified sampling. If provided, the split will
#'                 maintain the same distribution of class labels in
#'                 both the train and test sets.
#'
#' @param seed An optional seed for reproducibility. If NULL, no seed is set.
#'
#' @return A list containing the train and test sets.
#' @export
#'
#' @examples
#' data <- data.frame(
#'   features = rnorm(100),
#'   labels = sample(c('A', 'B'), 100, replace = TRUE)
#' )
#' split_data <- train_test_split(
#'   data,
#'   train_size = 0.7,
#'   stratify = "labels",
#'   seed = 123
#' )
#' print(head(split_data$train_set))
#' print(head(split_data$test_set))
#'
train_test_split <- function(
  data,
  train_size = 0.7,
  stratify = NULL,
  seed = NULL
) {
  if (!is.null(seed)) set.seed(seed)

  # check if train_size in ]0;1[
  if (train_size > 1 || train_size < 0) {
    stop("The train size must be a float between 0.0 and 1.0")
  }

  # check if stratity options is set and exist
  if (!is.null(stratify)) {
    if (is.null(data[[stratify]])) {
      stop("The stratyfy column doesn't exist")
    }
    stratified_index <- split_indices_stratified(data[[stratify]], train_size)
    train_index <- unlist(stratified_index$train)
  } else {
    train_index <- sample(
      seq_len(nrow(data)),
      size = floor(train_size * nrow(data))
    )
  }

  # split data
  train_set <- data[train_index, ]
  test_set <- data[-train_index, ]

  return(list(train_set = train_set, test_set = test_set))
}
