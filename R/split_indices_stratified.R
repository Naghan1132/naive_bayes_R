#' Title
#'
#' @param labels  A vector or factor containing the class labels
#' @param train_size The proportion of the dataset to include in the
#'                    train set. Should be a value between 0 and 1.
#'
#' @return A list containing the indexes of train samples
#'
split_indices_stratified <- function(labels, train_size) {

  unique_labels <- unique(labels)

  # Initialiser les indices de formation et de test
  train_indices <- list()

  for (label in unique_labels) {
    label_indices <- which(labels == label)
    shuffled_indices <- sample(label_indices)

    split_point <- floor(train_size * length(label_indices))

    train_indices[[label]] <- shuffled_indices[1:split_point]
  }

  return(list(train = unlist(train_indices)))
}
