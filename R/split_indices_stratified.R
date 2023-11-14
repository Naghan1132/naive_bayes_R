#' Title
#'
#' @param labels an array of labels to use to stratify data
#' @param train_size a number between 0 and 1 portion of number to include in train set
#'
#' @return indexes of train samples
#'
#' @examples
split_indices_stratified = function(labels, train_size) {

  unique_labels = unique(labels)

  # Initialiser les indices de formation et de test
  train_indices = list()

  for (label in unique_labels) {
    label_indices = which(labels == label)
    shuffled_indices = sample(label_indices)

    split_point = floor(train_size * length(label_indices))

    train_indices[[label]] = shuffled_indices[1:split_point]
  }

  return(list(train = unlist(train_indices)))
}
