#' @title naive_bayes - R6 Class for Naive Bayes Classification
#'
#' @description
#' This class implements a supervised learning methods based on applying Bayes’
#' theorem with strong (naive) feature independence assumption
#' @importFrom R6 R6Class
#'
#' @export
naive_bayes <- R6Class("naive_bayes",
  public = list(
    #' @field classes distinct classes
    classes = NULL,
    #' @field mean_ conditional mean
    mean_ = NULL,
    #' @field var_ conditional standard deviation
    var_ = NULL,
    #' @field prior_ propabality of each class
    prior_ = NULL,

    #' @description
    #' This method trains a Naive Bayes classifier on the given training data.
    #'
    #' @param x A data frame or matrix containing the feature variables.
    #' @param y A vector or factor containing the class labels.
    #'
    #' @return Invisibly returns the trained naive_bayes object.
    #'
    #' @examples
    #' data <- data.frame(
    #'   feature1 = c(1, 2, 3, 4),
    #'   feature2 = c(5, 6, 7, 8),
    #'   class = c('A', 'B', 'A', 'B')
    #' )
    #' classifier <- naive_bayes$new()
    #' classifier$fit(data[, c('feature1', 'feature2')], data$class)
    #'
    fit = function(x, y) {
      x <- private$encode(x)
      d <- dim(x)
      private$n_samples <- d[1]
      private$n_features <- d[2]
      self$classes <- unique(y)
      n_classes <- length(self$classes)

      self$mean_ <- t(sapply(
        self$classes,
        function(class_) colMeans(x[y == class_, ])
      ))
      self$var_ <- t(sapply(
        self$classes,
        function(class_) apply(x[y == class_, ], 2, var)
      ))
      self$prior_ <- sapply(
        self$classes,
        function(class_) sum(y == class_) / private$n_samples
      )
    },

    #' @description
    #' This method predicts class labels for new data using the trained Naive
    #' Bayes classifier.
    #'
    #' @param x A data frame or matrix containing the feature variables
    #'                 for which to predict class labels.
    #'
    #' @return A vector of predicted class labels.
    #'
    #' @examples
    #' new_data <- data.frame(feature1 = c(1, 2), feature2 = c(5, 6))
    #' ypred <- classifier$predict(new_data)
    #' print(ypred)
    #'
    predict = function(x) {
      x <- private$encode(x)
      ypred <- apply(x, 1, function(row) private$predict_(row))
      return(ypred)
    },

    #' @description
    #' This method predicts class probabilities for new data using the trained
    #' Naive Bayes classifier.
    #'
    #' @param x A data frame or matrix containing the feature variables
    #'                 for which to predict class probabilities.
    #'
    #' @return A data frame with columns representing class labels and their
    #'         corresponding probabilities for each observation in new_data.
    #'
    #' @examples
    #' new_data <- data.frame(feature1 = c(1, 2), feature2 = c(5, 6))
    #' probabilities <- classifier$predict_proba(new_data)
    #' print(probabilities)
    #'
    predict_proba = function(x) {
      x <- private$encode(x)
      ypred <- t(apply(x, 1, function(row) private$predict_(row, TRUE)))
      colnames(ypred) <- self$classes
      return(ypred)
    },

    #' @description
    #' This method prints a summary of the naive_bayes object,
    #' including information about the trained model.
    #'
    #' @param ... Additional parameters to be passed to the print method.
    #'
    #' @return Prints a summary of the naive_bayes object.
    #'
    #' @examples
    #' print(classifier)
    #'
    print = function(...) {
      print("Moyenne : ")
      print(self$mean_)
      print("Ecart-type : ")
      print(self$var_)
    }
  ),
  private = list(
    n_samples = NULL,
    n_features = NULL,
    encoder_ = NULL,

    encode = function(data) {
      # do nothing if there is no categorical variables
      if (sum(sapply(data, is.numeric)) == ncol(data)) {
        return(data)
      }
      # initialize encoder if it's not done yet
      if (is.null(private$encoder_)) {
        private$encoder_ <- encoder$new()
      }
      # get the indexes of categorical variables having more than 3 modalities
      cols_to_label_encode <- which(
        sapply(data,
          function(col) (!is.numeric(col) && length(unique(col)) > 3)
        )
      )
      # get the indexes of categorical variables having less than 3 modalities
      cols_to_onehot_encode <- which(
        sapply(data,
          function(col) (!is.numeric(col) && length(unique(col)) <= 3)
        )
      )
      # label encode if necessary
      if (length(cols_to_label_encode)) {
        encoded <- private$encoder_$LabelEncode(data[cols_to_label_encode])
        data <- data[, -cols_to_label_encode]
        data <- cbind(data, encoded)
      }
      # one-hot encode if necessary
      if (length(cols_to_onehot_encode)) {
        encoded <- private$encoder_$OneHotEncode(data[cols_to_onehot_encode])
        data <- data[, -cols_to_onehot_encode]
        data <- cbind(data, encoded)
      }
      return(data)
    },
    prob = function(label, x) {
      mean <- self$mean_[label, ]
      var <- self$var_[label, ]
      numerator <- exp((-(x - mean)**2) / (2 * var))
      denominator <- sqrt(2 * pi * var)
      return(numerator / denominator)
    },
    predict_ = function(x, with_prob = FALSE) {
      posteriors <- numeric(length(self$classes))

      for (idx in seq_along(self$classes)) {
        prior <- log(self$prior_[idx])

        class_conditional <- sum(log(private$prob(idx, x)))

        posterior <- prior + class_conditional
        posteriors[idx] <- posterior
      }
      if (with_prob) {
        return(metrics$softmax(posteriors))
      } else {
        return(self$classes[which.max(posteriors)])
      }
    }
  )
)
