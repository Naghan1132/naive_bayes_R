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
    #' @field classes_ distinct classes
    classes_ = NULL,

    #' @field theta_ Mean of each feature per class
    theta_ = NULL,

    #' @field sigma_ Variance of each feature per class
    sigma_ = NULL,

    #' @field prior_ probability of each class
    prior_ = NULL,

    #' @field feature_importance Importance of each feature
    feature_importance = NULL,

    #' @field feature_names_in_ Names of features seen during fit
    feature_names_in_ = NULL,

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
      private$check_integrity(x,y)
      private$training_set <- cbind(head(x), "target" = head(y))

      # Encode data before fit
      x <- private$encode(x)

      d <- dim(x)
      private$n_samples <- d[1]
      private$n_features <- d[2]

      self$classes_ <- unique(y)
      self$feature_names_in_ <- colnames(x)

      # Calculate mean of each feature by class labels
      self$theta_ <- t(sapply(
        self$classes_,
        function(class_) colMeans(x[y == class_, ])
      ))
      # Calculate  variance of each feature by class labels
      self$sigma_ <- t(sapply(
        self$classes_,
        function(class_) apply(x[y == class_, ], 2, var)
      ))
      # Calculate class probability
      self$prior_ <- sapply(
        self$classes_,
        function(class_) sum(y == class_) / private$n_samples
      )
      # Calculate feature importance based on a model's parameters.
      self$feature_importance <- (diff(self$theta_))^2 / colSums(self$sigma_)
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
      private$check_integrity(x)
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
      colnames(ypred) <- self$classes_
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
      string_status <- ifelse(
        is.null(private$training_set),
        "Unfitted",
        "Fitted"
      )
      print(paste(string_status, "object of class <naive_bayes>"))
    },

    summary = function() {
      self$print()
      if (!is.null(private$training_set)) {
        print("Training set sample :")
        print(private$training_set)
        print("Prior probas : ")
        print(paste(self$classes_, ":", self$prior_))
        print("Conditional means : ")
        print(as.data.frame(self$theta_, row.names = self$classes_))
        print("Conditional variances : ")
        print(as.data.frame(self$sigma_, row.names = self$classes_))
      }
    }
  ),
  private = list(
    n_samples = NULL,
    n_features = NULL,
    encoder_ = NULL,
    training_set = NULL,

    encode = function(data) {
      # Do nothing if there is no categorical variables
      if (sum(sapply(data, is.numeric)) == ncol(data)) {
        return(data)
      }

      # Initialize encoder if it's not done yet
      if (is.null(private$encoder_)) {
        private$encoder_ <- one_hot_encoder$new()
      }

      if (!private$encoder_$is_fitted) {
        private$encoder_$fit(data)
      }

      return(private$encoder_$transform(data))
    },
    #' @description
    #' Checks if data inputs are valid.
    #'
    #' This function evaluates explainatory and target variables on several criteria,
    #' it's an intermediate function used in fit and predict
    #'
    #' @param X The explainatory data set, whether it's training or test.
    #' @param Y The target data, if NULL, will act as a check integrity for prediction,
    #'          default=NULL
    #'
    #' @return Raises errors if not valid else void.

    check_integrity = function(X,Y=NULL){
      if(!is.null(Y)){
        if(!is.data.frame(X)){
          stop("ERROR : Explainatory variable(s) must be stored in a DataFrame")
        }
        if(nrow(X) != length(Y)){
          stop("ERROR : Explainatory variable(s) and target variable must have the same length")
        }
        if(any(is.na(X)) || any(is.na(Y))){
          stop("ERROR : You can't have any NA values in your training data")
        }
        validTypesX <- all(sapply(X, function(x) class(x) %in% c("factor","numeric")))
        if(!validTypesX){
          stop("ERROR : The explainatory variable(s) can only have feature(s) of class factor, numeric or integer")
        }
        validTypeY <- is.factor(Y)
        if(!validTypeY){
          stop("ERROR : The explainatory variable(s) can only be of class a factor")
        }
        if(length(unique(Y))<2){
          stop("ERROR : You need to at least have 2 different classes in your target variable")
        }
        if("target" %in% colnames(X)){
          stop("ERROR : Xtrain can't have a column named target")
        }
      }
      else {
        if(!is.data.frame(X)){
          stop("ERROR : Explainatory variable(s) must be stored in a DataFrame")
        }
        if(any(is.na(X))){
          stop("ERROR : You can't have any NA values in your test data")
        }
        validTypesX <- all(sapply(X, function(x) class(x) %in% c("factor","numeric")))
        if(!validTypesX){
          stop("ERROR : The explainatory variable(s) can only have feature(s) of class factor, numeric or integer")
        }
        Xtrain <- private$training_set[,-which(colnames(private$training_set)=="target")]
        if (!setequal(names(X), names(Xtrain)) || !setequal(sapply(X, class), sapply(Xtrain, class))) {
          cat("ERROR : Test dataset should be of the same structure as dataset used in fit, here is the structure of the training set : \n")
          return(str(Xtrain))
        }
      }
    },
    #' @description
    #' Calculate the probability density of classifier.
    #'
    #' This function calculates the probability density for a given label and a
    #' vector of observations.
    #'
    #' @param label The label for which the probability density should be
    #'              calculated.
    #' @param x The vector of observations for which the probability density
    #'          should be calculated.
    #'
    #' @return The probability density for the given label and vector of
    #'         observations.
    prob = function(label, x) {

      mean <- self$theta_[label, ]
      var <- self$sigma_[label, ]
      numerator <- exp((-(x - mean)**2) / (2 * var))
      denominator <- sqrt(2 * pi * var)
      return(numerator / denominator)
    },

    #' Predict the class or probabilities for a given vector using a model.
    #'
    #' This function predicts the class or probabilities for a given vector
    #' using a trained model.
    #'
    #' @param x The input vector for prediction.
    #' @param with_prob Logical, indicating whether to return class
    #'                  probabilities. Default is FALSE.
    #'
    #' @return If with_prob is FALSE, the predicted class for the input vector.
    #' If with_prob is TRUE, a vector of class probabilities for each class.
    predict_ = function(x, with_prob = FALSE) {
      posteriors <- numeric(length(self$classes_))

      for (idx in seq_along(self$classes_)) {
        prior <- log(self$prior_[idx])

        class_conditional <- sum(log(private$prob(idx, x)))

        posterior <- prior + class_conditional
        posteriors[idx] <- posterior
      }
      if (with_prob) {
        return(metrics$softmax(posteriors))
      } else {
        return(self$classes_[which.max(posteriors)])
      }
    }
  )
)
