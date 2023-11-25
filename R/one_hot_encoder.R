#' @title one_hot_encoder - R6 Class for Categorical Data Encoding.
#'
#' @description
#' This R6 class provides methods for one-hot encoding categorical data.
#'
#' @importFrom R6 R6Class
#'
#' @export
#'
#' @examples
#' encoder_ <- one_hot_encoder$new()
one_hot_encoder <- R6Class("one_hot_encoder",
  public = list(
    #' @description
    #' Method to train the encoder.
    #'
    #' @param data The data to be used for training.
    #'
    #' @return The trained OneHotEncoder object.
    #'
    fit = function(data) {
      private$columns <- names(Filter(Negate(is.numeric), data))
      for (col in private$columns) {
        private$unique_values[[as.character(col)]] <- unique(data[, col])
      }
      private$fitted <- TRUE
    },

    #' @description
    #' Method to perform one-hot encoding of data.
    #'
    #' @param data The data to be transformed.
    #' @return The encoded data.
    transform = function(data) {

      if (!private$check_unique_values(data)) {
        stop("Unseen modal in XTest comapared to XTrain")
      }
      for (var in private$columns) {
        encoded_data <- model.matrix(~ . - 1, data = data[var])
        data <- cbind(data, encoded_data[, sort(colnames(encoded_data))])
        data <- data[, -which(colnames(data) == var)]
      }
      return(data)
    },
    #' @description
    #' Method to check if the encoder is trained.
    #'
    #' @return TRUE if the encoder is trained, FALSE otherwise.
    is_fitted = function() {
      return(private$fitted)
    }
  ),
  private = list(
    columns = NULL,
    unique_values = NULL,
    fitted = FALSE,

    check_unique_values = function(data) {
      is_valid <- TRUE
      for (col in private$columns){
        if (!setequal(
          unique(data[, col]),
          private$unique_values[[as.character(col)]]
        )) {
          is_valid <- FALSE
          return(is_valid)
        }
      }
      return(is_valid)
    }
  )
)
