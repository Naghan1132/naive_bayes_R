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
      n_zeros <- as.data.frame(rep(0, nrow(data)))
      encoded_data <- NULL
      for (col in private$columns) {

        # If feature has more than one value
        if (length(unique(data[, col])) > 1) {
          encoded_data <- model.matrix(~ . - 1, data = data[col])
        } else {
          # Create column with 1
          var <- unique(data[, col])[1]
          col_name <- paste(col, var, sep = "")
          if (!is.null(encoded_data)) {
            encoded_data <- cbind(
              encoded_data,
              setNames(as.data.frame(rep(1, nrow(data))), col_name)
            )

          } else {
            encoded_data <- setNames(
              as.data.frame(rep(1, nrow(data))),
              col_name
            )
          }
        }

        # Binding existing modals that are in the fit but not in the
        # transform as a column with "[var][modal]" name format filled with 0
        for (var in setdiff(
          private$unique_values[[as.character(col)]],
          unique(data[, col])
        )) {
          col_name <- paste(col, var, sep = "")
          encoded_data <- cbind(encoded_data, setNames(n_zeros, col_name))
        }

        # Bindind encoded columns to data
        data <- cbind(data, encoded_data[, sort(colnames(encoded_data))])

        # Deleting original categorical column
        data <- data[, -which(colnames(data) == col)]
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
        # Checking new label 
        if (length(setdiff(
          unique(data[, col]),
          private$unique_values[[as.character(col)]]
        )) > 0) {
          is_valid <- FALSE
          return(is_valid)
        }
      }
      return(is_valid)
    }
  )
)
