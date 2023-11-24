one_hot_encoder <- R6Class("one_hot_encoder",
  public = list(
    is_fitted = FALSE,
    fit = function(data) {
      private$columns <- names(Filter(Negate(is.numeric), data))
      for (col in private$columns) {
        private$unique_values[[as.character(col)]] <- unique(data[, col])
      }
      self$is_fitted <- TRUE
    },
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
    }
  ),
  private = list(
    columns = NULL,
    unique_values = NULL,
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
