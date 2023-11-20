#' @title encoder - R6 Class for Categorical Data Encoding
#'
#' @description
#' This R6 class provides methods for one-hot encoding and label encoding
#' categorical data.
#' @importFrom R6 R6Class
#'
#' @export
#'
#' @examples
#' encoder_ <- encoder$new()
encoder <- R6Class("encoder",
  public = list(
    #' @description
    #' This function performs one-hot encoding on categorical data,
    #' converting categorical variables into binary vectors. Each unique
    #' category becomes a binary column in the resulting data frame.
    #'
    #' @param data A data frame containing categorical variables to be
    #' one-hot encoded.
    #'
    #' @return A data frame with the original quantitative columns and
    #'          additional columns representing the one-hot encoded categorical
    #'          variables.
    #'
    #' @examples
    #' data <- data.frame(category = c('A', 'B', 'A', 'C'))
    #' encoder_ <- encoder$new()
    #' encoded_data <- encoder_$OneHotEncode(data)
    OneHotEncode = function(data) {

      for (var in names(Filter(Negate(is.numeric), data))) {
        data <- cbind(data, model.matrix(~ . - 1, data = data[var]))
        data <- data[, -which(colnames(data) == var)]
      }
      return(data)
    },

    #' @description
    #' This function performs label encoding on categorical data, converting
    #' categorical variables into numeric labels. Each unique category is
    #' assigned a unique integer label in the resulting data frame.
    #'
    #' @param data A data frame containing categorical variables to be label
    #'              encoded.
    #'
    #' @return A data frame with the original columns replaced by numeric
    #'      labels for the categorical variables.
    #'
    #' @examples
    #' data <- data.frame(category = c('A', 'B', 'A', 'C'))
    #' encoder_ <- encoder$new()
    #' encoded_data <- encoder_$LabelEncode(data)
    LabelEncode = function(data) {
      return(
        sapply(data,
          function(col) if (is.numeric(col)) col else as.numeric(factor(col))
        )
      )
    }
  )
)
