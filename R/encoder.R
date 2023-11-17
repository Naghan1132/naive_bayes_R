#' naive_bayes
#'
#' This class encodes data
#'
#'
#'
#' @export
encoder <- R6Class("encoder",
                   public = list(
                     OneHotEncode = function(data){
                       for (var in colnames(data)) {
                         encoded <- model.matrix(~ . - 1, data = data[var])
                         colnames(encoded) <- paste0(var, "_", colnames(encoded))
                         data <- cbind(data, encoded)
                         data = data[,-which(colnames(data)==var)]
                       }
                       return(data)
                     },
                     LabelEncode = function(data){
                       encoded_df <- apply(data, 2,
                                           function(col){
                                             unique_values <- unique(col)
                                             encoding <- as.integer(factor(col, levels = unique_values))
                                             return(encoding)
                                           }
                       )
                       return(encoded_df)
                     }
                   )
)
