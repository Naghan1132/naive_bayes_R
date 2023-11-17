#' naive_bayes
#'
#' This class implements a supervised learning methods based on applying Bayes’
#' theorem with strong (naive) feature independence assumption
#' @importFrom R6 R6Class
#'
#' @export
naive_bayes <- R6Class("naive_bayes",
                       public = list(
                         fit = function(x, y) {
                           x_combined = private$preprocess(x)
                           d <- dim(x_combined)
                           private$n_samples <- d[1]
                           private$n_features <- d[2]
                           # Les différentes classes
                           private$classes <- unique(y)
                           # Le nombre de classes
                           n_classes <- length(private$classes)
                           private$mean_ <- t(sapply(
                             private$classes,
                             function(class_) colMeans(x_combined[y == class_, ])
                           ))
                           private$var_ <- t(sapply(
                             private$classes,
                             function(class_) apply(x_combined[y == class_, ], 2, var)
                           ))
                           private$prior_ <- sapply(
                             private$classes,
                             function(class_) sum(y == class_) / private$n_samples
                           )
                         },
                         predict = function(x) {
                           x_combined = private$preprocess(x)
                           ypred <- apply(x_combined, 1, function(row) private$predict_(row))
                           return(ypred)
                         },
                         predict_proba = function(x) {
                           x_combined = private$preprocess(x)
                           ypred <- apply(x_combined, 1, function(row) private$predict_(row, with_prob = TRUE))
                           return(ypred)
                         },
                         print = function(...) {
                           print("Moyenne : ")
                           print(private$mean_)
                           print("Ecart-type : ")
                           print(private$var_)
                         }
                       ),
                       private = list(
                         n_samples = NA,
                         n_features = NA,
                         classes = NA,
                         mean_ = NA,
                         var_ = NA,
                         prior_ = NA,
                         prob = function(label, x) {
                           mean <- private$mean_[label, ]
                           var <- private$var_[label, ]
                           numerator <- exp((-(x - mean)**2) / (2 * var))
                           denominator <- sqrt(2 * pi * var)
                           return(numerator / denominator)
                         },
                         encode = function(X){
                           cols_more_than_3_distinct <- which(sapply(X, function(col) {
                             if ((!is.numeric(col) && length(unique(col)) > 3)) {
                               return(TRUE)
                             } else {
                               return(FALSE)
                             }
                           }))

                           cols_3_or_fewer_distinct <- which(sapply(X, function(col) {
                             if (!is.numeric(col) && length(unique(col)) <= 3) {
                               return(TRUE)
                             } else {
                               return(FALSE)
                             }
                           }))
                           i=1
                           if(nrow(X)==1){
                             sapply(colnames(X),function(var){
                               colnames(X)[i] <<- paste0(var,"_",var,X[var])
                               X[1,i] <<- 1
                               i<<-i+1
                             })
                           }
                           else {
                             encoder = encoder$new()
                             dataToBeLabelEncoded = as.data.frame(X[,cols_more_than_3_distinct])
                             colnames(dataToBeLabelEncoded) = colnames(X)[cols_more_than_3_distinct]
                             label_encoded = encoder$LabelEncode(dataToBeLabelEncoded)
                             one_hot_encoded = encoder$OneHotEncode(X[,cols_3_or_fewer_distinct])
                             X[,cols_more_than_3_distinct] = label_encoded
                             if(length(cols_3_or_fewer_distinct)>0){
                               X = X[,-cols_3_or_fewer_distinct]
                               X = cbind(X,one_hot_encoded)
                             }
                           }
                           return(X)
                         },
                         preprocess = function(X){
                           x_cat_encoded = NULL
                           x_num = NULL
                           x_combined=NULL
                           if(length(which(sapply(X,function(x) is.factor(x))))>0){
                              x_cat_ind = sapply(X,function(x) is.factor(x))
                              x_cat_encoded = private$encode(X[,x_cat_ind])
                           }
                           if(length(which(sapply(X,function(x) !is.factor(x))))>0){
                              x_num_ind = sapply(X,function(x) !is.factor(x))
                              x_num = X[,x_num_ind]
                            if(length(which(sapply(X,function(x) !is.factor(x))))==1){
                              x_num = as.data.frame(x_num)
                              colnames(x_num)[1] = colnames(X)[which(x_num_ind)]
                            }
                           }
                           if (!is.null(x_num) && !is.null(x_cat_encoded)) {
                              x_combined = cbind(x_num,x_cat_encoded)
                           } else if (is.null(x_num)){
                             x_combined = x_cat_encoded
                           } else if (is.null(x_cat_encoded)){
                             x_combined = x_num
                           }
                           print(x_combined)
                           return(x_combined)
                         },
                         predict_ = function(x, with_prob = FALSE) {
                           posteriors <- numeric(length(private$classes))
                           for (idx in seq_along(private$classes)) {
                             prior <- log(private$prior_[idx])
                             class_conditional <- sum(log(private$prob(idx, x)))

                             posterior <- prior + class_conditional
                             posteriors[idx] <- posterior
                           }
                           if (with_prob) {
                             return(softmax(posteriors))
                           } else {
                             print(posteriors)
                             return(private$classes[which.max(posteriors)])
                           }
                         }
                       )
)
