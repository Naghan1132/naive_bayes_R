#' NaiveBayesClassifier
#'
#' This class implements a supervised learning methods based on applying Bayes’ theorem with strong (naive) feature independence assumption
#' @importFrom R6 R6Class
#' @export
NaiveBayesClassifier = R6Class('NaiveBayesClassifier',
                     public = list(
                       n_samples = NA,
                       n_features = NA,
                       classes = NA,
                       mean_ = NA,
                       var_ = NA,
                       prior_ = NA,

                       #' fit
                       #' #'
                       #' @param x features
                       #' @param y target
                       fit = function(x, y){
                         d = dim(x)
                         self$n_samples = d[1]
                         self$n_features = d[2]
                         # Les différentes classes
                         self$classes = unique(y)
                         # Le nombre de classes
                         n_classes = length(self$classes)

                         self$mean_ = t(sapply(self$classes, function(class_) colMeans(x[y==class_, ])))
                         self$var_ = t(sapply(self$classes, function(class_) apply(x[y==class_, ], 2, var)))
                         self$prior_ = sapply(self$classes, function(class_) sum(y == class_) / self$n_samples)
                       },
                       prob = function(label, x){
                         mean = self$mean_[label,]
                         var = self$var_[label,]
                         numerator = exp((-(x - mean)**2) / (2 * var))
                         denominator = sqrt(2 * pi * var)
                         return(numerator / denominator)
                       },
                       predict_ = function(x, with_prob=FALSE){
                         posteriors = numeric(length(self$classes))

                         for (idx in seq_along(self$classes)) {
                           prior = log(self$prior_[idx])

                           class_conditional = sum(log(self$prob(idx, x)))

                           posterior = prior + class_conditional
                           posteriors[idx] = posterior
                         }
                         if(with_prob){
                           return(softmax(posteriors))
                         } else {
                           return(self$classes[which.max(posteriors)])
                         }
                       },
                       predict = function(x){
                         ypred = apply(x, 1, function(row) self$predict_(row))
                         return(ypred)
                       },
                       predict_proba = function(x){
                         ypred = apply(x, 1, function(row) self$predict_(row, with_prob = TRUE))
                         return(ypred)
                       },
                       print = function(...){
                         print("Moyenne : ")
                         print(self$mean_)
                         print("Ecart-type : ")
                         print(self$var_)
                       }
                     )
)
