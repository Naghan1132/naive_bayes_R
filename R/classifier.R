# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'





# df = read.csv("/home/nathan/fac/m2/s1/R/package_R/titanic.csv")
# classifier = Classifier$new(df,"Survived","gaussian")




#' Une classe R6 pour un classificateur simple.
#'
#' Cette classe simule un classificateur avec des méthodes d'entrainement et de prédiction.
#' @importFrom R6 R6Class
#' @importFrom caret createDataPartition
#' @importFrom caret confusionMatrix
#' @export
Classifier <- R6Class("Classifier",
                      public = list(
                        df = NULL,
                        name_y = NULL,
                        method = NULL,

                        # Constructeur de la classe
                        initialize = function(df,name_y,method) {
                          if(!private$df_is_ok(df,name_y)){
                            stop("error")
                          }
                          self$df <- self$preprocessing(df)
                          self$method <- method
                          self$name_y <- name_y
                        },

                        preprocessing = function(df){

                          print(sum(is.na(df)))

                          # fill NaN
                          colonnes_quanti <- sapply(df, is.numeric)
                          colonnes_quali <- sapply(df, is.factor)

                          # Remplacer les NAs dans les colonnes quantitatives par la moyenne de chaque colonne
                          for (colonne in names(df)[colonnes_quanti]) {
                            mean_value <- mean(df[[colonne]], na.rm = TRUE)
                            df[[colonne]][is.na(df[[colonne]])] <- mean_value
                          }
                          # Remplacer les NAs dans les colonnes qualitatives par le mode de chaque colonne
                          for (colonne in names(data)[colonnes_quali]) {
                            mode_value <- as.character(names(sort(table(data[[colonne]]), decreasing = TRUE)[1]))
                            data[[colonne]][is.na(data[[colonne]])] <- mode_value
                          }

                          print(sum(is.na(df)))


                          return(df)
                        },

                        test = function(){

                          set.seed(41)
                          index <- createDataPartition(self$df[[self$name_y]], p = 0.7, list = FALSE)
                          train <- self$df[index, ]
                          test <- self$df[-index, ]

                          X_test <- test[, setdiff(names(test),self$name_y)]
                          Y_test <- test[[self$name_y]]


                          Y_pred <- self$naive_bayes_gaussian(train,X=X_test,Y=self$name_y)
                          print("Y_pred : ")
                          print(Y_pred)
                          vecteur_facteur <- as.factor(Y_pred)

                          print("Y_test : ")
                          print(Y_test)

                          conf_matrix <- confusionMatrix(data = factor(Y_pred, levels = levels(vecteur_facteur)), reference = factor(Y_test, levels = levels(Y_test)))

                          # Afficher la matrice de confusion
                          print(conf_matrix)
                          # Obtenir le score F1
                          f1_score <- conf_matrix$byClass['F1']
                          # Afficher le score F1
                          print(f1_score)

                        },

                        # Fonction pour calculer la prédiction Naive Bayes avec distribution gaussienne
                        naive_bayes_gaussian = function(train_df, X_test, Y) {
                          # Obtenir les noms des caractéristiques
                          features <- names(train_df)[-which(names(train_df) %in% Y)]

                          # Calculer la probabilité a priori
                          prior <- private$calculate_prior(train_df, Y)

                          # Prédictions
                          Y_pred <- numeric(nrow(X_test))


                          # Boucler sur chaque échantillon de données
                          for (i in 1:nrow(X_test)) {
                            # Calculer la vraisemblance
                            labels <- as.character(sort(unique(train_df[[Y]])))


                            likelihood <- rep(1, length(labels))

                            for (j in 1:length(labels)) {
                              for (k in 1:length(features)) {
                                likelihood[j] <- likelihood[j] * private$calculate_likelihood_gaussian(train_df, features[k], X_test[i, k], Y, labels[j])
                              }
                            }

                            # Calculer la probabilité a posteriori (numérateur seulement)
                            post_prob <- rep(1, length(labels))
                            for (j in 1:length(labels)) {
                              post_prob[j] <- likelihood[j] * prior[j]
                            }

                            # Prédiction : classe avec la probabilité la plus élevée
                            max_prob <- which.max(post_prob)
                            Y_pred[i] <- labels[max_prob]
                          }

                          return(Y_pred)
                        },

                        fit = function(X,y){
                          #####
                        },



                        # Méthode pour simuler la prédiction
                        predict = function(X) {

                        },

                        predict_proba = function(X) {
                        # qui renvoie les probabilités d’appartenance aux classes
                        },

                        summary = function() {
                          print(self$model)
                        }
                      ),

                      private = list(

                        df_is_ok = function(df,name_y){
                          if (name_y %in% names(df)) {
                            num_vars <- length(names(df)) - 1  # Soustraire 1 pour exclure la variable cible
                            if (num_vars >= 1) {
                              df[[name_y]] <- as.factor(df[[name_y]])
                              if(length(levels(df[[name_y]])) >= 2){
                                print(levels(df[[name_y]]))
                                return(TRUE)
                              }else{
                                return(FALSE)
                              }
                            }else{
                              return(FALSE)
                            }
                          }else{
                            return(FALSE)
                          }
                        },

                        calculate_prior = function(df,Y){
                          # calculate P(Y=y) for all possible y
                          classes <- unique(df[[Y]])
                          prior <- numeric(length(classes))
                          for (i in 1:length(classes)) {
                            prior[i] <- sum(df[[Y]] == classes[i]) / nrow(df)
                          }
                          return(prior)
                        },

                        calculate_likelihood_gaussian = function(df, feat_name, feat_val, Y, label) {
                          # Calculate P(X=x | Y=y) using gaussian dist
                          df <- df[df[[Y]] == label, ]  # Filtrer les lignes où Y est égal à label

                          mean_val <- mean(df[[feat_name]])  # Calculer la moyenne
                          std_dev <- sd(df[[feat_name]])     # Calculer l'écart-type

                          # Calculer la densité de probabilité pour une distribution normale (loi gaussienne)
                          p_x_given_y <- (1 / (sqrt(2 * pi) * std_dev)) * exp(-((feat_val - mean_val)^2) / (2 * std_dev^2))

                          return(p_x_given_y)
                        }
                      )
)
