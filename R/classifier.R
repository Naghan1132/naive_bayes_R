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


detection <- function(df,name_v_cible){
  if (name_v_cible %in% names(df)) {
    num_vars <- length(names(df)) - 1  # Soustraire 1 pour exclure la variable cible
    if (num_vars >= 1) {
      df[[name_v_cible]] <- as.factor(df[[name_v_cible]])
      if(length(levels(df[[name_v_cible]])) >= 2){
        print(levels(df[[name_v_cible]]))
        return(TRUE)
      }
    }
  }else{
    return(FALSE)
  }

}

#' Classi
#'
#' @param df a dataframe
#' @param name_v_cible STRING variable cible
#' @return naive bayes
#' @export
classi <- function(df,name_v_cible){
  if(detection(df,name_v_cible)){
    # on peut passer aux calculs
    print("check OK")
    fit <- function(df){

    }
  }else{
    return(FALSE)
  }
}


#' Une classe R6 pour un classificateur simple.
#'
#' Cette classe simule un classificateur avec des méthodes d'entrainement et de prédiction.
#' @importFrom R6 R6Class
#' @export
Classifier <- R6Class("Classifier",
                      public = list(
                        model = NULL,
                        # Constructeur de la classe
                        initialize = function() {
                          self$model <- "truc"
                        },

                        # Méthode pour simuler l'entrainement du modèle
                        entrainer = function() {
                          cat("Entrainement du modele \n")
                          # Ici, vous pouvez simuler l'entrainement d'un modèle réel
                          private$model <- "Modele entraine"
                        },

                        # Méthode pour simuler la prédiction
                        predict = function() {
                          if (is.null(private$model)) {
                            stop("pas entraine")
                          } else {
                            cat("prediction")
                            # Ici, vous pouvez simuler la prédiction d'un résultat réel
                          }
                        },
                        summary = function() {
                          print(self$model)
                        }
                      )
)
