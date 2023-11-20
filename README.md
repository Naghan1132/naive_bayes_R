# NaiveBayes
# Description

Here are some different features of our package that we will present to you below:
* train_test_split
* encoder
  * OneHotEncode
  * LabelEncode
* naive_bayes
  * fit
  * predict
  * predict_proba
  * print
* metrics
  * confusion_matrix
  * accuracy_score
  * recall_score
  * precision_score
  * softmax

---

## 1. Installation and loading

In order to use our package, you should install it from Github.
  
  **1.1 Install and load `devtools`**

  ```R
  install.packages("devtools")
  ```
  ```R
  library(devtools)
  ```

  **1.2 Install an load our package `NaiveBayes`**

  ```R
  install_github("Naghan1132/naive_bayes_R/")
  ```
  ```R
  library(NaiveBayes)
  ```

## 2. Documentation
  To access the complete documentation for this package, use the help functions built into R.

  You can get help on any class or function in the package by using the help() function in the R console. For example:

  ```R
  help("naive_bayes")
  ```
  Another way to get help is to use the ? symbol followed by the function name. For example:

  ```R
  ?naive_bayes
  ```

## 3. Use
  Below is a use of the `NaiveBayes` package with the **iris** dataset (150 observations, 4 explanatory variables and 1 target variable)

  ```R
  # load iris dataset
  data("iris")
  ```
  ### 3.1 train_test_split
  The `train_test_split` function takes a data frame as input and returns two datasets (a training dataset and a test dataset). As a parameter you can enter:
  - The proportional size of the training dataset `train_size`.
  - The name of the variable to use to `stratify` the split (the target variable). This ensures that the distribution of classes of this given variable in the training set is similar to that in the testing set.
  - The `seed` that ensures that the split results will be consistent each time the code runs.

  ```R
  sets = train_test_split(iris, train_size = 0.7, stratify = 'Species', seed = 123)
  ```

  - The train set
  ```R
  # 5 is the index of target variable Species
  Xtrain = sets$train_set[-5]
  ytrain = sets$train_set[[5]]
  ```
  - The test set

  ```R
  Xtest = sets$test_set[-5]
  ytest = sets$test_set[[5]]
  ```
  ![Train test split](screenshots/train-testsplit.png)

  ### 3.2 Naive bayes classifier
  Pour utiliser le classifier vous devez instancier la classe `naive_bayes`.
  ```R
  model = naive_bayes$new()
  ```

  Pour entrainer le model sur le jeu d'apprentissage vous devez utiliser la methode `fit` de la classe `naive_bayes`.
  ```R
  model <- model$fit(Xtrain, ytrain)
  ```

  Vous pouvez ensuite effectuez une prediction sur l'ensemble de test
  ```R
  ypred = model$predict(Xtest)
  ```
  ![Prediction ](screenshots/predict.png)

  Vous pouvez aussi obtenir les probabilités associées à chaque classe
  ```R
  probas = model$predict_proba(Xtest)
  ```
  ![Classes probabilities](screenshots/predict-proba.png)

  ### 3.3 Evaluation
  Il y'a un ensembe de fonctions disponible dans la classe `metrics` pour evaluer les performances de votre modèle.
  
  #### 3.3.1 confusion_matrix
  ```R
  metrics$confusion_matrix(ytest, ypred)
  ```
  #### 3.3.2 accuracy_score
  ```R
  metrics$accuracy_score(ytest, ypred)
  ```
  #### 3.3.3 recall_score
  ```R
  metrics$recall_score(ytest, ypred)
  ```
  #### 3.3.4 precision_score
  ```R
  metrics$precision_score(ytest, ypred)
  ```
  ![Some metrics](screenshots/metrics.png)

  ### 3.4 encoder
  Le package comporte egalement la classe `encoder` qui permet d'effectuer deux types d'encodage. Pour l'utiliser vous devez l'instancier
  ```R
  encoder_ = encoder$new()
  ```
  #### 3.2.1 OneHotEncode
  ```R
  encoder_$OneHotEncode(iris)
  ```
  ![One hot encode](screenshots/one-hot-encode.png)

  #### 3.2.2 LabelEncode
  ```R
  encoder_$LabelEncode(iris)
  ```
  ![Label encode](screenshots/label-encode.png)

  ### 3.3 naive_bayes
   #### 3.3.1 encode
   #### 3.3.5 print
   #### 3.3.6 prob
   #### 3.3.7 predict_