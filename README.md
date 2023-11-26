# NaiveBayes
# Description

This package was designed as part of an academic project at the Université Lumière Lyon 2. The objective was to develop an R package following the R6 standard which implements a naive Bayesian classification.<br>

The naive Bayesian classifier is a probabilistic classification method based on Bayes' theorem. The model assumes conditional independence between features, which simplifies calculations and allows rapid classification. Despite its simplicity, the naive Bayesian classifier is powerful and efficient.

# Key Features
Here are some different features of our package that we will present to you below.

* **Model training** <br>
  The package allow users to train the model by providing a training dataset with corresponding features and class labels.
* **Prediction**
  Once the model is trained, users will be able to make predictions on new data by providing the features, and the package will return the associated class predictions or probability of each class.
* **Categorical data handling**<br>
  The package supports data mixing and provides tools for encoding categorical variables.
* **Performance evaluation** <br>
  The package provide tools to evaluate the performance of the model, including accuracy, precision, recall and F1-measure.
* **Documentation** <br>
  Detailed documentation, including usage examples and explanations of settings, is be available to help users get the most out of the package.

We have developed an r-shiny application which allows you to test the different functionalities of the package.

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
  install_github("Naghan1132/naive_bayes_R")
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
  sets <- train_test_split(iris, train_size = 0.7, stratify = 'Species', seed <- 123)
  ```

  - The train set
  ```R
  # 5 is the index of target variable Species
  Xtrain <- sets$train_set[-5]
  ytrain <- sets$train_set[[5]]
  ```
  - The test set

  ```R
  Xtest <- sets$test_set[-5]
  ytest <- sets$test_set[[5]]
  ```
  ![Train test split](https://github.com/Naghan1132/naive_bayes_R/assets/75121872/50117d37-c0f4-40bf-80a7-d80bcd8811c1)

  ### 3.2 Naive bayes classifier
  To use the classifier you must instantiate the `naive_bayes` class.
  ```R
  model <- naive_bayes$new()
  ```
  You can use also use the classifier in parallel mode by specifying `multi_thread` and the number of CPU to use `n_cluster`.
  
  ```R
  model <- naive_bayes$new(multi_thread = TRUE, n_cluster=2)
  ```


  To train the model on the training game you must use the `fit` method of the `naive_bayes` class.
  ```R
  model$fit(Xtrain, ytrain)
  ```

  You can then perform a prediction on the test set
  ```R
  ypred <- model$predict(Xtest)
  ```
  ![Prediction ](http://www.image-heberg.fr/files/1700777629603041913.png)

  You can also get the probabilities associated with each class
  ```R
  probas <- model$predict_proba(Xtest)
  ```
  ![Classes probabilities](http://www.image-heberg.fr/files/17007776202038246646.png)

  ### 3.3 Evaluation
  There is a set of functions available in the `metrics` class to evaluate the performance of your model.
  
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
  ![Some metrics](http://www.image-heberg.fr/files/17007776043584823103.png)

  ### 3.4 one_hot_encode
  The package also includes the encoder class `one_hot_encode` which allows you to perform an one-hot of encoding
  
  Create an instance of the One-Hot Encoder
  ```R
  encoder_ <- one_hot_encoder$new()
  ```
  Fit the encoder to your data
  ```R
  encoder_$fit(iris)
  ```
  Transform your data using the fitted encoder.
  ```R
  encoder_$transform(iris)
  ```
  The `transform` method will return a modified version of your data with one-hot encoded categorical variables.
  ![One hot encode](http://www.image-heberg.fr/files/17007777941780451472.png)

## R-shiny application
We have developed an r-shiny application which allows you to test the different functionalities of ours package. This application allows users, whether novice or expert in R programming, to easily explore the capabilities of the Naive Bayesian classifier without requiring any prior knowledge in-depth programming. It allows, among other things, to train and save a model for later use.<br>
It is available at the following address: https://moiseberthe.shinyapps.io/naive-bayes-r-shiny/

![Shiny App](https://github.com/Naghan1132/naive_bayes_R/assets/75121872/283e9bd6-3205-48ba-8a49-2f0e5f17369a)

