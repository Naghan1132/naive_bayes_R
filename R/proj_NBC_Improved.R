library(R6)

NaiveBayesClassifier = R6Class("NaiveBayesClassifier",
                               public = list(
                                 # predict_probas = NULL,
                                 initialize = function(method){
                                   if(!method %in% c("multinomial","gaussian")){
                                     stop('ERROR : method can only be either "multinomial" or "gaussian"')
                                   }
                                   private$method = method
                                   cat(paste0('Class instance of \"NaiveBayesClassifier\" ','(',private$method,')',' created'))
                                 },
                                 fit = function(X,Y) {
                                   if(private$method=="multinomial"){
                                     private$checkIntegrity(X,Y)
                                     private$Xtrain = X
                                     private$Y = Y
                                     XTrainProcessedCat = private$encode(X[sapply(X,is.factor)])
                                     private$fitDiscretizationQuantiles(X[sapply(X,is.numeric)])
                                     XtrainProcessedNum = private$discretize(X[sapply(X,is.numeric)])
                                     private$X = cbind(XTrainProcessedCat,XtrainProcessedNum)
                                     private$class_probas = table(private$Y)/sum(table(private$Y))
                                     sapply(unique(private$Y), function(y){
                                       subset = private$X[private$Y==y,]
                                       sapply(names(subset),function(col){
                                         private$conditional_probas[[paste(y, col)]] <<- table(subset[, col]) / nrow(subset)
                                       })
                                     })
                                   }
                                 },
                                 predict = function(X){
                                   if(private$method=="multinomial"){
                                     predict_probas = self$predict_probas(X)
                                     return(colnames(predict_probas)[max.col(predict_probas)])
                                   }
                                 },
                                 predict_probas = function(X){
                                   if (!setequal(names(X), names(private$Xtrain)) || !setequal(sapply(X, class), sapply(private$Xtrain, class))) {
                                     cat("ERROR : Test dataset should be of the same structure as dataset used in fit, here is the structure of the training set : \n")
                                     return(str(private$Xtrain))
                                   }
                                   if(private$method=="multinomial"){
                                     index=1
                                     individual=1
                                     conditional=1
                                     XTestProcessedCat = private$encode(X[sapply(X,is.factor)])
                                     XTestProcessedNum = private$discretize(X[sapply(X,is.numeric)])
                                     XTestProcessed = cbind(XTestProcessedCat,XTestProcessedNum)
                                     predict_probas = matrix(nrow=nrow(XTestProcessed),ncol=length(unique(private$Y)))
                                     colnames(predict_probas) = unique(private$Y)
                                     sapply(1:nrow(XTestProcessed),function(line){
                                       sapply(unique(private$Y),function(y){
                                         class_probas <- private$class_probas[y]
                                         sapply(names(XTestProcessed),function(x){
                                           if(!is.na(private$conditional_probas[[paste(y, x)]][as.character(XTestProcessed[line,x])])){
                                             conditional = conditional * private$conditional_probas[[paste(y, x)]][as.character(XTestProcessed[line,x])]
                                           }
                                           class_probas = class_probas * conditional
                                           predict_probas[individual,index] <<- class_probas
                                           # self$predict_probas[individual,index] <<- ifelse(is.na(self$predict_probas[individual,index]),1,ifelse(self$predict_probas[individual,index]==0,1,self$predict_probas[individual,index])) * self$conditional_probas[[paste(y, x)]][as.character(XTestProcessed[line,x])]
                                         })
                                         index <<- index + 1
                                       })
                                       index <<- 1
                                       individual <<- individual+1
                                     })
                                   }
                                   return(predict_probas)
                                 }
                               ),
                               private = list(
                                 X=NULL,
                                 Y=NULL,
                                 Xtrain = NULL,
                                 conditional_probas = NULL,
                                 class_probas = NULL,
                                 quantiles_dict = NULL,
                                 method = "multinomial",
                                 #Vérification du type des vecteurs d'entrée
                                 checkIntegrity = function(X,Y){
                                   isValidX = all(sapply(X, function(x) class(x) %in% c("factor","numeric","integer")))
                                   if(!isValidX){
                                     stop("ERROR : The explicative variables can only have feature(s) of class factor, numeric or integer")
                                   }
                                   isValidY = all(sapply(Y, function(y) class(y) %in% c("factor")))
                                   if(!isValidY){
                                     stop("ERROR : The predictive variables can only be of class a factor")
                                   }
                                   if(length(unique(Y))<2){
                                     stop("ERROR : You need to at least have 2 different classes in your target variables")
                                   }
                                 },
                                 fitDiscretizationQuantiles = function(data,K=4){
                                   for (var_name in colnames(data)) {
                                     quantiles_list <- list()
                                     
                                     for (number in 1:K) {
                                       quantiles_array <- quantile(data[[var_name]], number / K)
                                       quantiles_list[[number]] <- quantiles_array
                                     }
                                     
                                     private$quantiles_dict[[var_name]] <- quantiles_list
                                   }
                                 },
                                 discretize = function(data,K=3){
                                   for(var in colnames(data)){
                                     quantile_array = private$quantiles_dict[[var]]
                                     vectGroups = array(NA,dim=nrow(data))
                                     for(i in 1:length(quantile_array)){
                                       q1 = quantile_array[i-1]
                                       if(i==1){
                                         q1 = 0
                                       }
                                       q2 = quantile_array[i]
                                       index = which(data[var] >= q1 & data[var] <= q2)
                                       vectGroups[index] = paste0("Group_",i)
                                     }
                                     data[var]=vectGroups
                                   }
                                   return(data)
                                 },
                                 
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
                                 },
                                 #Encodage catégoriel
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
                                     dataToBeLabelEncoded = as.data.frame(X[,cols_more_than_3_distinct])
                                     colnames(dataToBeLabelEncoded) = colnames(X)[cols_more_than_3_distinct]
                                     label_encoded = private$LabelEncode(dataToBeLabelEncoded)
                                     one_hot_encoded = private$OneHotEncode(X[,cols_3_or_fewer_distinct])
                                     X[,cols_more_than_3_distinct] = label_encoded
                                     if(length(cols_3_or_fewer_distinct)>0){
                                       X = X[,-cols_3_or_fewer_distinct]
                                       X = cbind(X,one_hot_encoded)
                                     }
                                   }
                                   return(X)
                                 }
                               )
                            )

# nbc = NaiveBayesClassifier$new("multinomial")
# nbc$fit(X = Titanic[-c(6, 21, 11, 2, 29, 20, 5, 16, 26, 31),-1],Y=Titanic$Class)
# Xtest = data.frame(
#   Freq = c(5,10,300,8),
#   Sex = as.factor(c("Male","Female","Male","Female")),
#   Survived = as.factor(c("No","Yes","No","Yes")),
#   Age = as.factor(c("Adult","Child","Child","Child"))
# )
# print(nbc$predict_probas(Xtest))
# print(nbc$predict(Xtest))

