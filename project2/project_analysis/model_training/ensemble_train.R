# LOAD PACKAGES----
library(magrittr)
library(data.table)

# PACKAGE REQUIREMETNS ----
if(!nzchar(system.file(package = "mccr"))){
  ans <- menu(choices = c("Y", "N"), 
              title = "Package mccr not installed in your system.\n\nDo you wish to install it? (R will quit if you choose 'N')")
  if(ans == 2L) quit()
  install.packages("mccr")
}
if(!nzchar(system.file(package = "rpart"))){
  ans <- menu(choices = c("Y", "N"), 
              title = "Package rpart not installed in your system.\n\nDo you wish to install it? (R will quit if you choose 'N')")
  if(ans == 2L) quit()
  install.packages("rpart")
}
if(!nzchar(system.file(package = "glmnet"))){
  ans <- menu(choices = c("Y", "N"), 
              title = "Package glmnet not installed in your system.\n\nDo you wish to install it? (R will quit if you choose 'N')")
  if(ans == 2L) quit()
  install.packages("glmnet")
}
if(!nzchar(system.file(package = "caret"))){
  ans <- menu(choices = c("Y", "N"), 
              title = "Package caret not installed in your system.\n\nDo you wish to install it? (R will quit if you choose 'N')")
  if(ans == 2L) quit()
  install.packages("caret")
}

# data partition -----

# load data
data <- readRDS("data/master.RDS")

# create hold out set
set.seed(1234)

holdout <- data[sample(x=.N, size=nrow(data)*0.2, replace=F)] # 20% of the data is set aside for prediction

saveRDS(holdout, "project_analysis/model_training/holdout.RDS")

remainingSet <- data[!holdout, on="ID"]

saveRDS(remainingSet, "project_analysis/model_training/remainingSet.RDS")

# check differences in teh frequency of the target variable
splitOutcome <- lapply(c("data", "holdout", "remainingSet"), 
       function(x){
         d1 <- get(x)
         p1 <- d1[, .N, by=.(ALLERGINM)] %>% 
           .[, N/sum(N, na.rm=T)] %>%
           scales::percent(.) %>% 
           matrix(ncol=2) %>% 
           data.table::as.data.table(.)
         p2 <- d1[, .N]
         cbind(p1, p2)
       }) %>% data.table::rbindlist(.) %>% 
  .[, Set := c("Complete set", "Hold-out set", "Remaining set")]

setnames(splitOutcome, names(splitOutcome), c("Has autoimmune", "No autoimmune", "Obs.", "Set"))
setcolorder(splitOutcome, c("Set", names(splitOutcome)[c(2,1,3)]))

saveRDS(splitOutcome, "project_analysis/tables/setSplits.RDS")

remainingSet[, ID:=NULL]

# split 80/20 - 400 samples
index <- lapply(c(1:4), function(x){
  index <- caret::createDataPartition(remainingSet$ALLERGINM, times = 100, 
                                      p = 0.8, 
                                      list=TRUE)
  })


names(index) <- c("tree_samples", "svm_samples", "logistic_samples", "nbc_samples")


# TREE CLASSIFICATION ----

# define formula for tree 
form <- as.formula(paste0("ALLERGINM ~ ", paste0(names(remainingSet)[-1], collapse=" + ")))

# randomly select values for parameters 
cpVals <- seq(0.001, 0.1, by=0.001)
minSplitVals <- seq(5, 20, by=1)

parms <- matrix(ncol=2, nrow = 100)

parms <- apply(parms, 1, function(x) c(sample(cpVals, size = 1), sample(minSplitVals, size=1)))

parms <- t(parms)

trees <- lapply(seq_along(index[["tree_samples"]]), function(x){
  set.seed(12345)
  cat(crayon::yellow(paste0("Starting to process model ", x, "...\n")))
  
  # define train and test test
  dataTrain <- remainingSet[index[["tree_samples"]][[x]]]
  dataTest <- remainingSet[!index[["tree_samples"]][[x]]]
  
  # run model ----
  cat(crayon::yellow(paste0("Running tree algo for model ", x, "...\n")))
  procTime <- system.time(
  treeMod <- rpart::rpart(form, data = dataTrain, 
                          method = "class", 
                          control = rpart::rpart.control(xval = 30, 
                                                         cp = parms[x, 1], 
                                                         minsplit = parms[x, 2]))
  )
  cat(crayon::yellow(paste0("Model succesfully estimated. Time elapsed: ", procTime[[3]], "...\n")))
  
  # predict model with test set
  fit <- predict(treeMod, dataTest[, names(dataTest)[-1], with=F], type="prob")
  
  # assign values based on a coin flip with the probability of the prediction assigned ---
  res <- apply(fit, 1, function(y) rbinom(n = 1, size = 1, prob=y[[2]]))
  
  # generate an actual and prediction dataset ----
  predictions <- data.table(Prediction=res, Actual=dataTest[, 1])
  
  # calculate the MCC ---
  mcc <- mccr::mccr(dataTest[, 1], res)
  
  # calculate true positive rate ----
  tablePred <- table(predictions)
  tpr <- tablePred[2, 2]/(tablePred[2, 2] + tablePred[1, 2])
  
  # return a list of objects for future analysis
  return(
    list(
      "Parameters" = parms[x, ],
      "Model" = treeMod,
      "MCC" = mcc,
      "TPR" = tpr,
      "Predictions" = predictions
    )
  )
  
})

# Logistic regression - Binomial Model ----

# randomly select values for parameters 
alphaVals <- seq(0, 1, by=0.01)

parms <- sample(alphaVals, size=100)

binomials <- lapply(seq_along(index[["logistic_samples"]]), function(x){
  set.seed(12345)
  cat(crayon::yellow(paste0("Starting to process model ", x, "...\n")))
  
  # define train and test test
  dataTrain <- remainingSet[index[["logistic_samples"]][[x]]]
  dataTest <- remainingSet[!index[["logistic_samples"]][[x]]]
  
  # run model ----
  cat(crayon::yellow(paste0("Running GLM binomial algo for model ", x, "...\n")))
  procTime <- system.time(
    binomMod <- glmnet::cv.glmnet(y = as.matrix(dataTrain[, 1]), 
                      x = as.matrix(dataTrain[, -1]), nfolds = 10, 
                      family="binomial", alpha=parms[[x]], type.measure = "class")
  )
  cat(crayon::yellow(paste0("Model succesfully estimated. Time elapsed: ", procTime[[3]], "...\n")))
  
  # predict model with test set
  fit <- predict(binomMod, as.matrix(dataTest[, -1]), type="response")
  
  # assign values based on a coin flip with the probability of the prediction assigned ---
  res <- apply(fit, 1, function(y) rbinom(1, 1, y))
  
  # generate an actual and prediction dataset ----
  predictions <- data.table(Prediction=res, Actual=dataTest[, 1])
  
  # calculate the MCC ---
  mcc <- mccr::mccr(dataTest[, 1], res)
  
  # calculate true positive rate ----
  tablePred <- table(predictions)
  tpr <- tablePred[2, 2]/(tablePred[2, 2] + tablePred[1, 2])
  
  # return a list of objects for future analysis
  return(
    list(
      "Parameters" = parms[[x]],
      "Model" = binomMod,
      "MCC" = mcc,
      "TPR" = tpr,
      "Predictions" = predictions
    )
  )
  
})

# Naive bayes ----

# Model parameters to be randomly selected:
# smoothing parameter [0,1] <- laplace correction

# randomly select values for parameters 
laplaceVals <- seq(0, 1, by=0.01)

parms <- sample(laplaceVals, 100, replace=T)

bayes <- lapply(seq_along(index[["nbc_samples"]]), function(x){
  set.seed(12345)
  cat(crayon::yellow(paste0("Starting to process model ", x, "...\n")))
  
  # define train and test test
  dataTrain <- remainingSet[index[["nbc_samples"]][[x]]]
  dataTest <- remainingSet[!index[["nbc_samples"]][[x]]]
  
  # y as factor
  y <- factor(as.matrix(dataTrain[, 1]), levels=c(0,1), labels = c("No", "Yes"))
  
  # run model ----
  cat(crayon::yellow(paste0("Running naive bayes classifier algo for model ", x, "...\n")))
  procTime <- system.time(
    bayesMod <- suppressWarnings(caret::train(x=dataTrain[, -1],
                                              y=y,
                                              method="nb", 
                                              trControl=caret::trainControl(method = "cv", number = 20), 
                                              tuneGrid=data.frame(fL=parms[[1]], usekernel=FALSE, adjust=FALSE)))
  )
  cat(crayon::yellow(paste0("Model succesfully estimated. Time elapsed: ", procTime[[3]], "...\n")))
  
  # predict model with test set
  fit <- suppressWarnings(predict(bayesMod, as.matrix(dataTest[, -1]), type="prob"))

  # assign values based on a coin flip with the probability of the prediction assigned ---
  res <- apply(fit, 1, function(y) rbinom(1, 1, y[2]))
  
  # generate an actual and prediction dataset ----
  predictions <- data.table(Prediction=res, Actual=dataTest[, 1])
  
  # calculate the MCC ---
  mcc <- mccr::mccr(dataTest[, 1], res)
  
  # calculate true positive rate ----
  tablePred <- table(predictions)
  tpr <- tablePred[2, 2]/(tablePred[2, 2] + tablePred[1, 2])
  
  # return a list of objects for future analysis
  return(
    list(
      "Parameters" = parms[[x]],
      "Model" = bayesMod,
      "MCC" = mcc,
      "TPR" = tpr,
      "Predictions" = predictions
    )
  )
  
})

# KNN ----

# Model parameters to be randomly selected:
# k -> max no. of k closest to the point of the data

# randomly select values for parameters 
kVals <- seq(5, 50, by=1)

parms <- sample(kVals, 100, replace=T)

knns <- lapply(seq_along(index[["svm_samples"]]), function(x){
  set.seed(12345)
  cat(crayon::yellow(paste0("Starting to process model ", x, "...\n")))
  
  # define train and test test
  dataTrain <- remainingSet[index[["svm_samples"]][[x]]]
  dataTest <- remainingSet[!index[["svm_samples"]][[x]]]
  
  # # y as factor
  y <- factor(as.matrix(dataTrain[, 1]), levels=c(0,1), labels = c("No", "Yes"))
  
  # run model ----
  cat(crayon::yellow(paste0("Running knn classifier algo for model ", x, "...\n")))
  procTime <- system.time(
    knnMod <- caret::train(x=as.matrix(dataTrain[, -1]),
                           y=as.matrix(dataTrain[, 1]) %>% 
                             factor(., levels=c(0,1), labels=c("No", "Yes")), method="knn", 
                           trControl=caret::trainControl(method = "cv", number = 20), 
                           tuneGrid=data.frame(k=parms[[x]]))
  )
  cat(crayon::yellow(paste0("Model succesfully estimated. Time elapsed: ", procTime[[3]], "...\n")))
  
  # predict model with test set
  fit <- suppressWarnings(predict(knnMod, as.matrix(dataTest[, -1]), type="prob"))
  
  # assign values based on a coin flip with the probability of the prediction assigned ---
  res <- apply(fit, 1, function(y) rbinom(1, 1, y[[2]]))
  
  # generate an actual and prediction dataset ----
  predictions <- data.table(Prediction=res, Actual=dataTest[, 1])
  
  # calculate the MCC ---
  mcc <- mccr::mccr(dataTest[, 1], res)
  
  # calculate true positive rate ----
  tablePred <- table(predictions)
  tpr <- tablePred[2, 2]/(tablePred[2, 2] + tablePred[1, 2])
  
  # return a list of objects for future analysis
  return(
    list(
      "Parameters" = parms[[x]],
      "Model" = knnMod,
      "MCC" = mcc,
      "TPR" = tpr,
      "Predictions" = predictions
    )
  )
  
})

# save models

models <- list("trees"=trees, 
               "nbc"=bayes, 
               "logistic"=binomials, 
               "knn"=knns)

saveRDS(models, file = "project_analysis/model_training/models.RDS")