# import models ----
models <- readRDS("project_analysis/model_training/models.RDS")

# unlist model types 
modelsRed <- Reduce(`c`, x=models) 

# read houldout set ----
holdout <- readRDS("project_analysis/model_training/holdout.RDS")

# generate holdout predictions, save mcc and tpr for weighting and prediction exclusion
head(predict(modelsRed[[205]], as.matrix(holdout[, !(names(holdout) %in% c("ID", "ALLERGINM"))]), type="response"))

predictions <- lapply(seq_along(names(models)), function(x){
  # to use same prediction parameters with logistic, need to specify the "s" (lambda) parameter
  if(names(models)[[x]] == "logistic"){
    res <- lapply(seq_along(models[[x]]), function(y){
      preds <- predict(models[[x]][[y]][[2]], as.matrix(holdout[, !(names(holdout) %in% c("ID", "ALLERGINM"))]), 
                                                    type="response")
      print(cat(crayon::yellow(paste0("Made prediction for model: ", y, " of ", x, "\n"))))
      return(list("preds"=preds, "weight"=models[[x]][[y]][[3]]*models[[x]][[y]][[4]]))
      })
    
    return(res)
  }else{
  res <- lapply(seq_along(models[[x]]), function(y){ 
    preds <- predict(models[[x]][[y]][[2]], holdout[, !(names(holdout) %in% c("ID", "ALLERGINM"))], 
                                                  type="prob")
    print(cat(crayon::yellow(paste0("Made prediction for model: ",y, " of ", x, "\n"))))
    return(list("preds"=preds[, 2], "weight"=models[[x]][[y]][[3]]*models[[x]][[y]][[4]]))
    })
  
  return(res)
  }
})

# Reduce lists to a single list, remove those with negative weight (due to negative mcc) ----
predictionsRed <- Reduce(`c`, predictions)

# remove negative weights due to negative mcc ----
predFiltered <- predictionsRed[which(sapply(predictionsRed, function(x) x[[2]]) > 0)]


# calculate predictions * weight, consolidate all predictions into a single data.frame with cols == no. of pred models
predDf <- do.call(cbind, lapply(predFiltered, function(x){
  data.frame(x[[1]] * x[[2]])
}))

# calculate the sum of valid weights ----
sumW <- sum(sapply(predFiltered, function(x) x[[2]]))

# calculate weighted avg of predictions and then use it for classification with randomly generated value 
# using a binom den function
set.seed(1234)
pred <- sapply(rowSums(predDf)/sumW, function(x) rbinom(1,1,x))

# compare with actual ----
res <- data.frame(actual=holdout[, 2], predicted = pred)
saveRDS(res, "project_analysis/results/prediction_res.RDS")


# calculate some performance metrics ----
# mcc
mcc <- mccr::mccr(res[, 1], res[, 2])

# tpr
tablePred <- table(res)
saveRDS(tablePred, "project_analysis/tables/table_pred.RDS")
tpr <- tablePred[2, 2]/(tablePred[2, 2] + tablePred[1, 2])


metrics <- data.frame(MCC=mcc, TPR=tpr)
saveRDS(metrics, "project_analysis/results/pred_metrics.RDS")

# save some data ---