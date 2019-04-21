# pkg requirements ----
pkgs <- c("magrittr", "data.table")
for(i in seq_along(pkgs)){
  if(!any(.packages() == pkgs[[i]])) suppressWarnings(
    suppressPackageStartupMessages(library(pkgs[[i]], character.only = T))
  )
}
rm(pkgs, i)

# load data ----
data <- readRDS("model/processed_data.RDS")

# create ID var
data[, ID:=seq_len(nrow(data))]
data[, sentiment:=as.integer(sentiment)]

# transform data into dfm ----
dfm <- copy(data) %>%
  tidytext::unnest_tokens(word, text) %>% # tokennize
  .[!(word %in% stopwords::stopwords("en", "stopwords-iso"))] %>% # remove stopwords
  .[!stringr::str_detect(word, "\\d")]  # remove digits

sparseWords <- dfm %>%
  .[, .(n=.N), by=c("ID", "word")]  # count terms by text ID

sparseWords <- data[, .(ID, sentiment)] %>% 
  .[sparseWords, on=c("ID")] # need sentiment columns, merge original data with term count data

sparseWords <- sparseWords %>% tidytext::cast_dfm(ID, word, n) # create dfm

# save the original matrix to use to intersect with new data features ---
saveRDS(sparseWords, "model/dfm_mod.RDS")

#### create train/test samples
set.seed(1234) # reproducibility

# data partition
indexTrain <- caret::createDataPartition(data$sentiment,p = 0.25,list=FALSE)
indexTest <- seq_len(nrow(data))[which(!(seq_len(nrow(data)) %in% indexTrain))]

#### run binomial model with cross validation

# features training set
trainSet <- sparseWords[indexTrain[, 1], ]

# response variable training set
y <- data[indexTrain[, 1], sentiment]

set.seed(1234) # reproducibility
mod <- glmnet::cv.glmnet(y = y, 
                         x = trainSet, nfolds = 10, 
                         family="binomial", type.measure = "class", alpha=1)

# create a test set
testSet <- sparseWords[indexTest, ]

#### test set model validation
# predict test set using resulting model
fit <- predict(mod, testSet, type="response")

# use a probability distribution to assign class
res <- apply(fit, 1, function(y) rbinom(1, 1, y))

# generate an actual and prediction dataset 
predictions <- data.table(Prediction=res, Actual=data[indexTest, sentiment])

# calculate the MCC 
mcc <- mccr::mccr(data[indexTest, sentiment], res) # 0.5425818

# calculate true positive rate
tablePred <- table(predictions)
tpr <- tablePred[2, 2]/(tablePred[2, 2] + tablePred[1, 2]) # ~ 77%

#### save model
saveRDS(mod, "model/trained_model.RDS")
