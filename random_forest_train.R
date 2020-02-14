# #Random Forest
# library(Metrics)
# library(ranger)
# set.seed(20200116)
rf_train <- function(df){
  library(Metrics)
  library(ranger)
  set.seed(20200116)
  indx <- df$table_name == 'train'
  train_set <- df[indx,]
  smp_size <- floor(0.8*nrow(train_set))
  splits <- sample(seq_len(nrow(train_set)), size = smp_size)
  train <- train_set[splits,]
  test <- train_set[-splits,]
  model_rf <- ranger(Survived ~ Pclass + family_size +Title + Sex + new_age , data = train, num.trees = 100, mtry = 4)
  prediction <- round(predict(model_rf, test, type = "response")$predictions)
  return(
    data.frame(
      accuracy = accuracy(test$Survived, prediction),
      precision = precision(test$Survived, prediction),
      recall = recall(test$Survived, prediction),
      auc = auc(test$Survived, prediction)
    )
  )
}

getOutputSchema <- function(){
  return(
    data.frame(
      accuracy = prep_decimal(),
      precision = prep_decimal(),
      recall = prep_decimal(),
      auc = prep_decimal()
    )
  )
}