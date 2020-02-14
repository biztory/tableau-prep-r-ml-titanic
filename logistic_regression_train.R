#Logistic Regression
# library(Metrics)
# library(rsample)
# set.seed(20200117)
log_reg_train <- function(df){
  library(Metrics)
  library(rsample)
  set.seed(20200117)
  indx <- df$table_name == 'train'
  train_set <- df[indx,]
  smp_size <- floor(0.8*nrow(train_set))
  splits <- sample(seq_len(nrow(train_set)), size = smp_size)
  train <- train_set[splits,]
  test <- train_set[-splits,]
  model <- glm(Survived ~ Pclass + family_size + Title + Sex + new_age , data = train, family = "binomial")
  prediction <- round(predict(model, test, type = "response"))
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