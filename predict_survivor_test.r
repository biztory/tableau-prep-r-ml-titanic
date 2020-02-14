#Logistic Regression
library(Metrics)
library(rsample)
set.seed(20200117)
log_reg_train <- function(df){
  smp_size <- floor(0.8*nrow(df))
  splits <- sample(seq_len(nrow(df)), size = smp_size)
  train <- df[splits,]
  test <- df[-splits,]
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