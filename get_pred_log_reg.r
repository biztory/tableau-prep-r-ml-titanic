#add predictions to train

pred_to_train <- function(df){
  set.seed(20200117)
  indx <- df$table_name == 'train'
  train_set <- df[indx,]
  idx_test <- df$table_name == 'test'
  test_set <- df[idx_test,]
  smp_size <- floor(0.8*nrow(train_set))
  splits <- sample(seq_len(nrow(train_set)), size = smp_size)
  train <- train_set[splits,]
  test <- train_set[-splits,]
  model <- glm(Survived ~ Pclass + family_size + Title + Sex + new_age , data = train, family = "binomial")
  prediction <- round(predict(model, test_set, type = "response"))
  return(
    data.frame(
      Survived = prediction,
      Title = test_set$Title,
      family_size = test_set$family_size,
      Sex = test_set$Sex,
      Pclass = test_set$Pclass,
      Name = test_set$Name,
      Age = test_set$new_age,
      PassengerId = test_set$PassengerId
    )
  )
}

getOutputSchema <- function(){
  return(
    data.frame(
      Survived = prep_int(),
      Title = prep_string(),
      family_size = prep_int(),
      Sex = prep_string(),
      Pclass = prep_int(),
      Name = prep_string(),
      Age = prep_int(),
      PassengerId = prep_string()
    )
  )
}