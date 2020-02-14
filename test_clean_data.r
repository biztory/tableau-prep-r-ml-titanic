### Script for Tableau Prep ###
library(stringr)
library(tidyverse)

age_null <- function(df){
  #Extract titles
  df$Title <- str_extract(df$Name, "\\w*[.]")
  #correct typo's from title
  df$Title <- str_replace_all(df$Title, "Mlle.", "Miss.")
  df$Title <- str_replace_all(df$Title, "Ms.", "Miss.")
  df$Title <- str_replace_all(df$Title, "Mme.", "Mrs.")
  #create family size variable
  df$family_size <- df$SibSp + df$Parch + 1
  return(df)
}

getOutputSchema <- function(){
  return(
    data.frame(
      PassengerId = prep_string(),
      Name = prep_string(),
      Title = prep_string(),
      Sex = prep_string(),
      Pclass = prep_int(),
      Age = prep_int(),
      family_size = prep_int()
    )
  )
}