#get the mean age of the each grouping Sex-Pclass

mean_age_per_group <- function(df){
  data.frame(
    Sex = c("female","female","female","male","male","male"),
    Pclass = c(1,2,3,1,2,3),
    mean_age = c(
        mean(df$Age[df$Sex == "female" & df$Pclass == 1], na.rm = T),
        mean(df$Age[df$Sex == "female" & df$Pclass == 2], na.rm = T),
        mean(df$Age[df$Sex == "female" & df$Pclass == 3 ], na.rm = T),
        mean(df$Age[df$Sex == "male" & df$Pclass == 1], na.rm = T),
        mean(df$Age[df$Sex == "male" & df$Pclass == 2], na.rm = T),
        mean(df$Age[df$Sex == "male" & df$Pclass == 3], na.rm = T)
        )
  )
}

getOutputSchema <- function(){
  return(
    data.frame(
     Sex = prep_string(),
     Pclass = prep_int(),
     mean_age = prep_int()
    )
  )
}