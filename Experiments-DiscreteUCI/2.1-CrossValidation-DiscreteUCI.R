library(RWeka)
library(tidyverse)
library(groupdata2)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

for (data in c("balance-scale", "nursery", "solar-flare")){
  df <- read.arff(paste("DiscreteUCI/", data, ".arff", sep = ""))
  idx <- data.frame(id = 1:nrow(df))
  idx <- fold(idx, k = 10)
  
  for (i in 1:10){
    train <- idx[idx$.folds != i,]
    train <- train %>% ungroup() %>% mutate(.folds = NULL) %>% .$id
    test <- idx[idx$.folds == i,]
    test <- test %>% ungroup() %>% mutate(.folds = NULL) %>% .$id
    write.arff(df[train,], 
               paste("DiscreteUCI-CV/", data, "-train-", as.character(i), ".arff", sep = ""))
    write.arff(df[test,], 
               paste("DiscreteUCI-CV/", data, "-test-", as.character(i), ".arff", sep = ""))
  }
}