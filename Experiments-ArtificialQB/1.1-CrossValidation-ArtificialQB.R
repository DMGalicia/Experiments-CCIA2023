library(RWeka)
library(tidyverse)
library(groupdata2)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

for (noiseLvl in c("0", "10", "20", "30", "40")){
  for (size in c("250", "500", "1000", "2000", "5000", "10000")){
    df <- read.arff(paste("ArtificialQB/QB-RCI-", noiseLvl, "-", size, ".arff", sep = ""))
    idx <- data.frame(id = 1:nrow(df))
    idx <- fold(idx, k = 10)
    
    for (i in 1:10){
      train <- idx[idx$.folds != i,]
      train <- train %>% ungroup() %>% mutate(.folds = NULL) %>% .$id
      test <- idx[idx$.folds == i,]
      test <- test %>% ungroup() %>% mutate(.folds = NULL) %>% .$id
      write.arff(df[train,], 
                 paste("ArtificialQB-CV/QB-RCI-", noiseLvl, "-", size, "-train-", as.character(i), ".arff", sep = ""))
      write.arff(df[test,], 
                 paste("ArtificialQB-CV/QB-RCI-", noiseLvl, "-", size, "-test-", as.character(i), ".arff", sep = ""))
    }
  }
}