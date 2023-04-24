library(RWeka)
library(tidyverse)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

source("IDFS.R")

for (noiseLvl in c("0", "10", "20", "30", "40")){
  for (size in c("250", "500", "1000", "2000", "5000", "10000")){
    for (i in 1:10){
      train <- read.arff(paste("ArtificialQB-CV/QB-RCI-", noiseLvl, "-", size, "-train-", 
                               as.character(i),  ".arff", sep = ""))
      train.copy <- train %>% mutate(Class = NULL)
      names(train.copy)[ncol(train.copy)] <- "class"
      test <- read.arff(paste("ArtificialQB-CV/QB-RCI-", noiseLvl, "-", size, "-test-", 
                              as.character(i),  ".arff", sep = ""))
      trainIDF <- IDF(train.copy)
      trainIDF <- train[as.numeric(unlist(trainIDF[[1]])),]
      trainIDF <- trainIDF %>% mutate(Class = NULL)
      trainIDF2 <- IDF2(train.copy)
      trainIDF2 <- train[as.numeric(unlist(trainIDF2[[1]])),]
      trainIDF2 <- trainIDF2 %>% mutate(Class = NULL)
      clean.test <- test %>% mutate(ObservedClass = Class, Class = NULL)
      test <- test %>% mutate(Class = NULL)
      write.arff(train, paste("ArtificialQB-CV-IDFs/QB-RCI-", noiseLvl, "-", size, 
                                 "-train-", as.character(i),  ".arff", sep = ""))
      write.arff(trainIDF, paste("ArtificialQB-CV-IDFs/QB-RCI-", noiseLvl, "-", size, 
                                 "-IDF-train-", as.character(i),  ".arff", sep = ""))
      write.arff(trainIDF2, paste("ArtificialQB-CV-IDFs/QB-RCI-", noiseLvl, "-", size, 
                                  "-IDF2-train-", as.character(i),  ".arff", sep = ""))
      write.arff(clean.test, paste("ArtificialQB-CV-IDFs/QB-RCI-", noiseLvl, "-", size, 
                                   "-clean-test-", as.character(i),  ".arff", sep = ""))
      write.arff(test, paste("ArtificialQB-CV-IDFs/QB-RCI-", noiseLvl, "-", size, 
                             "-noisy-test-", as.character(i),  ".arff", sep = ""))
    }
  }
}