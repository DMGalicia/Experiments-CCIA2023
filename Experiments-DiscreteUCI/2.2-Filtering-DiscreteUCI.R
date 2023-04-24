library(RWeka)
library(tidyverse)

source("IDFS.R")

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

for (data in c("balance-scale", "solar-flare", "nursery")){
  for (i in 1:10){
    train <- read.arff(paste("DiscreteUCI-CV/", data, "-train-", 
                             as.character(i),  ".arff", sep = ""))
    train.copy <- train 
    names(train.copy)[ncol(train.copy)] <- "class"
    test <- read.arff(paste("DiscreteUCI-CV/", data, "-test-", 
                            as.character(i),  ".arff", sep = ""))
    test.copy <- test
    names(test.copy)[ncol(test.copy)] <- "class"
    trainIDF <- IDF(train.copy)
    trainIDF <- train[unlist(trainIDF$conserved),]
    trainIDF2 <- IDF2(train.copy)
    trainIDF2 <- train[unlist(trainIDF2$conserved),]
    testIDF <- IDF(test.copy)
    testIDF <- test[unlist(testIDF$conserved),]
    testIDF2 <- IDF2(test.copy)
    testIDF2 <- test[unlist(testIDF2$conserved),]
    write.arff(train, paste("DiscreteUCI-CV-IDFs/", data, 
                            "-train-", as.character(i),  ".arff", sep = ""))
    write.arff(trainIDF, paste("DiscreteUCI-CV-IDFs/", data, 
                               "-IDF-train-", as.character(i),  ".arff", sep = ""))
    write.arff(trainIDF2, paste("DiscreteUCI-CV-IDFs/", data,
                                "-IDF2-train-", as.character(i),  ".arff", sep = ""))
    write.arff(test, paste("DiscreteUCI-CV-IDFs/", data,
                           "-noisy-test-", as.character(i),  ".arff", sep = ""))
    write.arff(testIDF, paste("DiscreteUCI-CV-IDFs/", data,
                              "-IDF-test-", as.character(i),  ".arff", sep = ""))
    write.arff(testIDF2, paste("DiscreteUCI-CV-IDFs/", data,
                               "-IDF2-test-", as.character(i),  ".arff", sep = ""))
  }
}