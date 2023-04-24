library(RWeka)
library(tidyverse)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

source("IDFS.R")

for (data in c("breast cancer wisconsin", "diabetes", "ecoli", "segment", "waveform-5000")){
  for (discretizer in c("CAIM", "AMEVA", "CACC", "CHI2", "MDLP")){
    if(!(data == "segment" & discretizer ==  "MDLP")){
      for (i in 1:10){
        train <- read.arff(paste("ContinuousUCI-CV/", discretizer, "/", data, "-train-", 
                                 as.character(i),  ".arff", sep = ""))
        train.copy <- train 
        names(train.copy)[ncol(train.copy)] <- "class"
        test <- read.arff(paste("ContinuousUCI-CV/", discretizer, "/", data, "-test-", 
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
        write.arff(train, paste("ContinuousUCI-CV-IDFs/", discretizer, "/", data,
                                "-train-", as.character(i),  ".arff", sep = ""))
        write.arff(trainIDF, paste("ContinuousUCI-CV-IDFs/", discretizer, "/", data, 
                                   "-IDF-train-", as.character(i),  ".arff", sep = ""))
        write.arff(trainIDF2, paste("ContinuousUCI-CV-IDFs/", discretizer, "/", data,
                                    "-IDF2-train-", as.character(i),  ".arff", sep = ""))
        write.arff(test, paste("ContinuousUCI-CV-IDFs/", discretizer, "/", data,
                               "-test-", as.character(i),  ".arff", sep = ""))
      }
    }
  }
}