library(RWeka)
library(tidyverse)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

source("IDFS.R")

testType <- c(Clean = "-clean", Noisy = "-noisy")
filterType <- c(J48 = "", IDF = "-IDF", IDF2 = "-IDF2")

cat("test,filter,size,noise,auc\n")

for (ttest in testType){
  testName <- names(testType)[testType == ttest]
  for (filter in filterType){
    filterName <- names(filterType)[filterType == filter]
    for (noiseLvl in c("0", "10", "20", "30", "40")){
      for (size in c("250", "500", "1000", "2000", "5000", "10000")){
        aucs <- c()
        for (i in 1:10){
          train <- read.arff(paste("ArtificialQB-CV-IDFs/QB-RCI-", noiseLvl, 
                                   "-", size, filter, "-train-", 
                                   as.character(i),  ".arff", sep = ""))
          train$Class <- NULL
          model <- J48(ObservedClass~., train, control = Weka_control(U = T))
          test <- read.arff(paste("ArtificialQB-CV-IDFs/QB-RCI-", noiseLvl, 
                                  "-", size, ttest, "-test-", 
                                  as.character(i),  ".arff", sep = ""))
          e <- evaluate_Weka_classifier(model, newdata = test, numFolds=0, class = T)
          indAUC <- e$detailsClass[,"areaUnderROC"]
          indProb <- table(test$ObservedClass)/nrow(test)
          auc <- sum(indAUC*indProb)
          aucs <- c(aucs, auc)
        }
        aucs <- aucs * 100
        cat(testName, ",", filterName, ",", as.numeric(size)*0.9, ",", noiseLvl, ",",
            sprintf('%#.2f', mean(aucs)), "\n", sep = "")
        
      }
    }
  }
}