library(RWeka)
library(tidyverse)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
filterType <- c(J48 = "", IDF = "-IDF", IDF2 = "-IDF2")

cat("test,filter,size,noise,auc\n")

for (filter in filterType){
  filterName <- names(filterType)[filterType == filter]
  for (data in c("breast cancer wisconsin", "diabetes", "ecoli", "segment", "waveform-5000")){
    for (discretizer in c("CAIM", "AMEVA", "CACC", "CHI2", "MDLP")){
      aucs <- c()
      if(!(data == "segment" & discretizer ==  "MDLP")){
        for (i in 1:10){
          train <- read.arff(paste("ContinuousUCI-CV-IDFs/", discretizer, "/", data, filter, "-train-", 
                                   as.character(i),  ".arff", sep = ""))
          names(train)[ncol(train)] <- "class"
          model <- J48(class~., train)
          test <- read.arff(paste("ContinuousUCI-CV-IDFs/", discretizer, "/", data, "-test-", 
                                  as.character(i),  ".arff", sep = ""))
          names(test)[ncol(test)] <- "class" 
          e <- evaluate_Weka_classifier(model, newdata = test, numFolds=0, class = T)
          indAUC <- e$detailsClass[,"areaUnderROC"]
          indProb <- table(test$class)/nrow(test)
          auc <- sum(indAUC*indProb, na.rm = T)
          aucs <- c(aucs, auc)
        }
      }
      aucs <- aucs * 100
      cat(filterName, ",", data, ",", discretizer, ",",
          sprintf('%#.2f', mean(aucs)), "\n", sep = "")
    }
  }
}