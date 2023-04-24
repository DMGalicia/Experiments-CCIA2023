library(RWeka)
library(tidyverse)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
filterType <- c(J48 = "", IDF = "-IDF", IDF2 = "-IDF2")

cat("filter,data,auc\n")

for (filter in filterType){
  filterName <- names(filterType)[filterType == filter]
  for (data in c("balance-scale", "solar-flare", "nursery")){
    aucs <- c()
    for (i in 1:10){
      train <- read.arff(paste("DiscreteUCI-CV-IDFs/", data, filter, "-train-", 
                               as.character(i),  ".arff", sep = ""))
      names(train)[ncol(train)] <- "class"
      model <- J48(class~., train)
      test <- read.arff(paste("DiscreteUCI-CV-IDFs/", data, "-noisy-test-", 
                              as.character(i),  ".arff", sep = ""))
      names(test)[ncol(test)] <- "class"
      e <- evaluate_Weka_classifier(model, newdata = test, numFolds=0, class = T)
      indAUC <- e$detailsClass[,"areaUnderROC"]
      indProb <- table(test$class)/nrow(test)
      auc <- sum(indAUC*indProb, na.rm = T)
      aucs <- c(aucs, auc)
    }
    aucs <- aucs * 100
    cat(filterName, ",", data, ",",
        sprintf('%#.2f', mean(aucs)), "\n", sep = "")
    
  }
}