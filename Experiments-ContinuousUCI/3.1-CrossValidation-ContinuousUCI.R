library(RWeka)
library(tidyverse)
library(groupdata2)
library(discretization)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

for (data in c("breast cancer wisconsin", "diabetes", "ecoli", "segment", "waveform-5000")){
  df <- read.arff(paste("continuousUCI/", data, ".arff", sep = ""))
  idx <- data.frame(id = 1:nrow(df))
  dd1 <- disc.Topdown(df, method = 1)$Disc.data
  dd1[] <- lapply(dd1, factor)
  dd2 <- disc.Topdown(df, method = 3)$Disc.data
  dd2[] <- lapply(dd2, factor)
  if(data != "segment"){
    dd3 <- mdlp(df)$Disc.data
    dd3[] <- lapply(dd3, factor)
  }
  dd4 <- disc.Topdown(df, method = 2)$Disc.data
  dd4[] <- lapply(dd4, factor)
  dd5 <- chi2(df)$Disc.data
  dd5[] <- lapply(dd5, factor)
  idx <- fold(idx, k = 10)

  for (i in 1:10){
    train <- idx[idx$.folds != i,]
    train <- train %>% ungroup() %>% mutate(.folds = NULL) %>% .$id
    test <- idx[idx$.folds == i,]
    test <- test %>% ungroup() %>% mutate(.folds = NULL) %>% .$id
    write.arff(df[train,], paste("continuousUCI-CV/NoDiscretization/", data, "-train-", i, ".arff", sep = ""))
    write.arff(df[test,], paste("continuousUCI-CV/NoDiscretization/", data, "-test-", i, ".arff", sep = ""))
    write.arff(dd1[train,], paste("continuousUCI-CV/CAIM/", data, "-train-", i, ".arff", sep = ""))
    write.arff(dd1[test,], paste("continuousUCI-CV/CAIM/", data, "-test-", i, ".arff", sep = ""))
    write.arff(dd2[train,], paste("continuousUCI-CV/AMEVA/", data, "-train-", i, ".arff", sep = ""))
    write.arff(dd2[test,], paste("continuousUCI-CV/AMEVA/", data, "-test-", i, ".arff", sep = ""))
    if(data != "segment"){
      write.arff(dd3[train,], paste("continuousUCI-CV/MDLP/", data, "-train-", i, ".arff", sep = ""))
      write.arff(dd3[test,], paste("continuousUCI-CV/MDLP/", data, "-test-", i, ".arff", sep = ""))
    }
    write.arff(dd4[train,], paste("continuousUCI-CV/CACC/", data, "-train-", i, ".arff", sep = ""))
    write.arff(dd4[test,], paste("continuousUCI-CV/CACC/", data, "-test-", i, ".arff", sep = ""))
    write.arff(dd5[train,], paste("continuousUCI-CV/CHI2/", data, "-train-", i, ".arff", sep = ""))
    write.arff(dd5[test,], paste("continuousUCI-CV/CHI2/", data, "-test-", i, ".arff", sep = ""))
  }
}
