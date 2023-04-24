library(RWeka)
library(tidyverse)
library(groupdata2)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

source("IDFS.R")

cat("data, conserved.idf, conserved.idf2")

for (data in c("breast cancer wisconsin", "diabetes", "ecoli", "segment", "waveform-5000")){
  for (discretizer in c("CAIM", "AMEVA", "CACC", "CHI2", "MDLP")){
    size.idf <- c()
    size.idf2 <- c()
    if(!(data == "segment" & discretizer ==  "MDLP")){
      for (i in 1:10){
        train <- read.arff(paste("ContinuousUCI-CV/", discretizer, "/", data, "-train-", 
                                 as.character(i),  ".arff", sep = ""))
        train.observed <- train 
        names(train.observed)[ncol(train.observed)] <- "class"
        idf <- IDF(train.observed)
        idf.conserved <- train[unlist(idf$conserved),]
        idf2 <- IDF2(train.observed)
        idf2.conserved <- train[unlist(idf2$conserved),]
        size.idf <- c(size.idf, nrow(idf.conserved)/nrow(train))
        size.idf2 <- c(size.idf2, nrow(idf2.conserved)/nrow(train))
      }
    }
    size.idf <- size.idf*100
    size.idf2 <- size.idf2*100
    cat(data, ",", discretizer, ",",
        sprintf('%#.2f', mean(size.idf)), ",", 
        sprintf('%#.2f', mean(size.idf2)), "\n", sep = "")
  }
}
