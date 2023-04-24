library(RWeka)
library(tidyverse)
library(groupdata2)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

source("IDFS.R")

cat("size, noise, conserved.idf, conserved.idf2, er1.idf, er2.idf, er1.idf2, er2.idf2\n")

for (noiseLvl in c("0", "10", "20", "30", "40")){
  for (size in c("250", "500", "1000", "2000", "5000", "10000")){
    er1.idf <- c()
    er2.idf <- c()
    er1.idf2 <- c()
    er2.idf2 <- c()
    size.idf <- c()
    size.idf2 <- c()
    for (i in 1:10){
      train <- read.arff(paste("ArtificialQB-CV/QB-RCI-", noiseLvl, "-", size, "-train-", 
                            as.character(i),  ".arff", sep = ""))
      train.observed <- train %>% mutate(Class = NULL)
      names(train.observed)[ncol(train.observed)] <- "class"
      idf <- IDF(train.observed)
      idf.conserved <- train[unlist(idf$conserved),]
      idf.discarted <- train[unlist(idf$deleted),]
      idf2 <- IDF2(train.observed)
      idf2.conserved <- train[unlist(idf2$conserved),]
      idf2.discarted <- train[unlist(idf2$deleted),]
      er1.idf <- c(er1.idf, nrow(idf.discarted %>% filter(ObservedClass == Class))/nrow(train))
      er2.idf <- c(er2.idf, nrow(idf.conserved %>% filter(ObservedClass != Class))/nrow(train))
      er1.idf2 <- c(er1.idf2, nrow(idf2.discarted %>% filter(ObservedClass == Class))/nrow(train))
      er2.idf2 <- c(er2.idf2, nrow(idf2.conserved %>% filter(ObservedClass != Class))/nrow(train))
      size.idf <- c(size.idf, nrow(idf.conserved)/nrow(train))
      size.idf2 <- c(size.idf2, nrow(idf2.conserved)/nrow(train))
    }
    er1.idf <- er1.idf*100
    er2.idf <- er2.idf*100
    er1.idf2 <- er1.idf2*100
    er2.idf2 <- er2.idf2*100
    size.idf <- size.idf*100
    size.idf2 <- size.idf2*100
    cat(as.numeric(size)*0.9, ",", noiseLvl, ",",
        sprintf('%#.2f', mean(size.idf)), ",", 
        sprintf('%#.2f', mean(size.idf2)), ",", 
        sprintf('%#.2f', mean(er1.idf)), ",", 
        sprintf('%#.2f', mean(er2.idf)), ",", 
        sprintf('%#.2f', mean(er1.idf2)), ",", 
        sprintf('%#.2f', mean(er2.idf2)), "\n", sep = "")
  }
}
