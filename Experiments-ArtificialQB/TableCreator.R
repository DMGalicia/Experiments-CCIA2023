library(reshape2)
library(tidyverse)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

df <- read.csv("1.3-Results-ErrorsNoise.csv")

idf <- dcast(df[,1:4], size ~ noise, value.var = c("conserved.idf"))
idf2 <- dcast(df[,1:4], size ~ noise, value.var = c("conserved.idf2"))
results <- as.data.frame(cbind(idf$size, idf$"10", idf2$"10", idf$"20", idf2$"20", 
                               idf$"30", idf2$"30", idf$"40", idf2$"40"))
names(results) <- c("size", "10.cons.idf", "10.cons.idf2", "20.cons.idf", "20.cons.idf2",
                    "30.cons.idf", "30.cons.idf2", "40.cons.idf", "40.cons.idf2")
write.table(results, "1.3-Table.ConservedExamples.txt", sep = " & ", row.names = F, quote = F)


er1.idf <- dcast(df[,c(1,2,5,6,7,8)], size ~ noise, value.var = c("er1.idf"))
er2.idf <- dcast(df[,c(1,2,5,6,7,8)], size ~ noise, value.var = c("er2.idf"))
er1.idf2 <- dcast(df[,c(1,2,5,6,7,8)], size ~ noise, value.var = c("er1.idf2"))
er2.idf2 <- dcast(df[,c(1,2,5,6,7,8)], size ~ noise, value.var = c("er2.idf2"))
results <- as.data.frame(cbind(idf$size, 
                               er1.idf$"10", er2.idf$"10", er1.idf2$"10", er2.idf2$"10", 
                               er1.idf$"20", er2.idf$"20", er1.idf2$"20", er2.idf2$"20", 
                               er1.idf$"30", er2.idf$"30", er1.idf2$"30", er2.idf2$"30", 
                               er1.idf$"40", er2.idf$"40", er1.idf2$"40", er2.idf2$"40"))
names(results) <- c("size", "10.er1.idf", "10.er2.idf", "10.er1.idf2", "10.er2.idf2",
                    "20.er1.idf", "20.er2.idf", "20.er1.idf2", "20.er2.idf2",
                    "30.er1.idf", "30.er2.idf", "30.er1.idf2", "30.er2.idf2",
                    "40.er1.idf", "40.er2.idf", "40.er1.idf2", "40.er2.idf2")
write.table(results, "1.3-Table.ErrorsNoise.txt", sep = " & ", row.names = F, quote = F)



df <- read.csv("1.4-Results-AUC.csv")
df1 <- dcast(df, test+size~filter+noise, value.var = "auc")
df1 <- df1[,c("test","size", "J48_10", "IDF_10", "IDF2_10", "J48_20", "IDF_20", "IDF2_20",
              "J48_30", "IDF_30", "IDF2_30", "J48_40", "IDF_40", "IDF2_40")]
write.table(df1, "1.4-Table.auc.txt", sep = " & ", row.names = F, quote = F)

df <- read.csv("04-Results-AUC-noPruning.csv")
df1 <- dcast(df, test+size~filter+noise, value.var = "auc")
df1 <- df1[,c("test","size", "J48_10", "IDF_10", "IDF2_10", "J48_20", "IDF_20", "IDF2_20",
              "J48_30", "IDF_30", "IDF2_30", "J48_40", "IDF_40", "IDF2_40")]
write.table(df1, "1.4-Table.auc.noPruning.txt", sep = ",", row.names = F, quote = F)
