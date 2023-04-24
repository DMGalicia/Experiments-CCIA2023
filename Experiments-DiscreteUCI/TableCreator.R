library(reshape2)
library(tidyverse)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

df <- read.csv("2.3-Results-Size-Noise.csv")
write.table(df, "2.3-Table.size.txt", sep = " & ", row.names = F, quote = F)

df <- read.csv("2.4-Results-AUC.csv")
df1 <- dcast(df, data~filter, value.var = "auc")
df1 <- df1[,c("data","J48", "IDF", "IDF2")]
write.table(df1, "2.4-Table.auc.txt", sep = " & ", row.names = F, quote = F)
