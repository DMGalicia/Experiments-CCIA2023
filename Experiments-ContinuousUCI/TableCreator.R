library(reshape2)
library(tidyverse)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

df <- read.csv("3.3-Results-Size-Noise.csv")
df1 <- dcast(df, data~"idf", value.var = "conserved.idf", fun.aggregate = mean, na.rm = TRUE)
df2 <- dcast(df, data~"idf2", variable.name = "sd", value.var = "conserved.idf2", fun.aggregate = mean, na.rm = TRUE)
df3 <- inner_join(df1,df2, by = c("data"))
write.table(df3, "3.3-Table.size.txt", sep = " & ", row.names = F, quote = F)

  df <- read.csv("3.4-Results-AUC.csv")
df1 <- dcast(df, data~filter, value.var = "auc", fun.aggregate = mean, na.rm = TRUE)
df1 <- df1[,c("data","J48", "IDF", "IDF2")]
write.table(df1, "3.4-Table.auc.txt", sep = " & ", row.names = F, quote = F)
