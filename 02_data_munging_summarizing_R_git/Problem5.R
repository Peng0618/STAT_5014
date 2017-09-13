library(dplyr)
library(tidyr)
library(knitr)

setwd('D:/Git/STAT_5015_homework/02_data_munging_summarizing_R_git')

Sensory <- read.delim('Sensory.dat')

Sensory2 <- read.csv('1.csv')

Sensory2$Item = as.character(Sensory2$Item)

TidySensory <- gather(Sensory2, Operator,Value, -Item)

summarize(TidySensory,mean(Value),min(Value),max(Value),quantile(Value))

summary(TidySensory)

knitr::kable(summary(TidySensory), caption="Sensory data summary")


