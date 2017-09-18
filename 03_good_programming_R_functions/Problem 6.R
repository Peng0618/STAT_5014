datasum <- function(dev1,dev2) {
    m1 <- mean(dev1)
    m2 <- mean(dev2)
    sd1 <- sd(dev1)
    sd2 <- sd(dev2)
    cov12 <- cov(dev1,dev2)/sd1/sd2
    result <- data.frame(Mean1 = m1, Std1 = sd1, Mean2 = m2, Std2 = sd2, Corr = cov12)
    return(result)
}


RawData <- readRDS('HW3_data.rds')

Observer <- NULL
Mean1 <- NULL
STD1 <- NULL
Mean2 <- NULL
STD2 <- NULL
Correlation <- NULL
    
for(i in 1:13){
    Obs <- subset(RawData,RawData$Observer == i)
    statresult <- datasum(Obs$dev1,Obs$dev2)
    
    Observer <- rbind(Observer, i)
    Mean1 <- rbind(Mean1, statresult$Mean1)
    STD1 <- rbind(STD1,statresult$Std1)
    Mean2 <- rbind(Mean2,statresult$Mean2)
    STD2 <- rbind(STD2,statresult$Std2)
    Correlation <- rbind(Correlation,statresult$Corr)
}

FinalResult <- data.frame(Observer = Observer, Mean1 = Mean1, Std1 = STD1, Mean2 = Mean2, 
                     Std2 = STD2, Cov = Correlation)


SpreadMean <- FinalResult$Mean2 - FinalResult$Mean1
SpreadSTD <- FinalResult$Std2 - FinalResult$Std1




