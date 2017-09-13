library(dplyr)
library(tidyr)
library(lubridate)
setwd('D:/Git')


Personal_car_detail <- read.csv('Personenauto_basisdata.csv')
Observed_Defects <- read.csv('Open_Data_RDW__Geconstateerde_Gebreken.csv')
Defect_Details <- read.csv('Open_Data_RDW__Gebreken.csv')

total1 <- merge(Personal_car_detail,Observed_Defects,by='Kenteken')
total2 <- merge(total1,Defect_Details,by='Gebrek.identificatie')

total3 <- select(total2,c('Merk','Handelsbenaming','Datum.tenaamstelling','Gebrek.identificatie','Aantal.gebreken.geconstateerd'))

total5 <- na.omit(total3)

###############
#temp_total <- head(total5)
#newformdate <- dmy(temp_total$Datum.tenaamstelling)
#temp_total2 <- cbind(temp_total,newformdate)
#temp_total2[format(temp_total2$newformdate,format = '%Y')=='2017',]
###############
temp_total <- total5
newformdate <- dmy(temp_total$Datum.tenaamstelling)
temp_total2 <- cbind(temp_total,newformdate)
total6 <- temp_total2[format(temp_total2$newformdate,format = '%Y')=='2017',]


length(levels(factor(total6$Merk)))
length(levels(factor(total6$Handelsbenaming)))

unique(total6$Gebrek.identificatie)
sort(table(total6$Gebrek.identificatie),decreasing = TRUE)
##############
temp_model <- filter(total6, Gebrek.identificatie == 'K05')
temp2 <- sort(table(temp_model$Handelsbenaming),decreasing = TRUE)
head(temp2)
temp_model2 <- filter(total6, Handelsbenaming == '206; 1.4 3DRS')