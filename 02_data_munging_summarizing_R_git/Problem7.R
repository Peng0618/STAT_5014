library(dplyr)
library(tidyr)
library(lubridate)
library(data.table)
setwd('D:/Git')

#### Preparation of Raw Data
Personal_car_detail <- read.csv('Personenauto_basisdata.csv')
Observed_Defects <- read.csv('Open_Data_RDW__Geconstateerde_Gebreken.csv')
Defect_Details <- read.csv('Open_Data_RDW__Gebreken.csv')

total1 <- merge(Personal_car_detail,Observed_Defects,by='Kenteken')
total2 <- merge(total1,Defect_Details,by='Gebrek.identificatie')
total3 <- select(total2,c('Merk','Handelsbenaming','Datum.tenaamstelling','Gebrek.identificatie','Aantal.gebreken.geconstateerd'))


#### Data Clean
total5 <- na.omit(total3)
colnames(total5) <- c('Make','Model','Shooting_Date','Defect_Code','Number_of_Defects')
FormalDate <- dmy(total5$Shooting_Date)
temp_total2 <- cbind(total5,FormalDate)
total_2017 <- temp_total2[format(temp_total2$FormalDate,format = '%Y')=='2017',]

total_2017 <- mutate(total_2017,Defect_Code_char = as.character(total_2017$Defect_Code))
total_2017 <- mutate(total_2017,Make_Model = paste(Make, Model, sep="/"))

write.table(total_2017,file="total_2017",row.names=TRUE,col.names=TRUE)

#### Summary of Make and Model
length(levels(factor(total_2017$Make)))
length(levels(factor(total_2017$Model)))

#### Summary of Top 5 Defects and Make/Models
Defect_list <- sort(table(total_2017$Defect_Code),decreasing = TRUE)
Top5_defect <- data.frame(Defect_list[1:5])

Defect_list <- NULL
Target_MakeModel_List <- NULL
for (i in 1:5){
    CertainDefect <- as.character(Top5_defect[i,1])
    Defect_list <- c(Defect_list,CertainDefect)
    temp <- filter(total_2017, Defect_Code_char == CertainDefect)
    MakeModel_List_CertainDefect <- sort(table(temp$Defect_Code),decreasing = TRUE)
    
    temp_summary_2017 <- temp %>% group_by(Make_Model) %>% 
        mutate(makemodel_defect = paste(Make_Model,Defect_Code,sep="_"))
    
    makemodel_defect_summary <- sort(tapply(temp_summary_2017$Number_of_Defects,temp_summary_2017$makemodel_defect,sum),decreasing = TRUE)
    Top5_makemodel_defect <- makemodel_defect_summary[1:5]
    Target_MakeModel <- row.names(Top5_makemodel_defect)[1]
    Target_MakeModel_List <- c(Target_MakeModel_List,Target_MakeModel)
}

Relation <- cbind(Defect_list,Target_MakeModel_List)
knitr::kable(Relation, caption="TOP Make/Models of Five Common Defects")


#### lm regression between make and defect
summary_2017 <- total_2017 %>% group_by(Make_Model, Defect_Code) %>% 
    mutate(makemodel_defect = paste(Make_Model,Defect_Code,sep="_"))

summary_2017 <- summary_2017 %>% group_by(Make, Defect_Code) %>% 
    mutate(make_defect = paste(Make,Defect_Code,sep="_"))

    
make_defect_summary_2017 <- data.frame(sort(tapply(summary_2017$Number_of_Defects,summary_2017$make_defect,sum),decreasing = TRUE))
colnames(make_defect_summary_2017) <- 'Count'
NameList <- row.names(make_defect_summary_2017)
Result <- cbind(NameList,as.integer(make_defect_summary_2017$Count))
colnames(Result) <- c('NameList','Count')
lm(NameList ~ Count, data = data.frame(Result[1:10,]))
aov(NameList ~ Count, data = data.frame(Result[1:10,]))

#### lm regression between model and defect
makemodel_defect_summary_2017 <- data.frame(sort(tapply(summary_2017$Number_of_Defects,summary_2017$makemodel_defect,sum),decreasing = TRUE))
colnames(makemodel_defect_summary_2017) <- 'Count'
NameList <- row.names(makemodel_defect_summary_2017)
Result <- cbind(NameList,as.integer(makemodel_defect_summary_2017$Count))
colnames(Result) <- c('NameList','Count')
lm(Count ~ NameList, data = data.frame(Result[1:10,]))



##############
temp_model <- filter(total_2017, Defect_Code == 'K05')
temp2 <- sort(table(temp_model$Model),decreasing = TRUE)
head(temp2)
temp_model2 <- filter(total_2017, Model == '206; 1.4 3DRS')

summary1 <- total_2017 %>% group_by(Defect_Code,Make) %>% 
      summarise(make_frequency = n()) %>% group_by(Defect_Code) %>% 
      mutate(defect_frequency = sum(make_frequency)) %>% 
      mutate(top_make_number = max(make_frequency)) %>%   
      filter(make_frequency %in% top_make_number) %>% 
      arrange(desc(defect_frequency)) %>% 
      select(-top_make_number)
kable(summary1[1:5,],caption = "Top 5 mose frequent defects and the top coresponding make")

### Yueyao
s = apply(total2,MARGIN = 1,function(x) sum(is.na(x)))>0
Merged_tidy <- total2 %>% mutate(has_missing_value = s) %>%
                 filter(has_missing_value == FALSE)
Merged_tidy_2017 <- Merged_tidy[grep("2017",Merged_tidy$Datum.tenaamstelling),]
diff_makes <- n_distinct(Merged_tidy_2017$make)
diff_models <- n_distinct(Merged_tidy_2017$model)