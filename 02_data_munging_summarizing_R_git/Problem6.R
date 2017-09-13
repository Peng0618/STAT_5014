# Path to data
library(swirl)
.datapath <- file.path(path.package('swirl'), 'Courses',
                       'R_Programming_E', 'Looking_at_Data',
                       'plant-data.txt')
# Read in data
plants <- read.csv(.datapath, strip.white=TRUE, na.strings="")
# Remove annoying columns
.cols2rm <- c('Accepted.Symbol', 'Synonym.Symbol')
plants <- plants[, !(names(plants) %in% .cols2rm)]
# Make names pretty
names(plants) <- c('Scientific_Name', 'Duration', 'Active_Growth_Period',
                   'Foliage_Color', 'pH_Min', 'pH_Max',
                   'Precip_Min', 'Precip_Max',
                   'Shade_Tolerance', 'Temp_Min_F')



plants_2 <- plants[,c('Foliage_Color','pH_Min','pH_Max')]
plants_3 <- na.omit(plants_2)


#color_list <- levels(factor(plants_3$Foliage_Color))


pH_mean <- (plants_3$pH_Min + plants_3$pH_Max)/2
plants_4 <- cbind(pH_mean,plants_3)

# White-Gray Gray-Green Green Dark Green Yellow-Green Red 
# 6           4         3       1           2         5

# Dark Green  Yellow-Green  Green   Gray-Green    Red   White-Gray
# 1           2             3       4             5     6
color_code <- gsub('Dark Green', '1', plants_3$Foliage_Color, fixed=TRUE)
color_code <- gsub('Yellow-Green', '2', color_code, fixed=TRUE)
color_code <- gsub('Gray-Green', '4', color_code, fixed=TRUE)
color_code <- gsub('Red', '5', color_code, fixed=TRUE)
color_code <- gsub('White-Gray', '6', color_code, fixed=TRUE)
color_code <- gsub('Green', '3', color_code, fixed=TRUE)

plants_5 <- cbind(plants_4,color_code)
plot(plants_5[,1],plants_5[,5])

pH <- as.numeric(plants_5[,1])
color <- as.numeric(plants_5[,5])
plants_6 <- cbind(pH,color)

fit <- lm(color ~ pH,data.frame(plants_6))
summary(fit)

fit2 <- aov(color ~ pH)
summary(fit2)
