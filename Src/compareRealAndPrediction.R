library(ggplot2)
library(lubridate)
library(plyr)
library(readr)

#Variation
station <- 55
month <- 4

realRentCountDataPath <- paste("../data/station",toString(station),"/tashu_stat",toString(station),"_rent2014Data.csv", sep="",collapse = NULL)
realReturnCountDataPath <- paste("../data/station",toString(station),"/tashu_stat",toString(station),"_return2014Data.csv", sep="",collapse = NULL)
predictiveRentCountDataPath <- paste("../data/station",toString(station),"/tashu_stat",toString(station),"_rentpredictionResult_2014.csv",sep="",collapse = NULL)
predictiveReturnCountDataPath <- paste("../data/station",toString(station),"/tashu_stat",toString(station),"_returnpredictionResult_2014.csv",sep="",collapse = NULL)

#get real rent/return Count Per Hour
real_rentCountData <- read.csv(realRentCountDataPath, stringsAsFactors = F)
real_returnCountData <- read.csv(realReturnCountDataPath, stringsAsFactors = F)

#get predictive rent/return Count per hour
predictive_rentCountData <- read.csv(predictiveRentCountDataPath, stringsAsFactors = F)
predictive_returnCountData <- read.csv(predictiveReturnCountDataPath, stringsAsFactors = F)

#Visualize real rent data and predictive rent data
day_summary <- ddply(real_rentCountData,.(rentWeekday, rentHour),summarise, rentCount = mean(rentCount))
ggplot(real_rentCountData, aes(x = rentHour, y = rentCount, colour = rentWeekday))+geom_point(data = day_summary, aes(group = rentWeekday))+geom_line(data = day_summary, aes(group=rentWeekday))+scale_x_discrete("Hour")+scale_y_continuous("Count")+theme_minimal()

predictive_rentCountData$rentWeekday <- wday(predictive_rentCountData$datetime, label = TRUE)
day_summary <- ddply(predictive_rentCountData,.(rentWeekday, rentHour),summarise, rentCount = mean(rentCount))
ggplot(predictive_rentCountData, aes(x = rentHour, y = rentCount, colour = rentWeekday))+geom_point(data = day_summary, aes(group = rentWeekday))+geom_line(data = day_summary, aes(group=rentWeekday))+scale_x_discrete("Hour")+scale_y_continuous("Count")+theme_minimal()

#Visualize real return data and predictive return data
day_summary <- ddply(real_returnCountData,.(returnWeekday, returnHour),summarise, returnCount = mean(returnCount))
ggplot(real_rentCountData, aes(x = returnHour, y = returnCount, colour = returnWeekday))+geom_point(data = day_summary, aes(group = returnWeekday))+geom_line(data = day_summary, aes(group=returnWeekday))+scale_x_discrete("Hour")+scale_y_continuous("Count")+theme_minimal()

predictive_returnCountData$rentWeekday <- wday(predictive_returnCountData$datetime, label = TRUE)
day_summary <- ddply(predictive_returnCountData,.(returnWeekday, returnHour),summarise, returnCount = mean(returnCount))
ggplot(predictive_returnCountData, aes(x = returnHour, y = returnCount, colour = returnWeekday))+geom_point(data = day_summary, aes(group = returnWeekday))+geom_line(data = day_summary, aes(group=returnWeekday))+scale_x_discrete("Hour")+scale_y_continuous("Count")+theme_minimal()

#Todo : check the accuracy between real data and prediction data