#Visualization
library(ggplot2)
library(ggthemes)
library(scales)

#classification algorithm
library(randomForest)

#date time library load
library(lubridate)

library(plyr)
library(readr)


extractFeatures <- function(data){
  features <- c ("season","rentMonth","rentHour", "rentWeekday")
  
  return(data[, features])
}

trainData <- read.csv("C:\\Users\\miw52\\Desktop\\Tashu2017_Prediction_Model\\data\\station3\\tashu_stat3_trainData.csv", stringsAsFactors = F)
testData <- read.csv("C:\\Users\\miw52\\Desktop\\Tashu2017_Prediction_Model\\data\\station3\\tashu_stat3_testData.csv", stringsAsFactors = F)

trainData$rentWeekday <- wday(trainData$datetime, label = TRUE)
testData$rentWeekday <- wday(testData$datetime, label = TRUE)
monthList <- unique(testData$rentMonth)
monthList <- monthList[!is.na(monthList)]


for (i_month in monthList){
  locs <- testData$rentMonth == i_month
  testSubSet <- testData[locs,]
  
  rf <- randomForest(extractFeatures(trainData),trainData$rentCount, ntree = 50)
  testData[locs,"rentCount"] <- predict(rf, extractFeatures(testSubSet))
}


realData <- read.csv('C:\\Users\\miw52\\Desktop\\Tashu2017_Prediction_Model\\data\\station3\\tashu_stat3_2015Data.csv', stringsAsFactors = F)

realData<- realData[realData$rentMonth == 3,]
testData <- testData[testData$rentMonth == 3,]
compareDF <- data.frame(datatime = realData$datetime, weekday = wday(realData$datetime),realData_rentCount = realData$rentCount, predict_rentCount = testData$rentCount)
trainData <- trainData[trainData$rentMonth == 3,]

day_summary <- ddply(trainData,.(rentWeekday, rentHour),summarise, rentCount = mean(rentCount))
ggplot(trainData, aes(x = rentHour, y = rentCount, colour = rentWeekday))+geom_point(data = day_summary, aes(group = rentWeekday))+geom_line(data = day_summary, aes(group=rentWeekday))+scale_x_discrete("Hour")+scale_y_continuous("Count")+theme_minimal()


day_summary <- ddply(testData,.(rentWeekday, rentHour),summarise, rentCount = mean(rentCount))
ggplot(testData, aes(x = rentHour, y = rentCount, colour = rentWeekday))+geom_point(data = day_summary, aes(group = rentWeekday))+geom_line(data = day_summary, aes(group=rentWeekday))+scale_x_discrete("Hour")+scale_y_continuous("Count")+theme_minimal()


realData$rentWeekDay <- wday(realData$datetime, label = TRUE)
day_summary <- ddply(realData,.(rentWeekDay, rentHour),summarise, rentCount = mean(rentCount))
ggplot(realData, aes(x = rentHour, y = rentCount, colour = rentWeekday))+geom_point(data = day_summary, aes(group = rentWeekday))+geom_line(data = day_summary, aes(group=rentWeekday))+scale_x_discrete("Hour")+scale_y_continuous("Count")+theme_minimal()


