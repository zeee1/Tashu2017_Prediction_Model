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
  features <- c ("season","rentMonth","rentHour", "rentWeekday","temperature", "humidity", "rainfall")
  
  return(data[, features])
}

trainData <- read.csv("C:\\Users\\miw52\\Desktop\\Tashu2017_Prediction_Model\\data\\station55\\tashu_stat55_trainData.csv", stringsAsFactors = F)
testData <- read.csv("C:\\Users\\miw52\\Desktop\\Tashu2017_Prediction_Model\\data\\station55\\tashu_stat55_testData.csv", stringsAsFactors = F)

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


realData <- read.csv('C:\\Users\\miw52\\Desktop\\Tashu2017_Prediction_Model\\data\\station55\\tashu_stat55_2015Data.csv', stringsAsFactors = F)

realData<- realData[realData$rentMonth == 5,]
testData <- testData[testData$rentMonth == 5,]
compareDF <- data.frame(datatime = realData$datetime, weekday = wday(realData$datetime),realData_rentCount = realData$rentCount, predict_rentCount = testData$rentCount)
trainData <- trainData[trainData$rentMonth == 5,]

day_summary <- ddply(trainData,.(rentWeekday, rentHour),summarise, rentCount = mean(rentCount))
ggplot(trainData, aes(x = rentHour, y = rentCount, colour = rentWeekday))+geom_point(data = day_summary, aes(group = rentWeekday))+geom_line(data = day_summary, aes(group=rentWeekday))+scale_x_discrete("Hour")+scale_y_continuous("Count")+theme_minimal()


day_summary <- ddply(testData,.(rentWeekday, rentHour),summarise, rentCount = mean(rentCount))
ggplot(testData, aes(x = rentHour, y = rentCount, colour = rentWeekday))+geom_point(data = day_summary, aes(group = rentWeekday))+geom_line(data = day_summary, aes(group=rentWeekday))+scale_x_discrete("Hour")+scale_y_continuous("Count")+theme_minimal()


realData$rentWeekday <- wday(realData$datetime, label = TRUE)
day_summary <- ddply(realData,.(rentWeekday, rentHour),summarise, rentCount = mean(rentCount))
ggplot(realData, aes(x = rentHour, y = rentCount, colour = rentWeekday))+geom_point(data = day_summary, aes(group = rentWeekday))+geom_line(data = day_summary, aes(group=rentWeekday))+scale_x_discrete("Hour")+scale_y_continuous("Count")+theme_minimal()

rf <- randomForest(extractFeatures(trainData),trainData$rentCount, ntree = 50, importance=TRUE)
imp <- importance(rf, type=1)
featureImportance <- data.frame(Feature=row.names(imp), Importance=imp[,1])

ggplot(featureImportance, aes(x=reorder(Feature, Importance), y=Importance)) +
  geom_bar(stat="identity", fill="#53cfff") +
  coord_flip() + 
  theme_light(base_size=20) +
  xlab("Importance") +
  ylab("") + 
  ggtitle("Random Forest Feature Importance\n") +
  theme(plot.title=element_text(size=18))
ggsave("2_feature_importance.png", p)
