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

#Variation
station <- 55
outputFilePath <- paste("../data/station",toString(station),"/tashu_stat",toString(station),"_returnpredictionResult_2014.csv",sep="",collapse = NULL)

#Feature ????
extractFeatures <- function(data){
  features <- c ("season","returnMonth","returnHour", "returnWeekday","temperature", "humidity", "rainfall")
  
  return(data[, features])
}

#load train data - rent Station : 55 rent date : 2013 01 01 ~ 2013 06 30
trainData <- read.csv("C:\\Users\\miw52\\Desktop\\Tashu2017_Prediction_Model\\data\\station55\\tashu_stat55_returnTrainData.csv", stringsAsFactors = F)


#load test data - rent Station : 55 rent date : 2014 01 01 ~ 2014 06 30
testData <- read.csv("C:\\Users\\miw52\\Desktop\\Tashu2017_Prediction_Model\\data\\station55\\tashu_stat55_returnTestData.csv", stringsAsFactors = F)


trainData$returnWeekday <- wday(trainData$datetime, label = TRUE)
testData$returnWeekday <- wday(testData$datetime, label = TRUE)

monthList <- unique(testData$returnMonth)
monthList <- monthList[!is.na(monthList)]

for (i_month in monthList){
  locs <- testData$returnMonth == i_month
  testSubSet <- testData[locs,]
  
  rf <- randomForest(extractFeatures(trainData),trainData$returnCount, ntree = 50)
  testData[locs,"returnCount"] <- predict(rf, extractFeatures(testSubSet))
}

write.csv(testData, file = outputFilePath, row.names=FALSE)

realData <- read.csv("C:\\Users\\miw52\\Desktop\\Tashu2017_Prediction_Model\\data\\station55\\tashu_stat55_return2014Data.csv", stringsAsFactors = F)
realData<- realData[realData$returnMonth == 5,]
testData <- testData[testData$returnMonth == 5,]
#compareDF <- data.frame(datatime = realData$datetime, weekday = wday(realData$datetime),realData_rentCount = realData$rentCount, predict_rentCount = testData$rentCount)
trainData <- trainData[trainData$returnMonth == 5,]

day_summary <- ddply(trainData,.(returnWeekday, returnHour),summarise, returnCount = mean(returnCount))
ggplot(trainData, aes(x = returnHour, y = returnCount, colour = returnWeekday))+geom_point(data = day_summary, aes(group = returnWeekday))+geom_line(data = day_summary, aes(group=returnWeekday))+scale_x_discrete("Hour")+scale_y_continuous("Count")+theme_minimal()

day_summary <- ddply(testData,.(returnWeekday, returnHour),summarise, returnCount = mean(returnCount))
ggplot(testData, aes(x = returnHour, y = returnCount, colour = returnWeekday))+geom_point(data = day_summary, aes(group = returnWeekday))+geom_line(data = day_summary, aes(group=returnWeekday))+scale_x_discrete("Hour")+scale_y_continuous("Count")+theme_minimal()

realData$returnWeekday <- wday(realData$datetime, label = TRUE)
day_summary <- ddply(realData,.(returnWeekday, returnHour),summarise, returnCount = mean(returnCount))
ggplot(realData, aes(x = returnHour, y = returnCount, colour = returnWeekday))+geom_point(data = day_summary, aes(group = returnWeekday))+geom_line(data = day_summary, aes(group=returnWeekday))+scale_x_discrete("Hour")+scale_y_continuous("Count")+theme_minimal()

rf <- randomForest(extractFeatures(trainData),trainData$returnCount, ntree = 50, importance=TRUE)
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