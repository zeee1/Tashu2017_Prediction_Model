#Visualization
library(ggplot2)
library('ggthemes')
library('scales')

#classification algorithm
library('randomForest')

#date time library load
library('lubridate')

#tashu2013Data for train, tashu2014Data for test
tashu2013Data <- read.csv('../data/tashu2013_1.csv', stringsAsFactors = F)
tashu2014Data <- read.csv('../data/tashu2014_1.csv', stringsAsFactors = F)

tashu2013Data$rentDateTime <- ymd_hms(tashu2013Data$RENT_DATE)
tashu2013Data$rentWeekday <- wday(tashu2013Data$rentDateTime)

tashu2014Data$rentDateTime <- ymd_hms(tashu2014Data$RENT_DATE)
tashu2014Data$rentWeekday <- wday(tashu2014Data$rentDateTime)

tashu2013Data <- na.omit(tashu2013Data)
tashu2014Data <- na.omit(tashu2014Data)

#Create train Data
rentStation <- 3

Locs <- tashu2013Data$RENT_STATION == rentStation
tashu2013Data <- tashu2013Data[Locs,]

Locs <- tashu2014Data$RENT_STATION == rentStation
tashu2014Data <- tashu2014Data[Locs,]

startDateTime <- ymd_hms(20130101000000)
endDateTime <- ymd_hms(20130630230000)
currentDateTime <- startDateTime

trainData = data.frame(datetime = as.Date(character()),rentMonth = character(), rentHour = character(), holiday = character(), rentCount = integer())

while (currentDateTime <= endDateTime){
  nextDateTime <- currentDateTime+hours(1)
  
  locCondition  <- tashu2013Data$rentDateTime >= currentDateTime & tashu2013Data$rentDateTime < nextDateTime
  trainSubSet <- tashu2013Data[locCondition,]
  isWeekend <- '0'
  
  if(wday(currentDateTime) >= 6){
    isWeekend <- '1'
  }
  
  trainData <- rbind(trainData,data.frame(datetime = currentDateTime,rentMonth = toString(month(currentDateTime)),rentHour = toString(hour(currentDateTime)),holiday = isWeekend,rentCount = NROW(trainSubSet)))
  
  currentDateTime <- nextDateTime
}

print("Complete making trainData")

#Create testData
testData = data.frame(datetime = as.Date(character()),rentMonth = character(),rentHour = character(), holiday = character(), rentCount = integer())

startDateTime <- ymd_hms(20140101000000)
endDateTime <- ymd_hms(20140630230000)
currentDateTime <- startDateTime

while (currentDateTime < endDateTime){
  nextDateTime <- currentDateTime+hours(1)
  
  isWeekend <- '0'
  if(wday(currentDateTime) >= 6){
    isWeekend <- '1'
  }
  testData <- rbind(testData,data.frame(datetime = currentDateTime,rentMonth = toString(month(currentDateTime)),rentHour = toString(hour(currentDateTime)),holiday = isWeekend,rentCount = NA))
  
  currentDateTime <- nextDateTime
}

print("Complete making testData")


monthList <- unique(testData$rentMonth)
monthList <- monthList[!is.na(monthList)]


extractFeatures <- function(data){
  features <- c ("rentMonth","rentHour", "holiday")
  
  return(data[, features])
}

for (i_month in monthList){
  locs <- testData$rentMonth == i_month
  testSubSet <- testData[locs,]
    
  rf <- randomForest(extractFeatures(trainData),trainData$rentCount, ntree = 100)
  testData[locs, "rentCount"] <- predict(rf, extractFeatures(testSubSet))
}

write.csv(testData, file = "myresult_3+month_1.csv", row.names=FALSE)

month <- 4

realData <- read.csv('data/tashu2014_rentCountPerHour.csv', stringsAsFactors = F)

realData<- realData[realData$rentMonth == 4,]
testData <- testData[testData$rentMonth == 4,]
compareDF <- data.frame(datatime = realData$datetime, weekday = wday(realData$datetime),realData_rentCount = realData$rentCount, predict_rentCount = testData$rentCount)

#ggplot(testData, aes(x = rentHour))+geon_bar(stat= "identity", position = "dodge")+labs(x = 'hour of Day')