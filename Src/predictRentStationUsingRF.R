#Visualization
library(ggplot2)
library('ggthemes')
library('scales')

#classification algorithm
library('randomForest')

#date time library load
library('lubridate')

#Create train Data
tashu2013Data <- read.csv('C:\\Users\\miw52\\Desktop\\data\\tashu2013_1.csv', stringsAsFactors = F)
tashu2014Data <- read.csv('C:\\Users\\miw52\\Desktop\\data\\tashu2014_1.csv', stringsAsFactors = F)

tashu2013Data$rentDateTime <- ymd_hms(tashu2013Data$RENT_DATE)
tashu2013Data$rentWeekday <- wday(tashu2013Data$rentDateTime)

tashu2014Data$rentDateTime <- ymd_hms(tashu2014Data$RENT_DATE)
tashu2014Data$rentWeekday <- wday(tashu2014Data$rentDateTime)

tashu2013Data <- na.omit(tashu2013Data)
tashu2014Data <- na.omit(tashu2014Data)

rentStation <- 48

Locs <- tashu2013Data$RENT_STATION == rentStation
tashu2013Data <- tashu2013Data[Locs,]

Locs <- tashu2014Data$RENT_STATION == rentStation
tashu2014Data <- tashu2014Data[Locs,]

startDateTime <- ymd_hms(20130101000000)
endDateTime <- ymd_hms(20130630230000)
currentDateTime <- startDateTime

trainData = data.frame(datetime = as.Date(character()),renthour = integer(), rentWeekday = integer(), rentCount = integer())

while (currentDateTime < endDateTime){
  nextDateTime <- currentDateTime+hours(1)
  
  locCondition  <- tashu2013Data$rentDateTime >= currentDateTime & tashu2013Data$rentDateTime < nextDateTime
  trainSubSet <- tashu2013Data[locCondition,]
  
  trainData <- rbind(trainData,data.frame(datetime = currentDateTime,renthour = hour(ymd_hms(currentDateTime)),rentWeekday = wday(currentDateTime),rentCount = NROW(trainSubSet)))
  
  currentDateTime <- nextDateTime
}
tmpLocs <- is.na(trainData$renthour)
trainData[tmpLocs,"renthour"]<- 0

#Create testData
testData = data.frame(datetime = as.Date(character()),renthour = integer(), rentWeekday = integer(), rentCount = integer())

startDateTime <- ymd_hms(20140101000000)
endDateTime <- ymd_hms(20140630230000)
currentDateTime <- startDateTime

while (currentDateTime < endDateTime){
  nextDateTime <- currentDateTime+hours(1)
  
  testData <- rbind(testData,data.frame(datetime = currentDateTime,renthour = hour(ymd_hms(currentDateTime)),rentWeekday = wday(currentDateTime),rentCount = NA))
  
  currentDateTime <- nextDateTime
}

tmpLocs <- is.na(testData$renthour)
testData[tmpLocs,"renthour"]<- 0

weekDayList <- unique(testData$rentWeekday)
weekDayList <- weekDayList[!is.na(weekDayList)]
hourList <- unique(testData$renthour)
hourList <- hourList[!is.na(hourList)]

extractFeatures <- function(data){
  features <- c ("renthour", "rentWeekday")
  
  return(data[, features])
}

for (i_weekday in weekDayList){
  for (i_hour in hourList){
    locs <- testData$rentWeekday == i_weekday & testData$renthour == i_hour
    testSubSet <- testData[locs,]
    
    rf <- randomForest(extractFeatures(trainData),trainData$rentCount, ntree = 50)
    testData[locs, "rentCount"] <- predict(rf, extractFeatures(testSubSet))
  }
}

write.csv(testData, file = "myresult.csv_48", row.names=FALSE)
