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
stationList <- c(1,3,9,17,21,24,55, 118, 122,123, 127, 143)
  # park  : 1,3
  # company : 24, 123
  # downtown : 17, 143
  # University : 122(hannam), 55(chungnam), 21(kaist)
  # Living : 9
  # Terminal / Train Station : 118(Bokhab terminal), 127(sintanjin train station)

tashu20132014Data <- read.csv("../data/tashu20132014.csv", stringsAsFactors = F)
tashu20132014Data <- na.omit(tashu20132014Data)
tashu20132014Data$rentDateTime <- ymd_hms(tashu20132014Data$RENT_DATE)
tashu20132014Data$returnDateTime <- ymd_hms(tashu20132014Data$RETURN_DATE)

tashu2015Data <- read.csv("../data/tashu2015.csv", stringsAsFactors = F)
tashu2015Data <- na.omit(tashu2015Data)
tashu2015Data$rentDateTime <- ymd_hms(tashu2015Data$RENT_DATE)
tashu2015Data$returnDateTime <- ymd_hms(tashu2015Data$RETURN_DATE)

weather2013Data <- read.csv("../data/weather/2013_weatherData.csv", stringsAsFactors = F)
weather2014Data <- read.csv("../data/weather/2014_weatherData.csv", stringsAsFactors = F)
weather2015Data <- read.csv("../data/weather/2015_weatherData.csv", stringsAsFactors = F)
weather2013Data$DT <- ymd_hm(weather2013Data$Datetime)
weather2014Data$DT <- ymd_hm(weather2014Data$Datetime)
weather2015Data$DT <- ymd_hm(weather2015Data$Datetime)
weather2013Data <- weather2013Data[minute(weather2013Data$DT) == 0,]
weather2014Data <- weather2014Data[minute(weather2014Data$DT) == 0,]
weather2015Data <- weather2015Data[minute(weather2015Data$DT) == 0,]

festivalData <- read.csv("../data/festival_info.csv", stringsAsFactors = F)
#festivalData$nearStation <- strsplit(festivalData$nearStation, ",")

testlocs <- grepl(festivalData$nearStation, toString(3)) == TRUE
festivalData <- festivalData[testlocs,]


#Feature extract Function
extractFeatures <- function(data){
  features <- c ("season","rentMonth","rentHour", "rentWeekday","temperature", "humidity", "rainfall")
  
  return(data[, features])
}

for (i_station in stationList){
  # Make Train DataFrame(20130101~20141231) : rentTrainDF, returnTrainDF
  rentSubsetInTrain <- tashu20132014Data[tashu20132014Data$RENT_STATION == i_station,]
  returnSubsetInTrain <- tashu20132014Data[tashu20132014Data$RETURN_STATION == i_station,]
  
  startDateTime <- ymd_hms(20130101000000)
  endDateTime <- ymd_hms(20141231230000)
  currentDateTime <- startDateTime
  
  rentTrainDF <- data.frame(datetime = as.Date(character()),season = character(), rentMonth = character(), rentHour = character(),rentWeekday = character(),temperature = integer(), humidity = integer(),rainfall = integer(), rentCount = integer())
  returnTrainDF <- data.frame(datetime = as.Date(character()),season = character(), rentMonth = character(), rentHour = character(),rentWeekday = character(),temperature = integer(), humidity = integer(),rainfall = integer(), returnCount = integer())
  
  while (currentDateTime <= endDateTime){
    nextDateTime <- currentDateTime+hours(1)
    
    rentTimeSubset <- rentSubsetInTrain[rentSubsetInTrain$rentDateTime >= currentDateTime & rentSubsetInTrain$rentDateTime < nextDateTime,]
    returnTimeSubset <- returnSubsetInTrain[returnSubsetInTrain$returnDateTime >= currentDateTime & returnSubsetInTrain$returnDateTime < nextDateTime]
    
    weatherSubset
    if (year(currentDateTime) == 2013){
      weatherSubset <- weather2013Data[weather2013Data$DT == currentDateTime,]
    }
    if (year(currentDateTime) == 2014){
      weatherSubset <- weather2014Data[weather2014Data$DT == currentDateTime,]
    }
    
    
    if(is.na(weatherSubset$Rainfall)){
      weatherSubset$Rainfall <- 0
    }
    
    season <- '0'
    currentMonth <- month(currentDateTime)
    
    if(currentMonth >= 3 && currentMonth < 6){
      season <- '1'#spring
    }
    if(currentMonth >= 6 && currentMonth < 9){
      season <- '2'#summer
    }
    if(currentMonth >= 9 && currentMonth < 12){
      season <- '3'#fall
    }
    if(currentMonth >= 11 || currentMonth < 3){
      season <- '4'#winter
    }
    
    rentTrainDF <- rbind(rentTrainDF,data.frame(datetime = currentDateTime,season = season, 
                                                rentMonth = toString(month(currentDateTime)),
                                                rentHour = toString(hour(currentDateTime)),
                                                rentWeekday = wday(currentDateTime, label = TRUE),
                                                temperature = weatherSubset$Temperature,
                                                humidity= weatherSubset$Humidity,rainfall = weatherSubset$Rainfall,
                                                rentCount = NROW(rentTimeSubset)))
    returnTrainDF <- rbind(returnTrainDF,data.frame(datetime = currentDateTime,season = season, 
                                                rentMonth = toString(month(currentDateTime)),
                                                rentHour = toString(hour(currentDateTime)),
                                                rentWeekday = wday(currentDateTime, label = TRUE),
                                                temperature = weatherSubset$Temperature,
                                                humidity= weatherSubset$Humidity,rainfall = weatherSubset$Rainfall,
                                                returnCount = NROW(returnTimeSubset)))
    
    currentDateTime <- nextDateTime
  }
  
  # Make Test DataFrame(20150101~20151231) - rentTestDF, returnTestDF
  rentSubsetInTest <- tashu2015Data[tashu2015Data$RENT_STATION == i_station,]
  returnSubsetInTest <- tashu2015Data[tashu2015Data$RETURN_STATION == i_station]
  
  startDateTime <- ymd_hms(20150101000000)
  endDateTime <- ymd_hms(20151231230000)
  currentDateTime <- startDateTime
  
  rentTestDF <- data.frame(datetime = as.Date(character()),season = character(),rentMonth = character(), rentHour = character(), rentWeekday = character(), temperature = integer(), humidity = integer(),rainfall = integer(), rentCount = integer())
  returnTestDF <- data.frame(datetime = as.Date(character()),season = character(),rentMonth = character(), rentHour = character(), rentWeekday = character(), temperature = integer(), humidity = integer(),rainfall = integer(), returnCount = integer())
  
  while (currentDateTime <= endDateTime){
    nextDateTime <- currentDateTime+hours(1)
    
    rentTimeSubset <- rentSubsetInTest[rentSubsetInTest$rentDateTime >= currentDateTime & rentSubsetInTest$rentDateTime < nextDateTime,]
    returnTimeSubset <- returnSubsetInTest[returnSubsetInTest$returnDateTime >= currentDateTime & returnSubsetInTest$returnDateTime < nextDateTime]
    
    weatherSubset <- weather2015Data[weather2015Data$DT == currentDateTime,]
    
    if(is.na(weatherSubset$Rainfall)){
      weatherSubset$Rainfall <- 0
    }
    
    season <- '0'
    currentMonth <- month(currentDateTime)
    
    if(currentMonth >= 3 && currentMonth < 6){
      season <- '1'#spring
    }
    if(currentMonth >= 6 && currentMonth < 9){
      season <- '2'#summer
    }
    if(currentMonth >= 9 && currentMonth < 12){
      season <- '3'#fall
    }
    if(currentMonth >= 11 || currentMonth < 3){
      season <- '4'#winter
    }
    
    rentTestDF <- rbind(rentTestDF,data.frame(datetime = currentDateTime,season = season, 
                                                rentMonth = toString(month(currentDateTime)),
                                                rentHour = toString(hour(currentDateTime)),
                                                rentWeekday = wday(currentDateTime, label = TRUE),
                                                temperature = weatherSubset$Temperature,
                                                humidity= weatherSubset$Humidity,rainfall = weatherSubset$Rainfall,
                                                rentCount = NROW(rentTimeSubset)))
    returnTestDF <- rbind(returnTestDF,data.frame(datetime = currentDateTime,season = season, 
                                                    rentMonth = toString(month(currentDateTime)),
                                                    rentHour = toString(hour(currentDateTime)),
                                                    rentWeekday = wday(currentDateTime, label = TRUE),
                                                    temperature = weatherSubset$Temperature,
                                                    humidity= weatherSubset$Humidity,rainfall = weatherSubset$Rainfall,
                                                    returnCount = NROW(returnTimeSubset)))
    
    currentDateTime <- nextDateTime
  }
  
  #Copy real Data to realRentDF, realReturnDF
  realRentDF <- rentTestDF
  realReturnDF <- returnTestDF
  rentTestDF$rentCount <- NA
  returnTestDF$returnCount <- NA
  
  #Todo : Write result of prediction into File. Save
  monthList <- unique(rentTestDF$rentMonth)
  monthList <- monthList[!is.na(monthList)]
  
  
  for(i_month in monthList){
    locs <- rentTestDF$rentMonth == i_month
    testSubSet <- rentTestDF[locs,]
    
    rf <- randomForest(as.factor(rentCount)~rentMonth+rentWeekday+temperature+humidity+rentHour+rainfall+season,data=rentTestDF, ntree = 50, mtry = 2)
    rentTestDF[locs,"rentCount"] <- predict(rf, extractFeatures(testSubSet))
    
    locs <- returnTestDF$rentMonth == i_month
    testSubSet <- returnTestDF[locs,]
    
    rf <- randomForest(as.factor(returnCount)~rentMonth+rentWeekday+temperature+humidity+rentHour+rainfall+season,data=returnTrainDF, ntree = 50, mtry = 2)
    returnTestDF[locs,"returnCount"] <- predict(rf, extractFeatures(testSubSet))
  }
  
  write.csv(rentTestDF, file = paste("stat",toString(i_station),"_ClassificationPredict_rentResult.csv",sep="",collapse = NULL), row.names=FALSE)
  write.csv(returnTestDF, file = paste("stat",toString(i_station),"_ClassificationPredict_returnResult.csv",sep="",collapse = NULL), row.names=FALSE)

  #Todo : Check Importance of features.
  #Todo : Check prediction accuracy.
  #Todo : Visualize real/prediction/train data by Month and Weekdays. Save.
}