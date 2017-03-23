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

#Feature extract Function
extractRentFeatures <- function(data){
  features <- c ("season","rentMonth","rentHour", "rentWeekday","temperature", "humidity", "rainfall")
  
  return(data[, features])
}

extractReturnFeatures <- function(data){
  features <- c ("season","returnMonth","returnHour", "returnWeekday","temperature", "humidity", "rainfall")
  
  return(data[, features])
}

for (i_station in 1:144){
  #Make Test DataFrame(20150101~20151231) - rentTestDF, returnTestDF
  rentSubsetInTest <- tashu2015Data[tashu2015Data$RENT_STATION == i_station,]
  returnSubsetInTest <- tashu2015Data[tashu2015Data$RETURN_STATION == i_station,]
  
  startDateTime <- ymd_hms(20150101000000)
  endDateTime <- ymd_hms(20151231230000)
  currentDateTime <- startDateTime
  
  rentTestDF <- data.frame(datetime = as.Date(character()),season = character(),rentMonth = character(), rentHour = character(), 
                           rentWeekday = character(), temperature = integer(), 
                           humidity = integer(),rainfall = integer(), rentCount = integer())
  returnTestDF <- data.frame(datetime = as.Date(character()),season = character(),returnMonth = character(), returnHour = character(), 
                             returnWeekday = character(), temperature = integer(), 
                             humidity = integer(),rainfall = integer(), returnCount = integer())
  
  while (currentDateTime <= endDateTime){
    nextDateTime <- currentDateTime+hours(1)
    
    rentTimeSubset <- rentSubsetInTest[rentSubsetInTest$rentDateTime >= currentDateTime & rentSubsetInTest$rentDateTime < nextDateTime,]
    returnTimeSubset <- returnSubsetInTest[returnSubsetInTest$returnDateTime >= currentDateTime & returnSubsetInTest$returnDateTime < nextDateTime,]
    
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
                                              RrentCount = NROW(rentTimeSubset), PrentCount = NA))
    returnTestDF <- rbind(returnTestDF,data.frame(datetime = currentDateTime,season = season, 
                                                  returnMonth = toString(month(currentDateTime)),
                                                  returnHour = toString(hour(currentDateTime)),
                                                  returnWeekday = wday(currentDateTime, label = TRUE),
                                                  temperature = weatherSubset$Temperature,
                                                  humidity= weatherSubset$Humidity,rainfall = weatherSubset$Rainfall,
                                                  RreturnCount = NROW(returnTimeSubset), PreturnCount = NA))
    
    currentDateTime <- nextDateTime
  }
  
  print("Creating Test DF completed")
  
  # Write result of prediction into File. Save
  monthList <- unique(rentTestDF$rentMonth)
  monthList <- monthList[!is.na(monthList)]
  
  rent_rf <- randomForest(extractRentFeatures(rentTrainDF),
                          rentTrainDF$rentCount, ntree = 50, mtry = 2, importance = TRUE)
  
  return_rf <- randomForest(extractReturnFeatures(returnTrainDF),
                            returnTrainDF$returnCount, ntree = 50, mtry = 2, importance = TRUE)
  
  # Prediction - Regression
  for (i_month in monthList){
    locs <- rentTestDF$rentMonth == i_month
    testSubSet <- rentTestDF[locs,]
    
    rentTestDF[locs,"PrentCount"] <- predict(rent_rf, extractRentFeatures(testSubSet))
    
    locs <- returnTestDF$returnMonth == i_month
    testSubSet <- returnTestDF[locs,]
    
    returnTestDF[locs,"PreturnCount"] <- predict(return_rf, extractReturnFeatures(testSubSet))
  }
  
  assign(paste("stat", toString(i_station), "_rentTestDF", sep="",collapse = NULL), rentTestDF)
  assign(paste("stat", toString(i_station), "_returnTestDF", sep="",collapse = NULL), returnTestDF)
}