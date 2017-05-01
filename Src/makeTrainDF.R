library(scales)
library(randomForest)
library(lubridate)
library(plyr)
library(readr)

#Make Train DataFrame(20130101~20141231) : rentTrainDF, returnTrainDF

for (i_station in 1:144){
  rentSubsetInTrain <- tashu20132014Data[tashu20132014Data$RENT_STATION == i_station,]
  returnSubsetInTrain <- tashu20132014Data[tashu20132014Data$RETURN_STATION == i_station,]
  
  startDateTime <- ymd_hms(20130101000000)
  endDateTime <- ymd_hms(20141231230000)
  currentDateTime <- startDateTime
  
  rentTrainDF <- data.frame(datetime = as.Date(character()),
                            season = character(), 
                            rentMonth = character(), 
                            rentHour = character(),
                            rentWeekday = character(),
                            temperature = integer(),
                            humidity = integer(),
                            rainfall = integer(),
                            isFestival = character(),
                            rentCount = integer())
  
  returnTrainDF <- data.frame(datetime = as.Date(character()),
                              season = character(), 
                              returnMonth = character(),
                              returnHour = character(),
                              returnWeekday = character(),
                              isFestival = character(), 
                              temperature = integer(), 
                              humidity = integer(),
                              rainfall = integer(), 
                              returnCount = integer())
  
  while (currentDateTime <= endDateTime){
    nextDateTime <- currentDateTime+hours(1)
    
    rentTimeSubset <- rentSubsetInTrain[rentSubsetInTrain$rentDateTime >= currentDateTime & rentSubsetInTrain$rentDateTime < nextDateTime,]
    returnTimeSubset <- returnSubsetInTrain[returnSubsetInTrain$returnDateTime >= currentDateTime & returnSubsetInTrain$returnDateTime < nextDateTime,]
    
    weatherSubset <- data.frame()
    
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
    
    isFestival <- '0'
    tmpFestData <- festivalData[festivalData$startDate <= currentDateTime & festivalData$endDate > currentDateTime,]
    nearStatList <- strsplit(tmpFestData$nearStation,",")
    for (i in nearStatList){
      if(toString(i_station) %in% i){
        isFestival <-'1'
      }
    }
    
    rentTrainDF <- rbind(rentTrainDF,data.frame(datetime = currentDateTime,season = season, 
                                                rentMonth = toString(month(currentDateTime)),
                                                rentHour = toString(hour(currentDateTime)),
                                                rentWeekday = wday(currentDateTime, label = TRUE),
                                                temperature = weatherSubset$Temperature,
                                                humidity= weatherSubset$Humidity,rainfall = weatherSubset$Rainfall,isFestival = isFestival,
                                                rentCount = NROW(rentTimeSubset)))
    
    returnTrainDF <- rbind(returnTrainDF,data.frame(datetime = currentDateTime,season = season, 
                                                    returnMonth = toString(month(currentDateTime)),
                                                    returnHour = toString(hour(currentDateTime)),
                                                    returnWeekday = wday(currentDateTime, label = TRUE),
                                                    temperature = weatherSubset$Temperature,
                                                    humidity= weatherSubset$Humidity,rainfall = weatherSubset$Rainfall,isFestival = isFestival,
                                                    returnCount = NROW(returnTimeSubset)))
    
    currentDateTime <- nextDateTime
  }
  
  assign(paste("stat",toString(i_station),"_rentTrainDF",sep="",collapse = NULL), rentTrainDF)
  assign(paste("stat",toString(i_station),"_returnTrainDF",sep="",collapse = NULL), returnTrainDF)
  print(paste(toString(i_station)," Train data frame was created.", sep="", collapse = NULL))
}