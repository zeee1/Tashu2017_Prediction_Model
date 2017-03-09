library(lubridate)

#variation
stationList <- c(3, 55, 60)
tashu2015DataFilePath <- "../data/tashu2015.csv" #target data file
tashu20132014DataFilePath <- "../data/tashu20132014.csv" #Train data file
weather2013DataFilePath <- "../data/weather/2013_weatherData.csv"
weather2014DataFilePath <- "../data/weather/2014_weatherData.csv"
weather2015DataFilePath <- "../data/weather/2015_weatherData.csv"
realOutFilePath <- paste("../data/station",toString(station),"/tashu_stat",toString(station),"_rent_realData.csv", sep="",collapse = NULL)
trainOutFilePath <- paste("../data/station",toString(station),"/tashu_stat",toString(station),"_rent_TrainData.csv", sep="",collapse = NULL)
testOutFilePath <- paste("../data/station",toString(station),"/tashu_stat",toString(station),"_rent_TestData.csv", sep="",collapse = NULL)
resultOutFilePath <- paste("../data/station",toString(station),"/tashu_stat",toString(station),"_rent_ResultData.csv", sep="",collapse = NULL)

#load Weather Data & remove unnecessary row.
weather2013Data <- read.csv(weather2013DataFilePath, stringsAsFactors = F)
weather2014Data <- read.csv(weather2014DataFilePath, stringsAsFactors = F)
weather2015Data <- read.csv(weather2015DataFilePath, stringsAsFactors = F)
weather2013Data$DT <- ymd_hm(weather2013Data$Datetime)
weather2014Data$DT <- ymd_hm(weather2014Data$Datetime)
weather2015Data$DT <- ymd_hm(weather2015Data$Datetime)
weather2013Data <- weather2013Data[minute(weather2013Data$DT) == 0,]
weather2014Data <- weather2014Data[minute(weather2014Data$DT) == 0,]
weather2015Data <- weather2015Data[minute(weather2015Data$DT) == 0,]

#load Tashu Data & remove rows containing NA.
tashu20132014Data <- read.csv(tashu20132014DataFilePath, stringsAsFactors = F)
tashu20132014Data <- na.omit(tashu20132014Data)
tashu2015Data <- read.csv(tashu2015DataFilePath, stringsAsFactors = F)
tashu2015Data <- na.omit(tashu2015Data)

# Function : Make real data frame for tashu 2015 data
createRealData_rent <- function(outfilePath,rentStation){
  #Collect 'rentStation' Data
  Locs <- tashu2015Data$RENT_STATION == 60
  tashuData <- tashu2015Data[Locs,]
  
  #Make New DataFrame(resultDF) : datetime, rentMonth, rentHour, rentWeekday, Temperature, Humidity, rainfall,rentCount
  startDateTime <- ymd_hm(201501010000)
  endDateTime <- ymd_hm(201512312300)
  currentDateTime <- startDateTime
  
  resultDF = data.frame(datetime = as.Date(character()),season = character(),rentMonth = character(), rentHour = character(), rentWeekday = character(),temperature = integer(), humidity = integer(), rainfall = integer(), rentCount = integer())
  
  while (currentDateTime <= endDateTime){
    nextDateTime <- currentDateTime+hours(1)
    
    tashuLoc  <- tashuData$rentDateTime >= currentDateTime & tashuData$rentDateTime < nextDateTime
    dataSubSet <- tashuData[tashuLoc,]
    
    weatherSubset <- weather2015Data[weather2015Data$DT == currentDateTime,]

    if(is.na(weatherSubset$Rainfall)){
      weatherSubset$Rainfall <- 0
    }
    
    season <- '0'
    
    if(month(currentDateTime)>= 3 && month(currentDateTime) < 6){
      season <- '1'#spring
    }
    if(month(currentDateTime)>= 6 && month(currentDateTime) < 9){
      season <- '2'#summer
    }
    if(month(currentDateTime)>= 9 && month(currentDateTime) < 12){
      season <- '3'#fall
    }
    if(month(currentDateTime)>= 11 || month(currentDateTime) < 3){
      season <- '4'#winter
    }
    
    resultDF <- rbind(resultDF,data.frame(datetime = currentDateTime,season = season, rentMonth = toString(month(currentDateTime)),rentHour = toString(hour(currentDateTime)),rentWeekday = wday(currentDateTime, label = TRUE),temperature = weatherSubset$Temperature,humidity= weatherSubset$Humidity,rainfall = weatherSubset$Rainfall,rentCount = NROW(dataSubSet)))
    
    currentDateTime <- nextDateTime
  }
  
  write.csv(resultDF, file = outfilePath, row.names=FALSE)
  print("Complete Making real Data.")
}

# Function : Create train data(2013~2014)
createTrainData_rent <- function(TfilePath,WfilePath1, WfilePath2,outfilePath, rentStation){
  #load Tashu 2 year Data(2013~2014) from January to June
  tashu2yearData <- read.csv(TfilePath, stringsAsFactors = F)
  tashu2yearData <- na.omit(tashu2yearData)
  tashu2yearData$rentDateTime <- ymd_hms(tashu2yearData$RENT_DATE)

  #load weather Data 2013, 2014
  weather2013Data <- read.csv(WfilePath1, stringsAsFactors = F)
  weather2013Data$DT <- ymd_hm(weather2013Data$Datetime)
  weather2014Data <- read.csv(WfilePath2, stringsAsFactors = F)
  weather2014Data$DT <- ymd_hm(weather2014Data$Datetime)
  
  #Collect weather Data(20130101~20130630)
  weather2013Data <- weather2013Data[weather2013Data$DT < ymd_hm(201307010000),]
  #remove xx:30 Data in weather2013Data
  weather2013Data <- weather2013Data[minute(weather2013Data$DT) == 0,]
  
  #Collect weather Data(20140101~20140630)
  weather2014Data <- weather2014Data[weather2014Data$DT < ymd_hm(201407010000),]
  #remove xx:30 Data in weather2014Data
  weather2014Data <- weather2014Data[minute(weather2014Data$DT) == 0,]
  
  #Collect 'rentStation' Data
  tashu2yearData <- tashu2yearData[tashu2yearData$RENT_STATION == rentStation,]
  
  #Make Train DataFrame(trainData) : datetime(2013-01-01 00:00 ~ 2013-06-30 23:00, 2014-01-01 00:00 ~ 2014-06-30 23:00), rentMonth, rentHour, rentWeekday, Temperature, Humidity,rainfall, rentCount
  startDateTime <- ymd_hms(20130101000000)
  endDateTime <- ymd_hms(20130630230000)
  currentDateTime <- startDateTime
  
  trainData <- data.frame(datetime = as.Date(character()),season = character(), rentMonth = character(), rentHour = character(),rentWeekday = character(),temperature = integer(), humidity = integer(),rainfall = integer(), rentCount = integer())
  
  while (currentDateTime <= endDateTime){
    nextDateTime <- currentDateTime+hours(1)
    
    tashuLoc  <- tashu2yearData$rentDateTime >= currentDateTime & tashu2yearData$rentDateTime < nextDateTime
    trainSubSet <- tashu2yearData[tashuLoc,]
    
    weatherSubset <- weather2013Data[weather2013Data$DT == currentDateTime,]
    
    if(is.na(weatherSubset$Rainfall)){
      weatherSubset$Rainfall <- 0
    }
  
    season <- '0'
    
    if(month(currentDateTime)>= 3 && month(currentDateTime) < 6){
      season <- '1'#spring
    }
    if(month(currentDateTime)>= 6 && month(currentDateTime) < 9){
      season <- '2'#summer
    }
    if(month(currentDateTime)>= 9 && month(currentDateTime) < 12){
      season <- '3'#fall
    }
    if(month(currentDateTime)>= 11 || month(currentDateTime) < 3){
      season <- '4'#winter
    }
    
    trainData <- rbind(trainData,data.frame(datetime = currentDateTime,season = season, rentMonth = toString(month(currentDateTime)),rentHour = toString(hour(currentDateTime)),rentWeekday = wday(currentDateTime, label = TRUE),temperature = weatherSubset$Temperature,humidity= weatherSubset$Humidity,rainfall = weatherSubset$Rainfall,rentCount = NROW(trainSubSet)))
    currentDateTime <- nextDateTime
  }
  
  startDateTime <- ymd_hms(20140101000000)
  endDateTime <- ymd_hms(20140630230000)
  currentDateTime <- startDateTime
  
  while (currentDateTime <= endDateTime){
    nextDateTime <- currentDateTime+hours(1)
    
    tashuLoc  <- tashu2yearData$rentDateTime >= currentDateTime & tashu2yearData$rentDateTime < nextDateTime
    trainSubSet <- tashu2yearData[tashuLoc,]

    weatherSubset <- weather2014Data[weather2014Data$DT == currentDateTime,]
    
    if(is.na(weatherSubset$Rainfall)){
      weatherSubset$Rainfall <- 0
    }
    season <- '0'
    
    if(month(currentDateTime)>= 3 && month(currentDateTime) < 6){
      season <- '1'#spring
    }
    if(month(currentDateTime)>= 6 && month(currentDateTime) < 9){
      season <- '2'#summer
    }
    if(month(currentDateTime)>= 9 && month(currentDateTime) < 12){
      season <- '3'#fall
    }
    if(month(currentDateTime)>= 11 || month(currentDateTime) < 3){
      season <- '4'#winter
    }
    trainData <- rbind(trainData,data.frame(datetime = currentDateTime,season = season,rentMonth = toString(month(currentDateTime)),rentHour = toString(hour(currentDateTime)),rentWeekday = wday(currentDateTime, label = TRUE),temperature = weatherSubset$Temperature,humidity= weatherSubset$Humidity,rainfall = weatherSubset$Rainfall,rentCount = NROW(trainSubSet)))
    
    currentDateTime <- nextDateTime
  }
  
  write.csv(trainData, file = outfilePath, row.names=FALSE)
  print("Complete Making Train Data.")
  
}

createTestData_rent<-function(TfilePath,WfilePath,outfilePath, rentStation){
  #load Tashu Data(2015), make date time column, remove NA row.
  tashu2015 <- read.csv(TfilePath, stringsAsFactors = F)
  tashu2015 <- na.omit(tashu2015)
  tashu2015$rentDateTime <- ymd_hms(tashu2015$RENT_DATE)
  
  #load weather Data(2015)
  weather2015Data <- read.csv(WfilePath, stringsAsFactors = F)
  weather2015Data$DT <- ymd_hm(weather2015Data$Datetime)
  #Collect weather Data(20150101~20150630)
  weather2015Data <- weather2015Data[weather2015Data$DT < ymd_hm(201601010000),]
  
  #remove xx:30 Data in weather2015Data
  weather2015Data <- weather2015Data[minute(weather2015Data$DT) == 0,]
  
  #Collect 'rentStation' Data
  tashu2015 <- tashu2015[tashu2015$RENT_STATION == rentStation,]
  
  #Make New DataFrame(resultDF) : datetime(2015-01-01 00:00 ~ 2015-06-30 23:00), rentMonth, rentHour, rentWeekday, Temperature, Humidity,rainfall, rentCount
  startDateTime <- ymd_hms(20150101000000)
  endDateTime <- ymd_hms(20151231230000)
  currentDateTime <- startDateTime
  
  testData <- data.frame(datetime = as.Date(character()),season = character(),rentMonth = character(), rentHour = character(), rentWeekday = character(), temperature = integer(), humidity = integer(),rainfall = integer(), rentCount = integer())
  
  while (currentDateTime <= endDateTime){
    nextDateTime <- currentDateTime+hours(1)
    
    tashuLoc  <- tashu2015$rentDateTime >= currentDateTime & tashu2015$rentDateTime < nextDateTime
    testSubset <- tashu2015[tashuLoc,]
    
    weatherSubset <- weather2015Data[weather2015Data$DT == currentDateTime,]
    
    if(is.na(weatherSubset$Rainfall)){
      weatherSubset$Rainfall <- 0
    }
    
    season <- '0'
    
    if(month(currentDateTime)>= 3 && month(currentDateTime) < 6){
      season <- '1'#spring
    }
    if(month(currentDateTime)>= 6 && month(currentDateTime) < 9){
      season <- '2'#summer
    }
    if(month(currentDateTime)>= 9 && month(currentDateTime) < 12){
      season <- '3'#fall
    }
    if(month(currentDateTime)>= 11 || month(currentDateTime) < 3){
      season <- '4'#winter
    }
    
    testData <- rbind(testData,data.frame(datetime = currentDateTime,season = season,rentMonth = toString(month(currentDateTime)),rentHour = toString(hour(currentDateTime)),rentWeekday = wday(currentDateTime, label = TRUE), temperature = weatherSubset$Temperature,humidity = weatherSubset$Humidity, rainfall = weatherSubset$Rainfall ,rentCount = NA))
    
    currentDateTime <- nextDateTime
  }
  write.csv(testData, file = outfilePath, row.names=FALSE)
  
  print("Complete Create Test Data")
}

createRealData_return <- function(TfilePath,WfilePath,outfilePath, returnStation){
  #load Tashu Data, make date time column, remove NA row.
  tashuData <- read.csv(TfilePath, stringsAsFactors = F)
  tashuData <- na.omit(tashuData)
  tashuData$returnDateTime <- ymd_hms(tashuData$RETURN_DATE)
  
  #load weather Data
  weatherData <- read.csv(WfilePath, stringsAsFactors = F)
  weatherData$DT <- ymd_hm(weatherData$Datetime)
  #Collect weather Data(201x0101~201x0630)
  weatherData <- weatherData[weatherData$DT < ymd_hm(201507010000),]
  
  #remove xx:30 Data in weather2015Data
  weatherData <- weatherData[minute(weatherData$DT) == 0,]
  
  #Collect 'rentStation' Data
  Locs <- tashuData$RETURN_STATION == returnStation
  tashuData <- tashuData[Locs,]
  
  #Make New DataFrame(resultDF) : datetime(2015-01-01 00:00 ~ 2015-06-30 23:00), returnMonth, returnHour, returnWeekday, Temperature, Humidity, rainfall, returnCount
  startDateTime <- ymd_hm(201401010000)
  endDateTime <- ymd_hm(201406302300)
  currentDateTime <- startDateTime
  
  resultDF = data.frame(datetime = as.Date(character()),season = character(), returnMonth = character(), returnHour = character(), returnWeekday = character(),temperature = integer(), humidity = integer(), rainfall = integer(), returnCount = integer())
  
  while (currentDateTime <= endDateTime){
    nextDateTime <- currentDateTime+hours(1)
    
    tashuLoc  <- tashuData$returnDateTime >= currentDateTime & tashuData$returnDateTime < nextDateTime
    dataSubSet <- tashuData[tashuLoc,]
    
    weatherSubset <- weatherData[weatherData$DT == currentDateTime,]
    
    if(is.na(weatherSubset$Rainfall)){
      weatherSubset$Rainfall <- 0
    }
    
    season <- '0'
    
    if(month(currentDateTime)>= 3 && month(currentDateTime) < 6){
      season <- '1'#spring
    }
    if(month(currentDateTime)>= 6 && month(currentDateTime) < 9){
      season <- '2'#summer
    }
    if(month(currentDateTime)>= 9 && month(currentDateTime) < 12){
      season <- '3'#fall
    }
    if(month(currentDateTime)>= 11 || month(currentDateTime) < 3){
      season <- '4'#winter
    }
    
    resultDF <- rbind(resultDF,data.frame(datetime = currentDateTime,season = season, returnMonth = toString(month(currentDateTime)),returnHour = toString(hour(currentDateTime)),returnWeekday = wday(currentDateTime, label = TRUE),temperature = weatherSubset$Temperature,humidity= weatherSubset$Humidity,rainfall = weatherSubset$Rainfall,returnCount = NROW(dataSubSet)))
    
    currentDateTime <- nextDateTime
  }
  
  write.csv(resultDF, file = outfilePath, row.names=FALSE)
  print("Complete Making real Data.")
}

createTrainData_return <- function(TfilePath,WfilePath1, WfilePath2,outfilePath, returnStation){
  ##load Tashu 2 year Data(2013~2014) from January to June
  tashu2yearData1to6 <- read.csv(TfilePath, stringsAsFactors = F)
  tashu2yearData1to6 <- na.omit(tashu2yearData1to6)
  tashu2yearData1to6$returnDateTime <- ymd_hms(tashu2yearData1to6$RETURN_DATE)
  
  ##load weather Data 2013, 2014
  weather2013Data <- read.csv(WfilePath1, stringsAsFactors = F)
  weather2013Data$DT <- ymd_hm(weather2013Data$Datetime)
  #weather2014Data <- read.csv(WfilePath2, stringsAsFactors = F)
  #weather2014Data$DT <- ymd_hm(weather2014Data$Datetime)
  
  ##Collect weather Data(20130101~20130630)
  weather2013Data <- weather2013Data[weather2013Data$DT < ymd_hm(201307010000),]
  ##remove xx:30 Data in weather2013Data
  weather2013Data <- weather2013Data[minute(weather2013Data$DT) == 0,]
  
  ##Collect weather Data(20140101~20140630)
  #weather2014Data <- weather2014Data[weather2014Data$DT < ymd_hm(201407010000),]
  ##remove xx:30 Data in weather2014Data
  #weather2014Data <- weather2014Data[minute(weather2014Data$DT) == 0,]
  
  ##Collect 'rentStation' Data
  tashu2yearData1to6 <- tashu2yearData1to6[tashu2yearData1to6$RETURN_STATION == returnStation,]
  
  ##Make Train DataFrame(trainData) : datetime(2013-01-01 00:00 ~ 2013-06-30 23:00, 2014-01-01 00:00 ~ 2014-06-30 23:00), returnMonth, returnHour, returnWeekday, Temperature, Humidity,rainfall, returnCount
  startDateTime <- ymd_hms(20130101000000)
  endDateTime <- ymd_hms(20130630230000)
  currentDateTime <- startDateTime
  
  trainData <- data.frame(datetime = as.Date(character()),season = character(), returnMonth = character(), returnHour = character(),returnWeekday = character(),temperature = integer(), humidity = integer(),rainfall = integer(), returnCount = integer())
  
  while (currentDateTime <= endDateTime){
    nextDateTime <- currentDateTime+hours(1)
    
    tashuLoc  <- tashu2yearData1to6$returnDateTime >= currentDateTime & tashu2yearData1to6$returnDateTime < nextDateTime
    trainSubSet <- tashu2yearData1to6[tashuLoc,]
    
    weatherSubset <- weather2013Data[weather2013Data$DT == currentDateTime,]
    
    if(is.na(weatherSubset$Rainfall)){
      weatherSubset$Rainfall <- 0
    }
    
    season <- '0'
    
    if(month(currentDateTime)>= 3 && month(currentDateTime) < 6){
      season <- '1'#spring
    }
    if(month(currentDateTime)>= 6 && month(currentDateTime) < 9){
      season <- '2'#summer
    }
    if(month(currentDateTime)>= 9 && month(currentDateTime) < 12){
      season <- '3'#fall
    }
    if(month(currentDateTime)>= 11 || month(currentDateTime) < 3){
      season <- '4'#winter
    }
    
    trainData <- rbind(trainData,data.frame(datetime = currentDateTime,season = season, returnMonth = toString(month(currentDateTime)),returnHour = toString(hour(currentDateTime)),returnWeekday = wday(currentDateTime, label = TRUE),temperature = weatherSubset$Temperature,humidity= weatherSubset$Humidity,rainfall = weatherSubset$Rainfall,returnCount = NROW(trainSubSet)))
    currentDateTime <- nextDateTime
  }
  
  #startDateTime <- ymd_hms(20140101000000)
  #endDateTime <- ymd_hms(20140630230000)
  #currentDateTime <- startDateTime
  
  #while (currentDateTime <= endDateTime){
  #  nextDateTime <- currentDateTime+hours(1)
    
  #  tashuLoc  <- tashu2yearData1to6$rentDateTime >= currentDateTime & tashu2yearData1to6$rentDateTime < nextDateTime
  #  trainSubSet <- tashu2yearData1to6[tashuLoc,]
    
  #  weatherSubset <- weather2014Data[weather2014Data$DT == currentDateTime,]
    
  #  if(is.na(weatherSubset$Rainfall)){
  #    weatherSubset$Rainfall <- 0
  #  }
  #  season <- '0'
    
  #  if(month(currentDateTime)>= 3 && month(currentDateTime) < 6){
  #    season <- '1'#spring
  #  }
  #  if(month(currentDateTime)>= 6 && month(currentDateTime) < 9){
  #    season <- '2'#summer
  #  }
  #  if(month(currentDateTime)>= 9 && month(currentDateTime) < 12){
  #    season <- '3'#fall
  #  }
  #  if(month(currentDateTime)>= 11 || month(currentDateTime) < 3){
  #    season <- '4'#winter
  #  }
  #  trainData <- rbind(trainData,data.frame(datetime = currentDateTime,season = season,rentMonth = toString(month(currentDateTime)),rentHour = toString(hour(currentDateTime)),rentWeekday = wday(currentDateTime, label = TRUE),temperature = weatherSubset$Temperature,humidity= weatherSubset$Humidity,rainfall = weatherSubset$Rainfall,rentCount = NROW(trainSubSet)))
    
  #  currentDateTime <- nextDateTime
  #}
  
  write.csv(trainData, file = outfilePath, row.names=FALSE)
  print("Complete Making Train Data.")
  
}

createTestData_return<-function(TfilePath,WfilePath,outfilePath, returnStation){
  #load Tashu Data, make date time column, remove NA row.
  tashuData <- read.csv(TfilePath, stringsAsFactors = F)
  tashuData <- na.omit(tashuData)
  tashuData$returnDateTime <- ymd_hms(tashuData$RETURN_DATE)
  
  #load weather Data
  weatherData <- read.csv(WfilePath, stringsAsFactors = F)
  weatherData$DT <- ymd_hm(weatherData$Datetime)
  #Collect weather Data
  weatherData <- weatherData[weatherData$DT < ymd_hm(201407010000),]
  
  #remove xx:30 Data in weather2015Data
  weatherData <- weatherData[minute(weatherData$DT) == 0,]
  
  #Collect 'rentStation' Data
  tashuData <- tashuData[tashuData$RETURN_STATION == returnStation,]
  
  #Make New DataFrame(resultDF) : datetime, returnMonth, returnHour, returnWeekday, Temperature, Humidity,rainfall, returnCount
  startDateTime <- ymd_hms(20140101000000)
  endDateTime <- ymd_hms(20140630230000)
  currentDateTime <- startDateTime
  
  testData <- data.frame(datetime = as.Date(character()),season = character(),returnMonth = character(), returnHour = character(), returnWeekday = character(), temperature = integer(), humidity = integer(),rainfall = integer(), returnCount = integer())
  
  while (currentDateTime <= endDateTime){
    nextDateTime <- currentDateTime+hours(1)
    
    tashuLoc  <- tashuData$returnDateTime >= currentDateTime & tashuData$returnDateTime < nextDateTime
    testSubset <- tashuData[tashuLoc,]
    
    weatherSubset <- weatherData[weatherData$DT == currentDateTime,]
    
    if(is.na(weatherSubset$Rainfall)){
      weatherSubset$Rainfall <- 0
    }
    
    season <- '0'
    
    if(month(currentDateTime)>= 3 && month(currentDateTime) < 6){
      season <- '1'#spring
    }
    if(month(currentDateTime)>= 6 && month(currentDateTime) < 9){
      season <- '2'#summer
    }
    if(month(currentDateTime)>= 9 && month(currentDateTime) < 12){
      season <- '3'#fall
    }
    if(month(currentDateTime)>= 11 || month(currentDateTime) < 3){
      season <- '4'#winter
    }
    
    testData <- rbind(testData,data.frame(datetime = currentDateTime,season = season,returnMonth = toString(month(currentDateTime)),returnHour = toString(hour(currentDateTime)),returnWeekday = wday(currentDateTime, label = TRUE), temperature = weatherSubset$Temperature,humidity = weatherSubset$Humidity, rainfall = weatherSubset$Rainfall ,returnCount = NA))
    
    currentDateTime <- nextDateTime
  }
  write.csv(testData, file = outfilePath, row.names=FALSE)
  
  print("Complete Create Test Data")
}

createRealData_rent(realOutFilePath, station)
#createTrainData_rent('C:\\Users\\miw52\\Desktop\\Tashu2017_Prediction_Model\\data\\tashu20132014Data1to6.csv','C:\\Users\\miw52\\Desktop\\Tashu2017_Prediction_Model\\data\\weather\\2013_weatherData.csv','C:\\Users\\miw52\\Desktop\\Tashu2017_Prediction_Model\\data\\weather\\2014_weatherData.csv',"C:\\Users\\miw52\\Desktop\\Tashu2017_Prediction_Model\\data\\station55\\tashu_stat55_trainData.csv",55)
#createTestData_rent('C:\\Users\\miw52\\Desktop\\Tashu2017_Prediction_Model\\data\\tashu2015.csv','C:\\Users\\miw52\\Desktop\\Tashu2017_Prediction_Model\\data\\weather\\2015_weatherData.csv',"C:\\Users\\miw52\\Desktop\\Tashu2017_Prediction_Model\\data\\station55\\tashu_stat55_testData.csv", 55)

#createRealData_return('C:\\Users\\miw52\\Desktop\\Tashu2017_Prediction_Model\\data\\tashu2014.csv',"C:\\Users\\miw52\\Desktop\\Tashu2017_Prediction_Model\\data\\weather\\2014_weatherData.csv", "C:\\Users\\miw52\\Desktop\\Tashu2017_Prediction_Model\\data\\station55\\tashu_stat55_return2014Data.csv", 55)
#createTrainData_return('C:\\Users\\miw52\\Desktop\\Tashu2017_Prediction_Model\\data\\tashu20132014Data1to6.csv','C:\\Users\\miw52\\Desktop\\Tashu2017_Prediction_Model\\data\\weather\\2013_weatherData.csv','C:\\Users\\miw52\\Desktop\\Tashu2017_Prediction_Model\\data\\weather\\2014_weatherData.csv',"C:\\Users\\miw52\\Desktop\\Tashu2017_Prediction_Model\\data\\station55\\tashu_stat55_returnTrainData.csv",55)
#createTestData_return('C:\\Users\\miw52\\Desktop\\Tashu2017_Prediction_Model\\data\\tashu2014.csv','C:\\Users\\miw52\\Desktop\\Tashu2017_Prediction_Model\\data\\weather\\2014_weatherData.csv',"C:\\Users\\miw52\\Desktop\\Tashu2017_Prediction_Model\\data\\station55\\tashu_stat55_returnTestData.csv", 55)