library(lubridate)

createRealData <- function(TfilePath,WfilePath, rentStation){
  #load Tashu Data(2015), make date time column, remove NA row.
  tashuData <- read.csv(TfilePath, stringsAsFactors = F)
  tashuData <- na.omit(tashuData)
  tashuData$rentDateTime <- ymd_hms(tashuData$RENT_DATE)
  
  #load weather Data(2015)
  weather2015Data <- read.csv(WfilePath, stringsAsFactors = F)
  weather2015Data$DT <- ymd_hm(weather2015Data$Datetime)
  #Collect weather Data(20150101~20150630)
  weather2015Data <- weather2015Data[weather2015Data$DT < ymd_hm(201507010000),]
  
  #remove xx:30 Data in weather2015Data
  weather2015Data <- weather2015Data[minute(weather2015Data$DT) == 0,]

  #Collect 'rentStation' Data
  Locs <- tashuData$RENT_STATION == rentStation
  tashuData <- tashuData[Locs,]
  
  #Make New DataFrame(resultDF) : datetime(2015-01-01 00:00 ~ 2015-06-30 23:00), rentMonth, rentHour, rentWeekday, Temperature, Humidity rentCount
  startDateTime <- ymd_hm(201501010000)
  endDateTime <- ymd_hm(201506302300)
  currentDateTime <- startDateTime
  
  resultDF = data.frame(datetime = as.Date(character()),rentMonth = character(), rentHour = character(), rentWeekday = character(),temperature = integer(), humidity = integer(), rentCount = integer())
  
  while (currentDateTime <= endDateTime){
    nextDateTime <- currentDateTime+hours(1)
    
    tashuLoc  <- tashuData$rentDateTime >= currentDateTime & tashuData$rentDateTime < nextDateTime
    dataSubSet <- tashuData[tashuLoc,]
    
    weatherSubset <- weather2015Data[weather2015Data$DT == currentDateTime,]
    
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
    
    resultDF <- rbind(resultDF,data.frame(datetime = currentDateTime,season = season, rentMonth = toString(month(currentDateTime)),rentHour = toString(hour(currentDateTime)),rentWeekday = wday(currentDateTime, label = TRUE),temperature = weatherSubset$Temperature,humidity= weatherSubset$Humidity,rentCount = NROW(dataSubSet)))
    
    currentDateTime <- nextDateTime
  }
  
  write.csv(resultDF, file = "C:\\Users\\miw52\\Desktop\\Tashu2017_Prediction_Model\\data\\station3\\tashu_stat3_2015Data.csv", row.names=FALSE)
  print("Complete Making real Data.")
}

createTrainData <- function(TfilePath,WfilePath1, WfilePath2, rentStation){
  #load Tashu 2 year Data(2013~2014) from January to June
  tashu2yearData1to6 <- read.csv(TfilePath, stringsAsFactors = F)
  tashu2yearData1to6 <- na.omit(tashu2yearData1to6)
  tashu2yearData1to6$rentDateTime <- ymd_hms(tashu2yearData1to6$RENT_DATE)

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
  tashu2yearData1to6 <- tashu2yearData1to6[tashu2yearData1to6$RENT_STATION == rentStation,]
  
  #Make Train DataFrame(trainData) : datetime(2013-01-01 00:00 ~ 2013-06-30 23:00, 2014-01-01 00:00 ~ 2014-06-30 23:00), rentMonth, rentHour, rentWeekday, Temperature, Humidity,rentCount
  startDateTime <- ymd_hms(20130101000000)
  endDateTime <- ymd_hms(20130630230000)
  currentDateTime <- startDateTime
  
  trainData <- data.frame(datetime = as.Date(character()),season = character(), rentMonth = character(), rentHour = character(),rentWeekday = character(),temperature = integer(), humidity = integer(), rentCount = integer())
  
  while (currentDateTime <= endDateTime){
    nextDateTime <- currentDateTime+hours(1)
    
    tashuLoc  <- tashu2yearData1to6$rentDateTime >= currentDateTime & tashu2yearData1to6$rentDateTime < nextDateTime
    trainSubSet <- tashu2yearData1to6[tashuLoc,]
    
    weatherSubset <- weather2013Data[weather2013Data$DT == currentDateTime,]
  
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
    
    trainData <- rbind(trainData,data.frame(datetime = currentDateTime,season = season, rentMonth = toString(month(currentDateTime)),rentHour = toString(hour(currentDateTime)),rentWeekday = wday(currentDateTime, label = TRUE),temperature = weatherSubset$Temperature,humidity= weatherSubset$Humidity,rentCount = NROW(trainSubSet)))
    currentDateTime <- nextDateTime
  }
  
  startDateTime <- ymd_hms(20140101000000)
  endDateTime <- ymd_hms(20140630230000)
  currentDateTime <- startDateTime
  
  while (currentDateTime <= endDateTime){
    nextDateTime <- currentDateTime+hours(1)
    
    tashuLoc  <- tashu2yearData1to6$rentDateTime >= currentDateTime & tashu2yearData1to6$rentDateTime < nextDateTime
    trainSubSet <- tashu2yearData1to6[tashuLoc,]

    weatherSubset <- weather2014Data[weather2014Data$DT == currentDateTime,]
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
    trainData <- rbind(trainData,data.frame(datetime = currentDateTime,season = season,rentMonth = toString(month(currentDateTime)),rentHour = toString(hour(currentDateTime)),rentWeekday = wday(currentDateTime, label = TRUE),temperature = weatherSubset$Temperature,humidity= weatherSubset$Humidity,rentCount = NROW(trainSubSet)))
    
    currentDateTime <- nextDateTime
  }
  
  write.csv(trainData, file = "C:\\Users\\miw52\\Desktop\\Tashu2017_Prediction_Model\\data\\station3\\tashu_stat3_trainData.csv", row.names=FALSE)
  print("Complete Making Train Data.")
  
}

createTestData<-function(TfilePath,WfilePath, rentStation){
  #load Tashu Data(2015), make date time column, remove NA row.
  tashu2015 <- read.csv(TfilePath, stringsAsFactors = F)
  tashu2015 <- na.omit(tashu2015)
  tashu2015$rentDateTime <- ymd_hms(tashu2015$RENT_DATE)
  
  #load weather Data(2015)
  weather2015Data <- read.csv(WfilePath, stringsAsFactors = F)
  weather2015Data$DT <- ymd_hm(weather2015Data$Datetime)
  #Collect weather Data(20150101~20150630)
  weather2015Data <- weather2015Data[weather2015Data$DT < ymd_hm(201507010000),]
  
  #remove xx:30 Data in weather2015Data
  weather2015Data <- weather2015Data[minute(weather2015Data$DT) == 0,]
  
  #Collect 'rentStation' Data
  tashu2015 <- tashu2015[tashu2015$RENT_STATION == rentStation,]
  
  #Make New DataFrame(resultDF) : datetime(2015-01-01 00:00 ~ 2015-06-30 23:00), rentMonth, rentHour, rentWeekday, Temperature, Humidity,rentCount
  startDateTime <- ymd_hms(20150101000000)
  endDateTime <- ymd_hms(20150630230000)
  currentDateTime <- startDateTime
  
  testData <- data.frame(datetime = as.Date(character()),season = character(),rentMonth = character(), rentHour = character(), rentWeekday = character(), temperature = integer(), humidity = integer(), rentCount = integer())
  
  while (currentDateTime <= endDateTime){
    nextDateTime <- currentDateTime+hours(1)
    
    tashuLoc  <- tashu2015$rentDateTime >= currentDateTime & tashu2015$rentDateTime < nextDateTime
    testSubset <- tashu2015[tashuLoc,]
    
    weatherSubset <- weather2015Data[weather2015Data$DT == currentDateTime,]
    
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
    
    testData <- rbind(testData,data.frame(datetime = currentDateTime,season = season,rentMonth = toString(month(currentDateTime)),rentHour = toString(hour(currentDateTime)),rentWeekday = wday(currentDateTime, label = TRUE), temperature = weatherSubset$Temperature,humidity = weatherSubset$Humidity ,rentCount = NA))
    
    currentDateTime <- nextDateTime
  }
  write.csv(testData, file = "C:\\Users\\miw52\\Desktop\\Tashu2017_Prediction_Model\\data\\station3\\tashu_stat3_testData.csv", row.names=FALSE)
  
  print("Complete Create Test Data")
}

createRealData('C:\\Users\\miw52\\Desktop\\Tashu2017_Prediction_Model\\data\\tashu2015.csv',"C:\\Users\\miw52\\Desktop\\Tashu2017_Prediction_Model\\data\\weather\\2015_weatherData.csv", 3)
createTrainData('C:\\Users\\miw52\\Desktop\\Tashu2017_Prediction_Model\\data\\tashu20132014Data1to6.csv','C:\\Users\\miw52\\Desktop\\Tashu2017_Prediction_Model\\data\\weather\\2013_weatherData.csv','C:\\Users\\miw52\\Desktop\\Tashu2017_Prediction_Model\\data\\weather\\2014_weatherData.csv',3)
createTestData('C:\\Users\\miw52\\Desktop\\Tashu2017_Prediction_Model\\data\\tashu2015.csv','C:\\Users\\miw52\\Desktop\\Tashu2017_Prediction_Model\\data\\weather\\2014_weatherData.csv', 3)