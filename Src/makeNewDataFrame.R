library(lubridate)

makeDataFrameForTrain <- function(filePath, rentStation){
  tashuData <- read.csv('data/tashu2014_1.csv', stringsAsFactors = F)
  
  tashuData$rentDateTime <- ymd_hms(tashuData$RENT_DATE)
  tashuData$rentWeekday <- wday(tashuData$rentDateTime)
  
  tashuData <- na.omit(tashuData)

  Locs <- tashuData$RENT_STATION == 3
  tashuData <- tashuData[Locs,]
  
  startDateTime <- ymd_hms(20140101000000)
  endDateTime <- ymd_hms(20140630230000)
  currentDateTime <- startDateTime
  
  resultDF = data.frame(datetime = as.Date(character()),rentMonth = integer(), rentHour = integer(), rentWeekday = integer(), rentCount = integer())
  
  while (currentDateTime <= endDateTime){
    nextDateTime <- currentDateTime+hours(1)
    
    locCondition  <- tashuData$rentDateTime >= currentDateTime & tashuData$rentDateTime < nextDateTime
    dataSubSet <- tashuData[locCondition,]
    
    resultDF <- rbind(resultDF,data.frame(datetime = currentDateTime,rentMonth = month(currentDateTime),rentHour = hour(currentDateTime),rentWeekday = wday(currentDateTime),rentCount = NROW(dataSubSet)))
    
    currentDateTime <- nextDateTime
  }
  
  write.csv(resultDF, file = "tashu_rentCountPerHour.csv", row.names=FALSE)
}

makeDataFrameForTrain('data/tashu2014_1.csv', 3)