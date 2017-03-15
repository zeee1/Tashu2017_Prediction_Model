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

#tashu20132014Data <- read.csv("../data/tashu20132014.csv", stringsAsFactors = F)
#tashu20132014Data <- na.omit(tashu20132014Data)
#tashu20132014Data$rentDateTime <- ymd_hms(tashu20132014Data$RENT_DATE)
#tashu20132014Data$returnDateTime <- ymd_hms(tashu20132014Data$RETURN_DATE)

#tashu2015Data <- read.csv("../data/tashu2015.csv", stringsAsFactors = F)
#tashu2015Data <- na.omit(tashu2015Data)
#tashu2015Data$rentDateTime <- ymd_hms(tashu2015Data$RENT_DATE)
#tashu2015Data$returnDateTime <- ymd_hms(tashu2015Data$RETURN_DATE)

print("Tashu Data loading Complete")

#weather2013Data <- read.csv("../data/weather/2013_weatherData.csv", stringsAsFactors = F)
#weather2014Data <- read.csv("../data/weather/2014_weatherData.csv", stringsAsFactors = F)
#weather2015Data <- read.csv("../data/weather/2015_weatherData.csv", stringsAsFactors = F)
#weather2013Data$DT <- ymd_hm(weather2013Data$Datetime)
#weather2014Data$DT <- ymd_hm(weather2014Data$Datetime)
#weather2015Data$DT <- ymd_hm(weather2015Data$Datetime)
#weather2013Data <- weather2013Data[minute(weather2013Data$DT) == 0,]
#weather2014Data <- weather2014Data[minute(weather2014Data$DT) == 0,]
#weather2015Data <- weather2015Data[minute(weather2015Data$DT) == 0,]

print("Weather Data loading Complete")
#festivalData <- read.csv("../data/festival_info.csv", stringsAsFactors = F)
#festivalData$nearStation <- strsplit(festivalData$nearStation, ",")

#testlocs <- grepl(festivalData$nearStation, toString(3)) == TRUE
#festivalData <- festivalData[testlocs,]


#Feature extract Function
extractRentFeatures <- function(data){
  features <- c ("season","rentMonth","rentHour", "rentWeekday","temperature", "humidity", "rainfall")
  
  return(data[, features])
}

#Feature extract Function
extractReturnFeatures <- function(data){
  features <- c ("season","returnMonth","returnHour", "returnWeekday","temperature", "humidity", "rainfall")
  
  return(data[, features])
}

print("Data Load Complete")
for (i_station in stationList){
  print("Station : ")
  print(i_station)
  # Make Train DataFrame(20130101~20141231) : rentTrainDF, returnTrainDF
  rentSubsetInTrain <- tashu20132014Data[tashu20132014Data$RENT_STATION == i_station,]
  returnSubsetInTrain <- tashu20132014Data[tashu20132014Data$RETURN_STATION == i_station,]
  
  startDateTime <- ymd_hms(20130101000000)
  endDateTime <- ymd_hms(20141231230000)
  currentDateTime <- startDateTime
  
  rentTrainDF <- data.frame(datetime = as.Date(character()),season = character(), rentMonth = character(),
                            rentHour = character(),rentWeekday = character(),temperature = integer(), 
                            humidity = integer(),rainfall = integer(), rentCount = integer())
  returnTrainDF <- data.frame(datetime = as.Date(character()),season = character(), returnMonth = character(), 
                              returnHour = character(),returnWeekday = character(),temperature = integer(), 
                              humidity = integer(),rainfall = integer(), returnCount = integer())
  
  print("Start rent/return TrainDF")
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
    
    rentTrainDF <- rbind(rentTrainDF,data.frame(datetime = currentDateTime,season = season, 
                                                rentMonth = toString(month(currentDateTime)),
                                                rentHour = toString(hour(currentDateTime)),
                                                rentWeekday = wday(currentDateTime, label = TRUE),
                                                temperature = weatherSubset$Temperature,
                                                humidity= weatherSubset$Humidity,rainfall = weatherSubset$Rainfall,
                                                rentCount = NROW(rentTimeSubset)))
    returnTrainDF <- rbind(returnTrainDF,data.frame(datetime = currentDateTime,season = season, 
                                                returnMonth = toString(month(currentDateTime)),
                                                returnHour = toString(hour(currentDateTime)),
                                                returnWeekday = wday(currentDateTime, label = TRUE),
                                                temperature = weatherSubset$Temperature,
                                                humidity= weatherSubset$Humidity,rainfall = weatherSubset$Rainfall,
                                                returnCount = NROW(returnTimeSubset)))
    
    currentDateTime <- nextDateTime
  }
  print("Creating rent/return Train df complete")
  # Make Test DataFrame(20150101~20151231) - rentTestDF, returnTestDF
  rentSubsetInTest <- tashu2015Data[tashu2015Data$RENT_STATION == i_station,]
  returnSubsetInTest <- tashu2015Data[tashu2015Data$RETURN_STATION == i_station,]
  
  startDateTime <- ymd_hms(20150101000000)
  endDateTime <- ymd_hms(20151231230000)
  currentDateTime <- startDateTime
  
  rentTestDF <- data.frame(datetime = as.Date(character()),season = character(),
                           rentMonth = character(), rentHour = character(), 
                           rentWeekday = character(), temperature = integer(), 
                           humidity = integer(),rainfall = integer(), rentCount = integer())
  returnTestDF <- data.frame(datetime = as.Date(character()),season = character(),
                             returnMonth = character(), returnHour = character(), 
                             returnWeekday = character(), temperature = integer(), 
                             humidity = integer(),rainfall = integer(), returnCount = integer())
  
  print("Start rent/return Test Data")
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
                                                rentCount = NROW(rentTimeSubset)))
    returnTestDF <- rbind(returnTestDF,data.frame(datetime = currentDateTime,season = season, 
                                                    returnMonth = toString(month(currentDateTime)),
                                                    returnHour = toString(hour(currentDateTime)),
                                                    returnWeekday = wday(currentDateTime, label = TRUE),
                                                    temperature = weatherSubset$Temperature,
                                                    humidity= weatherSubset$Humidity,rainfall = weatherSubset$Rainfall,
                                                    returnCount = NROW(returnTimeSubset)))
    
    currentDateTime <- nextDateTime
  }
  print("Creating Test DF completed")
  
  #Copy real Data to realRentDF, realReturnDF
  realRentDF <- rentTestDF
  realReturnDF <- returnTestDF
  rentTestDF$rentCount <- NA
  returnTestDF$returnCount <- NA
  
  #Write result of prediction into File. Save
  monthList <- unique(rentTestDF$rentMonth)
  monthList <- monthList[!is.na(monthList)]
  
  #rent/return data fit
  rent_rf <- randomForest(as.factor(rentCount)~rentMonth+rentWeekday+temperature+humidity+rentHour+rainfall+season,data=rentTrainDF, 
                          ntree = 50, mtry = 2, importance = TRUE)
  return_rf <- randomForest(as.factor(returnCount)~returnMonth+returnWeekday+temperature+humidity+returnHour+rainfall+season,data=returnTrainDF, 
                           ntree = 50, mtry = 2, importance=TRUE)
  
  for(i_month in monthList){
    #Predict rent Count
    locs <- rentTestDF$rentMonth == i_month
    testSubSet <- rentTestDF[locs,]
    rentTestDF[locs,"rentCount"] <- predict(rent_rf, extractRentFeatures(testSubSet))
    #Predict return Count
    locs <- returnTestDF$returnMonth == i_month
    testSubSet <- returnTestDF[locs,]
    returnTestDF[locs,"returnCount"] <- predict(return_rf, extractReturnFeatures(testSubSet))
  }
  
  write.csv(rentTestDF, file = paste("../data/stat",toString(i_station),"_ClassificationPredict_rentResult.csv",sep="",collapse = NULL), row.names=FALSE)
  write.csv(returnTestDF, file = paste("../data/stat",toString(i_station),"_ClassificationPredict_returnResult.csv",sep="",collapse = NULL), row.names=FALSE)
  print("Prediction completed")
  
  #Check Importance of rent/return features.
  imp <- importance(rent_rf, type=1)
  featureImportance <- data.frame(Feature=row.names(imp), Importance=imp[,1])
  
  ggplot(featureImportance, aes(x=reorder(Feature, Importance), y=Importance)) +
    geom_bar(stat="identity", fill="#53cfff") +
    coord_flip() + 
    theme_light(base_size=20) +
    xlab("Importance") +
    ylab("") + 
    ggtitle("Random Forest Feature Importance\n") +
    theme(plot.title=element_text(size=18))
  ggsave(paste("../images/sampleTest/stat",toString(i_station),"_rentFI.png",sep="",collapse = NULL ))
  
  imp <- importance(return_rf, type=1)
  featureImportance <- data.frame(Feature=row.names(imp), Importance=imp[,1])
  
  ggplot(featureImportance, aes(x=reorder(Feature, Importance), y=Importance)) +
    geom_bar(stat="identity", fill="#53cfff") +
    coord_flip() + 
    theme_light(base_size=20) +
    xlab("Importance") +
    ylab("") + 
    ggtitle("Random Forest Feature Importance\n") +
    theme(plot.title=element_text(size=18))
  ggsave(paste("../images/sampleTest/stat",toString(i_station),"_returnFI.png",sep="",collapse = NULL ))
  
  #Check prediction accuracy. - Root Mean Square Error
  compareRentDF <- data.frame(datetime = realRentDF$datetime, RrentCount = realRentDF$rentCount, PrentCount = rentTestDF$rentCount, 
                              error = realRentDF$rentCount-rentTestDF$rentCount)
  result <- sqrt(mean(compareRentDF$error^2))
  
  accurateRate <- 1-result
  print(paste("station",toString(i_station),"_rent_err.rate",sep="",collapse = NULL))
  print(result)
  print(paste("station",toString(i_station),"_rent_accuracy.rate",sep="",collapse = NULL))
  print(accurateRate)
  
  compareReturnDF <- data.frame(datetime = realReturnDF$datetime, RrentCount = realReturnDF$returnCount, PrentCount = returnTestDF$returnCount, 
                                error = realReturnDF$returnCount-returnTestDF$returnCount)
  result <- sqrt(mean(compareReturnDF$error^2))
  
  accurateRate <- 1-result
  print(paste("station",toString(i_station),"_return_err.rate",sep="",collapse = NULL))
  print(result)
  print(paste("station",toString(i_station),"_return_accuracy.rate",sep="",collapse = NULL))
  print(accurateRate)
  
  #Todo : Visualize real/prediction/train data by Month and Weekdays. Save.
  for (i_month in monthList){
    #Train_rent
    rentTrainSubset <- rentTrainDF[rentTrainDF$rentMonth == i_month,]
    day_summary <- ddply(rentTrainSubset,.(rentWeekday, rentHour),summarise, rentCount = mean(rentCount))
    ggplot(rentTrainSubset, aes(x = rentHour, y = rentCount, colour = rentWeekday))+
      geom_point(data = day_summary, aes(group = rentWeekday))+
      geom_line(data = day_summary, aes(group=rentWeekday))+
      scale_x_discrete("Hour")+
      scale_y_continuous("Count")+theme_minimal()
    ggsave(paste("../images/sampleTest/stat",toString(i_station),"_",toString(i_month),"_TrainrentCountMean.png",sep="",collapse = NULL ))
    
    #Train_return
    returnTrainSubset <- returnTrainDF[returnTrainDF$returnMonth == i_month,]
    day_summary <- ddply(returnTrainSubset,.(returnWeekday, returnHour),summarise, returnCount = mean(returnCount))
    ggplot(returnTrainSubset, aes(x = returnHour, y = returnCount, colour = returnWeekday))+
      geom_point(data = day_summary, aes(group = returnWeekday))+
      geom_line(data = day_summary, aes(group=returnWeekday))+
      scale_x_discrete("Hour")+
      scale_y_continuous("Count")+theme_minimal()
    ggsave(paste("../images/sampleTest/stat",toString(i_station),"_",toString(i_month),"_TrainreturnCountMean.png",sep="",collapse = NULL ))
    
    #Prediction_rent
    rentTestSubset <- rentTestDF[rentTestDF$rentMonth == i_month,]
    day_summary <- ddply(rentTestSubset,.(rentWeekday, rentHour),summarise, rentCount = mean(rentCount))
    ggplot(rentTestSubset, aes(x = rentHour, y = rentCount, colour = rentWeekday))+
      geom_point(data = day_summary, aes(group = rentWeekday))+
      geom_line(data = day_summary, aes(group=rentWeekday))+
      scale_x_discrete("Hour")+
      scale_y_continuous("Count")+theme_minimal()
    ggsave(paste("../images/sampleTest/stat",toString(i_station),"_",toString(i_month),"_PredictrentCountMean.png",sep="",collapse = NULL ))
    
    #Prediction_return
    returnTestSubset <- returnTestDF[returnTestDF$returnMonth == i_month,]
    day_summary <- ddply(returnTestSubset,.(returnWeekday, returnHour),summarise, returnCount = mean(returnCount))
    ggplot(returnTestSubset, aes(x = returnHour, y = returnCount, colour = returnWeekday))+
      geom_point(data = day_summary, aes(group = returnWeekday))+
      geom_line(data = day_summary, aes(group=returnWeekday))+
      scale_x_discrete("Hour")+
      scale_y_continuous("Count")+theme_minimal()
    ggsave(paste("../images/sampleTest/stat",toString(i_station),"_",toString(i_month),"_PredictreturnCountMean.png",sep="",collapse = NULL ))
    
    #Real_rent
    realRentSubset <- realRentDF[realRentDF$rentMonth == i_month,]
    day_summary <- ddply(realRentSubset,.(rentWeekday, rentHour),summarise, rentCount = mean(rentCount))
    ggplot(realRentSubset, aes(x = rentHour, y = rentCount, colour = rentWeekday))+
      geom_point(data = day_summary, aes(group = rentWeekday))+
      geom_line(data = day_summary, aes(group=rentWeekday))+
      scale_x_discrete("Hour")+
      scale_y_continuous("Count")+theme_minimal()
    ggsave(paste("../images/sampleTest/stat",toString(i_station),"_",toString(i_month),"_RealrentCountMean.png",sep="",collapse = NULL ))
    
    #Real_return
    realReturnSubset <- realReturnDF[realReturnDF$returnMonth == i_month,]
    day_summary <- ddply(realReturnSubset,.(returnWeekday, returnHour),summarise, returnCount = mean(returnCount))
    ggplot(realReturnSubset, aes(x = returnHour, y = returnCount, colour = returnWeekday))+
      geom_point(data = day_summary, aes(group = returnWeekday))+
      geom_line(data = day_summary, aes(group=returnWeekday))+
      scale_x_discrete("Hour")+
      scale_y_continuous("Count")+theme_minimal()
    ggsave(paste("../images/sampleTest/stat",toString(i_station),"_",toString(i_month),"_RealreturnCountMean.png",sep="",collapse = NULL ))
  }

}