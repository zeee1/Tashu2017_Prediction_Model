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


#Variation
#samplestationList <- c(1,3,9,17,21,24,55, 118, 122,123, 127, 143)
  # park  : 1,3
  # company : 24, 123
  # downtown : 17, 143
  # University : 122(hannam), 55(chungnam), 21(kaist)
  # Living : 9
  # Terminal / Train Station : 118(Bokhab terminal), 127(sintanjin train station)





print("Data Loading completed.")

for (i_station in 1:144){

  
  
  #write.csv(rentTestDF, file = paste("stat",toString(i_station),"_RegressionPredict_rentResult.csv",sep="",collapse = NULL), row.names=FALSE)
  #write.csv(returnTestDF, file = paste("stat",toString(i_station),"_RegressionPredict_returnResult.csv",sep="",collapse = NULL), row.names=FALSE)
  
  print("Prediction completed")
  
  #Check Importance of features.
  #imp <- importance(rent_rf, type=1)
  #featureImportance <- data.frame(Feature=row.names(imp), Importance=imp[,1])
  
  #ggplot(featureImportance, aes(x=reorder(Feature, Importance), y=Importance)) +
  #  geom_bar(stat="identity", fill="#53cfff") +
  #  coord_flip() + 
  #  theme_light(base_size=20) +
  #  xlab("Importance") +
  #  ylab("") + 
  #  ggtitle("Random Forest Feature Importance\n") +
  #  theme(plot.title=element_text(size=18))
  #ggsave(paste("../images/sampleTest/",toString(i_station),"_rentFI.png",sep="",collapse = NULL))
  
  #imp <- importance(return_rf, type=1)
  #featureImportance <- data.frame(Feature=row.names(imp), Importance=imp[,1])
  
  #ggplot(featureImportance, aes(x=reorder(Feature, Importance), y=Importance)) +
  #  geom_bar(stat="identity", fill="#53cfff") +
  #  coord_flip() + 
  #  theme_light(base_size=20) +
  #  xlab("Importance") +
  #  ylab("") + 
  #  ggtitle("Random Forest Feature Importance\n") +
  #  theme(plot.title=element_text(size=18))
  #ggsave(paste("../images/sampleTest/",toString(i_station),"_returnFI.png",sep="",collapse = NULL))
  
  
  
}

#Todo : Compute prediction accuracy Using RMSE
sumOfTestDT <- data.frame(datetime = as.Date(character()), sumOfRrentCount = integer(), 
                          sumOfPrentCount = integer(), sumOfRreturnCount= integer(), sumOfPreturnCount = integer())

startDateTime <- ymd_hm(201501010000)
endDateTime <- ymd_hm(201512312300)
currentDateTime <- startDateTime

while(currentDateTime <= endDateTime){
  nextDateTime <- currentDateTime + hours(1)
  
  sumOfRrentC <- 0
  sumOfPrentC <- 0
  sumOfRreturnC <- 0
  sumOfPreturnC <- 0
  
  for(i_station in stationList){
    locs <- get(paste("stat", toString(i_station), "_rentTestDF", sep="",collapse = NULL))$datetime == currentDateTime
    testSubset <- get(paste("stat", toString(i_station), "_rentTestDF", sep="",collapse = NULL))[locs,]
    
    sumOfRrentC <- sumOfRrentC + testSubset$RrentCount
    sumOfPrentC <- sumOfPrentC + testSubset$PrentCount
    
    locs <- get(paste("stat", toString(i_station), "_returnTestDF", sep="",collapse = NULL))$datetime == currentDateTime
    testSubset <- get(paste("stat", toString(i_station), "_returnTestDF", sep="",collapse = NULL))[locs,]
    sumOfRreturnC <- sumOfRreturnC + testSubset$RreturnCount
    sumOfPreturnC <- sumOfPreturnC + testSubset$PreturnCount
  }
  
  sumOfTestDT <- rbind(sumOfTestDT, 
                       data.frame(datetime = currentDateTime, sumOfRrentCount = sumOfRrentC,sumOfPrentCount = sumOfPrentC, 
                                  sumOfRreturnCount= sumOfRreturnC, sumOfPreturnCount = sumOfPreturnC))
  currentDateTime <- nextDateTime
}

for(i_station in stationList){
  #RMSE
  get(paste("stat", toString(i_station), "_rentTestDF", sep="",collapse = NULL))$RrentRatio <- 
    get(paste("stat", toString(i_station), "_rentTestDF", sep="",collapse = NULL))$RrentCount/sumOfTestDT$sumOfRrentCount
  get(paste("stat", toString(i_station), "_rentTestDF", sep="",collapse = NULL))$PrentRatio <- 
    get(paste("stat", toString(i_station), "_rentTestDF", sep="",collapse = NULL))$PrentCount/sumOfTestDT$sumOfPrentCount
  get(paste("stat", toString(i_station), "_returnTestDF", sep="",collapse = NULL))$RreturnRatio <- 
    get(paste("stat", toString(i_station), "_returnTestDF", sep="",collapse = NULL))$RreturnCount/sumOfTestDT$sumOfRreturnCount
  get(paste("stat", toString(i_station), "_returnTestDF", sep="",collapse = NULL))$RreturnRatio <- 
    get(paste("stat", toString(i_station), "_returnTestDF", sep="",collapse = NULL))$RreturnCount/sumOfTestDT$sumOfPreturnCount
  
  get(paste("stat", toString(i_station), "_rentTestDF", sep="",collapse = NULL))$RenterrorRatio <- 
    get(paste("stat", toString(i_station), "_rentTestDF", sep="",collapse = NULL))$RrentRatio-
    get(paste("stat", toString(i_station), "_rentTestDF", sep="",collapse = NULL))$PrentRatio
  
  get(paste("stat", toString(i_station), "_returnTestDF", sep="",collapse = NULL))$ReturnerrorRatio <- 
    get(paste("stat", toString(i_station), "_returnTestDF", sep="",collapse = NULL))$RreturnRatio-
    get(paste("stat", toString(i_station), "_returnTestDF", sep="",collapse = NULL))$PreturnRatio  
  
  #Power (rent Count ratio error)
  get(paste("stat", toString(i_station), "_rentTestDF", sep="",collapse = NULL))$RenterrorRatioPow <-
    pow(get(paste("stat", toString(i_station), "_rentTestDF", sep="",collapse = NULL))$RenterrorRatio,2)
  
  #Power (return Count ratio error)
  get(paste("stat", toString(i_station), "_returnTestDF", sep="",collapse = NULL))$ReturnerrorRatioPow <-
    pow(get(paste("stat", toString(i_station), "_returnTestDF", sep="",collapse = NULL))$ReturnerrorRatio,2)
  
  sumOfRentRatioError <- sum(get(paste("stat", toString(i_station), "_rentTestDF", sep="",collapse = NULL))$RenterrorRatioPow)
  print(i_station," Rent error ratio : ")
  print(sqrt(sumOfRentRatioError/8760))
  
  sumOfReturnRatioError <- sum(get(paste("stat", toString(i_station), "_returnTestDF", sep="",collapse = NULL))$ReturnerrorRatioPow)
  print(i_station," Return error ratio : ")
  print(sqrt(sumOfReturnRatioError/8760))
}

