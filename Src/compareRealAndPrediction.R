library(ggplot2)

#Variation
station <- 55

realRentCountDataPath <- paste("../data/station",toString(station),"/tashu_stat",toString(station),"_rent2014Data.csv", sep="",collapse = NULL)
realReturnCountDataPath <- paste("../data/station",toString(station),"/tashu_stat",toString(station),"_return2014Data.csv", sep="",collapse = NULL)
predictiveRentCountDataPath <- paste("../data/station",toString(station),"/tashu_stat",toString(station),"_rentpredictionResult_2014.csv",sep="",collapse = NULL)
predictiveReturnCountDataPath <- paste("../data/station",toString(station),"/tashu_stat",toString(station),"_returnpredictionResult_2014.csv",sep="",collapse = NULL)

#get real change value per hour
real_rentCountData <- read.csv(realRentCountDataPath, stringsAsFactors = F)
real_returnCountData <- read.csv(realReturnCountDataPath, stringsAsFactors = F)

#get predictive change value per hour
predictive_rentCountData <- read.csv(predictiveRentCountDataPath, stringsAsFactors = F)
predictive_returnCountData <- read.csv(predictiveReturnCountDataPath, stringsAsFactors = F)

bikeUsageData <- data.frame(datetime = real_rentCountData$datetime, station = toString(station), RchangeValue = real_returnCountData$returnCount-real_rentCountData$rentCount, PchangeValue = predictive_returnCountData$returnCount-predictive_rentCountData$rentCount)

#Visualize real data and predictive data
ggplot(bikeUsageData, aes(x = datetime, y= RchangeValue)) + geom_line()


#Todo : check the accuracy between real data and prediction data