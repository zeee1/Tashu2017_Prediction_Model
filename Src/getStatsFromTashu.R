## Get basic statistics from tashu data(2013 ~ 2015)

library(ggplot2)
library(lubridate)
library(scales)
library(plyr)
library(readr)

##Get total number of rent Count in 3 years(2013~2015)
totalNumOfRentCountIn3years <- NROW(tashu20132014Data)+NROW(tashu2015Data)

##Get daily average of rent Count.
avgOfDailyRentCount <- totalNumOfRentCountIn3years/365*3

##Get average of rent time.
durationTime1314 <- data.frame(durationTime = tashu20132014Data$returnDateTime-tashu20132014Data$rentDateTime)
durationTime15 <- data.frame(durationTime = tashu2015Data$returnDateTime-tashu2015Data$rentDateTime)
totalDurationTime <- sum(durationTime1314$durationTime)+sum(durationTime15$durationTime)
avgOfDurationTime <- totalDurationTime/totalNumOfRentCountIn3years

##Get monthly average of rent count using graph.
monthlyRentCountIn3year <- data.frame(month = as.character(), monthlyRentCount= as.integer())

for (i_month in monthList){
  locs <- month(tashu20132014Data$rentDateTime) == i_month
  monthlyData <- tashu20132014Data[locs,]
  numOfmonthlyDatain1314 <- NROW(monthlyData)
  
  locs <- month(tashu2015Data$rentDateTime) == i_month
  monthlyData <- tashu2015Data[locs,]
  numOfMonthlyDatain15 <- NROW(monthlyData)
  
  monthlyRentCountIn3year <- rbind(monthlyRentCountIn3year, data.frame(month = toString(i_month), 
                                                                       monthlyRentCount = (numOfmonthlyDatain1314+numOfMonthlyDatain15)
                                                                       )
                                   )
}
month_rentC_df <- ddply(monthlyRentCountIn3year,.(month, avgOfMonthlyRentCount), summarise, count = avgOfMonthlyRentCount)
ggplot(monthlyRentCountIn3year, aes(x = month, y=count))+
  geom_bar(data = month_rentC_df, stat = "identity", fill = "#55cc6b")


##Get total number of rent Count in each year(2013, 2014, 2015)
rentCountIn2013 <- NROW(tashu20132014Data[year(tashu20132014Data$rentDateTime) == 2013,])
rentCountIn2014 <- NROW(tashu20132014Data[year(tashu20132014Data$rentDateTime) == 2014,])
rentCountIn2015 <- NROW(tashu2015Data)

##Get average of rent Count per day.
avgOfRentCountIn2013 <- rentCountIn2013/365
avgOfRentCountIn2014 <- rentCountIn2014/365
avgOfRentCountIn2015 <- rentCountIn2015/365

##Get average of rent time.
tashu2013Data <- tashu20132014Data[year(tashu20132014Data$rentDateTime) == 2013,]
tashu2014Data <- tashu20132014Data[year(tashu20132014Data$rentDateTime) == 2014,]
durationTime13 <- data.frame(durationTime = tashu2013Data$returnDateTime-tashu2013Data$rentDateTime)
durationTime14 <- data.frame(durationTime = tashu2014Data$returnDateTime-tashu2014Data$rentDateTime)
durationTime15 <- data.frame(durationTime = tashu2015Data$returnDateTime-tashu2015Data$rentDateTime)
totalDurationTimeIn2013 <- sum(durationTime13$durationTime)
totalDurationTimeIn2014 <- sum(durationTime14$durationTime)
totalDurationTimeIn2015 <- sum(durationTime15$durationTime)

avgOfDurationTimeIn2013 <- (totalDurationTimeIn2013/rentCountIn2013)/60
avgOfDurationTimeIn2014 <- (totalDurationTimeIn2014/rentCountIn2014)/60
avgOfDurationTimeIn2015 <- (totalDurationTimeIn2015/rentCountIn2015)/60

