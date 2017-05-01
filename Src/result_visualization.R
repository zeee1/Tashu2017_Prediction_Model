library(ggplot2)
library(lubridate)
library(scales)
library(plyr)
library(readr)

## Draw mean(rent Count) per hour. geom_line for real Data in 2015, geom_bar for predictive data in station 3 in 2015.
sampleDF <- stat3_rentTestDF[stat3_rentTestDF$rentWeekday =='Sun',]

day_summary1 <- ddply(sampleDF,.(rentHour),summarise, count = mean(RrentCount), 대여량 = '실제 대여 량')
day_summary2 <- ddply(sampleDF,.(rentHour), summarise, count = mean(PrentCount),대여량 = '예측 대여 량')
newDF <- rbind(day_summary1, day_summary2)
ggplot(newDF, aes(x = rentHour, y = count, color = 대여량))+
  geom_point(data = day_summary1, aes(group=대여량)) +
  geom_line(data = day_summary1, aes(group=대여량)) +
  geom_point(data = day_summary2, aes(group=대여량), shape = 2) +
  geom_line(data = day_summary2, aes(group=대여량)) +
  #scale_x_discrete("Hour")+
  xlab("Hour")+
  ylab("Count")+
  theme(legend.position = c(1,1),
        legend.justification = c(1, 1),
        legend.text = element_text(size = 15), 
        axis.text.x = element_text(size = 15), 
        axis.text.y = element_text(size = 15)
        )

sampleDF <- stat11_rentTestDF[stat11_rentTestDF$rentWeekday =='Wed',]

day_summary1 <- ddply(sampleDF,.(rentHour),summarise, count = mean(RrentCount), 대여량 = '실제 대여 량')
day_summary2 <- ddply(sampleDF,.(rentHour), summarise, count = mean(PrentCount),대여량 = '예측 대여 량')
newDF <- rbind(day_summary1, day_summary2)

ggplot(newDF, aes(x = rentHour, y = count, color = 대여량))+
  geom_point(data = day_summary1, aes(group=대여량)) +
  geom_line(data = day_summary1, aes(group=대여량)) +
  geom_point(data = day_summary2, aes(group=대여량), shape = 2) +
  geom_line(data = day_summary2, aes(group=대여량)) +
  xlab("Hour")+
  ylab("Count")+
  theme(legend.position = c(1,1),
        legend.justification = c(1, 1),
        legend.text = element_text(size = 15), 
        axis.text.x = element_text(size = 15), 
        axis.text.y = element_text(size = 15)
  )


## Draw mean(rent Count) per hour. circle point for real Data in 2015, triangle point for predictive data in 2015.
ggplot(sampleDF, aes(x = rentHour, y = count, colour = rentWeekday))+
  geom_point(data = day_summary1, aes(group=rentWeekday)) +
  geom_line(data = day_summary1, aes(group=rentWeekday)) +
  geom_point(data = day_summary2, aes(group=rentWeekday), shape=2) +
  geom_line(data = day_summary2, aes(group=rentWeekday)) +
  scale_x_discrete("Hour")+
  theme_minimal()

sampleDF <- stat3_returnTestDF[stat3_returnTestDF$returnWeekday =='Sun'|stat3_returnTestDF$returnWeekday =='Wed',]

day_summary1 <- ddply(sampleDF,.(returnWeekday,returnHour),summarise, count = mean(RreturnCount))
day_summary2 <- ddply(sampleDF,.(returnWeekday, returnHour), summarise, count = mean(PreturnCount))

ggplot(sampleDF, aes(x = returnHour, y = count, colour = returnWeekday))+
  geom_bar(data = day_summary2, position = "dodge", stat = "identity", aes(group = returnWeekday), fill = "white")+
  geom_point(data = day_summary1, aes(group=returnWeekday)) +
  geom_line(data = day_summary1, aes(group=returnWeekday)) +
  scale_x_discrete("Hour")+
  theme_minimal()

## Draw mean(rent Count) per hour. geom_line for real Data in 2015, geom_bar for predictive data in station 21 in 2015.
sampleDF <- stat21_rentTestDF[stat21_rentTestDF$rentWeekday =='Sun'|stat21_rentTestDF$rentWeekday =='Wed',]

day_summary1 <- ddply(sampleDF,.(rentWeekday,rentHour),summarise, count = mean(RrentCount))
day_summary2 <- ddply(sampleDF,.(rentWeekday, rentHour), summarise, count = mean(PrentCount))

ggplot(sampleDF, aes(x = rentHour, y = count, colour = rentWeekday))+
  geom_bar(data = day_summary2, position = "dodge", stat = "identity", aes(group = rentWeekday), fill = "white")+
  geom_point(data = day_summary1, aes(group=rentWeekday)) +
  geom_line(data = day_summary1, aes(group=rentWeekday)) +
  scale_x_discrete("Hour")+
  theme_minimal()

## Draw mean(rent Count) per hour. geom_line for real Data in 2015, geom_bar for predictive data in station 9 in 2015.
sampleDF <- stat24_rentTestDF[stat24_rentTestDF$rentWeekday =='Sun'|stat24_rentTestDF$rentWeekday =='Wed',]

day_summary1 <- ddply(sampleDF,.(rentWeekday,rentHour),summarise, count = mean(RrentCount))
day_summary2 <- ddply(sampleDF,.(rentWeekday, rentHour), summarise, count = mean(PrentCount))

ggplot(sampleDF, aes(x = rentHour, y = count, colour = rentWeekday))+
  geom_bar(data = day_summary2, position = "dodge", stat = "identity", aes(group = rentWeekday), fill = "white")+
  geom_point(data = day_summary1, aes(group=rentWeekday)) +
  geom_line(data = day_summary1, aes(group=rentWeekday)) +
  scale_x_discrete("Hour")+
  theme_minimal()



## Draw montly rent Count and average temperature per month in 2015.
monthlyRentCountIn2015 <- data.frame(month = as.character(), avgOfTemp = as.integer(), totalRentCount= as.integer())

for (i_month in monthList){
  locs <- month(sumOfTestDT$datetime) == i_month
  locsDF <- sumOfTestDT[locs,]
  
  i_month_RentCount <- sum(locsDF$sumOfRrentCount)
  
  locs <- month(weather2015Data$Datetime) == i_month
  locsDF <- weather2015Data[locs,]
  
  meanOfTemperaturePerMonth <- mean(locsDF$Temperature)
  monthlyRentCountIn2015 <- rbind(monthlyRentCountIn2015, 
                                  data.frame(month = i_month,avgOfTemp = meanOfTemperaturePerMonth,totalRentCount = i_month_RentCount))
  
}

#month_rentC_df <- data.frame(month = monthlyRentCountIn2015$month, totalRentCount = monthlyRentCountIn2015$totalRentCount)
#month_temp_df <- data.frame(month = monthlyRentCountIn2015$month, avgOfTemp = monthlyRentCountIn2015$avgOfTemp)

month_rentC_df <- ddply(monthlyRentCountIn2015,.(month, totalRentCount), summarise, count = totalRentCount)
month_temp_df <- ddply(monthlyRentCountIn2015,.(month, avgOfTemp), summarise, count = avgOfTemp)

ggplot(monthlyRentCountIn2015, aes(x = month, y=count))+
  geom_bar(data = month_temp_df, stat = "identity", fill = "#55cc6b")+
  geom_point(data = month_rentC_df, aes(group = 1)) +
  geom_line(data = month_rentC_df,aes(group = 1)) +
  scale_x_discrete("Month")

