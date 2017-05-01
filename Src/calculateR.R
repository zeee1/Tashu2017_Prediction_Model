library(lubridate)

#Todo : Collect one month data 2015.May in station 3.
tashuRentDataInstat3_201505 <- stat3_rentTestDF[month(stat3_rentTestDF$datetime) == 5 | month(stat3_rentTestDF$datetime) == 6,]
tashuReturnDataInstat3_201505 <- stat3_returnTestDF[month(stat3_returnTestDF$datetime) == 5 | month(stat3_returnTestDF$datetime) == 6,]

#Todo : Collect One day data from 2015.05.01 ~ 2015.05.31
startDate <- ymd(20150501)
endDate <- ymd(20150630)
indexDate <- startDate

numOfImbalance <- 0
numOfTimeForRebalance <- 0
numOfOverRent <- 0
numOfOverReturn <- 0


while(indexDate <= endDate){
  nextDate <- indexDate + days(1)
  
  oneDayRentData <- tashuRentDataInstat3_201505[date(tashuRentDataInstat3_201505$datetime) == indexDate,]
  oneDayReturnData <- tashuReturnDataInstat3_201505[date(tashuReturnDataInstat3_201505$datetime) == indexDate,]
  numOfBikes <- 19
  
  hourList <- c(5:23)
  
  for(i_hour in hourList){
    oneHourRentData <- oneDayRentData[hour(oneDayRentData$datetime) == i_hour,]
    numOfRentBike <- oneHourRentData$PrentCount
    
    oneHourReturnData <- oneDayReturnData[hour(oneDayReturnData$datetime) == i_hour,]
    numOfReturnBike <- oneHourReturnData$PreturnCount
    
    numOfBikes <- numOfBikes-numOfRentBike+numOfReturnBike
    
    if(numOfBikes <= 0){
      print(oneHourRentData$datetime)
      print("잔여수 0 이하")
      #numOfBikes <- 10
      #numOfImbalance <- numOfImbalance+1
      #numOfTimeForRebalance <- numOfTimeForRebalance+1
      next
    }
    #if(numOfBikes < 6){
    #  print(oneHourRentData$datetime)
    #  print(paste("현재 잔여 자전거 수 : ", toString(numOfBikes)))
    #  print("점유율 30% 이하. 자전거 재배치 필요.")
    #  numOfBikes <- 10
    #  numOfTimeForRebalance <- numOfTimeForRebalance+1
    #  numOfOverRent<- numOfOverRent+1
    #}
    #else if(numOfBikes >= 6 && numOfBikes < 17){
      #print(oneHourRentData$datetime)
      #print(paste("현재 잔여 자전거 수 : ", toString(numOfBikes)))
    #}
    #if(numOfBikes >= 19){
     # print(oneHourRentData$datetime)
    #  print(paste("현재 잔여 자전거 수 : ", toString(numOfBikes)))
    #  print("점유율 90% 이상. 자전거 재배치 필요.")
      #numOfBikes <- 10
      #numOfTimeForRebalance <- numOfTimeForRebalance+1
      #numOfOverReturn<- numOfOverReturn+1
     # next
    #}
    
    #if(numOfBikes >= 19){
    #  print(oneHourRentData$datetime)
    #  print("불균형 발생")
    #  numOfBikes <- 10
    #  numOfImbalance <- numOfImbalance+1
    #  numOfTimeForRebalance <- numOfTimeForRebalance+1
    #}
  }
  indexDate <- nextDate
}

#
startDateTime <- ymd_hm(201506020500)
endDateTime <- ymd_hm(201506021600)
indexDateTime <- startDateTime

numOfBikesInStat3 <- 19

testDataForRearrange <- data.frame(datetime = as.Date(as.character()), count = integer(), check = as.character())

while(indexDateTime <= endDateTime){
  nextDateTime <- indexDateTime+hours(1)
  
  numOfPRentCount <- stat3_rentTestDF[stat3_rentTestDF$datetime == indexDateTime,"PrentCount"]
  numOfPRentCount <- round(numOfPRentCount)
  
  numOfPReturnCount <- stat3_returnTestDF[stat3_returnTestDF$datetime == indexDateTime,"PreturnCount"]
  numOfPReturnCount <- round(numOfPReturnCount)
  
  numOfBikesInStat3 <- numOfBikesInStat3-numOfPRentCount +numOfPReturnCount
  
  #print(indexDateTime)
  #print("잔여 수 : ")
  #print(numOfBikesInStat3)
  #print("대여 수 : ")
  #print(numOfPRentCount)
  #print("반납 수 : ")
  #print(numOfPReturnCount)
  
  testDataForRearrange <- rbind(testDataForRearrange, data.frame(
    datetime = indexDateTime, count = numOfPRentCount, check = 'Rent')) 
  
  testDataForRearrange <- rbind(testDataForRearrange, data.frame(
    datetime = indexDateTime, count = numOfPReturnCount, check = 'Return')) 
  
  testDataForRearrange <- rbind(testDataForRearrange, data.frame(
    datetime = indexDateTime, count = numOfBikesInStat3, check = 'Rest')) 
  
  if(numOfBikesInStat3 <= 0){
    break
  }
  indexDateTime <- nextDateTime
}
testDataForRearrange$hour <- hour(testDataForRearrange$datetime)

ggplot(testDataForRearrange, aes(x = hour, y= count, fill = check ))+
  geom_bar(data = testDataForRearrange,stat = "identity",position = "dodge", aes(group = check))+
  #xlab("Hour")+
  scale_x_continuous(breaks = seq(min(testDataForRearrange$hour), max(testDataForRearrange$hour),1))+
  theme(legend.position = c(0,1),
        legend.justification = c(0, 1),
        legend.text = element_text(size = 15), 
        axis.text.x = element_text(size = 15), 
        axis.text.y = element_text(size = 15))


