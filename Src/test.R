library(lubridate)

currentDT <- ymd_hm(201305051300)
tmpFestData <- festivalData[festivalData$startDate <= currentDT & festivalData$endDate > currentDT,]

i_station <- 18

nearStatList <- strsplit(tmpFestData$nearStation,",")
for (i in nearStatList){
  if(toString(i_station) %in% i){
    print("oh...")
  }
}
print("hi")
print("hi")
