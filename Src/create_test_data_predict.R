#Feature extract Function
extractRentFeatures <- function(data){
  features <- c ("season","rentMonth","rentHour", "rentWeekday","temperature", "humidity","windspeed", "rainfall","isFestival")
  
  return(data[, features])
}

rentSubsetInTest <- tashu2015Data[tashu2015Data$RENT_STATION == i_station,]
returnSubsetInTest <- tashu2015Data[tashu2015Data$RETURN_STATION == i_station,]

startDateTime <- ymd_hms(20150101000000)
endDateTime <- ymd_hms(20151231230000)
currentDateTime <- startDateTime


rentTestDF <- data.frame(datetime = as.Date(character()),
                         season = character(),
                         rentMonth = character(), 
                         rentHour = character(), 
                         rentWeekday = character(), 
                         temperature = integer(), 
                         humidity = integer(),
                         windspeed = integer(),
                         rainfall = integer(),
                         snowfall = integer(),
                         RrentCount = integer(), 
                         PrentCount = integer())

while (currentDateTime <= endDateTime){
  nextDateTime <- currentDateTime+hours(1)
  
  rentTimeSubset <- rentSubsetInTest[rentSubsetInTest$rentDateTime >= currentDateTime & rentSubsetInTest$rentDateTime < nextDateTime,]
  weatherSubset <- weather2015Data[weather2015Data$DT == currentDateTime,]
  
  if(is.na(weatherSubset$Rainfall)){
    weatherSubset$Rainfall <- 0
  }
  if(is.na(weatherSubset$Snowfall)){
    weatherSubset$Snowfall <- 0
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
                                            humidity= weatherSubset$Humidity,
                                            windspeed = weatherSubset$Windspeed,
                                            rainfall = weatherSubset$Rainfall,
                                            snowfall = weatherSubset$Snowfall,
                                            isFestival = isFestival,
                                            RrentCount = NROW(rentTimeSubset), 
                                            PrentCount = NA))
  

  currentDateTime <- nextDateTime
}