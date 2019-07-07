library(lubridate)

#Load Tashu rent/return data from 2013 to 2014
tashu20132014Data <- read.csv("../data/tashu20132014.csv", stringsAsFactors = F)

#Remove rows that contain NA
tashu20132014Data <- na.omit(tashu20132014Data)
tashu20132014Data$rentDateTime <- ymd_hms(tashu20132014Data$RENT_DATE)
#tashu20132014Data$returnDateTime <- ymd_hms(tashu20132014Data$RETURN_DATE)

minute(tashu20132014Data$rentDateTime) <- 0
second(tashu20132014Data$rentDateTime) <- 0

#Load Tashu rent/return data in 2015.
tashu2015Data <- read.csv("../data/tashu2015.csv", stringsAsFactors = F)
#Remove rows that contain NA
tashu2015Data <- na.omit(tashu2015Data)
tashu2015Data$rentDateTime <- ymd_hms(tashu2015Data$RENT_DATE)
minute(tashu2015Data$rentDateTime) <- 0
second(tashu2015Data$rentDateTime) <- 0
#tashu2015Data$returnDateTime <- ymd_hms(tashu2015Data$RETURN_DATE)

#Load weather data from 2013 to 2015 in Daejeon
weather2013Data <- read.csv("../data/weather/2013_weatherData.csv", stringsAsFactors = F)
weather2014Data <- read.csv("../data/weather/2014_weatherData.csv", stringsAsFactors = F)
weather2015Data <- read.csv("../data/weather/2015_weatherData.csv", stringsAsFactors = F)
weather2013Data$Datetime <- ymd_hm(weather2013Data$Datetime)
weather2014Data$Datetime <- ymd_hm(weather2014Data$Datetime)
weather2015Data$Datetime <- ymd_hm(weather2015Data$Datetime)
weather2013Data <- weather2013Data[minute(weather2013Data$Datetime) == 0,]
weather2014Data <- weather2014Data[minute(weather2014Data$Datetime) == 0,]
weather2015Data <- weather2015Data[minute(weather2015Data$Datetime) == 0,]

weather2013Data['Datetime'] <- as.POSIXct(weather2013Data$Datetime, tz = 'EST')
weather2014Data['Datetime'] <- as.POSIXct(weather2014Data$Datetime, tz = 'EST')
weather2015Data['Datetime'] <- as.POSIXct(weather2015Data$Datetime, tz = 'EST')

weather20132014Data <- rbind(weather2013Data, weather2014Data)


