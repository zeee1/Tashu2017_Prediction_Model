library(lubridate)

#Load Tashu rent/return data from 2013 to 2014
tashu20132014Data <- read.csv("../data/tashu20132014.csv", stringsAsFactors = F)
#Remove rows that contain NA
tashu20132014Data <- na.omit(tashu20132014Data)
tashu20132014Data$rentDateTime <- ymd_hms(tashu20132014Data$RENT_DATE)
tashu20132014Data$returnDateTime <- ymd_hms(tashu20132014Data$RETURN_DATE)

#Load Tashu rent/return data in 2015.
tashu2015Data <- read.csv("../data/tashu2015.csv", stringsAsFactors = F)
#Remove rows that contain NA
tashu2015Data <- na.omit(tashu2015Data)
tashu2015Data$rentDateTime <- ymd_hms(tashu2015Data$RENT_DATE)
tashu2015Data$returnDateTime <- ymd_hms(tashu2015Data$RETURN_DATE)

#Load weather data from 2013 to 2015 in Daejeon
weather2013Data <- read.csv("../data/weather/2013_weatherData.csv", stringsAsFactors = F)
weather2014Data <- read.csv("../data/weather/2014_weatherData.csv", stringsAsFactors = F)
weather2015Data <- read.csv("../data/weather/2015_weatherData.csv", stringsAsFactors = F)
weather2013Data$DT <- ymd_hm(weather2013Data$Datetime)
weather2014Data$DT <- ymd_hm(weather2014Data$Datetime)
weather2015Data$DT <- ymd_hm(weather2015Data$Datetime)
weather2013Data <- weather2013Data[minute(weather2013Data$DT) == 0,]
weather2014Data <- weather2014Data[minute(weather2014Data$DT) == 0,]
weather2015Data <- weather2015Data[minute(weather2015Data$DT) == 0,]

#Load festival data from 2013 to 2015 in Daejeon
festivalData <- read.csv("../data/festival_info.csv", stringsAsFactors = F)
festivalData$startDate <- ymd_hm(festivalData$startDate)
festivalData$endDate <- ymd_hm(festivalData$endDate)

