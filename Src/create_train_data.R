library(scales)
library(randomForest)
library(lubridate)
library(plyr)
library(readr)
library(dplyr)

i_station <- 3

# rentTrainDF = 학습 데이터가 저장될 변수
rentTrainDF <- data.frame(datetime = seq(as_datetime("2013/01/01 00:00:00", tz='EST'), as_datetime("2014/12/31 23:00:00",tz='EST'), "hours"))

# 학습데이터에 시간 feature(시간/월/요일) 추가
rentTrainDF['hour'] <- as.character(hour(rentTrainDF$datetime))
rentTrainDF['month'] <- as.character(month(rentTrainDF$datetime))
rentTrainDF['weekday'] <- as.character(wday(rentTrainDF$datetime))
rentTrainDF['season'] <- as.character()
rentTrainDF[rentTrainDF$month >= 3 & rentTrainDF$month <=5, 'season'] <- "1"
rentTrainDF[rentTrainDF$month >= 6 & rentTrainDF$month <=8, 'season'] <- "2"
rentTrainDF[rentTrainDF$month >= 9 & rentTrainDF$month <=11, 'season'] <- "3"
rentTrainDF[rentTrainDF$month == 12 | rentTrainDF$month <=2, 'season'] <- "4"

# 기상 feature(기온/풍속/습도/강수량/적설량) 데이터 불러오기 
feature_weatherData <- weather20132014Data[, c("Datetime", "Temperature", "Windspeed", "Humidity", "Rainfall", "Snowfall")]
colnames(feature_weatherData) <- c("datetime", "Temperature", "Windspeed", "Humidity", "Rainfall", "Snowfall")
feature_weatherData['datetime'] <- as_datetime(feature_weatherData$datetime, tz = 'EST')

# 3번 정류소에서 대여된 기록 수집
rentSubsetInTrain <- tashu20132014Data[tashu20132014Data$RENT_STATION == i_station,]

# 1시간마다 대여된 자전거 수 계산
rentSubsetInTrain <- data.frame(table(rentSubsetInTrain$rentDateTime))
colnames(rentSubsetInTrain) <- c("datetime", "rentcount")
rentSubsetInTrain['datetime'] <- as.POSIXct(rentSubsetInTrain$datetime, tz='EST')

# 학습 데이터에 기상 데이터, 대여 수 추가
rentTrainDF <- left_join(rentTrainDF, feature_weatherData)
rentTrainDF <- left_join(rentTrainDF, rentSubsetInTrain)

# NA 값으로 표기된 강수량, 대여 수 0으로 치환
rentTrainDF[is.na(rentTrainDF)] <- 0

rentTrainDF$rentcount <- rentTrainDF$rentcount+1

assign(paste("stat",toString(i_station),"_rentTrainDF",sep="",collapse = NULL), rentTrainDF)
print(paste(toString(i_station)," Train data frame was created.", sep="", collapse = NULL))
