i_station <- 3

# rentTestDF = 테스 데이터가 저장될 변수
rentTestDF <- data.frame(datetime = seq(as_datetime("2015/01/01 00:00:00", tz='EST'), as_datetime("2015/12/31 23:00:00",tz='EST'), "hours"))

# 테스트 데이터에 시간 feature(시간/월/요일) 추가
rentTestDF['hour'] <- as.character(hour(rentTestDF$datetime))
rentTestDF['month'] <- as.character(month(rentTestDF$datetime))
rentTestDF['weekday'] <- as.character(wday(rentTestDF$datetime))
rentTestDF[rentTestDF$month >= 3 & rentTestDF$month <=5, 'season'] <- "1"
rentTestDF[rentTestDF$month >= 6 & rentTestDF$month <=8, 'season'] <- "2"
rentTestDF[rentTestDF$month >= 9 & rentTestDF$month <=11, 'season'] <- "3"
rentTestDF[rentTestDF$month == 12 | rentTestDF$month <=2, 'season'] <- "4"

# 기상 feature(기온/풍속/습도/강수량/적설량) 데이터 불러오기 
feature_weatherData <- weather2015Data[, c("Datetime", "Temperature", "Windspeed", "Humidity", "Rainfall", "Snowfall")]
colnames(feature_weatherData) <- c("datetime", "Temperature", "Windspeed", "Humidity", "Rainfall", "Snowfall")
feature_weatherData['datetime'] <- as_datetime(feature_weatherData$datetime, tz = 'EST')

# 3번 정류소에서 대여된 기록 수집
rentSubsetInTest <- tashu2015Data[tashu2015Data$RENT_STATION == i_station,]

# 1시간마다 대여된 자전거 수 계산
rentSubsetInTest <- data.frame(table(rentSubsetInTest$rentDateTime))
colnames(rentSubsetInTest) <- c("datetime", "rentcount")
rentSubsetInTest['datetime'] <- as.POSIXct(rentSubsetInTest$datetime, tz='EST')

# 테스트 데이터에 기상 데이터, 대여 수 추가트
rentTestDF <- left_join(rentTestDF, feature_weatherData)
rentTestDF <- left_join(rentTestDF, rentSubsetInTest)

# NA 값으로 표기된 강수량, 대여 수 0으로 치환
rentTestDF[is.na(rentTestDF)] <- 0

rentTestDF$rentcount <- rentTestDF$rentcount+1

assign(paste("stat",toString(i_station),"_rentTestDF",sep="",collapse = NULL), rentTestDF)
print(paste(toString(i_station)," Test data frame was created.", sep="", collapse = NULL))
