library(randomForest)

#Feature extract Function
extractRentFeatures <- function(data){
  features <- c ("season","rentMonth","rentHour", "rentWeekday","temperature", "humidity","windspeed", "rainfall","snowfall" )
  
  return(data[, features])
}

extractReturnFeatures <- function(data){
  features <- c ("season","returnMonth","returnHour", "returnWeekday","temperature", "humidity", "rainfall", "snowfall", "windspeed")
  
  return(data[, features])
}

i_station = 3
rent_rf <- randomForest(extractRentFeatures(get(paste("station",toString(i_station),"_rentTrainDF",sep="",collapse = NULL))),
                        get(paste("station",toString(i_station),"_rentTrainDF",sep="",collapse = NULL))$rentCount,
                        ntree = 50, mtry = 2, importance = TRUE)
  
return_rf <- randomForest(extractReturnFeatures(get(paste("station",toString(i_station),"_returnTrainDF",sep="",collapse = NULL))),
                          get(paste("station",toString(i_station),"_returnTrainDF",sep="",collapse = NULL))$returnCount,
                          ntree = 50, mtry = 2, importance = TRUE)
  
assign(paste("rent_rf_", toString(i_station), sep="",collapse = NULL), rent_rf)
assign(paste("return_rf_", toString(i_station), sep="",collapse = NULL), return_rf)

