library(lubridate)
library(randomForest)

stationNum <- 3
strStatDF <- paste("stat",toString(stationNum),"_rentTrainDF", sep = "", collapse = NULL)

rent_rf <- randomForest()