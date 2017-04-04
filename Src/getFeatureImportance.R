library(randomForest)

#Feature extract Function
extractRentFeatures <- function(data){
  features <- c ("season","rentMonth","rentHour", "rentWeekday","temperature", "humidity", "rainfall","isFestival")
  
  return(data[, features])
}

extractReturnFeatures <- function(data){
  features <- c ("season","returnMonth","returnHour", "returnWeekday","temperature", "humidity", "rainfall", "isFestival")
  
  return(data[, features])
}

for (i_station in 1:144){
  rent_rf <- randomForest(extractRentFeatures(get(paste("stat",toString(i_station),"_rentTrainDF",sep="",collapse = NULL))),
                          get(paste("stat",toString(i_station),"_rentTrainDF",sep="",collapse = NULL))$rentCount, ntree = 50, mtry = 2, importance = TRUE)
  
  return_rf <- randomForest(extractReturnFeatures(get(paste("stat",toString(i_station),"_returnTrainDF",sep="",collapse = NULL))),
                            get(paste("stat",toString(i_station),"_returnTrainDF",sep="",collapse = NULL))$returnCount, ntree = 50, mtry = 2, importance = TRUE)
  imp <- importance(rent_rf, type=1)
  featureImportance <- data.frame(Feature=row.names(imp), Importance=imp[,1])
  
  ggplot(featureImportance, aes(x=reorder(Feature, Importance), y=Importance)) +
    geom_bar(stat="identity", fill="#53cfff") +
    coord_flip() + 
    theme_light(base_size=20) +
    xlab("Importance") +
    ylab("") + 
    ggtitle("Random Forest Feature Importance\n") +
    theme(plot.title=element_text(size=18))
  ggsave(paste("../images/sampleTest/rent/",toString(i_station),"_rentFI.png",sep="",collapse = NULL))
  
  imp <- importance(return_rf, type=1)
  featureImportance <- data.frame(Feature=row.names(imp), Importance=imp[,1])
  
  ggplot(featureImportance, aes(x=reorder(Feature, Importance), y=Importance)) +
    geom_bar(stat="identity", fill="#53cfff") +
    coord_flip() + 
    theme_light(base_size=20) +
    xlab("Importance") +
    ylab("") + 
    ggtitle("Random Forest Feature Importance\n") +
    theme(plot.title=element_text(size=18))
  ggsave(paste("../images/sampleTest/return/",toString(i_station),"_returnFI.png",sep="",collapse = NULL))
  
}