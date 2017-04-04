###
### modifyTrainDF.R - delete rent Data from 0:00 am to 4:00 am
### Train Data consists of data from 5:00 to 23:00
###

for(i_station in 1:144){
  #Todo : delete data(0:00 ~ 4:00) in rentTrainDF
  get(paste("stat",toString(i_station),"_rentTrainDF",sep = "", collapse = NULL))<-
    get(paste("stat",toString(i_station),"_rentTrainDF",sep = "", collapse = NULL))[
      as.numeric(get(paste("stat",toString(i_station),"_rentTrainDF",sep = "", collapse = NULL))$rentHour) > 5,]
  
  #Todo : delete data(0:00 ~ 4:00) in rentTestDF
  
}