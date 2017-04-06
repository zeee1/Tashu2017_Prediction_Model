library(lubridate)

sumOfTestDT <- data.frame(datetime = as.Date(character()), sumOfRrentCount = integer(), 
                          sumOfPrentCount = integer(), sumOfRreturnCount= integer(), sumOfPreturnCount = integer())

startDateTime <- ymd_hm(201501010000)
endDateTime <- ymd_hm(201512312300)
currentDateTime <- startDateTime

while(currentDateTime <= endDateTime){
  nextDateTime <- currentDateTime + hours(1)
  if(hour(currentDateTime) >= 5){
    sumOfRrentC <- 0
    sumOfPrentC <- 0
    sumOfRreturnC <- 0
    sumOfPreturnC <- 0
    
    for(i_station in 1:144){
      locs <- get(paste("stat", toString(i_station), "_rentTestDF", sep="",collapse = NULL))$datetime == currentDateTime
      testSubset <- get(paste("stat", toString(i_station), "_rentTestDF", sep="",collapse = NULL))[locs,]
      
      sumOfRrentC <- sumOfRrentC + testSubset$RrentCount
      sumOfPrentC <- sumOfPrentC + testSubset$PrentCount
      
      locs <- get(paste("stat", toString(i_station), "_returnTestDF", sep="",collapse = NULL))$datetime == currentDateTime
      testSubset <- get(paste("stat", toString(i_station), "_returnTestDF", sep="",collapse = NULL))[locs,]
      sumOfRreturnC <- sumOfRreturnC + testSubset$RreturnCount
      sumOfPreturnC <- sumOfPreturnC + testSubset$PreturnCount
    }
    
    sumOfTestDT <- rbind(sumOfTestDT, 
                         data.frame(datetime = currentDateTime, sumOfRrentCount = sumOfRrentC,sumOfPrentCount = sumOfPrentC, 
                                    sumOfRreturnCount= sumOfRreturnC, sumOfPreturnCount = sumOfPreturnC))
  }
  
  currentDateTime <- nextDateTime
}


for(i_station in 1:144){
  tmpDF <- data.frame(datetime = get(paste("stat", toString(i_station), "_rentTestDF", sep="",collapse = NULL))$datetime, 
                      RrentRatio = 0,
                      PrentRatio = 0,
                      RreturnRatio = 0,
                      PreturnRatio = 0)
  
  #RMSE
  
  tmpDF$RrentRatio <- get(paste("stat", toString(i_station), "_rentTestDF", sep="",collapse = NULL))$RrentCount/sumOfTestDT$sumOfRrentCount
  locs <- is.nan(tmpDF$RrentRatio)
  tmpDF[locs,]$RrentRatio <- 0
  tmpDF$PrentRatio <- get(paste("stat", toString(i_station), "_rentTestDF", sep="",collapse = NULL))$PrentCount/sumOfTestDT$sumOfPrentCount
  #tmpDF$RreturnRatio <- get(paste("stat", toString(i_station), "_returnTestDF", sep="",collapse = NULL))$RreturnCount/sumOfTestDT$sumOfRreturnCount
  #locs <- is.nan(tmpDF$RreturnRatio)
  #tmpDF[locs,]$RreturnRatio <- 0
  #tmpDF$PreturnRatio <- get(paste("stat", toString(i_station), "_returnTestDF", sep="",collapse = NULL))$PreturnCount/sumOfTestDT$sumOfPreturnCount
  

  tmpDF$RenterrorRatio <- tmpDF$RrentRatio-tmpDF$PrentRatio
  

  #tmpDF$ReturnerrorRatio <- tmpDF$RreturnRatio-tmpDF$PreturnRatio
  
  #Power (rent Count ratio error)

  tmpDF$RenterrorRatioPow <- tmpDF$RenterrorRatio^2
  

  #tmpDF$ReturnerrorRatioPow <- tmpDF$ReturnerrorRatio^2
  
  sumOfRentRatioError <- sum(tmpDF$RenterrorRatioPow)
  
  write(paste("stat", toString(i_station)," : ",toString(sqrt(sumOfRentRatioError/8760)), sep="", collapse = NULL),
        file = "accuracy_output.txt", append=TRUE)
  #sumOfReturnRatioError <- sum(tmpDF$ReturnerrorRatioPow)
  
  #write(paste("stat", toString(i_station),"return_Error_Ratio : ",toString(sqrt(sumOfReturnRatioError/8760)), sep="", collapse = NULL),
  #      file = "accuracy_output.txt", append=TRUE)
  #write("\n\n", file = "output.txt", append=TRUE)
  #assign(paste("stat",toString(i_station),"_ratioDF", sep="", collapse = NULL), )
}


