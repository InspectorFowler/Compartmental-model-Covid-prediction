
MAPE<-function(data, weight = FALSE, na.rm = TRUE){
  
  # Data should contain two columns, first - actuals, second - prediction
  names(data)<-c("Actual","forecast")
  if(na.rm == TRUE) data<-data[complete.cases(data),]
  
  if(nrow(data)==0){
    return(NA)
  }else{
    
    data<-data %>%
      mutate(AE = abs(Actual - forecast),
             APE = ifelse((Actual == 0 & forecast == 0),0,
                          (ifelse((Actual == 0 & forecast != 0),1, AE/Actual))))
    
    MAPE<-ifelse(weight == TRUE,weighted.mean(data$APE,data$Actual,na.rm = TRUE),mean(data$APE,na.rm = TRUE))
    return(MAPE)
  }
}