seir_fit <- function(beta,case0,pred_day){
  
  # Initial state values 
  init = rep(0,4)
  names(init) <- c('S',"E",'I','R')
  
  init[1] = 1
  init[2] = case0
  init[3] = 0
  init[4] = 0
  
  # Parameter values
  param = rep(0,4)
  names(param) <- c('mu','beta','sigma','gamma')
  param[1] = 0
  param[2] = beta
  param[3] = 1/5.2 
  param[4] = 1/10
  
  seir_data<-seir_simulate(init,param,100)
  val_fit<-seir_data[pred_day+1,'I']
  
  return(as.numeric(val_fit))
}