
seir_simulate <- function(init,param,n){
  
  mu = param[1]
  beta = param[2]
  sigma = param[3] 
  gamma = param[4] 
  
  seir_output <- matrix(data = 0, nrow = n+1, ncol = 5, byrow = FALSE,dimnames = NULL) %>%
                  as.data.frame() %>%
                  setNames(c('t','S','E','I','R')) %>%
                  mutate(S = as.double(S),
                         E = as.double(E),
                         I = as.double(I),
                         R = as.double(R))
  
  seir_output$t[1] = 0
  seir_output$S[1] = init[1]
  seir_output$E[1] = init[2]
  seir_output$I[1] = init[3]
  seir_output$R[1] = init[4]
  
  for (i in 2:(n+1)){
    
    seir_output$t[i] = i-1
    seir_output$S[i] = as.numeric(seir_output$S[i-1] - (beta*seir_output$S[i-1]*seir_output$I[i-1]))
    seir_output$E[i] = as.numeric(seir_output$E[i-1] + (beta*seir_output$S[i-1]*seir_output$I[i-1]) - sigma*seir_output$E[i-1])
    seir_output$I[i] = as.numeric(seir_output$I[i-1] + (sigma*seir_output$E[i-1]) - (gamma*seir_output$I[i-1]))
    seir_output$R[i] = as.numeric(seir_output$R[i-1] + (gamma*seir_output$I[i-1]))
  }
  
  return(seir_output)
}