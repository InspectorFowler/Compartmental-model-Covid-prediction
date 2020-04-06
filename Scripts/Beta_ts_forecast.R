
######################################################################################
# Load libraries
######################################################################################

# wrangling
suppressMessages(library(data.table)) # data table manipulation
suppressMessages(library(dplyr)) # Data wrangling
suppressMessages(library(tidyr)) # Data wrangling
suppressMessages(library(stringr)) # Data wrangling

# Modeling
suppressMessages(library(caret)) # rmse and metrics
suppressMessages(library(forecast)) # time series
suppressMessages(library(smooth)) # RF

# !diagnostics off
options(scipen=999)

######################################################################################
#Working directory and environment
######################################################################################

setwd('C:/Users/Parikshit_verma/Desktop/Covid-19')

######################################################################################
# Functions
######################################################################################

source('Functions/mape.R')

######################################################################################
# Read data
######################################################################################

covid_us_st<-read.csv('Output/Estimation/US_state_beta.csv',stringsAsFactors = FALSE)

######################################################################################
# Data modelling - Time series
######################################################################################

select_states<-covid_us_st %>%
               group_by(State) %>% 
               summarise(total = n()) %>% 
               arrange(desc(total)) %>% 
               filter(total > 15) %>% 
               dplyr::select(c('State')) %>% 
               unlist()

covid_us_st<-covid_us_st %>%
             drop_na() %>%
             filter(State %in% select_states) %>%
             dplyr::select(c('Day','State','Beta'))

ts_output = data.frame(State=character(length(select_states)),
                       min_mape=double(length(select_states)),
                       model=character(length(select_states)),
                       param=character(length(select_states)),
                       stringsAsFactors=FALSE)

for (i in 1:length(select_states)){
  
  ts_output$State[i] = select_states[i]
  
  sub_covid<-covid_us_st %>% filter(State == select_states[i])
  
  split<-nrow(sub_covid) - 5
  
  train = sub_covid %>% dplyr::slice(1:split)
  test = sub_covid %>% dplyr::slice(split+1:nrow(sub_covid))
  
  # Models
  ets_fit = ets(ts(train$Beta))
  ma_fit=sma(train$Beta,4)
  rw_fit <- rwf(train$Beta,drift=TRUE, h=nrow(test))
  arima_fit<-auto.arima(train$Beta)
  wn_fit<-auto.arima(train$Beta, d=0, D=0, max.p=0, max.q = 0,max.Q=0, max.P = 0)
  holt_fit<-holt(train$Beta,type='additive',damped = TRUE)
  
  # predict
  ets_pred<-as.numeric(forecast(ets_fit,h=nrow(test))$mean)
  ma_pred<-as.numeric(forecast(ma_fit,h=nrow(test))$forecast)
  rw_pred<-as.numeric(forecast(rw_fit,h=nrow(test))$mean)
  arima_pred<-as.numeric(forecast(arima_fit,h=nrow(test))$mean)
  wn_pred<-as.numeric(forecast(wn_fit,h=nrow(test))$mean)
  holt_pred<-as.numeric(forecast(holt_fit,h=nrow(test))$mean)
  
  # mape
  ets_mape<-MAPE(data.frame(cbind(test$Beta,ets_pred)))
  ma_mape<-MAPE(data.frame(cbind(test$Beta,ma_pred)))
  rw_mape<-MAPE(data.frame(cbind(test$Beta,rw_pred)))
  arima_mape<-MAPE(data.frame(cbind(test$Beta,arima_pred)))
  wn_mape<-MAPE(data.frame(cbind(test$Beta,wn_pred)))
  holt_mape<-MAPE(data.frame(cbind(test$Beta,holt_pred)))
  
  min_mape = min(ets_mape,ma_mape,rw_mape,arima_mape)
  ts_output$min_mape[i] = min_mape
  ts_output$model[i] = ifelse(ets_mape == min_mape,'ETS',
                              ifelse(ma_mape == min_mape,'MA',
                                     ifelse(arima_mape == min_mape,'ARIMA',
                                            ifelse(wn_mape == min_mape,'WN',
                                                   ifelse(holt_mape == min_mape,'Holt','RW')))))
  ts_output$param[i] = ifelse(ts_output$model[i] == 'ETS',ets_fit$method,
                              ifelse(ts_output$model[i] == 'ARIMA',paste(arimaorder(arima_fit), collapse = "-"),
                                     ifelse(ts_output$model[i] == 'Holt',str_c(holt_fit$model$par,sep = '_'),'')))
  
  # Output
  output <- test %>%
            cbind(.,data.frame(ets_pred)) %>%
            cbind(.,data.frame(ma_pred)) %>%
            cbind(.,data.frame(rw_pred)) %>%
            cbind(.,data.frame(arima_pred)) %>%
            cbind(.,data.frame(wn_pred)) %>%
            cbind(.,data.frame(holt_pred)) %>%
            setNames(c('Day','State','Beta','ets','ma','rw','arima','wn','holt')) %>%
            mutate(min_mape = min_mape,
                   model = ts_output$model[i],
                   param = ts_output$param[i])
  
  if(i==1){
    ts_model_output = output
  }else{
    ts_model_output = rbind(ts_model_output,output)
  }
}
