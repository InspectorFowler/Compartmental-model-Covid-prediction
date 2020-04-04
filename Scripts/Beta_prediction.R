
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
suppressMessages(library(leaps)) # stepwise regression
suppressMessages(library(ridge)) # rmse and metrics
suppressMessages(library(e1071)) # svm
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

source('Functions/seir_simulate.R')
source('Functions/mape.R')
source('Functions/seir_fit.R')

accuracy_check <- function(model,init,train_data,test_data){
  
  summary(model)
  
  data <- rbind(train_data,test_data) %>%
          mutate(Beta_pred = predict(model,.)) %>%
          dplyr::select('Day','State','CPM','Beta','Beta_pred') %>%
          left_join(.,init, by = c('State' = 'State')) %>%
          mutate(CPM_pred = 0)
  
  for (i in 1:nrow(data)){
    
    data$CPM_pred[i] = round(seir_fit(data$Beta_pred[i],data$Init[i],data$Day[i])*1000000,2)
  } 
  
  data <- data %>%
          mutate(Cases = round(CPM*Population/1000000,0),
                 Cases_pred = round(CPM_pred*Population/1000000,0),
                 Beta_pred = round(Beta_pred,4)) %>%
          dplyr::select(c('Day','State',
                          'Cases','Cases_pred',
                          'CPM','CPM_pred',
                          'Beta','Beta_pred'))
  
  rows = nrow(data[data$State != 'Texas',])
  beta_test_scale = max(data[data$State == 'Texas','Beta']) - min(data[data$State == 'Texas','Beta'])
  beta_train_scale = max(data[data$State != 'Texas','Beta']) - min(data[data$State != 'Texas','Beta'])
  case_test_scale = max(data[data$State == 'Texas','Cases']) - min(data[data$State == 'Texas','Cases'])
  case_train_scale = max(data[data$State != 'Texas','Cases']) - min(data[data$State != 'Texas','Cases'])
  
  # Train RMSE
  print(paste0('Training mape (Beta) : ', round(MAPE(data.frame(cbind(pull(data[data$State != 'Texas','Beta']),
                                                     pull(data[data$State != 'Texas','Beta_pred'])))),4)))
  
  # Test RMSE
  print(paste0('Testing mape (Beta) : ', round(MAPE(data.frame(cbind(pull(data[data$State == 'Texas','Beta']),
                                                    pull(data[data$State == 'Texas','Beta_pred'])))),4)))
  
  # Train RMSE
  print(paste0('Training mape (Cases) : ', round(MAPE(data.frame(cbind(pull(data[data$State != 'Texas','Cases']),
                                                      pull(data[data$State != 'Texas','Cases_pred'])))),4)))
  
  # Test RMSE
  print(paste0('Testing mape (Cases) : ', round(MAPE(data.frame(cbind(pull(data[data$State == 'Texas','Cases']),
                                                     pull(data[data$State == 'Texas','Cases_pred'])))),4)))
  
  return(data)
  
}


######################################################################################
# Read data
######################################################################################

covid_us_st<-read.csv('Output/Estimation/US_state_beta.csv',stringsAsFactors = FALSE)

pop<-read.csv('Input/Population/US_state_population.csv',stringsAsFactors = FALSE) %>%
     dplyr::select(c('NAME','POPESTIMATE2019')) %>%
     setNames(c('State','Population'))

st_age<-read.csv('Input/Demographics/State_agegroup.csv',stringsAsFactors = FALSE) %>%
        setNames(c("State","Child_0_18","Adults_19_25","Adults_26_34","Adults_35_54","Adults_55_64","Adults_65" ))

st_homeless<-read.csv('Input/Demographics/State_homeless.csv',stringsAsFactors = FALSE)

st_hospital<-read.csv('Input/Demographics/State_hospital.csv',stringsAsFactors = FALSE)

st_popdensity<-read.csv('Input/Demographics/State_popdensity.csv',stringsAsFactors = FALSE) %>%
               setNames(c("State","Population","Area","PopDensity" ))

st_race<-read.csv('Input/Demographics/State_race.csv',stringsAsFactors = FALSE) %>%
         setNames(c('State',"White","Black","Hispanic","Native_indian","Asian","Hawaiian","Two_more"))

st_urban<-read.csv('Input/Demographics/State_urban.csv',stringsAsFactors = FALSE) %>%
          dplyr::select(-c('FIPS'))

######################################################################################
# Data wrangling and feature mapping
######################################################################################

covid_us_st <- covid_us_st %>%
               mutate(CPM = Cases_Percent*1000000,
                      DPM = Death_Percent*1000000) %>%
               dplyr::select(c('Day','State','CPM','DPM','Beta')) %>%
               arrange(State,Day) %>%
               group_by(State) %>%
               mutate(CPM_lag1 = dplyr::lag(CPM, n = 1, default = NA),
                      CPM_lag2 = dplyr::lag(CPM, n = 2, default = NA),
                      CPM_lag3 = dplyr::lag(CPM, n = 3, default = NA),
                      CPM_lag4 = dplyr::lag(CPM, n = 4, default = NA),
                      DPM_lag1 = dplyr::lag(DPM, n = 1, default = NA),
                      DPM_lag2 = dplyr::lag(DPM, n = 2, default = NA),
                      DPM_lag3 = dplyr::lag(DPM, n = 3, default = NA),
                      DPM_lag4 = dplyr::lag(DPM, n = 4, default = NA),
                      Beta_lag1 = dplyr::lag(Beta, n = 1, default = NA),
                      Beta_lag2 = dplyr::lag(Beta, n = 2, default = NA),
                      Beta_lag3 = dplyr::lag(Beta, n = 3, default = NA),
                      Beta_lag4 = dplyr::lag(Beta, n = 4, default = NA))

covid_init <- covid_us_st %>%
              filter(Day == 1) %>%
              left_join(.,pop, by = c('State' = 'State')) %>%
              mutate(Init = 2.4*(CPM/1000000)) %>%
              dplyr::select(c('State','Init','Population'))

features <- st_age %>% 
            left_join(.,st_popdensity, by = c('State' = 'State')) %>%
            left_join(.,st_race, by = c('State' = 'State')) %>%
            left_join(.,st_urban, by = c('State' = 'State')) %>%
            left_join(.,st_hospital, by = c('State' = 'State')) %>%
            left_join(.,st_homeless, by = c('State' = 'State')) %>%
            mutate(Homeless = round(HomlessPop*100/Population,4)) %>%
            dplyr::select(-c('Population','Area','HomlessPop'))

covid_us_st <- covid_us_st %>%
               filter(!State %in% c('District of Columbia','Puerto Rico')) %>%
               left_join(.,features, by = c('State' = 'State')) %>%
               mutate(Urban = Urban/100) %>%
               ungroup() %>%
               drop_na()

rm(features,st_age,st_homeless,st_hospital,st_popdensity,st_race,st_urban)

covid_us_st <- covid_us_st %>%
                mutate_at(.vars = vars(Beds,PopDensity,Hospitals),
                          .funs = scale) %>%
                filter(Day >= 6)

######################################################################################
# Data modelling - single point
######################################################################################

# Training and testing split

split_type = 'Byrow'

if(split_type != 'Byrow'){
  
  train_states = c('New York','New Jersey','California','Michigan','Massachusetts',
                   'Florida','Washington','Illinois','Louisiana','Pennsylvania')
  
  test_states = c('Texas')
  
  covid_test <- covid_us_st %>% filter(State == test_states)  
  covid_train <- covid_us_st %>% filter(State != test_states) 
}else{
  
  smp_size <- floor(0.60 * nrow(mtcars))
  set.seed(123)
  
  train_ind <- sample(seq_len(nrow(covid_us_st)), size = smp_size)
  
  covid_test <- covid_us_st[train_ind,]
  covid_train <- covid_us_st[-train_ind,]
}

# ------------------------------------ Linear model ----------------------------------

lm_model <- lm(Beta ~ Day + 
                      Beta_lag1 + Beta_lag2 + Beta_lag3 + Beta_lag4 + 
                      I(Beta_lag1^2) + I(Beta_lag2^2) + I(Beta_lag3^2) + I(Beta_lag4^2) +
                      Beta_lag1*Beta_lag2 + Beta_lag1*Beta_lag3 + Beta_lag1*Beta_lag4 +
                      Beta_lag2*Beta_lag3 + Beta_lag2*Beta_lag4 +
                      Beta_lag3*Beta_lag4 +
                      Adults_19_25 + Adults_26_34 + Adults_35_54 + Adults_55_64 + Adults_65 + 
                      PopDensity + 
                      White + Black + Hispanic + Native_indian + Asian + Hawaiian + Two_more + 
                      Beds + Homeless,
               data = covid_train)

model_output <- accuracy_check(lm_model,covid_init,covid_train,covid_test)

# ------------------------------------ Linear ridge model ----------------------------

lm_ridge_model <- linearRidge(Beta ~ Day + 
                                     Beta_lag1 + Beta_lag2 + Beta_lag3 + Beta_lag4 + 
                                     Beta_lag1*Beta_lag2, 
                              data = covid_train) 

model_output <- accuracy_check(lm_ridge_model,covid_init,covid_train,covid_test)

# ------------------------------------ svm model -------------------------------------

svm_model <- svm(Beta ~ .,data = covid_train) 

model_output <- accuracy_check(svm_model,covid_init,covid_train,covid_test)

# ------------------------------------ RF model -------------------------------------

rf_model <- randomForest(Beta ~ .,data = (covid_train %>% dplyr::select(-c('State','CPM','DPM'))))

model_output <- accuracy_check(rf_model,covid_init,covid_train,covid_test)

# ------------------------------------ Xgboost model -------------------------------------

xgb_train <- xgb.DMatrix(data = data.matrix((covid_train %>% dplyr::select(-c('State','CPM','DPM')))), label = covid_train$Beta)
xgb_model <- xgboost(data = xgb_train, nrounds = 20, verbose = 1) 

model_output <- accuracy_check(xgb_model,covid_init,covid_train,covid_test)

# ------------------------------------ Neural Network ---------------------------------

n<-names(covid_train)
f <- as.formula(paste("Beta ~", paste(n[!n %in% "Beta"], collapse = " + ")))

nn <- neuralnet(f,data=covid_train,
                hidden=10,
                rep = 10,
                threshold = 0.04,
                stepmax = 1e7,
                learningrate = 0.001,
                linear.output=T)

######################################################################################
# Data modelling - multi point
######################################################################################

states = unique(covid_us_st$State)

for (i in 1:length(states)){
  
  sub_covid<-covid_us_st %>% filter(State == states[i])
  
  split = ceiling(0.9*nrow(sub_covid))
  
  train = sub_covid %>% dplyr::slice(1:split) 
  test = sub_covid %>% dplyr::slice(split+1:nrow(sub_covid))
  
  if(i == 1){
    
    covid_train = train
    covid_test = test
  }else{
    
    covid_train = rbind(covid_train,train)
    covid_test = rbind(covid_test,test)
  }
}

lm_ridge_model <- linearRidge(Beta ~ Day + 
                                      Beta_lag1 + Beta_lag2 + Beta_lag3 + Beta_lag4 + 
                                      Beta_lag1*Beta_lag2, 
                              data = covid_train)

test_start<- covid_test %>%
             group_by(State) %>%
             summarise(Day = min(Day, na.rm = TRUE)) %>%
             mutate(Dummy = 1) %>%
             left_join(covid_test,., by = c("Day", "State")) %>%
             filter(!is.na(Dummy))

for (i in 1:nrow(states)){
  
  sub_covid<-covid_test %>% 
             filter(State == states[i]) %>% 
             arrange(-desc(Day))
  
  for (j in 1:nrow(sub_covid)){
    
    row = sub_covid %>% dplyr::slice(j:j)
    beta_pred = as.numeric(predict(lm_ridge_model,sub_covid))
  }
}
             

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
             filter(State %in% select_states) %>%
             dplyr::select(c('Day','State','Beta'))

ts_output = data.frame(State=character(length(select_states)),
                       min_mape=double(length(select_states)),
                       model=character(length(select_states)),
                       param=character(length(select_states)),
                       stringsAsFactors=FALSE)

for (i in 1:length(select_states)){
  
  ts_output$State[i] = select_states[i]
  
  sub_covid<-covid_us_st %>%
             filter(State == select_states[i])
  
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

