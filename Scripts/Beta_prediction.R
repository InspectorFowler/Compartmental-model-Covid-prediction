
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
suppressMessages(library(xgboost)) # Xgboost
suppressMessages(library(randomForest)) # Random forest


# !diagnostics off
options(scipen=999)

######################################################################################
#Working directory and environment
######################################################################################

rm(list = ls())
setwd('C:/Users/Parikshit_verma/Documents/GitHub/Covid-19')

######################################################################################
# Functions
######################################################################################

source('Functions/seir_simulate.R')
source('Functions/mape.R')
source('Functions/seir_fit.R')

accuracy_check <- function(model,init,train_data,test_data){
  
  summary(model)
  
  train_data<-train_data %>% mutate(Type = 'Train')
  test_data<-test_data %>% mutate(Type = 'Test')

  data <- rbind(train_data,test_data) %>%
          mutate(Beta_pred = predict(model,.)) %>%
          dplyr::select('Type','Day','State','CPM','Beta','Beta_pred') %>%
          left_join(.,init, by = c('State' = 'State')) %>%
          mutate(CPM_pred = 0)
  
  for (i in 1:nrow(data)){
    
    data$CPM_pred[i] = round(seir_fit(data$Beta_pred[i],data$Init[i],data$Day[i])*1000000,2)
  } 
  
  data <- data %>%
          mutate(Cases = round(CPM*Population/1000000,0),
                 Cases_pred = round(CPM_pred*Population/1000000,0),
                 Beta_pred = round(Beta_pred,4)) %>%
          dplyr::select(c('Type','Day','State',
                          'Cases','Cases_pred',
                          'CPM','CPM_pred',
                          'Beta','Beta_pred'))
  
  # Train RMSE
  print(paste0('Training MAPE (Beta) : ', round(MAPE(data.frame(cbind(pull(data[data$Type == 'Train','Beta']),
                                                     pull(data[data$Type == 'Train','Beta_pred'])))),4)))
  
  # Test RMSE
  print(paste0('Testing MAPE (Beta) : ', round(MAPE(data.frame(cbind(pull(data[data$Type == 'Test','Beta']),
                                                    pull(data[data$Type == 'Test','Beta_pred'])))),4)))
  
  # Train RMSE
  print(paste0('Training MAPE (Cases) : ', round(MAPE(data.frame(cbind(pull(data[data$Type == 'Train','Cases']),
                                                      pull(data[data$Type == 'Train','Cases_pred'])))),4)))
  
  # Test RMSE
  print(paste0('Testing MAPE (Cases) : ', round(MAPE(data.frame(cbind(pull(data[data$Type == 'Test','Cases']),
                                                     pull(data[data$Type == 'Test','Cases_pred'])))),4)))
  
  return(data)
  
}

######################################################################################
# Read data
######################################################################################

covid_us_st<-read.csv('Output/Estimation/US_state_beta.csv',stringsAsFactors = FALSE)

pop<-read.csv('Input/Population/US_population.csv',stringsAsFactors = FALSE) %>%
     group_by(state) %>%
     summarise(Population = sum(population, na.rm = TRUE)) %>%
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

covid_init <- covid_us_st %>%
              filter(Day == 1) %>%
              mutate(Init = (2.4*Cases)/Population) %>%
              dplyr::select(c('State','Init','Population'))

covid_us_st <- covid_us_st %>%
               mutate(CPM = Cases_Percent*1000000,
                      DPM = Death_Percent*1000000) %>%
               dplyr::select(c('Day','State','CPM','DPM','Beta')) %>%
               arrange(State,Day) %>%
               group_by(State) %>%
               mutate(Beta_lag1 = dplyr::lag(Beta, n = 1, default = NA),
                      Beta_lag2 = dplyr::lag(Beta, n = 2, default = NA),
                      Beta_lag3 = dplyr::lag(Beta, n = 3, default = NA),
                      Beta_lag4 = dplyr::lag(Beta, n = 4, default = NA))

features <- st_age %>% 
            left_join(.,st_popdensity, by = c('State' = 'State')) %>%
            left_join(.,st_race, by = c('State' = 'State')) %>%
            left_join(.,st_urban, by = c('State' = 'State')) %>%
            left_join(.,st_hospital, by = c('State' = 'State')) %>%
            left_join(.,st_homeless, by = c('State' = 'State')) %>%
            mutate(Homeless = round(HomlessPop*100/Population,4)) %>%
            dplyr::select(-c('Population','Area','HomlessPop')) %>%
            mutate(Urban = Urban/100) %>%
            mutate_at(.vars = vars(Beds,PopDensity,Hospitals),
                      .funs = scale)
            
covid_us_st <- covid_us_st %>%
               left_join(.,features, by = c('State' = 'State')) %>%
               ungroup() %>%
               drop_na() %>%
               filter(Day >= 3)

rm(st_age,st_homeless,st_hospital,st_popdensity,st_race,st_urban)

######################################################################################
# Data modeling - single point
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

# ------------------------------------ RF model -------------------------------------

rf_model <- randomForest(Beta ~ .,data = (covid_train %>% dplyr::select(-c('State','CPM','DPM'))))

model_output <- accuracy_check(rf_model,covid_init,covid_train,covid_test)

# ------------------------------------ Xgboost model -------------------------------------

xgb_train <- xgb.DMatrix(data = data.matrix((covid_train %>% dplyr::select(-c('State','CPM','DPM')))), label = covid_train$Beta)
xgb_model <- xgboost(data = xgb_train, nrounds = 20, verbose = 1) 

model_output <- accuracy_check(xgb_model,covid_init,covid_train,covid_test)

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
# Forecast
######################################################################################

states<-unique(covid_us_st$State)
horizon = 50
model<-lm_model

for (i in 1:length(states)){
  
  print(paste0(states[i],'...',i,' of ',length(states)))
  
  sub_covid<-covid_us_st %>% 
             filter(State == states[i]) %>% 
             dplyr::select(c('Day','State','Beta')) 
  
  start_day<-min(sub_covid$Day)
  sub_covid<-data.frame(seq(start_day,(start_day+horizon))) %>%
             setNames(c('Day')) %>%
             left_join(.,sub_covid, by = c('Day' = 'Day')) %>%
             mutate(State = states[i],
                    Beta_lag1 = dplyr::lag(Beta, n = 1, default = NA),
                    Beta_lag2 = dplyr::lag(Beta, n = 2, default = NA),
                    Beta_lag3 = dplyr::lag(Beta, n = 3, default = NA),
                    Beta_lag4 = dplyr::lag(Beta, n = 4, default = NA)) %>%
             filter(is.na(Beta)) %>%
             left_join(.,features, by = c('State' = 'State'))
  
  for (j in 1:nrow(sub_covid)){
    
    beta_pred<-predict(model,sub_covid[j,])
    sub_covid$Beta[j] = beta_pred
    
    if (j+4 <= nrow(sub_covid)) sub_covid$Beta_lag4[j+4] = beta_pred
    if (j+3 <= nrow(sub_covid)) sub_covid$Beta_lag3[j+3] = beta_pred
    if (j+2 <= nrow(sub_covid)) sub_covid$Beta_lag2[j+2] = beta_pred
    if (j+1 <= nrow(sub_covid)) sub_covid$Beta_lag1[j+1] = beta_pred
  }
  
  sub_covid<-sub_covid %>% dplyr::select(c('Day','State','Beta'))
  
  if(i == 1){
    beta_fcst<-sub_covid
  }else{
    beta_fcst<-rbind(beta_fcst,sub_covid)
  }
}
