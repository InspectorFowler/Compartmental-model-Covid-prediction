
######################################################################################
# Load libraries
######################################################################################

# wrangling
suppressMessages(library(data.table)) # data table manipulation
suppressMessages(library(dplyr)) # Data wrangling
suppressMessages(library(tidyr)) # Data wrangling
suppressMessages(library(stringr)) # Data wrangling

# Visualization
suppressMessages(library(plotly)) # animated visualization

# !diagnostics off
options(scipen=999)

######################################################################################
#Working directory and environment
######################################################################################

setwd('C:/Users/Parikshit_verma/Documents/GitHub/Covid-19/')

######################################################################################
# Functions
######################################################################################

source('Functions/seir_simulate.R')

hline <- function(y = 0.25, color = "red"){
  list(type = "line", 
       x0 = 0, 
       x1 = 1, 
       xref = "paper",
       y0 = y, 
       y1 = y, 
       line = list(color = color))
}

######################################################################################
# Read data
######################################################################################

covid_us_st<-read.csv('Output/Estimation/US_state_beta.csv',stringsAsFactors = FALSE)
covid<-read.csv('Output/Estimation/Global_beta.csv',stringsAsFactors = FALSE)

beta_global_fcst<-read.csv('Output/Prediction/Beta_global_prediction.csv',stringsAsFactors = FALSE)
beta_us_fcst<-read.csv('Output/Prediction/Beta_US_state_prediction.csv',stringsAsFactors = FALSE)

######################################################################################
# SEIR Simulate
######################################################################################

for (k in 1:2){
  
  if(k == 1){
    
    covid_data<-covid %>% dplyr::rename(State = Country)
    beta_pred<-beta_global_fcst %>% dplyr::rename(State = Country) %>% mutate(Type = 'Forecast')
    print('Simulating SEIR model for global data...')
  }else{
    
    covid_data<-covid_us_st
    beta_pred<-beta_us_fcst %>% mutate(Type = 'Forecast')
    print('Simulating SEIR model for US data...')
  }
  
  # Initial state values 
  init = rep(0,4)
  names(init) <- c('S',"E",'I','R')
  
  init[1] = 1
  init[2] = 0.001
  init[3] = 0
  init[4] = 0
  
  # Parameter values
  param = rep(0,4)
  names(param) <- c('mu','beta','sigma','gamma')
  param[1] = 0
  param[2] = 1/3
  param[3] = 1/5.2 
  param[4] = 1/10 
  
  horizon = 365
  
  covid_init<-covid_data %>%
              filter(Day == 1) %>%
              mutate(Init = 2.4*Cases_Percent) %>%
              dplyr::select(c('State','Init','Population'))
  
  date_map<-full_join((data.frame(unique(covid_data$State)) %>% mutate(Dummy = 1)),
                      (data.frame(seq(0:365)-1) %>% mutate(Dummy = 1)), 
                      by = "Dummy") %>% 
            dplyr::select(-c('Dummy')) %>% 
            setNames(c('State','Horizon')) %>%
            mutate_if(is.factor,as.character) %>%
            left_join(.,(covid_data %>%
                         group_by(State) %>% 
                         summarise(start_date = min(Date))),
                      by = c('State' = 'State')) %>%
            mutate(start_date = as.Date(start_date),
                   Horizon_date = start_date + Horizon) %>%
            dplyr::select(-c('start_date'))
  
  covid_data<-covid_data %>%
              filter(State %in% (covid_data %>% 
                                   group_by(State) %>%
                                   summarise(n = n()) %>%
                                   filter(n >= 10) %>%
                                   dplyr::select(c('State')) %>%
                                   unlist())) %>%
              filter(Day >= 4) %>%
              dplyr::select(c('State','Day','Beta'))
  
  start_filter<-covid_data %>%
                filter(Beta > 0.1) %>%
                dplyr::select(c('State','Day')) %>%
                mutate(Dummy = 1)
  
  covid_data<-covid_data %>%
              left_join(.,start_filter, by =c('State' = 'State',
                                              'Day' = 'Day')) %>%
              filter(!is.na(Dummy)) %>%
              dplyr::select(-c('Dummy'))
  
  covid_data<-covid_data %>%
              mutate(Type = 'Actual') %>%
              rbind(beta_pred) %>%
              arrange(State,Day) %>%
              mutate(Beta = ifelse(Beta<0,0,Beta))
  
  for (i in 1:nrow(covid_data)){

    day<-covid_data$Day[i]
    state<-covid_data$State[i]
    pop<-covid_init$Population[covid_init$State == state]
    param[2]<-covid_data$Beta[i]
    init[2]<-covid_init$Init[covid_init$State == state]
    
    print(paste0(state,"...",day))
    
    simulated<-seir_simulate(init,param,horizon) %>%
               dplyr::select(c('t','I','R')) %>%
               mutate(I = round(I,10),
                      R = round(R,10),
                      State = state,
                      Day = day) %>%
               dplyr::select(c('State','Day','t','I','R')) %>%
               setNames(c('State','Day','Sim_Day','Sim_Infected','Sim_Recovered')) %>%
               left_join(.,date_map, by = c('State' = 'State',
                                            'Sim_Day' = 'Horizon')) %>%
               dplyr::rename(Sim_Date = Horizon_date) %>%
               dplyr::select(c('State','Day','Sim_Date','Sim_Day','Sim_Infected','Sim_Recovered'))
    
    if(i == 1){
      covid_sim<-simulated
    }else{
      covid_sim<-rbind(covid_sim,simulated)
    }
  }
  
  rm(init,param,day,state,pop,simulated,horizon,i,covid_init,start_filter)
  
  filter_sim <- covid_sim %>% 
                group_by(State,Day) %>% 
                summarise(max_inf = max(Sim_Infected)) %>% 
                filter(max_inf > 0) %>% 
                mutate(Dummy = 1)
  
  covid_sim <- covid_sim %>%
               left_join(.,filter_sim, by = c('State' = 'State',
                                              'Day' = 'Day')) %>%
               filter(!is.na(Dummy)) %>%
               dplyr::select(-c('Dummy','max_inf')) %>%
               dplyr::rename(Region = State)
    
  if(k == 1){
    covid_global_sim<-covid_sim %>%
                      left_join(.,covid, by = c('Region' = 'Country',
                                                'Day' = 'Day')) %>%
                      left_join(covid_data %>% dplyr::select(c('State','Day','Type')), 
                                by =c('Region' = 'State',
                                      'Day' = 'Day')) %>%
                      mutate(Sim_Cases = round(Population*(Sim_Recovered+Sim_Infected),0),
                             Infected = Cases_Percent,
                             Flag = ifelse(Sim_Day == Day,'Y','N')) %>%
                      dplyr::select(c('Type','Region','Date','Day','Sim_Date','Sim_Day','Flag',
                                      'Beta','Cases','Sim_Cases','Infected','Sim_Infected',
                                      'Population'))
  }else{
    covid_us_st_sim<-covid_sim %>%
                     left_join(.,covid_us_st, by = c('Region' = 'State',
                                                     'Day' = 'Day')) %>%
                     left_join(covid_data %>% dplyr::select(c('State','Day','Type')), 
                               by =c('Region' = 'State',
                                     'Day' = 'Day')) %>%
                     mutate(Sim_Cases = round(Population*(Sim_Recovered+Sim_Infected),0),
                            Infected = Cases_Percent,
                            Flag = ifelse(Sim_Day == Day,'Y','N')) %>%
                     dplyr::select(c('Type','Region','Date','Day','Sim_Date','Sim_Day','Flag',
                                     'Beta','Cases','Sim_Cases','Infected','Sim_Infected',
                                     'Population'))
  } 
}
  
rm(k,filter_sim,covid_sim,date_map,covid_us_st,covid,covid_data,
   beta_global_fcst,beta_us_fcst,beta_pred)

######################################################################################
# Visualization
######################################################################################

covid_sim<-covid_us_st_sim

# ------------------------------ Animate -------------------------------------

region = 'New York'

covid_sim %>%
filter(Region == region) %>%
plot_ly(x = ~Sim_Date,
        y = ~Sim_Infected,
        frame = ~Day,
        color = ~Type,
        type = 'scatter',
        mode = 'lines',
        fill = 'tozeroy') %>% 
layout(title = paste0('Covid Curve Progression : ', region),
       showlegend = FALSE,
       xaxis = list(title = 'Date',
                    type = 'date',
                    range = c(as.numeric(as.POSIXct("2020-01-01", format="%Y-%m-%d"))*1000,
                              as.numeric(as.POSIXct("2020-12-31", format="%Y-%m-%d"))*1000),
                    tickformat =  "%d %b %Y"),  
       yaxis = list(title = '% of Population',
                    tickformat = "%"),
       shapes = list(hline()))

# ------------------------------ States -------------------------------------

covid_sim %>%
left_join(.,covid_sim %>% 
            group_by(Region) %>% 
            summarise(max_day = max(Day)) %>% 
            mutate(Dummy = 1), by = c('Region' = 'Region',
                                      'Day' = 'max_day')) %>%
filter(!is.na(Dummy)) %>%
dplyr::select(-c('Dummy')) %>%
plot_ly(x = ~Sim_Date,
        y = ~Sim_Infected,
        color = ~Region,
        type = 'scatter',
        mode = 'lines',
        fill = 'tozeroy') %>% 
layout(title = 'Covid Curve Progression by Region',
       xaxis = list(title = 'Days',
                    type = 'date',
                    range = c(as.numeric(as.POSIXct("2020-01-01", format="%Y-%m-%d"))*1000,
                              as.numeric(as.POSIXct("2020-12-31", format="%Y-%m-%d"))*1000),
                    tickformat =  "%d %B %Y"), 
       yaxis = list(title = 'Population %',
                    range = c(0,0.8),
                    tickformat = "%"),
       shapes = list(hline()))

# ------------------------------ Beta -------------------------------------

region = 'Illinois'

covid_sim %>% 
filter(Region == region & Flag == 'Y') %>%
plot_ly(x = ~Date,
        y = ~Beta,
        type = 'scatter',
        mode = 'lines') %>%
layout(title = paste0('Beta Curve Progression : ', region),
       xaxis = list(title = 'Date',
                    type = 'date',
                    tickformat =  "%d %B %Y"), 
       yaxis = list(title = 'Beta'))

######################################################################################
# Write to file
######################################################################################

write.csv(covid_global_sim,'Output/Visualization/Global_viz.csv',row.names = FALSE)
write.csv(covid_us_st_sim,'Output/Visualization/US_state_viz.csv',row.names = FALSE)
