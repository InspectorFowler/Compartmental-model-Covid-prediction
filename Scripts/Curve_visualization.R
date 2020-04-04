
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

hline <- function(y = 0.5, color = "red"){
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

######################################################################################
# SEIR Simulate
######################################################################################

for (k in 1:2){
  
  if(k == 1){
    covid_data<-covid %>% dplyr::rename(State = Country)
  }else{
    covid_data<-covid_us_st
  }
  
  covid_init <- covid_data %>%
                filter(Day == 1) %>%
                mutate(Init = 2.4*Cases_Percent) %>%
                dplyr::select(c('State','Init','Population'))
  
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
  
  for (i in 1:nrow(covid_data)){
    
    day<-covid_data$Day[i]
    state<-covid_data$State[i]
    pop<-covid_init$Population[covid_init$State == state]
    param[2]<-covid_data$Beta[i]
    init[2]<-covid_init$Init[covid_init$State == state]
    
    simulated<-seir_simulate(init,param,horizon) %>%
                dplyr::select(c('t','I')) %>%
                mutate(I = round(I,4),
                       State = state,
                       Day = day) %>%
                dplyr::select(c('State','Day','t','I')) %>%
                setNames(c('State','Day','Sim_Day','Infected'))
    
    if(i == 1){
      covid_sim<-simulated
    }else{
      covid_sim<-rbind(covid_sim,simulated)
    }
  }
  
  rm(init,param,day,state,pop,simulated,horizon,i,
     covid_init,start_filter)
  
  filter_sim <- covid_sim %>% 
                group_by(State,Day) %>% 
                summarise(max_inf = max(Infected)) %>% 
                filter(max_inf > 0) %>% 
                mutate(Dummy = 1)
  
  covid_sim <- covid_sim %>%
                left_join(.,filter_sim, by = c('State' = 'State',
                                               'Day' = 'Day')) %>%
                filter(!is.na(Dummy)) %>%
                dplyr::select(-c('Dummy','max_inf'))
  
  if(k == 2) covid_us_st_sim<-covid_sim
}

rm(k,filter_sim)

######################################################################################
# Visualization
######################################################################################

# ------------------------------ Animate -------------------------------------

state = 'Texas'

covid_sim %>%
filter(State == state) %>%
plot_ly(x = ~Sim_Day,
        y = ~Infected,
        frame = ~Day,
        type = 'scatter',
        mode = 'lines',
        fill = 'tozeroy') %>% 
layout(title = paste0('Covid Curve Progression : ', state),
       showlegend = FALSE,
       xaxis = list(title = 'Days'), 
       yaxis = list(title = 'Population Percentage',
                    range = c(0,0.8),
                    tickformat = "%"),
       shapes = list(hline()))

# ------------------------------ States -------------------------------------

covid_sim %>%
left_join(.,covid_sim %>% 
            group_by(State) %>% 
            summarise(max_day = max(Day)) %>% 
            mutate(Dummy = 1), by = c('State' = 'State',
                                      'Day' = 'max_day')) %>%
filter(!is.na(Dummy)) %>%
dplyr::select(-c('Dummy')) %>%
plot_ly(x = ~Sim_Day,
        y = ~Infected,
        color = ~State,
        type = 'scatter',
        mode = 'lines',
        fill = 'tozeroy') %>% 
layout(title = 'Covid Curve Progression by State',
       xaxis = list(title = 'Days'), 
       yaxis = list(title = 'Population %',
                    range = c(0,0.8),
                    tickformat = "%"),
       shapes = list(hline()))
