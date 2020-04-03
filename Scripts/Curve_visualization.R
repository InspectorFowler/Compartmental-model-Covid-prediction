
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

######################################################################################
# Read data
######################################################################################

covid_us_st<-read.csv('Output/Estimation/US_state_Covid19_output.csv',
                      stringsAsFactors = FALSE)

######################################################################################
# Data wrangling
######################################################################################

covid_init <- covid_us_st %>%
              filter(Day == 1) %>%
              mutate(Init = 2.4*Cases_Percent) %>%
              dplyr::select(c('State','Init','Population'))

covid_us_st<-covid_us_st %>%
             filter(State %in% (covid_us_st %>% 
                                group_by(State) %>%
                                summarise(n = n()) %>%
                                filter(n >= 25) %>%
                                dplyr::select(c('State')) %>%
                                unlist())) %>%
             filter(Day >= 8) %>%
             dplyr::select(c('State','Day','Beta'))



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

horizon = 200

for (i in 1:nrow(covid_us_st)){
  
  state<-covid_us_st$State[i]
  param[2]<-covid_us_st$Beta[i]
  init[2]<-covid_init$Init[covid_init$State == state]
  
  simulated<-seir_simulate(init,param,horizon)
  
}




