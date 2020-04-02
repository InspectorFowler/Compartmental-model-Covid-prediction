
######################################################################################
# Load libraries
######################################################################################

# wrangling
suppressMessages(library(data.table)) # data table manipulation
suppressMessages(library(dplyr)) # Data wrangling
suppressMessages(library(tidyr)) # Data wrangling
suppressMessages(library(stringr)) # Data wrangling

# Modeling
suppressMessages(library(deSolve)) # ODE solver
suppressMessages(library(EpiDynamics)) # SEIR simulator
suppressMessages(library(Metrics)) # RMSE calculator

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

######################################################################################
# Read data
######################################################################################

covid<-read.csv('Input/Cases/Covid19_global.csv',stringsAsFactors = FALSE)

wpop<-read.csv('Input/Population/World_population.csv',stringsAsFactors = FALSE) %>%
      setNames(c('Country','Population')) %>%
      mutate(Country = tolower(Country),
             Country = case_when(Country %like% 'iran' ~ 'iran',
                                 Country %like% 'egypt' ~ 'egypt',
                                 Country == 'korea, rep.' ~ 'korea, south',
                                 Country %like% 'russia' ~ 'russia',
                                 Country %like% 'syria' ~ 'syria',
                                 Country == 'united states' ~ 'us',
                                 TRUE ~ Country))

cpop<-read.csv('Input/Population/China_population.csv',stringsAsFactors = FALSE) %>%
      setNames(c('Region','Population')) %>%
      mutate(Region = paste0('china_',tolower(Region)))

covid_us_st<-read.csv('Input/Cases/Covid19_US_states.csv',stringsAsFactors = FALSE) %>%
             mutate(Population = as.numeric(Population),
                    date = as.Date(date, format = "%m/%d/%Y"))

covid_us_cn<-read.csv('Input/Cases/Covid19_US_counties.csv',stringsAsFactors = FALSE) %>%
             mutate(Population = as.numeric(Population),
                    date = as.Date(date, format = "%m/%d/%Y"))

######################################################################################
# Data wrangling
######################################################################################

# ---------------------------- GLOBAL -----------------------------------------------

covid <- covid %>%
         dplyr::select(c('Date','Country.Region','Province.State','Confirmed','Recovered','Deaths')) %>%
         setNames(c('Date','Country','Region','Cases','Recovered','Death')) %>%
         mutate(Date = as.Date(Date),
                Country = tolower(Country),
                Region = tolower(Region))

w_covid <- covid %>%
           filter(Country != 'china') %>%
           group_by(Date,Country) %>%
           summarise(Cases = sum(Cases,na.rm = TRUE),
                     Recovered = sum(Recovered,na.rm = TRUE),
                     Death = sum(Death,na.rm = TRUE)) %>%
           as.data.frame() %>%
           left_join(.,wpop,by=c('Country' = 'Country')) %>%
           filter(!is.na(Population))

c_covid <- covid %>%
           filter(Country == 'china') %>%
           mutate(Country = paste0(Country,"_",Region)) %>%
           dplyr::select(-c('Region')) %>%
           left_join(.,cpop,by=c('Country' = 'Region')) %>%
           filter(!is.na(Population))

covid <- rbind(w_covid,c_covid) %>%
         arrange(Country,Date) %>%
         filter(Cases != 0)

covid <- left_join(covid,(covid %>% group_by(Country) %>% summarize(start_date = min(Date, na.rm = TRUE))),
                   by = c('Country' = 'Country')) %>%
         mutate(Day = as.numeric(Date - start_date)) %>%
         dplyr::select(c('Date','Day','Country',
                         'Cases','Recovered','Death','Population')) %>%
         as.data.frame() %>%
         mutate(Cases_Percent = round(Cases/Population,8),
                Recovered_Percent = round(Recovered/Population,8),
                Death_Percent = round(Death/Population,8)) 

rm(c_covid,w_covid,cpop,wpop)

# ------------------------------- United States --------------------------------------

covid_us_st <- covid_us_st %>%
               filter(!is.na(Population)) %>%
               setNames(c('Date','State','Cases','Death','Population'))

covid_us_st <- left_join(covid_us_st,(covid_us_st %>% group_by(State) %>% summarize(start_date = min(Date, na.rm = TRUE))),
                          by = c('State' = 'State')) %>%
               mutate(Day = as.numeric(Date - start_date)) %>%
               dplyr::select(c('Date','Day','State','Cases','Death','Population')) %>%
               mutate(Cases_Percent = round(Cases/Population,8),
                      Death_Percent = round(Death/Population,8))

covid_us_cn <- covid_us_cn %>%
               filter(!is.na(Population)) %>%
               setNames(c('Date','County','State','Cases','Death','Population'))

covid_us_cn <- left_join(covid_us_cn,(covid_us_cn %>% group_by(County,State) %>% summarize(start_date = min(Date, na.rm = TRUE))),
                         by = c('State' = 'State','County' = 'County')) %>%
               mutate(Day = as.numeric(Date - start_date),
                      County_state = paste0(County,'_',State)) %>%
               dplyr::select(c('Date','Day','County_state','Cases','Death','Population')) %>%
               mutate(Cases_Percent = round(Cases/Population,8),
                      Death_Percent = round(Death/Population,8))

######################################################################################
# Parameter estimation
######################################################################################

for (k in 1:3){
  
  if(k == 1){
    
    covid_data<-covid %>%
                mutate(State = Country) %>%
                dplyr::select(c("Date","Day","State","Cases","Death",
                                "Population","Cases_Percent","Death_Percent"))
    
    covid_data<-covid_data %>% filter(State %in% unique(covid_data$State))
  }else if(k == 2){
    
    covid_data <- covid_us_st%>%
                  dplyr::select(c("Date","Day","State","Cases","Death",
                                  "Population","Cases_Percent","Death_Percent"))
    
    covid_data<-covid_data %>% filter(State %in% unique(covid_data$State))
  }else{
    
    covid_data<-covid_us_cn %>%
                mutate(State = County_state) %>%
                dplyr::select(c("Date","Day","State","Cases","Death",
                                "Population","Cases_Percent","Death_Percent"))
    
    covid_data<-covid_data %>% filter(State %in% unique(covid_data$State))
  }
  
  if (k !=2) next
  
  states <- covid_data %>% distinct(State) %>% unlist()
  
  for (i in 1:length(states)){
    
    print(paste0(states[i],"...",i,' of ',length(states)))
    
    sub_covid <- covid_data %>% 
                 filter(State == states[i])
    
    # Initial state values 
    init = rep(0,4)
    names(init) <- c('S',"E",'I','R')
    
    init[1] = 1
    init[2] = 2.4*sub_covid$Cases_Percent[1]
    init[3] = 0
    init[4] = 0
    
    # Parameter values
    param = rep(0,4)
    names(param) <- c('mu','beta','sigma','gamma')
    param[1] = 0
    param[3] = 1/5.2 
    param[4] = 1/10 
    
    beta_pool = c(seq(0.001,1,0.002),
                  seq(1,2,0.025),
                  seq(2,5,0.05),
                  seq(5,20,0.5))
    
    #Re-adjust data with initial parameters
    sub_covid<-sub_covid %>%
               rbind(sub_covid %>%
                     dplyr::slice(1:1) %>%
                     mutate(Date = sub_covid$Date[1]-1,
                            Day = -1,
                            State = sub_covid$State[1],
                            Cases = 0,
                            Death = 0,
                            Population = sub_covid$Population[1],
                            Cases_Percent = 0,
                            Death_Percent = 0),.) %>%
               mutate(Day = Day+1)
    
    actual <- sub_covid$Cases_Percent
    days <- sub_covid$Day
    
    for (j in 1:length(beta_pool)){
      
      param[2] = beta_pool[j]
      
      # SIER model with specified parameters
      seir <- seir_simulate(init,param,100)
      
      sim_infected <- seir[1:nrow(sub_covid),'I'] %>% unlist()
      sim_recovered <- seir[1:nrow(sub_covid),'R'] %>% unlist()
      simulated <-sim_infected + sim_recovered
      
      cal_data <- cbind(days,actual,simulated) %>%
                  as.data.frame() %>%
                  mutate(State = states[i],
                         beta = beta_pool[j]) %>%
                  as.data.frame() %>%
                  setNames(c('Day','Actual','Simulated','State','Beta')) %>%
                  dplyr::select(c('State','Day','Beta','Actual','Simulated'))
      
      if(j == 1 & i == 1){
        beta <- cal_data
      }else{
        beta <- rbind(beta,cal_data)
      }
    }
    
    if(i == 1){
      covid_data_updt<-sub_covid
    }else{
      covid_data_updt<-rbind(covid_data_updt,sub_covid)
    }
  }
  
  covid_data <- covid_data_updt
  
  beta <- beta %>%
          rowwise() %>%
          mutate(rmse = rmse(Actual,Simulated)) %>%
          arrange(State,Day,Beta)
  
  beta <- beta %>%
          left_join(.,(beta %>%
                         group_by(State,Day) %>%
                         summarise(min_rmse = min(rmse,na.rm = TRUE))),
                    by = c('State' = 'State',
                           'Day' = 'Day')) %>%
          mutate(min_flag = ifelse(min_rmse == rmse,1,0)) %>%
          filter(min_flag == 1) %>%
          group_by(State,Day) %>%
          summarise(Beta = min(Beta,na.rm = TRUE))
  
  covid_data <- covid_data %>%
                left_join(.,beta,by = c('State' = 'State', 'Day' = 'Day'))
  
  if(k == 1){
    
    covid<-covid_data %>%
           mutate(Country = State) %>%
           dplyr::select(c("Date","Day","Country","Cases","Death",
                           "Population","Cases_Percent","Death_Percent",'Beta'))
  }else if(k == 2){
    
    covid_us_st<-covid_data
  }else{
    
    covid_us_cn <- covid_data %>%
                   mutate(County_state = State) %>%
                   dplyr::select(c("Date","Day","County_state","Cases","Death",
                                   "Population","Cases_Percent","Death_Percent",'Beta'))
  }
  
}

rm(cal_data,seir,simulated,actual,
   sub_covid,beta_pool,i,j,k,states,
   days,param,init,beta,covid_data,covid_data_updt)

######################################################################################
# Write to file
######################################################################################

write.csv(covid,'Output/Estimation/Global_Covid19_output.csv',row.names = FALSE)
write.csv(covid_us_st,'Output/Estimation/US_state_Covid19_output.csv',row.names = FALSE)
write.csv(covid_us_cn,'Output/Estimation/US_county_Covid19_output.csv',row.names = FALSE)


