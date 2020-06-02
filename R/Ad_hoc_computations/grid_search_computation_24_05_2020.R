library(dplyr)
library(tidyverse)
library(Rcpp)
library(pbapply)
source('R/main.R')
source('R/Ad_hoc_computations/alpha_k_grid_search.R')
source('R/grid_search_method_helper.R')
source('R/calibration_loop.R')
source('R/inner_calibration_loop.R')
source('R/data_loader.R')
Rcpp::sourceCpp('src/inner_loop.cpp')
global_confirmed_cases <- read_csv("/home/nitay/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
global_confirmed_deaths <- read_csv("/home/nitay/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
global_confirmed_recovered <- read_csv("/home/nitay/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")
population_list <- list(
  US =     330806424 ,   
  Brazil =  212405664 ,
  Russia    = 145928315 ,
  Spain     =   46752999 ,
  UK        =    67850075 ,
  Italy     =     60470472 ,
  France    =  65259187 ,
  Germany   =   83757235 ,
  Turkey    =  84244944 ,
  India     =   1378604014 ,
  Iran      =     83880266 ,
  Peru      =    32923430 ,
  Canada    =  37708187 ,
  China     =    1439323776 ,
  Saudi     =         34756224 ,
  Chile     =     19099374 ,
  Mexico    =   128792446 ,
  Belgium   =  11584564 ,
  Netherlds =   17131112 ,
  Ecuador   =    17614626 ,
  Sweden    =    10092886 ,
  Israel    =         8645060 ,
  Austria   =      9001207 ,
  Argentina =    45153114 
) 

states_list <- c('Israel')
for(state_name in states_list){
  cat(sprintf('Current state: %s',state_name), '\n')
  cat(sprintf('Sarting time: %s',Sys.time()), '\n')
  grid_search_results <- mainFunction(state_name, 60, population_list[[state_name]])
  save(x = grid_search_results, file = sprintf('R/Ad_hoc_computations/grid_search_results/%s.RData',state_name))
  cat(sprintf('Ending time: %s',Sys.time()), '\n')
}


plotTrajectories(israel_grid_search_results$environment_data,israel_trajectories,dates,last_date,state,ylim = c(0,1.7),scale_factor = 1e4)