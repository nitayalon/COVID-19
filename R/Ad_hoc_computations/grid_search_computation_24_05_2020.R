library(dplyr)
library(tidyverse)
library(Rcpp)
library(pbapply)
source('R/main.R')
source('R/Ad_hoc_computations/alpha_k_grid_search.R')
source('R/calibration_loop.R')
source('R/inner_calibration_loop.R')
source('R/data_loader.R')
source('R/load_transformed_data.R')
source('R/grid_search_method_helper.R')
source('R/predict_covid_trajectory.R')
source('R/report_grid_search_greeks.R')
source('R/Visualization/plot_trajectoires.R')
Rcpp::sourceCpp('src/inner_loop.cpp')
global_confirmed_cases <- read_csv("/home/nitay/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
global_confirmed_deaths <- read_csv("/home/nitay/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
global_confirmed_recovered <- read_csv("/home/nitay/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")
transformed_data <- read_csv("/home/nitay/COVID-19/Data/data_from_linear_models.csv",col_names = F)
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
  Argentina =    45153114, 
  Switzerland =     8651338 
) 

# state <- 'US'
# print(state)
# date = '25_05_2020'
# us_grid_search_results_25_05_2020 <- mainFunction(state, 60, population_list[[state]], alpha_grid = seq(0.2,0.4,length.out = 40), cut_off_day = 125)
# save(x = us_grid_search_results_25_05_2020, file = sprintf('/home/nitay/COVID-19/R/Ad_hoc_computations/grid_search_results/%s_%s.RData',state,date))
# state <- 'Switzerland'
# print(state)
date = Sys.Date()
state <- 'Chile'
transformed_state_data = read.csv('Data/Empirical_data_for_transformations/israel_processed_data.csv')
chile_new_data <- mainFunction(state, 61, population_list[[state]], alpha_grid = seq(0.5,0.7, length.out = 40), hhh_grid = c(0.1, 25))
write.csv(x = chile_new_data, file = sprintf('/home/nitay/COVID-19/Data/Empirical_data_for_transformations/%s_%s.csv',state,date))
dim(transformed_data)
