library(dplyr)
library(tidyverse)
library(Rcpp)
library(pbapply)
source('R/main.R')
source('R/Ad_hoc_computations/alpha_k_grid_search.R')
source('R/calibration_loop.R')
source('R/inner_calibration_loop.R')
source('R/data_loader.R')
Rcpp::sourceCpp('src/inner_loop.cpp')
global_confirmed_cases <- read_csv("/home/nitay/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
global_confirmed_deaths <- read_csv("/home/nitay/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
global_confirmed_recovered <- read_csv("/home/nitay/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")
states_list <- c('US','Italy')

for(state_name in states_list){
  grid_search_results <- mainFunction(state_name,60)
  save(x = grid_search_results, file = sprintf('R/Ad_hoc_computations/grid_search_results/%s.RData',state_name))
}
