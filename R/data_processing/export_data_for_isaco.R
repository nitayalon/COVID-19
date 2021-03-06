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
selected_states <- c('Italy','Germany','US','Switzerland','Israel','Brazil','Iran','Chile')
us_states <- c('Florida', 'New York','California', 'Texas','New Jersey', 'North Carolina', 'Tennessee', 'Louisiana', 'Pennsylvania', 'Ohio', 'Massachusetts', 'Indiana')
date = Sys.Date()
dir.create(sprintf('/home/nitay/COVID-19/Data/Empirical_data_for_transformations/%s', date))
for(state in selected_states)
{
  grid_search_results <- mainFunction(state, 55, population_list[[state]], export_data_for_transformations = T)
  write.csv(x = grid_search_results, file = sprintf('/home/nitay/COVID-19/Data/Empirical_data_for_transformations/%s/%s_%s.csv',date,state,date))
}	

for(state in us_states)
{
  raw_data <- ExportUSStateData(state)
  write.csv(x = raw_data, file = sprintf('/home/nitay/COVID-19/Data/Empirical_data_for_transformations/%s/%s_%s.csv',date,state,date),row.names = F)
}	


