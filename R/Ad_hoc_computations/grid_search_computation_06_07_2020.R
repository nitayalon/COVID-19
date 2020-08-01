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
transformed_data <- read_csv("/home/nitay/COVID-19/Data/Empirical_data_for_transformations/2020-07-06/all_states_processed_relevant.csv",col_names = F)
grid_search_parameters = read_csv("/home/nitay/COVID-19/Data/starting_day_followup_table.csv")
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
date = Sys.Date()
states <- c('Chile','Italy','France','Germany','US','Switzerland','Brazil','Peru','Iran','Israel','Turkey','India','Belgium')
selected_states <- c('Italy','Germany','US','Switzerland','Israel','Iran','Brazil')
lags = c(0.0000000e+00,1.3000000e+01,0.0000000e+00,0.0000000e+00,8.0000000e+00,1.3000000e+01,1.0000000e+01,1.3000000e+01,9.0000000e+00,9.0000000e+00,1.1000000e+01,1.3000000e+01,1.0000000e+01)
for(state in selected_states)
{
  start_day = grid_search_parameters %>% filter(State == state) %>% select(`Start day`)
  low_alpha = grid_search_parameters %>% filter(State == state) %>% select(`Lower alpha`)
  up_alpha = grid_search_parameters %>% filter(State == state) %>% select(`upper alpha`)
  low_hhh = grid_search_parameters %>% filter(State == state) %>% select(`hhh_low`)
  up_hhh = grid_search_parameters %>% filter(State == state) %>% select(`hhh_upper`)
  cut_off_day = grid_search_parameters %>% filter(State == state) %>% select(`End Day`)
  grid_search_results <- mainFunction(state, start_day$`Start day`, cut_off_day = cut_off_day$`End Day`, population_list[[state]], 
                                      alpha_grid = seq(low_alpha$`Lower alpha`,up_alpha$`upper alpha`, length.out = 40), hhh_grid = c(low_hhh$hhh_low, up_hhh$hhh_upper),
                                 lag = lags[which(sort(states) == state)], export_data_for_transformations = T)
  write.csv(x = grid_search_results, file = sprintf('/home/nitay/COVID-19/Data/Empirical_data_for_transformations/2020-07-11/%s_%s.csv',state,date))
  next 	
  file_name = sprintf('%s_%s.RData',state,Sys.Date())
  save(grid_search_results, file = sprintf('Data/Grid_search_results/09_07_2020/%s', file_name))
  plot(grid_search_results$nation_wide_rtt_results$inner_grid_search_results$profile_likelihood_K, main = state)
  plot(grid_search_results$nation_wide_rtt_results$inner_grid_search_results$profile_likelihood_alpha, main = state)
  K_grid <- c(grid_search_results$nation_wide_rtt_results$inner_grid_search_results$K_CI[1], grid_search_results$nation_wide_rtt_results$inner_grid_search_results$profile_likelihood_K$k[which.max(grid_search_results$nation_wide_rtt_results$inner_grid_search_results$profile_likelihood_K$llk)], 
                      grid_search_results$nation_wide_rtt_results$inner_grid_search_results$K_CI[2])
  alpha_grid <- c(grid_search_results$nation_wide_rtt_results$inner_grid_search_results$profile_likelihood_alpha$alpha[which.max(grid_search_results$nation_wide_rtt_results$inner_grid_search_results$profile_likelihood_alpha$llk > -2.5)],
                         grid_search_results$nation_wide_rtt_results$inner_grid_search_results$profile_likelihood_alpha$alpha[which.max(grid_search_results$nation_wide_rtt_results$inner_grid_search_results$profile_likelihood_alpha$llk)], 
                  grid_search_results$nation_wide_rtt_results$inner_grid_search_results$profile_likelihood_alpha$alpha[max(which(grid_search_results$nation_wide_rtt_results$inner_grid_search_results$profile_likelihood_alpha$llk > -2.5))])
  trajectories <- lapply(1:3, function(i){predictCovidTrajectory(grid_search_results$nation_wide_rtt_results$environment_data, K_grid[i],
                                                                        alpha_grid[i], alpha_grid = seq(low_alpha$`Lower alpha`,up_alpha$`upper alpha`, length.out = 60))})
  reportCovidGreeks(trajectories,K_grid, alpha_grid)
}
