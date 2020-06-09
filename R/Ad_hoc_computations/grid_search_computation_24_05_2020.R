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
source('R/load_transformed_data.R')
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
  Argentina =    45153114 
) 

state <- 'Belgium'
date = '07_06_2020'
belgium_grid_search_results_07_06_2020 <- mainFunction(state, 60, population_list[[state]], alpha_grid = seq(0.2,0.6,length.out = 40))
save(x = belgium_grid_search_results_07_06_2020, file = sprintf('/home/nitay/COVID-19/R/Ad_hoc_computations/grid_search_results/%s_%s.RData',state,date))

# brazil_K_grid <- c(brazil_grid_search_results$nation_wide_rtt_results$inner_grid_search_results$K_CI[1], 
#                     brazil_grid_search_results$nation_wide_rtt_results$inner_grid_search_results$profile_likelihood_K$k[which.max(brazil_grid_search_results$nation_wide_rtt_results$inner_grid_search_results$profile_likelihood_K$llk)], 
#                     brazil_grid_search_results$nation_wide_rtt_results$inner_grid_search_results$K_CI[2])
# brazil_alpha_grid <- c(NA, brazil_grid_search_results$nation_wide_rtt_results$inner_grid_search_results$profile_likelihood_alpha$alpha[which.max(brazil_grid_search_results$nation_wide_rtt_results$inner_grid_search_results$profile_likelihood_alpha$llk)], NA)
# brazil_trajectories <- lapply(1:3, function(i){predictCovidTrajectory(brazil_grid_search_results$nation_wide_rtt_results$environment_data, brazil_K_grid[i],brazil_alpha_grid[i],alpha_grid = seq(0.5,0.9,length.out = 40))})
# brazil_alpha_grid <- c(brazil_trajectories[[1]]$alpha, brazil_alpha_grid[2], brazil_trajectories[[3]]$alpha)
# 
# plot(brazil_grid_search_results$nation_wide_rtt_results$inner_grid_search_results$profile_likelihood_K)
# abline(h = -2.5, col = 'red')
# plot(brazil_grid_search_results$nation_wide_rtt_results$inner_grid_search_results$profile_likelihood_alpha)
# abline(h = -2.5, col = 'red')
# dates <- seq(as.Date('22/01/2020',format = '%d/%m/%y'),as.Date('01/12/2020',format = '%d/%m/%y'),'day')
# last_date <- as.Date('01/12/2020',format = '%d/%m/%y')
# plotTrajectories(brazil_grid_search_results$nation_wide_rtt_results$environment_data,
#                  brazil_grid_search_results$covid_data_sets$transformed_covid_data[start_day:nrow(brazil_grid_search_results$covid_data_sets$transformed_covid_data),],
#                  brazil_trajectories,dates,last_date,'brazil',ylim = c(0,30),starting_day = start_day,scale_factor = 1e6)
# reportCovidGreeks(brazil_trajectories, brazil_K_grid, brazil_alpha_grid)