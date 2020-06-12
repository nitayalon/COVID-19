summarizeGridSearchResultsForPaper <- function(grid_search_results,
                                               alpha_grid_search = seq(0.1,0.6,length.out = 50),
                                               starting_day = 60,
                                               scale_factor = 1e6,
                                               ylim_a = c(0,3.2),
                                               ylim_b = c(0,3.2),
                                               dates = seq(as.Date('22/01/2020',format = '%d/%m/%y'),as.Date('01/09/2020',format = '%d/%m/%y'),'day'),
                                               last_date_a = as.Date('01/09/2020',format = '%d/%m/%y'),
                                               last_date_b = as.Date('01/09/2020',format = '%d/%m/%y'),
                                               last_fitting_date = as.Date('07/06/2020',format = '%d/%m/%y')
                                               ){
  K_grid <- c(grid_search_results$nation_wide_rtt_results$inner_grid_search_results$K_CI[1], grid_search_results$nation_wide_rtt_results$inner_grid_search_results$profile_likelihood_K$k[which.max(grid_search_results$nation_wide_rtt_results$inner_grid_search_results$profile_likelihood_K$llk)], 
                      grid_search_results$nation_wide_rtt_results$inner_grid_search_results$K_CI[2])
  alpha_grid <- c(NA, grid_search_results$nation_wide_rtt_results$inner_grid_search_results$profile_likelihood_alpha$alpha[which.max(grid_search_results$nation_wide_rtt_results$inner_grid_search_results$profile_likelihood_alpha$llk)], NA)
  trajectories <- lapply(1:3, function(i){predictCovidTrajectory(grid_search_results$nation_wide_rtt_results$environment_data, 
                                                                 K_grid[i],
                                                                 alpha_grid[i],
                                                                 alpha_grid_search)})
  alpha_grid <- c(trajectories[[1]]$alpha, alpha_grid[2], trajectories[[3]]$alpha)
  plotTrajectories(grid_search_results$covid_data_sets$empirical_covid_data,
                   grid_search_results$covid_data_sets$transformed_covid_data,
                   trajectories,
                   dates,
                   last_date_a,
                   scale_factor = scale_factor,
                   ylim = ylim_a,
                   starting_day = starting_day,
                   last_fitting_date = last_fitting_date)
  
  plotTrajectoriesB(grid_search_results$covid_data_sets$empirical_covid_data,
                    grid_search_results$covid_data_sets$transformed_covid_data,
                    trajectories,
                    dates,
                    last_date = last_date_b,
                    ylim = ylim_b,
                    starting_day = starting_day, 
                    scale_factor = scale_factor,
                    last_fitting_date = last_fitting_date)

}

