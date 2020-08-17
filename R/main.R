mainFunction <- function(state_name, 
                         starting_day,
                         population, 
                         alpha_grid = seq(0.4,0.8,length.out = 40),
                         hhh_grid = c(1,400),
                         cut_off_day = NULL, 
                         other_parameters = NULL,
                         only_lower_bounds = F,
                         sweden_data = F,
                         just_data = F,
                         export_data_for_transformations = F,
                         transformed_data = NULL,
                         lag = 0){
  if(export_data_for_transformations){
    covid_data_sets = uploadTransformData(state_name,just_empirical_data = T)
    return(as_tibble(covid_data_sets))
  }
  else{
    covid_data_sets = uploadTransformData(state_name, just_data, transformed_data) 
  }
  print('Data ready')
  nation_wide_rtt_results <- gridSearchMainFunction(covid_data_sets$transformed_covid_data, 
                                                    population, 
                                                    starting_day, 
                                                    cut_off_day,
                                                    only_lower_bounds,
                                                    just_data = just_data, 
                                                    alpha_grid = alpha_grid,
                                                    hhh_grid = hhh_grid,
                                                    lag = lag)
  return(list(covid_data_sets = covid_data_sets, 
              nation_wide_rtt_results = nation_wide_rtt_results))
}