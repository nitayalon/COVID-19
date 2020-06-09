mainFunction <- function(state_name, 
                         starting_day,
                         population, 
                         alpha_grid = seq(0.4,0.8,length.out = 40),
                         cut_off_day = NULL, 
                         other_parameters = NULL,
                         only_lower_bounds = F,
                         sweden_data = F,
                         just_data = F){
  
  covid_data_sets = uploadTransformData(state_name) 
  print('Data ready')
  nation_wide_rtt_results <- gridSearchMainFunction(covid_data_sets$transformed_covid_data, population, starting_day, cut_off_day,only_lower_bounds,just_data = just_data, alpha_grid = alpha_grid)
  return(list(covid_data_sets = covid_data_sets, 
              nation_wide_rtt_results = nation_wide_rtt_results))
}