mainFunction <- function(state_name, 
                         starting_day, 
                         population, 
                         cut_off_day = NULL, 
                         other_parameters = NULL,
                         only_lower_bounds = F,sweden_data = F,just_data = F){
  
  covid_data_sets = uploadTransformData(state_name) 
  print('Data ready')
  nation_wide_rtt_results <- gridSearchMainFunction(covid_data_sets$transformed_covid_data, population, starting_day, cut_off_day,only_lower_bounds,just_data = just_data)
  return(nation_wide_rtt_results)
}