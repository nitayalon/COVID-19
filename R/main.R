mainFunction <- function(state_name, starting_day, population, cut_off_day = NULL, other_parameters = NULL,
                         only_lower_bounds = F,sweden_data = F){
  
  confirmed_cases <- global_confirmed_cases %>% 
    select(-`Province/State`, -Lat, -Long) %>% 
    group_by(`Country/Region`) %>% 
    filter(`Country/Region` == state_name) %>% 
    summarise_all(list(total = sum))
  confirmed_deaths <- global_confirmed_deaths %>% 
    select(-`Province/State`, -Lat, -Long) %>% 
    group_by(`Country/Region`) %>% 
    filter(`Country/Region` == state_name) %>% 
    summarise_all(list(total = sum))
  confirmed_recovered <- global_confirmed_recovered %>% 
      select(-`Province/State`, -Lat, -Long) %>% 
      group_by(`Country/Region`) %>% 
      filter(`Country/Region` == state_name) %>% 
      summarise_all(list(total = sum))
  
  full_data_for_export <- full_join(
    full_join(confirmed_cases %>%
                gather(var, val, 2:ncol(confirmed_cases)) %>%
                select(var, val),
              confirmed_deaths %>%
                gather(var, val, 2:ncol(confirmed_cases)) %>%
                select(var, val),
              by = "var",
              suffix = c('.confirmed_cases','.confirmed_deaths')),
    confirmed_recovered %>% 
      gather(var, val, 2:ncol(confirmed_cases)) %>%
      select(var, val),
    by = "var"
  ) 
  covid_data <- 
    full_data_for_export %>% 
    select(-var) %>% 
    as.matrix()
  if(sweden_data)
  {
    covid_data[,3] <- swedish_data$V1
  }
  print('Data ready')
  nation_wide_rtt_results <- gridSearchMainFunction(covid_data, population, starting_day, cut_off_day,only_lower_bounds)
  return(nation_wide_rtt_results)
}