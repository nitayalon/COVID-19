mainFunction <- function(state_name, starting_day, cut_off_day = NULL, other_parameters = NULL){
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
  print('Data ready')
  nation_wide_rtt_results <- RunFullCycle(covid_data, starting_day, cut_off_day)
  return(nation_wide_rtt_results)
}