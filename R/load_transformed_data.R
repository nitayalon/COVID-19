uploadTransformData <- function(state_name){
  states_list <- sort(c('US','Brazil','Israel','Italy','Germany','France','Sweden','Chile','Belgium'))
  # Original data 
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
  empirical_covid_data <- 
    full_data_for_export %>% 
    select(-var) %>% 
    as.matrix()
  # linear regression data
  index = which(states_list == state_name)
  transformed_covid_data <- transformed_data[,(3 * index + -2) : (3 * index)]
  names(transformed_covid_data) <- c('X','Y','VW')
  return(list(empirical_covid_data = empirical_covid_data,
              transformed_covid_data = transformed_covid_data))
}