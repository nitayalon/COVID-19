uploadTransformData <- function(state_name, 
                                just_empirical_data = F,
                                transformed_state_data = NULL){
  states_list <- sort(c('Chile','Italy','France','Germany','US','Switzerland','Brazil','Peru','Iran','Israel','Turkey','India','Belgium'))
  # Original data 
  confirmed_cases <- ExtractStateData(global_confirmed_cases, state_name)
  confirmed_deaths <- ExtractStateData(global_confirmed_deaths, state_name)
  confirmed_recovered <- ExtractStateData(global_confirmed_recovered, state_name)
  
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
  colnames(empirical_covid_data) = c('X','V','W')
  if(just_empirical_data){
    return(empirical_covid_data)
  }
  # linear regression data
  if(is.null(transformed_state_data)){
    index = which(states_list == state_name)
    transformed_covid_data <- transformed_data[,(3 * index + -2) : (3 * index)]
    names(transformed_covid_data) <- c('X','Y','VW')
  }
  else{
    transformed_covid_data  = transformed_state_data
    names(transformed_covid_data) <- c('X','Y','VW')
  }
  empirical_data = empirical_covid_data %>% 
    as.tibble() %>% 
    mutate(Y = X - (V+W), VW = V+W) %>% 
    select(X,Y,VW)
  return(list(empirical_covid_data = empirical_data,
              transformed_covid_data = transformed_covid_data))
}

ExtractStateData <- function(data_set, state_name){
  return(data_set %>% 
    select(-`Province/State`, -Lat, -Long) %>% 
    group_by(`Country/Region`) %>% 
    filter(`Country/Region` == state_name) %>% 
    summarise_all(list(total = sum)))
}

ExtractProvinceData <- function(data_set, state_name, province_name){
  browser()
  new_ds = data_set %>% 
    select(-Lat, -Long) %>% 
    group_by(`Country/Region`, `Province/State`) %>% 
    filter(`Country/Region` == state_name & `Province/State` == province_name) %>% 
    summarise_all(list(total = sum))
  return(new_ds)
}