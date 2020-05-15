findInitialDay <- function(state){
  confirmed_cases <- global_confirmed_cases %>% 
    select(-`Province/State`, -Lat, -Long) %>% 
    group_by(`Country/Region`) %>% 
    filter(`Country/Region` == state) %>% 
    summarise_all(list(total = sum))
  confirmed_deaths <- global_confirmed_deaths %>% 
    select(-`Province/State`, -Lat, -Long) %>% 
    group_by(`Country/Region`) %>% 
    filter(`Country/Region` == state) %>% 
    summarise_all(list(total = sum))
  confirmed_recovered <- global_confirmed_recovered %>% 
    select(-`Province/State`, -Lat, -Long) %>% 
    group_by(`Country/Region`) %>% 
    filter(`Country/Region` == state) %>% 
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
  starting_day <- seq(40,75,1)
  starting_day_params <- lapply(starting_day, function(i){RunFullCycle(covid_data, i)})
  print('Plotting results')
  which.min(sapply(starting_day_params, function(x){x$std}))
  
  plot(starting_day, sapply(starting_day_params, function(x){x$std}), type = 'b',ylab = 'mean(diag(cov))',
       xlab = 'Starting day', main = state)
  abline(a = min(sapply(starting_day_params, function(x){x$std})) * 2.5, b = 0, col = 'red')
  abline(a = min(sapply(starting_day_params, function(x){x$std})) * 1.5, b = 0, col = 'green')
}
nation_list <- c('Belgium', 'Canada', 'Chile', 'France','Germany','Israel', 'Italy','Netherlands',
                 'Sweden','United Kingdom', 'US')
lapply(nation_list[2:length(nation_list)], function(n){findInitialDay(n)})

