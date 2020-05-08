library(dplyr)
library(tidyverse)
usa_confirmed_cases <- read_csv("./csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")
global_confirmed_cases <- read_csv("./csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
usa_confirmed_cases %>% 
  str()
usa_confirmed_cases %>% 
  select(-iso2,-iso3,-Admin2, -UID, -code3, -FIPS, -Country_Region, -Lat, -Long_, -Combined_Key) %>% 
  group_by(Province_State) %>% 
  filter(Province_State == 'New York') %>% 
  summarise_all(list(total = sum))
  
# Belgium, Spain, France, UK, Italy  
