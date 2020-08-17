ExportUSStateData <- function(state_name){
  us_data_files = list.files(path = '/home/nitay/COVID-19/csse_covid_19_data/csse_covid_19_daily_reports_us/', pattern="*.csv")
  columns = c('Confirmed','Deaths',	'Recovered',	'Active', 'People_Tested','People_Hospitalized')
  # Original data 
  daily_data = sapply(us_data_files, function(x){ExtractStateData(x,state_name)}) %>% t() %>% as_tibble()
  daily_data$date = sapply(us_data_files, function(x){(strsplit(x, '[.]'))[[1]][1]})
  daily_data[is.na(daily_data)] = 0
  return(daily_data)
}

ExtractStateData <- function(file_name, state_name){
  data_set = read.csv(sprintf('/home/nitay/COVID-19/csse_covid_19_data/csse_covid_19_daily_reports_us/%s',file_name))
  return(data_set %>% 
           filter(Province_State == state_name) %>% 
           select(Confirmed,Deaths,	Recovered,	Active, People_Tested,People_Hospitalized) %>% unlist())
}
