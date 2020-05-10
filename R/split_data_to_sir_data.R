split_data_to_SIR <- function(covid_data, starting_day_index = NULL){
  if(is.null(starting_day_index)){
    starting_day_index = floor(nrow(covid_data) / 2)
  }
  n = nrow(covid_data)
  X = covid_data[starting_day_index:n,1]
  V = covid_data[starting_day_index:n,2]
  W = covid_data[starting_day_index:n,3]
  VW = V+W
  Y = X - VW
  return(list(X = X,
              VW = VW,
              Y=Y)) 
}