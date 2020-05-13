library(tidyverse)
DataLoader <- function(national_data, starting_day)
{
  min_feasible_date <- which.max(apply(covid_data, 1, function(x){all(x > 0)}))
  ind <- max(starting_day,min_feasible_date)
  X = covid_data[ind:nrow(covid_data),1]
  V = covid_data[ind:nrow(covid_data),2]
  W = covid_data[ind:nrow(covid_data),3]
  VW = V+W
  Y = X - VW
  n <- length(X)
  Y_middle=(Y[1:(n-1)]+Y[2:n])/2
  Y_middle=c(Y_middle, Y[n])
  X_middle=(X[1:(n-1)]+X[2:n])/2
  X_middle=c(X_middle, X[n])
  return(list(X = X,
              VW = VW,
              Y = Y,
              n = n,
              X_middle = X_middle,
              Y_middle = Y_middle))
}