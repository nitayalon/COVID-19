DataLoader <- function(covid_data, starting_day,cut_off_day= NULL, lag = 0)
{
  min_feasible_date <- which.max(apply(covid_data, 1, function(x){all(x > 0)}))
  ind <- max(starting_day,min_feasible_date)
  if(is.null(cut_off_day)){
    cutoff <- nrow(covid_data) - lag
  }
  else{
    cutoff <- min(nrow(covid_data), cut_off_day - lag)
  }
  X = covid_data$X[ind:cutoff]
  VW = covid_data$VW[ind:cutoff]
  Y = covid_data$Y[ind:cutoff]
  n = length(X)
  Y_middle=(Y[1:(n-1)]+Y[2:n])/2
  Y_middle=c(Y_middle, Y[n])
  X_middle=(X[1:(n-1)]+X[2:n])/2
  X_middle=c(X_middle, X[n])
  return(list(full_data = covid_data,
              X = X,
              VW = VW,
              Y = Y,
              n = n,
              X_middle = X_middle,
              Y_middle = Y_middle))
}