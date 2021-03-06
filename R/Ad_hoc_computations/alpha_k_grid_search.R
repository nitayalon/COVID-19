gridSearchMainFunction <- function(national_data, 
                                   population, 
                                   starting_day,
                                   alpha_grid = seq(0.4,0.8,length.out = 40),
                                   hhh_grid = c(1,400),
                                   cut_off_day = NULL, 
                                   prediction_period = NULL,
                         environment_parameter_list = NULL,
                         only_bounds = F,
                         just_data = F,
                         lag = 0){
  partition_parameter = 100
  del = 1/partition_parameter 
  environment_data <- DataLoader(national_data,starting_day, cut_off_day,lag = lag)
  if(just_data){
    return(environment_data)
  }
  X = environment_data$X
  VW = environment_data$VW
  Y = environment_data$Y
  X_middle = environment_data$X_middle
  Y_middle = environment_data$Y_middle
  n = environment_data$n
  hhh_upper_limit = hhh_grid[2]
  hhh_lower_limit = hhh_grid[1]
  NNN = floor(max(X) / 100) #What is the grid of K
  hhh <- seq(hhh_lower_limit, hhh_upper_limit, length.out = 50)
  
  # Outer grid
  k_grid <- c(max(X) + hhh * NNN) #, population)
  outer_grid_search_results <- gridSearchMethodHelper(alpha_grid,k_grid,X,VW,Y, del,Y_middle,X_middle,partition_parameter,return_profile = T)
  # Inner grid
  lower_inner_k_grid <- max(k_grid[which.max(outer_grid_search_results$k_profile) - 2], min(k_grid), na.rm = T)
  upper_inner_k_grid <- min(k_grid[which.max(outer_grid_search_results$k_profile) + 2],population,na.rm = T)
  inner_k_grid <- seq(lower_inner_k_grid, upper_inner_k_grid, length.out = 40)
  inner_grid_search_results <- gridSearchMethodHelper(alpha_grid,inner_k_grid,X,VW,Y, del,Y_middle,X_middle,partition_parameter,return_profile = F)
  return(list(environment_data = environment_data,
              inner_grid_search_results = inner_grid_search_results))
}
