gridSearchMainFunction <- function(national_data, 
                                   population, 
                                   starting_day, 
                                   cut_off_day = NULL, 
                                   prediction_period = NULL,
                         environment_parameter_list = NULL,
                         only_bounds = F){
  partition_parameter = 100
  del = 1/partition_parameter 
  environment_data <- DataLoader(national_data,starting_day, cut_off_day)
  X = environment_data$X
  VW = environment_data$VW
  Y = environment_data$Y
  X_middle = environment_data$X_middle
  Y_middle = environment_data$Y_middle
  n = environment_data$n
  hhh_upper_limit = 200
  hhh_lower_limit = 1
  NNN = floor(max(X) / 100) #What is the grid of K
  hhh <- seq(hhh_lower_limit, hhh_upper_limit, length.out = 50)
  
  # Outer grid
  alpha_grid = seq(0.2,0.6,length.out = 40)
  k_grid <- c(max(X) + hhh * NNN, population)
  outer_grid_search_results <- gridSearchMethodHelper(alpha_grid,k_grid,X,VW,Y, del,Y_middle,X_middle,partition_parameter)
  # Inner grid
  lower_inner_k_grid <- max(k_grid[which.max(outer_grid_search_results$k_profile) - 2], min(k_grid), na.rm = T)
  upper_inner_k_grid <- min(k_grid[which.max(outer_grid_search_results$k_profile) + 2],population,na.rm = T)
  inner_k_grid <- seq(lower_inner_k_grid, upper_inner_k_grid, length.out = 40)
  inner_grid_search_results <- gridSearchMethodHelper(alpha_grid,inner_k_grid,X,VW,Y, del,Y_middle,X_middle,partition_parameter,return_profile = F)
  return(list(environment_data = environment_data,
              inner_grid_search_results = inner_grid_search_results))
}
