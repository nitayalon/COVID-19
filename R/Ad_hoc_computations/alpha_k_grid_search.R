gridSearchMainFunction <- function(national_data, population, starting_day, cut_off_day = NULL, prediction_period = NULL,
                         environment_parameter_list = NULL){
  partition_parameter = 100
  del = 1/partition_parameter # dt
  environment_data <- DataLoader(national_data,starting_day, cut_off_day)
  X = environment_data$X
  VW = environment_data$VW
  Y = environment_data$Y
  X_middle = environment_data$X_middle
  Y_middle = environment_data$Y_middle
  n = environment_data$n
  hhh_upper_limit = 10000
  hhh_lower_limit = 1
  NNN = floor(max(X) / 100) #What is the grid of K
  hhh <- seq(hhh_lower_limit, hhh_upper_limit, length.out = 40)
  alpha_grid = seq(0.2,0.8,length.out = 40)
  k_grid <- c(max(X) + hhh * NNN, population)
  outer_grid_parameters <- expand.grid(alpha_grid,k_grid)
  print("Starting outer grid search")
  starting_time <- Sys.time()
  grid_search_results <- pblapply(1:nrow(outer_grid_parameters), function(i){GridSearchInnerLoop(X,VW,Y,outer_grid_parameters$Var1[i],
                                                                                                 outer_grid_parameters$Var2[i],del,Y_middle,X_middle,partition_parameter)})
  end_time <- Sys.time()
  COV <- lapply(grid_search_results, function(x){x$COV})
  OBJ <- sapply(grid_search_results, function(x){x$OBJ})
  OBJB <- sapply(grid_search_results, function(x){x$OBJB})
  
  likelihood_matrix <- matrix(OBJ, nrow = length(unique(outer_grid_parameters$Var2)),
                              ncol = length(unique(outer_grid_parameters$Var1)), byrow = T)
  
  minimal_llk_per_k <- apply(likelihood_matrix, 1, min)
  minimal_llk_per_k <- -minimal_llk_per_k - max(-minimal_llk_per_k)
  minimal_llk_per_alpha <- apply(likelihood_matrix, 2, min)
  minimal_llk_per_alpha <- -minimal_llk_per_alpha - max(-minimal_llk_per_alpha)
  lower_alpha_inner_grid <- max(alpha_grid[which.max(minimal_llk_per_alpha)] - 0.2, 0.0)
  upper_alpha_inner_grid <- min(alpha_grid[which.max(minimal_llk_per_alpha)] + 0.2, 1.0)
  inner_alpha_grid <- seq(lower_alpha_inner_grid ,upper_alpha_inner_grid,length.out = 20)
  browser()
  lower_inner_k_grid <- max(unique(outer_grid_parameters$Var2)[which.max(minimal_llk_per_k) - 1], min(k_grid), na.rm = T)
  upper_inner_k_grid <- min(unique(outer_grid_parameters$Var2)[which.max(minimal_llk_per_k) + 1],population,na.rm = T)
  inner_k_grid <- seq(lower_inner_k_grid, upper_inner_k_grid, length.out = 40)
  
  inner_grid_parameters <- expand.grid(inner_alpha_grid,inner_k_grid)
  print("Starting inner grid search")
  inner_grid_search_results <- pblapply(1:nrow(inner_grid_parameters), function(i){GridSearchInnerLoop(X,VW,Y,inner_grid_parameters$Var1[i],
                                                                                                 inner_grid_parameters$Var2[i],del,Y_middle,X_middle,partition_parameter)})
  
  COV_inner <- lapply(inner_grid_search_results, function(x){x$COV})
  OBJ_inner <- sapply(inner_grid_search_results, function(x){x$OBJ})
  OBJB_inner <- sapply(inner_grid_search_results, function(x){x$OBJB})
  
  inner_likelihood_matrix <- matrix(OBJ_inner, nrow = length(unique(inner_grid_parameters$Var2)),
                              ncol = length(unique(inner_grid_parameters$Var1)), byrow = T)
  
  inner_minimal_llk_per_k <- apply(inner_likelihood_matrix, 1, min)
  inner_minimal_llk_per_k <- -inner_minimal_llk_per_k - max(-inner_minimal_llk_per_k)
  inner_minimal_llk_per_alpha <- apply(inner_likelihood_matrix, 2, min)
  inner_minimal_llk_per_alpha <- -inner_minimal_llk_per_alpha - max(-inner_minimal_llk_per_alpha)
  
  return(list(outer_grid_parameters = outer_grid_parameters,
              inner_grid_parameters = inner_grid_parameters,
              environment_data = environment_data,
              COV_matrix = COV_inner,
              OBJ = OBJ_inner,         
              OBJB = OBJB_inner,         
              outer_grid_llk_matrix = likelihood_matrix,
              inner_grid_llk_matrix = inner_likelihood_matrix))
}
