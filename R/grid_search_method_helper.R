gridSearchMethodHelper <- function(alpha_grid, k_grid, X,VW,Y, del,Y_middle,X_middle,partition_parameter,return_profile = T){
  grid_parameters <- expand.grid(alpha_grid,k_grid)
  grid_search_results <- pblapply(1:nrow(grid_parameters), function(i){GridSearchInnerLoop(X,VW,Y,grid_parameters$Var1[i],grid_parameters$Var2[i],del,Y_middle,X_middle,partition_parameter)})
  COV <- lapply(grid_search_results, function(x){x$COV})
  T_final <- lapply(grid_search_results, function(x){x$T_final})
  OBJ <- sapply(grid_search_results, function(x){x$OBJ})
  OBJB <- sapply(grid_search_results, function(x){x$OBJB})
  likelihood_matrix <- matrix(OBJB, nrow = length(unique(grid_parameters$Var2)),
                              ncol = length(unique(grid_parameters$Var1)), byrow = T)
  
  minimal_llk_per_k <- apply(likelihood_matrix, 1, function(x){min(x,na.rm = T)})
  minimal_llk_per_k <- -minimal_llk_per_k - max(-minimal_llk_per_k)
  minimal_llk_per_alpha <- apply(likelihood_matrix, 2, function(x){min(x,na.rm = T)})
  minimal_llk_per_alpha <- -minimal_llk_per_alpha - max(-minimal_llk_per_alpha)
  if(return_profile){
    return(list(alpha_profile = minimal_llk_per_alpha,
                k_profile = minimal_llk_per_k))
  }
  profile_likelihood_alpha = tibble(alpha = alpha_grid, llk = minimal_llk_per_alpha)
  profile_likelihood_K  = tibble(k = k_grid, llk = minimal_llk_per_k)
  K_CI = c(k_grid[min(which(minimal_llk_per_k > -2.5))],k_grid[max(which(minimal_llk_per_k > -2.5))])
  
  return(list(profile_likelihood_alpha = profile_likelihood_alpha,
              COV = COV[[which.max(OBJB)]],
              T_final = T_final[which.max(OBJB)],
              profile_likelihood_K = profile_likelihood_K,
              likelihood_matrix = likelihood_matrix,
              K_CI = K_CI))
}