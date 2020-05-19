computeQuadraticFit <- function(llk_data, parameter_grid, environment_data){
  X <- environment_data$X
  NNN = floor(max(X) / 100)
  minimal_llk_per_k <- apply(llk_data,1 ,min)
  minimal_llk_per_k_modified <- -minimal_llk_per_k - max(-minimal_llk_per_k)
  K_grid <- (1:length(unique(parameter_grid$Var2)))[minimal_llk_per_k_modified >= -3]
  y <- minimal_llk_per_k[minimal_llk_per_k_modified >= -3]
  x_matrix <- data.frame(k = K_grid, k2 = K_grid^2, y = y)
  linear_model_coefs = (lm(formula = y ~ . ,data = x_matrix))$coef
  minimum_location = -linear_model_coefs[2]/(2*linear_model_coefs[3]) # minimum location - pm 1 from single_dim_llk_ind
  FISHER_information_number = (2 * linear_model_coefs[3]) / (NNN^2)
  K = max(X) + minimum_location*NNN
  return(c(K - 2.5/sqrt(FISHER_information_number), K, K + 2.5/sqrt(FISHER_information_number)))
}

findAlphaMLE <- function(llk_data, parameter_grid){
  minimal_llk_per_k <- apply(llk_data,1 ,min)
  minimal_llk_per_k_modified <- -minimal_llk_per_k - max(-minimal_llk_per_k)
  alpha_mle <- unique(parameter_grid$Var1)[which.max(minimal_llk_per_k_modified)]
  return(alpha_mle)
}

usa_k_ci <- computeQuadraticFit(likelihood_matrix, usa_data_fine_alpha_grid$grid_parameters, usa_data_fine_alpha_grid$environment_data)
italy_k_ci <- computeQuadraticFit(likelihood_matrix_italy, italy_data_fine_alpha_grid$grid_parameters, italy_data_fine_alpha_grid$environment_data)

# Find alpha max
alpha_max_us <- findAlphaMLE(likelihood_matrix, usa_data_fine_alpha_grid$grid_parameters)
alpha_max_italy <- findAlphaMLE(likelihood_matrix_italy, italy_data_fine_alpha_grid$grid_parameters)

# compute x,vw,y


# plot all together
