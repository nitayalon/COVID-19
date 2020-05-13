computeOptimalK <- function(single_dim_llk,
                            two_dim_llk,
                            NNN,X)
{
  single_dim_min_llk=min(single_dim_llk)
  single_dim_llk_ind=which.min(single_dim_llk)
  
  two_dim_min_llk=min(two_dim_llk)
  two_dim_llk_ind=which.min(two_dim_llk)
  if(min(single_dim_llk_ind,two_dim_llk_ind) > 5){
    single_dim_CI <- computeConfInterForK(single_dim_llk, single_dim_llk_ind, NNN, X)
    two_dim_CI <- computeConfInterForK(two_dim_llk, two_dim_llk_ind, NNN, X)
  }
  else{
    single_dim_CI = NULL
    two_dim_CI = NULL
  }
  return(list(
    single_dim_CI = single_dim_CI,
    two_dim_CI = two_dim_CI
  ))
}

computeConfInterForK <- function(llk_grid, llk_index, NNN, X) {
  x_matrix = tryCatch({
    matrix(c(t((llk_index-5):(llk_index+5)), t((llk_index-5):(llk_index+5))^2), nrow = 11)},
    error=function(e)
    {
      NULL
    })
  linear_model_coefs = (lm(formula = llk_grid[(llk_index-5):(llk_index+5)] ~ x_matrix))$coef
  minimum_location = -linear_model_coefs[2]/(2*linear_model_coefs[3]) # minimum location - pm 1 from single_dim_llk_ind
  FISHER_information_number = (2 * linear_model_coefs[3]) / (NNN^2)
  K = max(X) + minimum_location*NNN
  return(c(K - 2/sqrt(FISHER_information_number), K, K + 2/sqrt(FISHER_information_number)))
}