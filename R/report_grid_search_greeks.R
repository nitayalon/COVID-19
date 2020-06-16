reportCovidGreeks <- function(trajectoris, k_ci, alpha_ci)
{
  gamma <- sapply(trajectoris, function(x){x$gamma})
  beta <- sapply(trajectoris, function(x){x$beta})
  K <- k_ci
  I0 = (beta / gamma) ^ (1 / (1 - alpha_ci))
  slope_1 = beta ^ (1 / (1 - alpha_ci))
  slope_2 = gamma ^ (alpha_ci / (1 - alpha_ci))
  results = tibble(CI = c('Lower','MLE','Upper'),
                   K = k_ci,
                   Alpha = alpha_ci,
                   Gamma = gamma,
                   Beta = beta,
                   I0 = I0,
                   Ratio = slope_1 / slope_2)
  return(results)
}