pde_parameter_estimation <- function(covid_data, config_file, ...) {
  partition_parameter = config_file$partition_parameter
  hhh_upper_limit = config_file$hhh_upper_limit
  number_of_inner_loops = config_file$number_of_inner_loops
  calibration_loops = config_file$calibration_loops
  
  sir_list <- split_data_to_SIR(covid_data, 56)
  gammahat = betahat = asympt = c()
  LOGL = VAR1 = OBJ = OBJB = c()
  x = y = vw = c()
  
  # Compute mid-day data
  n <- length(sir_list$X)
  NNN=floor(max(sir_list$X) / 100) #What is the grid of K
  Y_middle=(sir_list$Y[1:(n-1)]+sir_list$Y[2:n])/2
  Y_middle=c(Y_middle, sir_list$Y[n])
  X_middle=(sir_list$X[1:(n-1)]+sir_list$X[2:n])/2
  X_middle=c(X_middle, X[n])
  
  del = 1/partition_parameter # dt
  # Set the first 100 obs to same value - can be 52,53
  x[1:partition_parameter]=X[1]
  y[1:partition_parameter]=Y[1]
  vw[1:partition_parameter]=VW[1]
  T_final = T1 = matrix(0, ncol = 2, nrow = n)
  
  for(hhh in 1:hhh_upper_limit)
  {
    K=max(X) + hhh * NNN
    asympt[hhh] = K
    
    betahat[hhh] = (X[n]-X[1])/sum(sqrt(max(1,Y_middle-Y_middle[1]))*(K-(X_middle-X_middle[1])))
    beta=betahat[hhh]
    
    gammahat[hhh] = (VW[n]-VW[1])/sum(max(1,Y_middle-Y_middle[1]))
    gamma=gammahat[hhh]
    
    for(jjj in 1:calibration_loops)
    {
      for(i in (number_of_inner_loops + 1):(number_of_inner_loops * 5 * n))
      {
        # infection rate growth
        x[i]=x[i-1]+beta*sqrt(y[i-1])*max(0,K-x[i-1])*del
        # removed rate
        vw[i]=vw[i-1]+gamma*y[i-1]*del
        # Current ills
        y[i]=max(0,x[i]-vw[i])
      }
      for(i in 1:n)
      {
        # random time transformation
        # The events where the pde solution equals to the emphirical values
        T1[i,1]=sum(x<=X[i])
        T1[i,2]=sum(vw<=VW[i])
      }
      T1=T1*del
      beta=beta*T1[n,1]/n
      gamma=gamma*T1[n,2]/n
      betahat[hhh]=beta
      gammahat[hhh]=gamma
      # Check convergence of T1 should by NNN
    }
    #(TODO) - this goes to function
    for(i in (number_of_inner_loops + 1):(number_of_inner_loops * 5 * n))
    {
      x[i]=x[i-1]+beta*sqrt(y[i-1])*max(0,K-x[i-1])*del
      
      vw[i]=vw[i-1]+gamma*y[i-1]*del
      
      y[i]=max(0,x[i]-vw[i])
    }
    for(i in 1:n){
      T_final[i,1]=sum(x<=X[i])
      T_final[i,2]=sum(vw<=VW[i])
    }
    T_final=T_final*del
    TT=diff(T_final)-1
    
    COV=t(TT)%*%TT/(n-1)
    # The solution deviratives - Jacobi matrix
    AD=beta*sqrt(Y_middle)*max(K-X_middle,0)
    BD=gamma*Y_middle
    
    OBJ1=sum(log(AD)+log(BD)) #single figure - denom
    OBJ[hhh]=OBJ1+(n/2)*log(det(COV)) # likelihood when we use to BM's
    LOGL[hhh]=log(det(COV))
    VAR1[hhh]=mean(diag(COV))
    OBJB[hhh]=sum(log(AD))+(n/2)*log(COV[1,1]) # BM for single component for X only
  }
  
}
