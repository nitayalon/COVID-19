split_dead_and_recovered_data <- function(v,w,bb1,MMM,n) {
  v[1]=vw[1]/2
  w[1]=v[1]
  for (i in 2:5*MMM*n){
    v[i]=v[i-1]+(vw[i]-vw[i-1])/(1+exp(bb1*(i/100+AA-t1)))
    w[i]=w[i-1]+(vw[i]-vw[i-1])*(1-1/(1+exp(bb1*(i/100+AA-t1))))
  }
  return((list(v = v,
               w = w)))
}