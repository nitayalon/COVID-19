plotTransoformedData <- function(nation_data,
                              dates, 
                              last_date = NULL, 
                              ylim = c(0,25),
                              scale_factor = 1e5,
                              starting_day = 50){
  if(is.null(last_date)){
    n = length(nation_data$X)
  }
  else{
    n = which(dates == last_date)
  }
  
  empirical_x = nation_data$X[starting_day:n]
  empirical_vw = nation_data$VW[starting_day:n]
  empirical_y = nation_data$Y[starting_day:n]
  transformed_y = nation_data$YY[starting_day:n]
  transformed_vw = nation_data$VWY[starting_day:n]
  
  days = dates[starting_day:n]
  plot(days, empirical_x / scale_factor, col = 'blue', type = 'b', pch=7, lwd = 1, 
       ylab = sprintf("Population / %s", scale_factor),
       ylim = ylim,
       lty = 1, 
       cex = 0.5,
       cex.lab=1.5, 
       cex.axis=1.5, 
       cex.main=1.5, 
       cex.sub=1.5)
  grid()
  lines(days, empirical_y / scale_factor, col = 'blue', type = 'b', pch=13, lwd = 1, cex = 0.5)
  lines(days, empirical_vw / scale_factor, col = 'blue', type = 'b', pch=19, lwd = 1, cex = 0.5)
  lines(days, transformed_y / scale_factor, col = 'green', type = 'b', pch=13, lwd = 1, cex = 0.5)
  lines(days, transformed_vw / scale_factor, col = 'green', type = 'b', pch=19, lwd = 1, cex = 0.5)
  
  
  
  # points(current_date, current_data, col = 'black', pch = 3, cex = 1.5)
  # 
  # segments(x0 = last_fitting_date,
  #          y0 = 0, 
  #          x1 = last_fitting_date,
  #          y1 = ylim[2],
  #          col = 'black',
  #          lty = 2,
  #          lwd = 0.5)
  # segments(x0 = starting_day,
  #          y0 = 0, 
  #          x1 = starting_day,
  #          y1 = ylim[2],
  #          col = 'black',
  #          lty = 2,
  #          lwd = 0.5)
}
