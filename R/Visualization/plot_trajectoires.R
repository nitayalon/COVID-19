plotTrajectories <- function(empirical_nation_data,
                             transformed_nation_data, 
                             trajectories,
                             dates, 
                             last_date, 
                             ylim = c(0,25),
                             scale_factor = 1e5,
                             starting_day = 60,
                             empirical_day_start = 30,
                             last_fitting_date = as.Date('07/06/2020', format = '%d/%m/%y'),
                             last_data_date = as.Date('07/06/2020', format = '%d/%m/%y'),
                             current_data = NULL, 
                             current_date = NULL, 
                             plot_maximal_infection = F,
                             plot_maximal_empiric_infection = T){
  
  starting_day <- dates[starting_day]
  emprical_starting_day <- dates[empirical_day_start]
  days <- seq(starting_day, last_date, 1)

  x_mle_upper <- trajectories[[3]]$x
  y_mle_upper <- trajectories[[3]]$y
  # vw_mle_upper <- trajectories[[3]]$vw
  
  x_mle_mid <- trajectories[[2]]$x
  y_mle_mid <- trajectories[[2]]$y
  vw_mle_mid <- trajectories[[2]]$vw
  
  
  x_mle_lower <- trajectories[[1]]$x
  y_mle_lower <- trajectories[[1]]$y
  # vw_mle_lower <- trajectories[[1]]$vw
  
  # max_infection_upper <- max(y_mle_upper[(1:length(x_mle_upper)) %% 100 == 0][1:length(days)] / scale_factor)
  # max_infection_mid <- max(y_mle_mid[(1:length(x_mle_mid)) %% 100 == 0][1:length(days)] / scale_factor)
  # max_infection_lower <- max(y_mle_lower[(1:length(x_mle_lower)) %% 100 == 0][1:length(days)] / scale_factor)
  # max_infection_actual <- max(empirical_nation_data$Y / scale_factor)
  # 
  # max_infection_upper_index <- which.max(y_mle_upper[(1:length(x_mle_upper)) %% 100 == 0][1:length(days)] / scale_factor)
  # max_infection_mid_index <- which.max(y_mle_mid[(1:length(x_mle_mid)) %% 100 == 0][1:length(days)] / scale_factor)
  # max_infection_lower_index <- which.max(y_mle_lower[(1:length(x_mle_lower)) %% 100 == 0][1:length(days)] / scale_factor)
  # max_infection_actual_index <- which.max(empirical_nation_data$Y / scale_factor)
  
  plot(days, 
       x_mle_upper[(1:length(x_mle_upper)) %% 100 == 0][1:length(days)] / scale_factor, col = 'orange' , type = 'b', pch=7, panel.first = grid(),
       ylab = sprintf("Number of cases * %s",scale_factor), xlab = 'Days', 
       xlim = c(emprical_starting_day, last_date),
       ylim = ylim,
       lty = 1, 
       lwd = 2,
       cex = 0.5,
       cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
  # lines(days, vw_mle_upper[(1:length(x_mle_upper)) %% 100 == 0][1:length(days)] / scale_factor,col = 'orange', type = 'b', pch=13, lwd = 2, cex = 0.5)
  lines(days, y_mle_upper[(1:length(x_mle_upper)) %% 100 == 0][1:length(days)] / scale_factor, col = 'orange', type = 'b', pch=19, lwd = 2, cex = 0.5)
  
  if(plot_maximal_infection){
    important_dates <- c(min(days),
                         last_fitting_date, 
                         days[max_infection_upper_index],
                         days[max_infection_mid_index], days[max_infection_lower_index],
                         max(days))
  }
  else{
    important_dates <- c(min(days),
                         starting_day,
                         last_fitting_date, 
                         max(days))
  }
  
  labels <- format(important_dates, "%b %d")
  text(important_dates, par("usr")[3], labels=labels, srt=315,
       xpd=TRUE, adj=c(-0.2,1.2), cex=0.9)
  
  empirical_x = empirical_nation_data$X
  empirical_vw = empirical_nation_data$VW
  empirical_y = empirical_nation_data$Y
  
  # lines(seq(emprical_starting_day,last_data_date - 1, 'day'), empirical_x[empirical_day_start:length(empirical_x)] / scale_factor, col = 'blue', type = 'b', pch=7, lwd = 1, cex = 0.5)
  # lines(seq(emprical_starting_day,last_data_date - 1, 'day'), empirical_vw[empirical_day_start:length(empirical_x)] / scale_factor, col = 'blue', type = 'b', pch=13, lwd = 1, cex = 0.5)
  # lines(seq(emprical_starting_day,last_data_date - 1, 'day'), empirical_y[empirical_day_start:length(empirical_y)] / scale_factor, col = 'blue', type = 'b', pch=19, lwd = 1, cex = 0.5)
  lines(seq(emprical_starting_day,last_data_date - 1, 'day'), transformed_nation_data$X[empirical_day_start:length(transformed_nation_data$X)] / scale_factor, col = 'blue4', type = 'b', pch=7, lwd = 1, cex = 0.5)
  lines(seq(emprical_starting_day,last_data_date - 1, 'day'), transformed_nation_data$VW[empirical_day_start:length(transformed_nation_data$X)] / scale_factor, col = 'blue4', type = 'b', pch=13, lwd = 1, cex = 0.5)
  lines(seq(emprical_starting_day,last_data_date - 1, 'day'), transformed_nation_data$Y[empirical_day_start:length(transformed_nation_data$X)] / scale_factor, col = 'blue4', type = 'b', pch=19, lwd = 1, cex = 0.5)
   
  lines(days,x_mle_mid[(1:length(x_mle_mid)) %% 100 == 0][1:length(days)] / scale_factor, col = 'red', type = 'b', pch=7, lwd = 1, cex = 0.5)
  lines(days,vw_mle_mid[(1:length(vw_mle_mid)) %% 100 == 0][1:length(days)]/ scale_factor, col = 'red', type = 'b', pch=13, lwd = 1, cex = 0.5)
  lines(days,y_mle_mid[(1:length(y_mle_mid)) %% 100 == 0][1:length(days)] / scale_factor, col = 'red', type = 'b', pch=19, lwd = 1, cex = 0.5)
  
  lines(days,x_mle_lower[(1:length(x_mle_lower)) %% 100 == 0][1:length(days)] / scale_factor, col = 'green', type = 'b', pch=7, lwd = 1, cex = 0.5)
  # lines(days,vw_mle_lower[(1:length(vw_mle_lower)) %% 100 == 0][1:length(days)]/ scale_factor, col = 'green', type = 'b', pch=13, lwd = 1, cex = 0.5)
  lines(days,y_mle_lower[(1:length(y_mle_lower)) %% 100 == 0][1:length(days)] / scale_factor, col = 'green', type = 'b', pch=19, lwd = 1, cex = 0.5)
  
  points(current_date, current_data, col = 'black', pch = 3, cex = 1.5)
  # segments(x0 = 0,
  #          y0 = max_infection_upper,
  #          x1 = days[max_infection_upper_index],
  #          y1 = max_infection_upper,
  #          col = 'orange',
  #          lty = 1,
  #          lwd = 3)
  # if(plot_maximal_empiric_infection){
  # segments(x0 = 0,
  #          y0 = max_infection_actual,
  #          x1 = days[max_infection_actual_index],
  #          y1 = max_infection_actual,
  #          col = 'blue',
  #          lty = 1,
  #          lwd = 3)
  # }
  # segments(x0 = 0,
  #          y0 = max_infection_mid, 
  #          x1 = days[max_infection_mid_index],
  #          y1 = max_infection_mid,
  #          col = 'red',
  #          lty = 1,
  #          lwd = 3)
  # segments(x0 = 0,
  #          y0 = max_infection_lower, 
  #          x1 = days[max_infection_lower_index],
  #          y1 = max_infection_lower,
  #          col = 'green',
  #          lty = 1,
  #          lwd = 3)
  segments(x0 = last_fitting_date,
           y0 = 0, 
           x1 = last_fitting_date,
           y1 = ylim[2],
           col = 'black',
           lty = 2,
           lwd = 0.5)
  segments(x0 = starting_day,
           y0 = 0, 
           x1 = starting_day,
           y1 = ylim[2],
           col = 'black',
           lty = 2,
           lwd = 0.5)
}
