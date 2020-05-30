plotTrajectories <- function(nation_data, 
                             trajectories,
                             dates, 
                             last_date, 
                             nation_name, 
                             ylim = c(0,25),
                             current_data = NULL, current_date = NULL, plot_maximal_infection = T){
  
  starting_day <- dates[60]
  days <- seq(starting_day, last_date, 1)

  x_mle_upper <- trajectories[[3]]$x
  y_mle_upper <- trajectories[[3]]$y
  vw_mle_upper <- trajectories[[3]]$vw
  
  x_mle_mid <- trajectories[[2]]$x
  y_mle_mid <- trajectories[[2]]$y
  vw_mle_mid <- trajectories[[2]]$vw
  
  
  x_mle_lower <- trajectories[[1]]$x
  y_mle_lower <- trajectories[[1]]$y
  vw_mle_lower <- trajectories[[1]]$vw
  
  max_infection_upper <- max(y_mle_upper[(1:length(x_mle_upper)) %% 100 == 0][1:length(days)] / 1e5)
  max_infection_mid <- max(y_mle_mid[(1:length(x_mle_mid)) %% 100 == 0][1:length(days)] / 1e5)
  max_infection_lower <- max(y_mle_lower[(1:length(x_mle_lower)) %% 100 == 0][1:length(days)] / 1e5)
  max_infection_actual <- max(nation_data$Y / 1e5)
  
  max_infection_upper_index <- which.max(y_mle_upper[(1:length(x_mle_upper)) %% 100 == 0][1:length(days)] / 1e5)
  max_infection_mid_index <- which.max(y_mle_mid[(1:length(x_mle_mid)) %% 100 == 0][1:length(days)] / 1e5)
  max_infection_lower_index <- which.max(y_mle_lower[(1:length(x_mle_lower)) %% 100 == 0][1:length(days)] / 1e5)
  max_infection_actual_index <- which.max(nation_data$Y / 1e5)
  
  plot(days, 
       x_mle_upper[(1:length(x_mle_upper)) %% 100 == 0][1:length(days)] / 1e5, col = 'orange' , type = 'b', pch=7, panel.first = grid(),
       ylab = "Number of cases * 10^5", xlab = 'Days', 
       ylim = ylim,
       lty = 1, lwd = 2,
       main = sprintf("Covid19 data %s",nation_name),
       cex = 0.5)
  lines(days, vw_mle_upper[(1:length(x_mle_upper)) %% 100 == 0][1:length(days)] / 1e5,col = 'orange', type = 'b', pch=13, lwd = 2, cex = 0.5)
  lines(days, y_mle_upper[(1:length(x_mle_upper)) %% 100 == 0][1:length(days)] / 1e5, col = 'orange', type = 'b', pch=19, lwd = 2, cex = 0.5)
  
  if(plot_maximal_infection){
    important_dates <- c(min(days),min(days) + length(nation_data$X), 
                         days[max_infection_upper_index],
                         days[max_infection_mid_index], days[max_infection_lower_index],
                         max(days))
  }
  else{
    important_dates <- c(min(days),min(days) + length(nation_data$X), 
                         max(days))
  }
  
  labels <- format(important_dates, "%b %d")
  text(important_dates, par("usr")[3], labels=labels, srt=315,
       xpd=TRUE, adj=c(-0.2,1.2), cex=0.9)
  lines(seq(min(days),min(days) + (length(nation_data$X) - 1), 'day'), nation_data$X / 1e5, col = 'blue', type = 'b', pch=7, lwd = 1, cex = 0.5)
  lines(seq(min(days),min(days) + (length(nation_data$X) - 1), 'day'), nation_data$VW / 1e5, col = 'blue', type = 'b', pch=13, lwd = 1, cex = 0.5)
  lines(seq(min(days),min(days) + (length(nation_data$X) - 1), 'day'), nation_data$Y / 1e5, col = 'blue', type = 'b', pch=19, lwd = 1, cex = 0.5)
  
  lines(days,x_mle_mid[(1:length(x_mle_mid)) %% 100 == 0][1:length(days)] / 1e5, col = 'red', type = 'b', pch=7, lwd = 1, cex = 0.5)
  lines(days,vw_mle_mid[(1:length(vw_mle_mid)) %% 100 == 0][1:length(days)]/ 1e5, col = 'red', type = 'b', pch=13, lwd = 1, cex = 0.5)
  lines(days,y_mle_mid[(1:length(y_mle_mid)) %% 100 == 0][1:length(days)] / 1e5, col = 'red', type = 'b', pch=19, lwd = 1, cex = 0.5)
  
  
  
  lines(days,x_mle_lower[(1:length(x_mle_lower)) %% 100 == 0][1:length(days)] / 1e5, col = 'green', type = 'b', pch=7, lwd = 1, cex = 0.5)
  lines(days,vw_mle_lower[(1:length(vw_mle_lower)) %% 100 == 0][1:length(days)]/ 1e5, col = 'green', type = 'b', pch=13, lwd = 1, cex = 0.5)
  lines(days,y_mle_lower[(1:length(y_mle_lower)) %% 100 == 0][1:length(days)] / 1e5, col = 'green', type = 'b', pch=19, lwd = 1, cex = 0.5)
  
  points(current_date, current_data, col = 'black', pch = 3, cex = 1.5)
  segments(x0 = 0,
           y0 = max_infection_upper,
           x1 = days[max_infection_upper_index],
           y1 = max_infection_upper,
           col = 'orange',
           lty = 1.5,
           lwd = 3)
  segments(x0 = 0,
           y0 = max_infection_actual,
           x1 = days[max_infection_actual_index],
           y1 = max_infection_actual,
           col = 'blue',
           lty = 1.5,
           lwd = 3)
  segments(x0 = 0,
           y0 = max_infection_mid, 
           x1 = days[max_infection_mid_index],
           y1 = max_infection_mid,
           col = 'red',
           lty = 1.5,
           lwd = 3)
  segments(x0 = 0,
           y0 = max_infection_lower, 
           x1 = days[max_infection_lower_index],
           y1 = max_infection_lower,
           col = 'green',
           lty = 1.5,
           lwd = 3)
  segments(x0 = min(days) + length(nation_data$X),
           y0 = 0, 
           x1 = min(days) + length(nation_data$X),
           y1 = ylim[2],
           col = 'black',
           lty = 1.5,
           lwd = 3)
}
