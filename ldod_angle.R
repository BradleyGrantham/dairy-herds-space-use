data <- data.frame(X = c(1.0, 1.5, 8.0), Y=c(4.0, 1.0, 1.25))
pattern_plot <- ppp(data$X, data$Y, window = owin(xrange = c(-12.0, 12.0), yrange= c(-12.0, 12.0)))
circular_windows <- discs(pattern_plot, radii = 5, separate=TRUE, mask = FALSE, delta = 0.05)

plot(pattern_plot, main = "", xlim = c(pattern_plot$window$xrange), ylim = c(pattern_plot$window$yrange), cex = 1)

for (the_cow in 1:1){
  
  cow_window <- circular_windows[[the_cow]]
  cow_boundary <- as.data.frame.owin(cow_window)
  cow_centre <- centroid.owin(cow_window)
  cow_ldod <- cow_boundary
  
  for (other_cow in 1:length(circular_windows)){
    
    if(other_cow != the_cow){
      print(the_cow)
      print(other_cow)
      intersection_win <- intersect.owin(circular_windows[[the_cow]], circular_windows[[other_cow]])
      intersection <- as.data.frame.owin(intersection_win)
      intersection_centre <- centroid.owin(intersection_win)
      intersection_win <- affine.owin(intersection_win, mat = diag(c(1.05, 1.05)))
      new_centre <- centroid.owin(intersection_win)
      intersection_win <- affine.owin(intersection_win, mat = diag(c(1, 1)), vec = c(intersection_centre$x - new_centre$x, intersection_centre$y - new_centre$y))
      
      #plot(intersection_win, add =TRUE)
      #Sys.sleep(10)
      max_dist <- 0
      
      #this loop finds the end points of the bisector
      for (i in 1:(length(intersection[[1]]))){
        for (j in 1:(length(intersection[[1]]))){
          if (i!=j){
            dist <- sqrt((intersection[[1]][i]-intersection[[1]][j])^2 + (intersection[[2]][i]-intersection[[2]][j])^2)
            if (dist > max_dist){
              max_dist <- dist
              min_point = c(intersection[[1]][i], intersection[[2]][i])
              max_point = c(intersection[[1]][j], intersection[[2]][j])
            }
          }
        }  
      }
      
      #this loop removes points outside of the new window. new window includes bisector
      
      # for (i in 1:(length(cow_boundary[[1]]))){
      #   if (inside.owin(cow_boundary[[1]][i], cow_boundary[[2]][i], intersection_win)){
      #     count <- count + 1
      #     remove_rows[count] <- i
      #     #cat(cow_boundary[[1]][i], cow_boundary[[2]][i], "\n")
      #   }
      # }
      # 
      # remove_rows <- sort(remove_rows, decreasing = TRUE)
      cow_ldod <- rbind(cow_ldod, max_point, min_point)
      extra_points <- pointsOnLines(psp(min_point[1], min_point[2], max_point[1], max_point[2], window = intersection_win), np = 4)
      
      for (i in 1:(length(extra_points$x))){
        cow_ldod <- rbind(cow_ldod, c(extra_points$x[1], extra_points$y[1]))
      }
      #calculates polar angle of each coordinate
      cow_ldod$angle <- 0
      for (i in 1:(length(cow_ldod[[1]]))){
        cow_ldod$angle[i] <- atan2(cow_ldod$y[i]-cow_centre$y, cow_ldod$x[i]-cow_centre$x)
      }
      
      cow_ldod <- cow_ldod[with(cow_ldod, order(angle)), ]
      cow_ldod$angle <- NULL
      cow_window <- owin(poly = list(x = cow_ldod[[1]], y = cow_ldod[[2]]))
      plot(cow_window, add = TRUE)
      cow_boundary <- cow_ldod
      
      
      
    }
  }
  plot(cow_window, main = "", xlim = c(pattern_plot$window$xrange), ylim = c(pattern_plot$window$yrange), add = TRUE)
  #Sys.sleep(10)
}