require(XLConnect)
require(spatstat)
require(ggplot2)

data <- data.frame(X = c(1.0, 5.0, 3.0), Y=c(4.0, 2.0, 6.0))
pattern_plot <- ppp(data$X, data$Y, window = owin(xrange = c(-12.0, 12.0), yrange= c(-12.0, 12.0)))

ldod <- function(X){
  stopifnot(is.ppp(X))
  
  circular_windows <- discs(X, radii = 5, separate=TRUE, mask = FALSE, delta = 1)
  
  for (the_cow in 1:length(circular_windows)){
    
    cow_window <- circular_windows[[the_cow]]
    cow_boundary <- as.data.frame.owin(cow_window)
    
    for (other_cow in 1:length(circular_windows)){
      if(other_cow != the_cow){
        
        intersection_win <- intersect.owin(cow_window, circular_windows[[other_cow]])
        intersection_centre <- centroid.owin(intersection_win)
        intersection_win <- rescale.owin(intersection_win, 0.9)
        intersection_win <- shift.owin(intersection_win, vec= c(-0.4, -0.6))
        intersection <- as.data.frame.owin(intersect.owin(cow_window, circular_windows[[other_cow]]))
        
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
        remove_rows <- vector()
        cow_ldod <- cow_boundary
        count <- 0
        
        for (i in 1:(length(cow_boundary[[1]]))){
          if (inside.owin(cow_boundary[[1]][i], cow_boundary[[2]][i], intersection_win) == TRUE){
            count <- count + 1
            remove_rows[count] <- i
            #cat(cow_boundary[[1]][i], cow_boundary[[2]][i], "\n")
          }
        }
        
        remove_rows <- sort(remove_rows, decreasing = TRUE)
        cow_ldod <- cow_ldod[-remove_rows, ]
        cow_ldod <- rbind(cow_ldod, max_point, min_point)
        cow_ldod$angle <- 0
        
        #calculates polar angle of each coordinate
        for (i in 1:(length(cow_ldod[[1]]))){
          cow_ldod$angle[i] <- atan2(cow_ldod$y[i], cow_ldod$x[i])
        }
        
        cow_ldod <- cow_ldod[with(cow_ldod, order(angle)), ]
        cow_ldod$angle <- NULL
        cow_window <- owin(poly = list(x = cow_ldod[[1]], y = cow_ldod[[2]]))
        cow_window_data <- as.data.frame.owin(cow_window)
        
        plot(win, main = "", xlim = c(X$window$xrange), ylim = c(X$window$yrange), add=TRUE)
        return(win)
      }
    }
  }
}

areas2 <- ldod(pattern_plot)

