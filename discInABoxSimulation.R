# simulate a circular disc in a box to see how often this touches the edge
# the results can then be compared against the same simulation but for
# real clusters

trialwindow <- owin(xrange=c(0,L), yrange=c(0,L))
flag1 <- 0
for (i in 1:1000){
  trialcircle <- disc(radius = R, centre = runifpoint(1, win = trialwindow))
  flagarea <- overlap.owin(trialwindow, trialcircle)
  if (signif(flagarea,3) != signif(area(trialcircle),3)){
    flag1 <- flag1 + 1
  }
  
  # plot(trialwindow)
  # plot(trialcircle, add = TRUE)
  # 
  # Sys.sleep(5)
}