# randomly move the clusters and check to see if they touch the edge

require(spatstat)
require(ggplot2)
flag <- 1
window <- clusters[[3]]$window
cluster <- as.owin(ldod_cluster_plots[[3]])

for (i in 1:4999){
  cluster.centre <- centroid.owin(cluster, as.ppp = TRUE)
  cluster.centre <- c(cluster.centre$x, cluster.centre$y)
  cluster.newcentre <- runifpoint(1, win = window)
  cluster.newcentre <- c(cluster.newcentre$x, cluster.newcentre$y)
  shift.dist <- cluster.newcentre - cluster.centre
  cluster <- shift(cluster, shift.dist)
  flagarea <- overlap.owin(window, cluster)
  if (signif(flagarea,3) != signif(area.owin(cluster),3)){
    flag <- flag + 1
  }
  
  # plot(window)
  # plot(cluster, add = TRUE)
  # 
  # Sys.sleep(5)

}