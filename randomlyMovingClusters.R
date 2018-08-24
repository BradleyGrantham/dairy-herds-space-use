# this script takes a cluster window, and randomly moves it around the field

# need to run readAllData.R first because this script makes use of the clusters

require(spatstat)
require(ggplot2)

clusterprob <- c()
circleprob <- c()

Ls <- c()
Rs <- c()
YN <-c()

for (i in 1:73){
  window <- clusters[[i]]$window
  cluster <- as.owin(ldod_cluster_plots[[i]])
  L <- sqrt(area(window))
  R<- sqrt(area(cluster)/pi)
  boundaryDist <- bdist.points(clusters[[i]])
  
  if (min(boundaryDist) < 5){
    YN <- c(YN, 'Y')
  } else {
    YN <- c(YN, 'N')
  }
  Rs <- c(Rs, R)
  Ls <- c(Ls, L)
  
  flag <- 0
  
  for (n in 1:100){
    cluster.centre <- centroid.owin(cluster, as.ppp = TRUE)
    cluster.centre <- c(cluster.centre$x, cluster.centre$y)
    cluster.newcentre <- runifpoint(1, win = window)
    cluster.newcentre <- c(cluster.newcentre$x, cluster.newcentre$y)
    shift.dist <- cluster.newcentre - cluster.centre
    cluster <- shift(cluster, shift.dist)
    cluster <- rotate.owin(cluster, angle = runif(1, 0, 2*pi), centre = "centroid")
    # boundaryDist <- ppp(cluster.newcentre[[1]], cluster.newcentre[[2]], window = window)
    # boundaryDist <- bdist.points(boundaryDist)
    # Ls <- c(L, boundaryDist)
    # trialcircle <- disc(radius = R, centre = runifpoint(1, win = window))
    flagarea <- overlap.owin(window, cluster)
    if (signif(flagarea,3) != signif(area(cluster),3)){
      flag <- flag + 1
    }

    # plot(window)
    # plot(cluster, add = TRUE)
    #
    # Sys.sleep(5)

  }
  clusterprob <- c(clusterprob, flag/100)
  circleprob <- c(circleprob, 4*R/L - (4*R*R)/(L*L))
}