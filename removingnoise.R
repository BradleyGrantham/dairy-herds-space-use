#THIS NEEDS testnewfieldedges.R to work properly. testnewfieldedges.R sets up all the
#point patterns, ldod plots and everything else
#this gets the largest clusters

postcode <- c()
clusters <- vector(mode="list", length = 73) #this length was set by observation
z <- 0 
x <- 19
x1 <- 40000
r <- 5
for (i in 1:length(filenames)){
  conn <- connected(point_patterns[[i]], 2*r)
  conntab <- table(conn$marks)
  for (j in 1:length(conntab)){
    if (conntab[[j]] > x && conntab[[j]] < x1){
      z = z + 1
      jstr <- as.character(j)
      clusters[[z]] <- subset(conn, marks == jstr, drop = TRUE)
      postcode <- c(postcode, filenames[[i]])
    }
  }
}
  
mostdense <- vector(mode="list", length = length(clusters))
for (i in 1:length(clusters)){
  mostdense[[i]] <- as.data.frame(nndensity(clusters[[i]], k = length(clusters[[i]]$x), verbose = FALSE)) #can change k in this... currently set to sqrt(numberofcows)
  mostdense[[i]] <- mostdense[[i]][which.max(mostdense[[i]]$value),]
  mostdense[[i]] <- ppp(mostdense[[i]]$x, mostdense[[i]]$y, window = clusters[[i]]$window)
}
  
coeff <- c()
rsquared <- c()
pvalue <- c()

ldod_cluster_plots <- vector(mode = "list", length = length(clusters))
for (i in 1:length(clusters)){
  
  actualdensity<- vector(mode="list", length = length(clusters))
  distances <- c()
  bdistances <- c()
  nnk1 <- c()
  nnk2 <- c()
  ldod.area <- c()
  
    distances <- c(distances, as.vector(crossdist(clusters[[i]], mostdense[[i]])))
    actualdensity[[i]] <- nndensity(clusters[[i]], k = length(clusters[[i]]$x), verbose = FALSE)
    nnk1 <- c(nnk1, nndist(clusters[[i]], k = 1))
    nnk2 <- c(nnk2, nndist(clusters[[i]], k = 2))
    spaceuse <- as.owin((discs(clusters[[i]], radii=5, trim=FALSE)))
    voronois <- (dirichlet(clusters[[i]]))
    ldod_plot <- intersect.tess(voronois, spaceuse)
    ldod_cluster_plots[[i]] <- ldod_plot
    theareas <- c()
    bdistances <- c(bdistances, bdist.points(clusters[[i]]))
    ldod.area <- c(ldod.area, theareas)
  
  
  # dfcluster <- data.frame(DensDist = distances, Area = ldod.area, NNDist = nndist(clusters[[i]]))
  # dfcluster$NNDist <- dfcluster$NNDist/10
  # model <- lm(Area ~ DensDist, data = dfcluster)
  # 
  # model <- summary(model)
  # plot(ggplot(data = dfcluster, mapping = aes(x = DensDist, y = Area)) + geom_point(aes(colour = 'Area')) + geom_smooth(method = lm, se = FALSE, aes(x = DensDist, y = Area, colour = 'Area')) + geom_point(data = dfcluster, mapping = aes(x = DensDist, y = NNDist, colour = 'NNDist')) + geom_smooth(method = lm, se = FALSE, aes(x = DensDist, y = NNDist, colour = 'NNDist')) + xlab('Distance from Densest Point') + ylab('Distance measure') + ggtitle(i))
  # rsquared <-  c(rsquared, model$r.squared)
  # numberofclusters <- z
  # pvalue <- c(pvalue, model$coefficients[2,4])
  # coeff <- c(coeff, model$coefficients[2,1])
  
  
  
}

#ggplot(data = df[[1]], mapping = aes(x = ClusterSize, y = Coeffs)) + geom_boxplot() + geom_boxplot(data = df[[2]], mapping = aes(x = ClusterSize, y = Coeffs)) + geom_boxplot(data = df[[3]], mapping = aes(x = ClusterSize, y = Coeffs)) + geom_boxplot(data = df[[4]], mapping = aes(x = ClusterSize, y = Coeffs)) + geom_boxplot(data = df[[5]], mapping = aes(x = ClusterSize, y = Coeffs)) + geom_boxplot(data = df[[6]], mapping = aes(x = ClusterSize, y = Coeffs)) + geom_boxplot(data = df[[7]], mapping = aes(x = ClusterSize, y = Coeffs)) + geom_boxplot(data = df[[8]], mapping = aes(x = ClusterSize, y = Coeffs)) + xlab("Cluster size") + ylab("Coefficient") + ggtitle("Coefficients vs. Cluster size")