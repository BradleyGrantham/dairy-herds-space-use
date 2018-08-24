# quadrat test, this is only being done with one field in this script

require(XLConnect)
require(spatstat)
require(ggplot2)

#read in file
filename <- "BA10_0ND"
file <- readWorksheetFromFile(paste(filename, ".xlsx", sep=""), sheet=1)

#take first two colums of data and put into a data frame
file[,1:2]
file <- data.frame(file[,1:2])

no_of_cows <- length(file$X)

#set plots to be square
#par(pty="s")

#plot cow points
#plot(file$X, file$Y, xlim=c(55, 255), ylim=c(55, 255), type="p", main=filename, xlab = "x", ylab="y", cex = 1, pty='s')

#assign cow distance values to matrix
matr <- matrix(NA, nrow=length(file$X), ncol=length(file$Y))
for (i in 1:(length(file$X)-1)){
  for (j in (i+1):length(file$X)){
    xdist <- abs(file$X[i] - file$X[j])
    ydist <- abs(file$Y[i] - file$Y[j])
    seperation <- sqrt(xdist^2 + ydist^2)
    matr[i,j] <- seperation
    matr[j,i] <- seperation
  }  
}

#create pattern plot to use with spatstat package
ba10pattern <- ppp(file$X, file$Y, window = bounding.box.xy(file$X, file$Y))

#set up poisson point pattern and get average number of clusters for cluster size
no_of_clusters_pois <- vector()
max_clusters <- as.vector(matrix(0,nrow=64))
min_clusters <- as.vector(matrix(100,nrow=64))

for (i in 1:100){
  poisson_point <- runifpoint(69, win = bounding.box.xy(file$X, file$Y))
  rand_nn <- rknn(69, k=1, d=2, lambda = 69/area.owin(bounding.box.xy(file$X, file$Y)))
  
  for (j in 1:64){
    clusters_pois <- connected.ppp(poisson_point, j/2)
    no_of_clusters_pois[i] <- no_of_clusters_pois[i] + length(levels(clusters_pois$marks))
    if (length(levels(clusters_pois$marks)) > max_clusters[j]) {
      max_clusters[j] <- length(levels(clusters_pois$marks))
    }
    if (length(levels(clusters_pois$marks)) < min_clusters[j]) {
      min_clusters[j] <- length(levels(clusters_pois$marks))
    }
  }
}

for (i in 1:length(no_of_clusters_pois)){
  no_of_clusters_pois[i] = no_of_clusters_pois[i]/length(no_of_clusters_pois)
}

#read in file
filename <- "BA10_0ND"
file <- readWorksheetFromFile(paste(filename, ".xlsx", sep=""), sheet=1)

#take first two colums of data and put into a data frame
file[,1:2]
file <- data.frame(file[,1:2])

no_of_cows <- length(file$X)

#plot cow points
#plot(file$X, file$Y, xlim=c(55, 255), ylim=c(55, 255), type="p", main=filename, xlab = "x", ylab="y", cex = 1, pty='s')

#assign cow distance values to matrix
matr <- matrix(NA, nrow=length(file$X), ncol=length(file$Y))
for (i in 1:(length(file$X)-1)){
  for (j in (i+1):length(file$X)){
    xdist <- abs(file$X[i] - file$X[j])
    ydist <- abs(file$Y[i] - file$Y[j])
    seperation <- sqrt(xdist^2 + ydist^2)
    matr[i,j] <- seperation
    matr[j,i] <- seperation
  }  
}

#create pattern plot to use with spatstat package
ba10pattern <- ppp(file$X, file$Y, window = bounding.box.xy(file$X, file$Y))

#split up into quadrants and plot them
quadrat_split <- quadrats(ba10pattern)

#split pattern plot into a list of pattern plots defined by quadrats
quadrat_split <- split.ppp(ba10pattern, quadrat_split)

#plot the quadrats and the cows
plot.splitppp(quadrat_split, main.panel = "", mar.panel = 0, main = "Quadrat Plot")

#identify the clusters
cluster_size <- vector()
no_of_clusters_ba10 <- vector()
no_of_clusters_pois <- vector()

for (i in 1:64){
  clusters_ba10 <- connected.ppp(ba10pattern, i/2)
  clusters_pois <- connected.ppp(poisson_point, i/2)
  no_of_clusters_ba10[i] <- length(levels(clusters_ba10$marks))
  no_of_clusters_pois[i] <- length(levels(clusters_pois$marks))
  cluster_size[i] <- i/2
}

#plot the clusters
#plot(clusters_ba10, main = "Clusters")

#put data into data frame
df <- data.frame(Cluster = (cluster_size), MyNumber = (no_of_clusters_ba10), PoisNumber = (no_of_clusters_pois), Max = (max_clusters), Min = (min_clusters))

#plot of standard poisson and our data - radius vs number of clusters
myplot <- ggplot(df, aes(x = Cluster, y = MyNumber, colour="BA10")) + geom_smooth(se=FALSE, span = 0.4) + geom_smooth(data = df, aes(y = PoisNumber, colour="Poisson", se=FALSE, span = 0.4)) 
myplot <- myplot + labs(title = "Radius of Cows vs Number of Clusters", x = ("Radius"), y = ("Number of Clusters"))
myplot <- myplot + annotate("text", x = 20, y = 60, label = "Poisson run \n100 times")
myplot <- myplot + geom_errorbar(data = df, aes(ymax = Max, ymin = Min), colour = "red", alpha = 0.2)
myplot <- myplot + scale_colour_manual("", values = c("BA10"="blue", "Poisson"="red"))

print(myplot)

#quadrat poisson test. output is a quadrat plot
quadrattest <- quadrat.test(ba10pattern, nx = 5, ny = 5)
sometext <- paste0("X2: ", trunc(quadrattest$statistic), "\ndf: ", trunc(quadrattest$parameter), "\np-value: ", signif(quadrattest$p.value, 5))
graph <- plot(quadrattest, main = "Top left: Observed  Top right: Expected\n  Bottom: Pearson residual")
graph <- graph + text(-5, 150, labels = sometext)

