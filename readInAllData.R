# this script simply reads in all the data from the relevant directories and
# creates the data types (point patterns, LDOD areas etc.)

require(spatstat)
require(ggplot2)

filenames <- readLines("./Data_with_edges/New fields/Coords/list.txt", ok = TRUE)
Areafilepath <- "./Data_with_edges/New fields/Coords/"
XYfilepath <- "./Data_with_edges/XY coordinates/"

#read in files for XYs of cows
cow_data <- vector(mode="list", length = length(filenames))
for (i in 1:length(filenames)){
  cow_data[[i]] <- read.table(paste(XYfilepath, filenames[[i]], sep=""))
  cow_data[[i]] <- cow_data[[i]][-c(1,2),]
  row.names(cow_data[[i]]) <- NULL
}
names(cow_data) <- filenames

#read in windows
field_windows <- vector(mode="list", length = length(filenames))
for (i in 1:length(filenames)){
  field_windows[[i]] <- read.table(paste(Areafilepath, filenames[[i]], sep=""))
  row.names(field_windows[[i]]) <- NULL
}
names(field_windows) <- filenames

#create point patterns
point_patterns <- vector(mode="list", length = length(filenames))
for (i in 1:length(filenames)){
  point_patterns[[i]] <- ppp(cow_data[[i]]$X, cow_data[[i]]$Y, window = owin(poly = list(x = (field_windows[[i]]$X), y = (field_windows[[i]]$Y))))
}
names(point_patterns) <- filenames

#get filenames
filenames2 <- readLines("./Data_with_edges/list.txt", ok=TRUE)


XYfilepath2 <- "./Data_with_edges/XYforR/"
Areafilepath2 <- "./Data_with_edges/XYareasforR/"

#read in files for XYs of cows
cow_data2 <- vector(mode="list", length = length(filenames2))
for (i in 1:length(filenames2)){
  cow_data2[[i]] <- read.csv(paste(XYfilepath2, filenames2[[i]], ".csv", sep=""))
  row.names(cow_data2[[i]]) <- NULL
}
names(cow_data2) <- filenames2

#read in windows
field_windows2 <- vector(mode="list", length = length(filenames2))
for (i in 1:length(filenames2)){
  field_windows2[[i]] <- read.csv(paste(Areafilepath2, filenames2[[i]], ".csv", sep=""))
  row.names(field_windows2[[i]]) <- NULL
}
names(field_windows2) <- filenames2

#create point patterns
point_patterns2 <- vector(mode="list", length = length(filenames2))
for (i in 1:length(filenames2)){
  point_patterns2[[i]] <- ppp(cow_data2[[i]]$X, cow_data2[[i]]$Y, window = owin(poly = list(x = rev(field_windows2[[i]]$X), y = rev(field_windows2[[i]]$Y))))
}
names(point_patterns2) <- filenames2

filenames <- c(filenames, filenames2)
cow_data <- c(cow_data, cow_data2)
field_windows <- c(field_windows, field_windows2)
point_patterns <- c(point_patterns, point_patterns2)
remove(cow_data2, filenames2, field_windows2, point_patterns2)

#ldod areas
ldod_plots <- vector(mode="list", length = length(filenames))
ldod_areas <- vector(mode="list", length = length(filenames))
for (i in 1:length(filenames)){
  spaceuse <- as.owin((discs(point_patterns[[i]], radii=5, trim=TRUE)))
  voronois <- (dirichlet(point_patterns[[i]]))
  ldod_plots[[i]] <- intersect.tess(voronois, spaceuse)
  
  for (j in 1:length(point_patterns[[i]]$x)){
    ldod_areas[[i]][j] <- area(ldod_plots[[i]][j])
  }
  
}
#plot(ldod_plots$`BA4 6EL XY`, do.labels = TRUE)
names(ldod_plots) <- filenames
names(ldod_areas) <- filenames

#this is edge analysis for all fields
nearest_edges <- vector(mode="list", length = length(filenames))
for (i in 1:length(filenames)){
  nearest_edges[[i]] <- bdist.points(point_patterns[[i]])
}
names(nearest_edges) <- filenames

nearest_neighbours <- vector(mode="list", length = length(filenames))
for (i in 1:length(filenames)){
  nearest_neighbours[[i]] <- nndist(point_patterns[[i]], k = c(1, 2, 3, 4))
}
names(nearest_neighbours) <- filenames

# this part of the script does analysis on certain cluster sizes

clusters <- vector(mode="list", length = 73) #this length was set by observation
z <- 0 # simply a counter
x <- 19 # size of the largest cluster
for (i in 1:length(filenames)){
  conn <- connected(point_patterns[[i]], 10)
  conntab <- table(conn$marks)
  for (j in 1:length(conntab)){
    if (conntab[[j]] > x){
      z = z + 1
      jstr <- as.character(j)
      clusters[[z]] <- subset(conn, marks == jstr, drop = TRUE)
    }
  }
}

mostdense <- vector(mode="list", length = length(clusters))
for (i in 1:length(clusters)){
  mostdense[[i]] <- as.data.frame(nndensity(clusters[[i]], k = length(clusters[[i]]$x), verbose = FALSE)) #can change k in this... currently set to sqrt(numberofcows)
  mostdense[[i]] <- mostdense[[i]][which.max(mostdense[[i]]$value),]
  mostdense[[i]] <- ppp(mostdense[[i]]$x, mostdense[[i]]$y, window = clusters[[i]]$window)
}

# analysis on the density of points in the pattern by both the built in
# density function and also using the first and second nearest neighbours

actualdensity<- vector(mode="list", length = length(clusters))
distances <- c()
bdistances <- c()
nnk1 <- c()
nnk2 <- c()
ldod.area <- c()
for (i in 1:length(clusters)){
  distances <- c(distances, as.vector(crossdist(clusters[[i]], mostdense[[i]])))
  actualdensity[[i]] <- nndensity(clusters[[i]], k = length(clusters[[i]]$x), verbose = FALSE)
  nnk1 <- c(nnk1, nndist(clusters[[i]], k = 1))
  nnk2 <- c(nnk2, nndist(clusters[[i]], k = 2))
  spaceuse <- as.owin((discs(clusters[[i]], radii=5, trim=TRUE)))
  voronois <- (dirichlet(clusters[[i]]))
  ldod_plot <- intersect.tess(voronois, spaceuse)
  theareas <- c()
  # bdistances <- c(bdistances, bdist.points(clusters[[i]]))
  for (j in 1:length(clusters[[i]]$x)){
    theareas <- c(theareas, area(ldod_plot[j]))
  }
  ldod.area <- c(ldod.area, theareas)
}

# densvalues <- c()
# for (i in 1:length(clusters)){
#   df <- as.data.frame(actualdensity[[i]])
#   point <- as.data.frame(clusters[[i]])
#   closest <- nn2(data= df[, 1:2], query = point, k=1)
#   densvalues <- c(densvalues, df[closest$nn.idx[,1], ]$value)
#   
# }
# 
# df <- data.frame(NN1 = nnk1, NN2 = nnk2, DensDist = distances, Area = ldod.area, Boundary = bdistances, DensityMeasure = densvalues)
# model <- lm(Area ~ DensDist, data = df)
# ggplot(data = df, mapping = aes(x = DensDist, y = Area, colour = DensityMeasure)) + geom_point() + scale_colour_gradientn(colours=rainbow(4)) + xlab("Distance from densest point") + ylab("LDOD Area") + ggtitle("Cluster size = 20 Cows interacting = 4m")
# coeffs <- c(coeffs, model$coefficients[[2]])
# model <- summary(model)
# rsquareds <- c(rsquareds, model$r.squared)
# clustersize <- c(clustersize, x+1)
# numberofclusters <- c(numberofclusters, z)

# edgeneighbour <- vector()
# nndistance1 <- vector()
# nndistance2 <- vector()
# nndistance3 <- vector()
# nndistance4 <- vector()
# edgedist <- vector()
# areas <- vector()
# field <- vector()
# fieldarea_indv <- vector()
# fieldarea <- vector()
# numcows_indv <- vector()
# numcows <- vector()
# dens_to_edge <- vector()
# plotdist <- vector()
# numinteractions <- vector()
# distfromdens <- vector()
# for(i in 1:length(filenames)){
#   most_dense <- as.data.frame(nndensity(point_patterns[[i]], verbose = FALSE))
#   most_dense <- most_dense[which.max(most_dense$value),]
#   most_dense <- ppp(most_dense$x, most_dense$y, window = point_patterns[[i]]$window)
#   dens_to_edge <- c(dens_to_edge, bdist.points(most_dense))
#   most_dense <- as.vector(crossdist(point_patterns[[i]], most_dense))
#   numcows <- c(numcows, length(point_patterns[[i]]$x))
#   fieldarea <- c(fieldarea, area(point_patterns[[i]]$window))
#   for (j in 1:length(point_patterns[[i]]$x)){
#     if(mean(c(nearest_neighbours[[i]][j,1], nearest_neighbours[[i]][j,2], nearest_neighbours[[i]][j,3])) < nearest_edges[[i]][j]){
#       edgeneighbour <- c(edgeneighbour, "neighbour")
#       plotdist <- c(plotdist, nearest_neighbours[[i]][j,1])
#     } else {
#       edgeneighbour <- c(edgeneighbour, "edge")
#       plotdist <- c(plotdist, nearest_edges[[i]][j])
#     }
#     field <- c(field, i)
#     areas <- c(areas, ldod_areas[[i]][j])
#     edgedist <- c(edgedist, nearest_edges[[i]][j])
#     nndistance1 <- c(nndistance1, nearest_neighbours[[i]][j,1])
#     nndistance2 <- c(nndistance2, nearest_neighbours[[i]][j,2])
#     nndistance3 <- c(nndistance3, nearest_neighbours[[i]][j,3])
#     nndistance4 <- c(nndistance4, nearest_neighbours[[i]][j,4])
#     fieldarea_indv <- c(fieldarea_indv, area(point_patterns[[i]]$window))
#     numcows_indv <- c(numcows_indv, length(point_patterns[[i]]$x))
#     numinteractions <- c(numinteractions, closepaircounts(point_patterns[[i]], 10)[j])
#     distfromdens <- c(distfromdens, most_dense[j])
#   }
# }
# 
# edge_analysis <- data.frame(Field = (filenames), DensEdge = (dens_to_edge), NumCows = (numcows), FieldArea = (fieldarea))
# edge_analysis_indvcows <- data.frame(Field = (field), CountNum = (numinteractions), EdgeNeighbour = (edgeneighbour), EdgeDist = (edgedist), NNDist1 = (nndistance1), PlotDist = (plotdist), NNDist2 = (nndistance2), NNDist3 = (nndistance3), NNDist4 = (nndistance4), ldodarea = (areas), numcows_indv = (numcows_indv), fieldarea_indv = (fieldarea_indv), CrossDist = (distfromdens))
# edge_analysis_indvcows$nnmean <- rowMeans(subset(edge_analysis_indvcows, select = c(NNDist1, NNDist2, NNDist3)))
# edge_analysis_indvcows$nnmeanEdge <- ifelse(edge_analysis_indvcows$EdgeNeighbour == "edge",rowMeans(subset(edge_analysis_indvcows, select = c(NNDist1, NNDist2, EdgeDist))) , rowMeans(subset(edge_analysis_indvcows, select = c(NNDist1, NNDist2, NNDist3))))
# 
# edge_analysis_onefield <- edge_analysis_indvcows[ which(edge_analysis_indvcows$Field==113), ]
