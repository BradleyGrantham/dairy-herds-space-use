require(XLConnect)
require(spatstat)
require(ggplot2)

#get filenames
filenames <- readLines("./Data_with_edges/list.txt", ok=TRUE)


XYfilepath <- "./Data_with_edges/XYforR/"
Areafilepath <- "./Data_with_edges/XYareasforR/"

#read in files for XYs of cows
cow_data <- vector(mode="list", length = length(filenames))
for (i in 1:length(filenames)){
  cow_data[[i]] <- read.csv(paste(XYfilepath, filenames[[i]], ".csv", sep=""))
  row.names(cow_data[[i]]) <- NULL
}
names(cow_data) <- filenames

#read in windows
field_windows <- vector(mode="list", length = length(filenames))
for (i in 1:length(filenames)){
  field_windows[[i]] <- read.csv(paste(Areafilepath, filenames[[i]], ".csv", sep=""))
  row.names(field_windows[[i]]) <- NULL
}
names(field_windows) <- filenames

#create point patterns
point_patterns <- vector(mode="list", length = length(filenames))
for (i in 1:length(filenames)){
  point_patterns[[i]] <- ppp(cow_data[[i]]$X, cow_data[[i]]$Y, window = owin(poly = list(x = rev(field_windows[[i]]$X), y = rev(field_windows[[i]]$Y))))
}
names(point_patterns) <- filenames

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

edgeneighbour <- vector()
nndistance1 <- vector()
nndistance2 <- vector()
nndistance3 <- vector()
nndistance4 <- vector()
edgedist <- vector()
areas <- vector()
field <- vector()
fieldarea_indv <- vector()
fieldarea <- vector()
numcows_indv <- vector()
numcows <- vector()
dens_to_edge <- vector()
plotdist <- vector()
numinteractions <- vector()
distfromdens <- vector()
for(i in 1:length(filenames)){
  most_dense <- as.data.frame(nndensity(point_patterns[[i]], verbose = FALSE))
  most_dense <- most_dense[which.max(most_dense$value),]
  most_dense <- ppp(most_dense$x, most_dense$y, window = owin(poly = list(x = rev(field_windows[[i]]$X), y = rev(field_windows[[i]]$Y))))
  dens_to_edge <- c(dens_to_edge, bdist.points(most_dense))
  most_dense <- as.vector(crossdist(point_patterns[[i]], most_dense))
  numcows <- c(numcows, length(point_patterns[[i]]$x))
  fieldarea <- c(fieldarea, area(point_patterns[[i]]$window))
  for (j in 1:length(point_patterns[[i]]$x)){
    if(mean(c(nearest_neighbours[[i]][j,1], nearest_neighbours[[i]][j,2], nearest_neighbours[[i]][j,3])) < nearest_edges[[i]][j]){
      edgeneighbour <- c(edgeneighbour, "neighbour")
      plotdist <- c(plotdist, nearest_neighbours[[i]][j,1])
    } else {
      edgeneighbour <- c(edgeneighbour, "edge")
      plotdist <- c(plotdist, nearest_edges[[i]][j])
    }
    field <- c(field, i)
    areas <- c(areas, ldod_areas[[i]][j])
    edgedist <- c(edgedist, nearest_edges[[i]][j])
    nndistance1 <- c(nndistance1, nearest_neighbours[[i]][j,1])
    nndistance2 <- c(nndistance2, nearest_neighbours[[i]][j,2])
    nndistance3 <- c(nndistance3, nearest_neighbours[[i]][j,3])
    nndistance4 <- c(nndistance4, nearest_neighbours[[i]][j,4])
    fieldarea_indv <- c(fieldarea_indv, area(point_patterns[[i]]$window))
    numcows_indv <- c(numcows_indv, length(point_patterns[[i]]$x))
    numinteractions <- c(numinteractions, closepaircounts(point_patterns[[i]], 10)[j])
    distfromdens <- c(distfromdens, most_dense[j])
  }
}

edge_analysis <- data.frame(Field = (filenames), DensEdge = (dens_to_edge), NumCows = (numcows), FieldArea = (fieldarea))
edge_analysis_indvcows <- data.frame(Field = (field), CountNum = (numinteractions), EdgeNeighbour = (edgeneighbour), EdgeDist = (edgedist), NNDist1 = (nndistance1), PlotDist = (plotdist), NNDist2 = (nndistance2), NNDist3 = (nndistance3), NNDist4 = (nndistance4), ldodarea = (areas), numcows_indv = (numcows_indv), fieldarea_indv = (fieldarea_indv), CrossDist = (distfromdens))
edge_analysis_indvcows$nnmean <- rowMeans(subset(edge_analysis_indvcows, select = c(NNDist1, NNDist2, NNDist3)))
edge_analysis_indvcows$nnmeanEdge <- ifelse(edge_analysis_indvcows$EdgeNeighbour == "edge",rowMeans(subset(edge_analysis_indvcows, select = c(NNDist1, NNDist2, EdgeDist))) , rowMeans(subset(edge_analysis_indvcows, select = c(NNDist1, NNDist2, NNDist3))))

edge_analysis_onefield <- edge_analysis_indvcows[ which(edge_analysis_indvcows$Field==113), ]


#plot stuff needed
plot(point_patterns$`TQ7 4EX`, main = "TQ7 4EX", cex = 0.4)
plot(owin(poly = list(x = rev(field_windows$`TQ7 4EX`$X), y = rev(field_windows$`TQ7 4EX`$Y))), main = "TQ7 4EX")
plot(ldod_plots$`TQ7 4EX`, add = TRUE, do.labels = TRUE, labelargs = c(cex = 0.5))
