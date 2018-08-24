#this was previously testnewfieldedges.R

require(XLConnect)
require(spatstat)
require(ggplot2)

filenames <- readLines("./Data_with_edges/New fields/Coords/list.txt", ok = TRUE)
Areafilepath <- "./Data_with_edges/New fields/Coords/"
XYfilepath <- "./Data/SOUTH WEST data Aug08/XY coordinates/"

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
