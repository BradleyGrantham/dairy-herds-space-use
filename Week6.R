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

window_edges <- vector(mode="list", length = length(filenames))
for (i in 1:length(filenames)){
  window_edges[[i]] <- edges(point_patterns[[i]])
}
names(window_edges) <- filenames


#create poisson distributions
#set up poisson point pattern and get average number of clusters for cluster size

# #poisson_analysis <- function(X, N) {
#   #is.ppp(X)
#   whichnn <- vector()
#   whichnn_av <- as.vector(matrix(0,nrow=4))
#   numbernn <- vector()
#   numbernn_av <- as.vector(matrix(0,nrow=4))
#   percentagenn <- vector()
#   percentagenn_av <- as.vector(matrix(0,nrow=4))
#   for (l in 1:N){
#     poisson_point_pattern <- runifpoint(length(X$x), win = X$window)
#     for (k in 1:4){
#       count <- 0
#       nearest_neighbour <- as.data.frame(nnwhich(poisson_point_pattern, k = seq(1, k, 1)))
#       groups <- factor(apply(cbind(nearest_neighbour, seq_len(nrow(nearest_neighbour))), 1, function(i) paste(sort(i), collapse="_")))
#       groups <- table(groups)
#       for(j in 1:length(groups)){
#         if (groups[[j]] == k+1){
#           count <- count + 1
#         }
#       }
#       whichnn <- c(whichnn, k+1)
#       numbernn <- c(numbernn, count*(k+1))
#       percentagenn <- c(percentagenn, count*(k+1)/length(poisson_point_pattern$x))
#     }
#     
#   }
#   for (k in 1:4){
#     for (i in seq(k, 4*N, by = 4)){
#       whichnn_av[[k]] <- whichnn_av[[k]] + whichnn[[i]]
#       numbernn_av[[k]] <- numbernn_av[[k]] + numbernn[[i]]
#       percentagenn_av[[k]] <- percentagenn_av[[k]] + percentagenn[[i]]
#     }
#   }
#   nearest_neighbour_analysis <- data.frame(nn = (whichnn_av/100), actual_count = (numbernn_av/100), percentage_count = ((numbernn_av/100)/length(X$x)))
#   return(nearest_neighbour_analysis)
# }

# #create list of distance matrices
# distance_matrices <- vector(mode="list", length = length(filenames))
# for (i in 1:length(filenames)){
#   distance_matrices[[i]] <- pairdist(point_patterns[[i]])
# }
# names(distance_matrices) <- filenames
# 
# #ldod areas
# ldod_plots <- vector(mode="list", length = length(filenames))
# ldod_areas <- vector(mode="list", length = length(filenames))
# for (i in 1:length(filenames)){
#   spaceuse <- as.owin((discs(point_patterns[[i]], radii=5, trim=TRUE)))
#   voronois <- (dirichlet(point_patterns[[i]]))
#  
#   ldod_plots[[i]] <- intersect.tess(voronois, spaceuse)
#   
#   for (j in 1:length(cow_data[[i]]$X)){
#     ldod_areas[[i]][j] <- area(ldod_plots[[i]][j])
#   }
#   ldod_areas[[i]] <- data.frame(Area = (ldod_areas[[i]]))
# }
# #plot(ldod_plots$`BA4 6EL XY`, do.labels = TRUE)
# names(ldod_plots) <- filenames
# names(ldod_areas) <- filenames
# 
# #how many pairs of cows are closer to each other than any other cow
# nearest_neighbour_analysis <- vector(mode = "list", length = length(filenames))
# for (l in 1:length(filenames)){
#   whichnn <- vector()
#   numbernn <- vector()
#   percentagenn <- vector()
#   for (k in 1:4){
#     count <- 0
#     nearest_neighbour <- as.data.frame(nnwhich(point_patterns[[l]], k = seq(1, k, 1)))
#     groups <- factor(apply(cbind(nearest_neighbour, seq_len(nrow(nearest_neighbour))), 1, function(i) paste(sort(i), collapse="_")))
#     groups <- table(groups)
#     for(j in 1:length(groups)){
#       if (groups[[j]] == k+1){
#         count <- count + 1
#       }
#     }
#     whichnn <- c(whichnn, k+1)
#     numbernn <- c(numbernn, count*(k+1))
#     percentagenn <- c(percentagenn, count*(k+1)/length(cow_data[[l]]$X))
#   }
#   nearest_neighbour_analysis[[l]] <- data.frame(nn = (whichnn), actual_count = (numbernn), percentage_count = (percentagenn))
# }
# names(nearest_neighbour_analysis) <- filenames
# 
# 
# 
# #plot the nearest neighbour relationships
# #need to adjust this loop to plot all files
# nnplot <- ggplot(nearest_neighbour_analysis$`BA10 0ND XY`, aes(x = nn, y = percentage_count, colour = "BA10")) + geom_point() + geom_point(data = nearest_neighbour_analysis$`BA9 9PL XY`, aes(x = nn, y = percentage_count, colour = "BA9"))
# for (i in 2:length(filenames)){
#   nnplot <- nnplot + geom_point(data = nearest_neighbour_analysis[[i]], aes(x = nn, y = percentage_count, colour = "blue"))
# }
# nnplot <- nnplot + geom_point(data = ba10_poisson, aes(x = nn, y = percentage_count, colour = "BA9 Poisson")) + geom_point(data = ba10_poisson, aes(x = nn, y = percentage_count, colour = "BA10 Poisson")) 
# nnplot <- nnplot + labs(title = "Groups assigned by nearest neighbours", x = ("Group size"), y = ("Number of Groups"))
# #nnplot <- nnplot + scale_colour_manual("", values = c("BA10"="blue", "BA9"="red", "BA10 Poisson"="green", "BA9 Poisson"="pink"))
# print(nnplot)
# 
# #Which cow is furthest away and how much space would be required if all 
# #cows wanted to be this far away?
# 
# #calculate max and min nn distances
# max_mean_min <- vector(mode = "list", length = length(filenames))
# for (i in 1:length(filenames)){
#   max_mean_min[[i]] <- c((maxnndist(point_patterns[[i]])), mean(nndist(point_patterns[[i]])), (minnndist(point_patterns[[i]])))
# }
# names(max_mean_min) <- filenames
# 
# area_needed <- vector(mode = "list", length = length(filenames))
# area_needed_max <- vector()
# area_needed_min <- vector()
# area_needed_mean <- vector()
# for (i in 1:length(filenames)){
#   max_dist <- max_mean_min[[i]][1]
#   mean_dist <- max_mean_min[[i]][2]
#   min_dist <- max_mean_min[[i]][3]
#   area_needed[[i]][1] <- (pi*(max_dist/2)^2)*length(cow_data[[i]]$X)/sum(ldod_areas[[i]])
#   area_needed[[i]][2] <- (pi*(mean_dist/2)^2)*length(cow_data[[i]]$X)/sum(ldod_areas[[i]])
#   area_needed[[i]][3] <- (pi*(min_dist/2)^2)*length(cow_data[[i]]$X)/sum(ldod_areas[[i]])
#   area_needed_max <- c(area_needed_max, (pi*(max_dist/2)^2)*length(cow_data[[i]]$X)/sum(ldod_areas[[i]]))
#   area_needed_min <- c(area_needed_min, (pi*(min_dist/2)^2)*length(cow_data[[i]]$X)/sum(ldod_areas[[i]]))
#   area_needed_mean <- c(area_needed_mean, (pi*(mean_dist/2)^2)*length(cow_data[[i]]$X)/sum(ldod_areas[[i]]))
# }
# names(area_needed) <- filenames
# area_needed_df <- data.frame(Field = (filenames), Max = (area_needed_max), Mean = (area_needed_mean), Min = (area_needed_min), RoundMax = (round(area_needed_max, digits = 2)), RoundMean = (round(area_needed_mean, digits = 2)), RoundMin = (round(area_needed_min, digits = 2)))
# 
# areaplot <- ggplot(area_needed_df, aes(x = Field, y = Max, colour = factor(Field), label = RoundMax)) + geom_point() + geom_text(hjust = 0, nudge_x = 0.08) 
# areaplot <- areaplot + geom_point(data = area_needed_df, aes(x = Field, y = Min, colour = factor(Field))) + geom_text(data = area_needed_df, aes(x = Field, y = Min, colour = factor(Field), label = RoundMin), hjust = 0, nudge_x = 0.08) 
# areaplot <- areaplot + geom_point(data = area_needed_df, aes(x = Field, y = Mean, colour = factor(Field))) + geom_text(data = area_needed_df, aes(x = Field, y = Mean, colour = factor(Field), label = RoundMean), hjust = 0, nudge_x = 0.08) 
# areaplot <- areaplot + scale_y_log10()
# areaplot <- areaplot + labs(title = "Area needed if all cows were to have the max/mean/min nndist", x = ("Field"), y = ("Area in terms of current ldod area"))
# #areaplot <- areaplot + scale_colour_manual("", values = c("Max"="green", "Mean" = "blue", "Min"="red"))
# 
# print(areaplot)
# 
# # Vary R and count the number of Voronoi areas affected
# areas <- vector()
# radii <- vector()
# field <- vector()
# for (r in 1:8){
#   ldod_plots_aff <- vector(mode="list", length = length(filenames))
#   ldod_areas_aff <- vector(mode="list", length = length(filenames))
#   for (i in 1:length(filenames)){
#     count <- 0
#     disks <- as.owin((discs(point_patterns[[i]], radii=r, trim=TRUE)))
#     disks_separated <- discs(point_patterns[[i]], radii = r, separate = TRUE)
#     new_ldods <- (dirichlet(point_patterns[[i]]))
# 
#     ldod_plots_aff[[i]] <- intersect.tess(new_ldods, disks)
# 
#     for (j in 1:length(cow_data[[i]]$X)){
#       ldod_areas_aff[[i]][j] <- area(ldod_plots_aff[[i]][j])
#     }
#     ldod_areas_aff[[i]] <- data.frame(Area = (ldod_areas[[i]]))
#     for (j in 1:length(ldod_areas_aff[[i]]$Area)){
#       if(round(ldod_areas_aff[[i]]$Area[j])<round(area.owin(disks_separated[[i]]))){
#         count <- count + 1
#       }
#     }
#     field <- c(field, filenames[[i]])
#     radii <- c(radii, r)
#     areas <- c(areas, count/length(cow_data[[i]]$X))
#     
#     
#     }
# }
# df<-data.frame(Field = (field), R = (radii), Freq = (areas))
# affected_areas_plot <- ggplot(df, aes(x=R, y=Freq, fill = factor(field))) + geom_bar(position = "dodge", stat = "identity")
# affected_areas_plot <- affected_areas_plot + labs(title = "Percentage of cows social space affected at R", x = ("Radius"), y = ("Percentage of cows affected"))
# print(affected_areas_plot)