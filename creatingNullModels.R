# the title of this is self-explanatory, creates null models for different characteristics

require(spatstat)

# ####### Create a NULL model for the distribution of LDODs ###########
# 
# #nearest neighbours
# poisson.meanneighbour <- vector(mode="list", length = length(filenames))
# for (i in 1:length(filenames)){
#   poisson.neighbours <- c()
#   poissonPointProcesses <- runifpoint(point_patterns[[i]]$n, win=point_patterns[[i]]$window, nsim=4999)
#   for (j in 1:4999){
#     poisson.neighbours <- c(poisson.neighbours, mean(nndist(poissonPointProcesses[[j]])))
#   }
#   poisson.meanneighbour[[i]] <- poisson.neighbours
# }
# 
# #get pvalues
# meanneighbours.allfields <- c()
# pvalue.meanneighbour <- c()
# for (i in 1:length(filenames)){
#   count <- 1
#   meanneighbours.allfields <- c(meanneighbours.allfields, poisson.meanneighbour[[i]])
#   for (j in 1:length(poisson.meanneighbour[[i]])){
#     if (poisson.meanneighbour[[i]][j] < mean(nndist(point_patterns[[i]]))){
#       count <- count + 1
#       }
#   }
#   pvalue.meanneighbour <- c(pvalue.meanneighbour, count/5000)
# }
# 
# #hist for all fields
# meanneighbours.allfields <- c()
# for (i in 1:4999){
#   holdingvector <- c()
#   for (j in 1:length(filenames)){
#     holdingvector <- c(holdingvector, poisson.meanneighbour[[j]][i])
#   }
#   meanneighbours.allfields <- c(meanneighbours.allfields,mean(holdingvector))
# }
# realmeanneighbour <- c()
# for (i in 1:length(filenames)){
#   realmeanneighbour <- c(realmeanneighbour, mean(nndist(point_patterns[[i]])))
# }
# realmeanneighbour <- mean(realmeanneighbour)
# meanneighbours.allfields <- c(meanneighbours.allfields, realmeanneighbour)
# 

####### Create a NULL model for the distribution of LDODs ###########

#ldod areas
all.poisson.areas <- c()
poisson.ldod_areas <- vector(mode="list", length = length(filenames))
for (i in 1:length(filenames)){
  cat(i)
  all.poisson.areas <- c()
  poissonPointProcesses <- runifpoint(point_patterns[[i]]$n, win=point_patterns[[i]]$window, nsim=999)
  for (j in 1:999){
    poisson.spaceuse <- as.owin((discs(poissonPointProcesses[[j]], radii=5, trim=TRUE, delta = pi)))
    poisson.voronois <- (dirichlet(poissonPointProcesses[[j]]))
    poisson.ldod_plot <- intersect.tess(poisson.voronois, poisson.spaceuse)

    for (k in 1:point_patterns[[i]]$n){
      poisson.ldod_areas[[i]][k] <- area.owin(poisson.ldod_plot$tiles[[k]])
    }
    all.poisson.areas <- c(all.poisson.areas, poisson.ldod_areas[[i]])
  }
  all.poisson.areas <- all.poisson.areas/maxarea
  poisson.ldod_areas[[i]] <- all.poisson.areas
}

####### Create a NULL model for the distance to boundaries ###########

#this is edge analysis for all fields
# nearest_edges <- vector(mode="list", length = length(filenames))
# for (i in 1:length(filenames)){
#   nearest_edges[[i]] <- bdist.points(point_patterns[[i]])
# }
# names(nearest_edges) <- filenames

# poisson.meandistcount <- vector(mode="list", length = length(filenames))
# for (i in 38:length(filenames)){
#   cat(i)
#   poisson.bdistancescount <- c()
#   poissonPointProcesses <- runifpoint(point_patterns[[i]]$n, win=point_patterns[[i]]$window, nsim=4999)
#   for (j in 1:4999){
#     poisson.bdistancescount <- c(poisson.bdistancescount, sum(bdist.points(poissonPointProcesses[[j]]) < 5))
#   }
#   poisson.meandistcount[[i]] <- poisson.bdistancescount
# }
# 
#get pvalues
pvalue.ldod <- c()
for (i in 1:length(filenames)){
  count <- 1
  for (j in 1:length(poisson.ldod_areas.mean[[i]])){
    if (poisson.ldod_areas.mean[[i]][j] > mean(ldod_areas[[i]])){
      count <- count + 1
      }
  }
  pvalue.ldod <- c(pvalue.ldod, count/1000)
}
