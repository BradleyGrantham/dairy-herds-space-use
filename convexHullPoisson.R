# to use this script, the data must have been read in using readAllData.R
# because it specifically uses the clusters object
# it then creates a convex hull for each cluster and slowly increases the
# size of this cluster, so analysis can be done on the Poisson limit

R <- vector(mode="list", length = length(73))
pvalue <- vector(mode="list", length = length(73))
areas <- vector(mode="list", length = length(73))
expansionfactor <- vector(mode="list", length = length(73))
poissondfs <- vector(mode="list", length = length(73))

for (n in 1:73){
  
  subR <- c()
  subpvalue <- c()
  subareas <- c()
  subexpfact <- c()
  
  for (i in 1:30){
    cluster.window <- as.owin(discs(clusters[[n]], r = i, trim = FALSE))
    #newppp <- ppp(clusters[[n]]$x, clusters[[n]]$y, window = expand.owin(cluster.window, c(distance = i)))
    newppp <- ppp(clusters[[n]]$x, clusters[[n]]$y, window = cluster.window)
    test <- clarkevans.test(newppp, alternative = "clustered")
    subR <- c(subR, test$statistic[[1]])
    subpvalue <- c(subpvalue, test$p.value)
    #subareas <- c(subareas, area.owin(expand.owin(cluster.window, c(distance = i))))
    subareas <- c(subareas, area.owin(cluster.window)/(maxarea*clusters[[n]]$n))
    subexpfact <- c(subexpfact, i)
    #plot(newppp, main = signif(area.owin(cluster.window)/area.owin(clusters[[n]]$window), 3), cex = 0.2, labels = c(i, ""))
  }
  
  R[[n]] <- subR
  pvalue[[n]] <- subpvalue
  areas[[n]] <- subareas
  expansionfactor[[n]] <- subexpfact
}

for (i in 1:73){
  poissondfs[[i]] <- data.frame(R = R[[i]], pvalue = pvalue[[i]], areas = areas[[i]], expfact = expansionfactor[[i]])
}

for (i in 23:23){
  YN <- c()
  for (j in 1:30){
    if (poissondfs[[i]]$pvalue[[j]] < 0.05){
      YN <- c(YN, '1')
    } else {
      YN <- c(YN, '0')
    }
  }
  poissondfs[[i]]$YN <- YN

}

for (i in 23:23){
  poissondfs[[i]]$spaceallowance <- poissondfs[[i]]$areas/clusters[[i]]$n
}

for (i in 23:23){
  poissondfs[[i]]$areaAsPercentage <- poissondfs[[i]]$areas/area.owin(clusters[[i]]$window)
  poissondfs[[i]]$spaceallowanceAsPercentage <- poissondfs[[i]]$spaceallowance/(area.owin(clusters[[i]]$window)/clusters[[i]]$n)
}

for (i in 23:23){
  plot(ggplot(poissondfs[[i]], aes(x = spaceallowanceAsPercentage, y = YN, group = 1)) + geom_point() + geom_line() + ggtitle(i) + ylab('p value') + xlab('Space allowance'))
}
