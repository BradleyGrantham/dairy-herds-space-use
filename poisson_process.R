require(spatstat)

max_nos <- vector()
sample <- list()

for (monte_carlo in 1:10000){

  
  sample[[1]] <- sample(55:255, 69, replace = TRUE)
  sample[[2]] <- sample(55:255, 69, replace = TRUE)
  
  matr <- matrix(NA, nrow=length(sample[[1]]), ncol=length(sample[[2]]))
  for (i in 1:(length(sample[[1]])-1)){
    for (j in (i+1):length(sample[[2]])){
      xdist <- abs(sample[[1]][i] - sample[[1]][j])
      ydist <- abs(sample[[2]][i] - sample[[2]][j])
      seperation <- sqrt(xdist^2 + ydist^2)
      matr[i,j] <- seperation
      matr[j,i] <- seperation
    }  
  }
  
  max_nos[monte_carlo] <- round(max(matr, na.rm = TRUE))
  print.default (monte_carlo)
}

df <- as.data.frame(table(max_nos))
plot(df)
lines(df$max_nos, df$Freq)

summary(max_nos)
