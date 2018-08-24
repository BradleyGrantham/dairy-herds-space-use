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

#create pattern plot and get nn distance
ba10pattern <- ppp(file$X, file$Y, window = bounding.box.xy(file$X, file$Y))

#get data
my_nn <- vector()
pois_nn <- vector()
knum <- vector()

for (k in 1:8){
  
  #set up poisson point pattern and get average number of clusters for cluster size
  av_vect <- vector()
  for (i in 1:100){
    rand_ppp <- rpoispp(lambda = 0.001725, win = bounding.box.xy(file$X, file$Y))
    rand_nn <- rknn(69, k=k, d=2, lambda = 69/area.owin(bounding.box.xy(file$X, file$Y)))
  
    av_vect[i] <- mean(rand_nn)
  }
  pois_nn[k] <- mean(av_vect)
  my_nn[k] <- mean(nndist(ba10pattern, k=k))
  knum[k] <- k
}

#put data into data frame
df <- data.frame(K = (knum), MyNumber = (my_nn), PoisNumber = (pois_nn))

#plot of standard poisson and our data 
myplot <- ggplot(df, aes(x = K, y = MyNumber, colour="BA10")) + geom_smooth() + geom_smooth(data = df, aes(y = PoisNumber, colour="Poisson")) 
myplot <- myplot + labs(title = "Kth Nearest Neighbour vs Mean Dist", x = ("Kth Neighbour"), y = ("Mean Dist"))
myplot <- myplot + scale_colour_manual("", values = c("BA10"="blue", "Poisson"="red"))
myplot <- myplot + annotate("text", x = 7, y = 16, label = "Poisson run \n100 times")
print(myplot)

nndist_ba10 <- nndist(ba10pattern)
rand_local_point <- nncross(runifpoint(69, win=bounding.box.xy(file$X, file$Y)), ba10pattern)

#analysis of poisson 1
rmean <- my_nn[1]
expec_mean <- 1/(2*sqrt(0.001725))
std_err <- 0.26136/sqrt(69*0.001725)
Z1 <- (rmean - expec_mean)/std_err
pvalue_1 <- 2*pnorm(-abs(Z1))

#analysis of poisson
sum_rand <- 0
sum_pp <- 0 

for (i in 1:length(rand_local_point[[1]])){
  sum_rand <- sum_rand + (rand_local_point[[1]][i])^2
  sum_pp <- sum_pp + (nndist_ba10[i])^2
}

C <- sum_rand/sum_pp
y <- C/(1+C)
Z2 <- 2*(y-1/2)*sqrt(2*69 +1)
pvalue_2 <- 2*pnorm(-abs(Z2))