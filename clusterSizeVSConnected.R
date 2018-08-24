for (i in 1:24){
  maxi <- c()
  rvec <- c()
  for (r in seq(0, 6, by = 0.5)){
    conn <- connected(clusters[[i]], 2*r)
    conntab <- table(conn$marks)
    rvec <- c(rvec, r)
    maxi <- c(maxi, max(conntab))
  }
  df <- data.frame(R = rvec, Max = maxi)
  plot(ggplot(data = df, aes(x = R, y = Max)) + geom_point() + xlab('LDOD Radius') + ylab('Biggest Cluster Size') + ggtitle(i))
}