require(XLConnect)
require(spatstat)
require(ggplot2)

BA10_edge <- readWorksheetFromFile(paste("BA10 0ND Field Edge", ".xlsx", sep=""), sheet=1)
BA10_edge <- BA10_edge[-(1:4), ]
BA10_edge <- BA10_edge[, 4:5]
BA10_win <- owin(poly = list(x = BA10_edge$X.old, y = BA10_edge$Y.old))

BA9_edge <- readWorksheetFromFile(paste("BA9 9PL Field Edge", ".xlsx", sep=""), sheet=1)
BA9_edge <- BA9_edge[-(1:3), ]
BA9_edge <- BA9_edge[, 4:5]
BA9_win <- owin(poly = list(x = BA9_edge$X.old, y = BA9_edge$Y.old))

BA10_cows <- readWorksheetFromFile(paste("BA10 0ND XY", ".xlsx", sep=""), sheet=1)
BA10_cows <- BA10_cows[c(-1, -2), 6:7]

BA9_cows <- readWorksheetFromFile(paste("BA9 9PL XY", ".xlsx", sep=""), sheet=1)
BA9_cows <- BA9_cows[c(-1, -2), 6:7]


BA10_pattern <- ppp(BA10_cows$X, BA10_cows$Y, window = BA10_win)
BA9_pattern <- ppp(BA9_cows$X, BA9_cows$Y, window = BA9_win)

plot(BA10_pattern)
plot(BA9_pattern)
