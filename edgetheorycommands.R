conn.TR1.1SY.a <- connected.ppp(point_patterns$`TR1 1SY a.xls`, 10)
conn.TR1.1SY.a <- subset(conn.TR1.1SY.a, marks == "1", drop = TRUE)

mostdense.TR1.1SY.a <- as.data.frame(nndensity(conn.TR1.1SY.a, k = length(conn.TR1.1SY.a$x), verbose = FALSE))
mostdense.TR1.1SY.a <- mostdense.TR1.1SY.a[which.max(mostdense.TR1.1SY.a$value),]
mostdense.TR1.1SY.a <- ppp(mostdense.TR1.1SY.a$x, mostdense.TR1.1SY.a$y, window = conn.TR1.1SY.a$window)
plot(mostdense.TR1.1SY.a)
distances <- as.vector(crossdist(conn.TR1.1SY.a, mostdense.TR1.1SY.a))
plot(conn.TR1.1SY.a)
bdistances <- bdist.points(conn.TR1.1SY.a)
spaceuse <- as.owin((discs(conn.TR1.1SY.a, radii=5, trim=TRUE)))
voronois <- (dirichlet(conn.TR1.1SY.a))
ldod_plot.TR1.1SY.a <- intersect.tess(voronois, spaceuse)
plot(ldod_plot.TR1.1SY.a)
ldod_area.TR1.1SY.a <- c()
for (j in 1:length(conn.TR1.1SY.a$x)){
  ldod_area.TR1.1SY.a <- c(ldod_area.TR1.1SY.a, area(ldod_plot.TR1.1SY.a[j]))
}
anal3 <- data.frame(Area = ldod_area.TR1.1SY.a, Boundary = bdistances, Density = distances)
model <- lm(Area ~ Density, anal3)
summary(model)

nn.TR1.1SY.a <- nndist(conn.TR1.1SY.a, k = c(1, 2, 3))
