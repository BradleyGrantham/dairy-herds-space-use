meannndist1 <- c()
meannndist2 <- c()
nndist1 <- c()
nndist2 <- c()
bdistall <- c()
meanbdist <- c()
indv.N <- c()
coll.N <- c()
field.area <- c()
field.areas <- c()
spaceallowance <- c()
isolationcount <- c()
for (pointpattern in point_patterns){
  field.area <- c(field.area, area.owin(pointpattern$window))
  spaceallowance <- c(spaceallowance, rep(area.owin(pointpattern$window)/pointpattern$n,pointpattern$n))
  field.areas <- c(field.areas, rep(area.owin(pointpattern$window),pointpattern$n))
  nndist1 <- c(nndist1, nndist(pointpattern))
  nndist2 <- c(nndist2, nndist(pointpattern, k = 2))
  bdistall <- c(bdistall, bdist.points(pointpattern))
  indv.N <- c(indv.N, rep(pointpattern$n, pointpattern$n))
  coll.N <- c(coll.N, pointpattern$n)
  meannndist1 <- c(meannndist1, mean(nndist(pointpattern)))
  meannndist2 <- c(meannndist2, mean(nndist(pointpattern, k = 2)))
  meanbdist <- c(meanbdist, mean(bdist.points(pointpattern)))
  isolationcount <- c(isolationcount, sum(bdist.points(pointpattern) > 10))
}

indv.df <- data.frame(NN1 = nndist1, N2 = nndist2, BDist = bdistall, N = indv.N, SpaceAllowance = spaceallowance, Area = field.areas)
coll.df <- data.frame(NN1 = meannndist1, N2 = meannndist2, area = field.area, N = coll.N, bdist = meanbdist, isolationcount = isolationcount)

coll.df$spaceallowance <- coll.df$area/coll.df$N

isolated.df <- df[which(df$NN1 > 10), ]
notisolated.df <- df[which(df$NN1 <= 10),]

test.df <- coll.df[which(coll.df$N < 50),]
test1.df <- coll.df[which(coll.df$N < 100 & 50 >= coll.df$N),]
test2.df <- coll.df[which(coll.df$N >= 100),]
