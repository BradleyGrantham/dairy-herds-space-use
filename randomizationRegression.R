# this is a simple script to randomize a regression analysis to obtain a p value

tstats <- c()
for (i in 1:4999){
lmods <- lm(NumberOfClusters ~  HerdSize + FieldArea + runif(length(NumberOfClusters)), fields.df)
tstats <- c(tstats, summary(lmods)$coeff[4,3])
}
mean(abs(tstats) > summary(model)$coeff[4,3])

Fstats <- c()

for (i in 1:4999){
lmods <- lm(sample(NumberOfClusters) ~ HerdSize + FieldArea, fields.df)
Fstats <- c(Fstats, summary(lmods)$fstat[1])
}
mean(Fstats > summary(model)$fstat[1])

ggplot(data = fields.df, aes(x = SpaceAllowance, y = SpaceUsePercentageN)) + geom_smooth(method = 'lm', se = FALSE, col = 'grey') +geom_point(shape = 4) + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = 'bottom', text = element_text(size=16, family="Georgia"), axis.ticks.length = unit(-0.1, "cm"), panel.border = element_rect(fill = NULL, colour = 'black'), axis.text.y = element_text(margin = margin(r = 4)))+ ylab('') + xlab(expression('Field area m'^2)) + ylab('Mean NN Distance m')
