require(XLConnect)
require(spatstat)
require(ggplot2)
require(Hmisc)

data <- readWorksheetFromFile("W8-Results (1).xlsx", sheet=1)

data <- as.data.frame(data)
real_data <- data[data$Real.R..or.Poisson.P.=='R',]
poisson_data <- data[data$Real.R..or.Poisson.P.=='P',]


model<-lm(real_data$Max.Escape.Distance ~ real_data$Total.Field.Area)
summary(model)

ggplot(real_data, aes(x=Total.Field.Area, y=A.possible..A.field.)) + geom_point() + geom_smooth(method="lm", se = TRUE) + geom_text(x=90000, y =7500, label = "R^2 = 0.07284")
