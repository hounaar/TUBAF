
library(gstat)
library(sp)
library(sf)
library(automap)
library(ggplot2)

data <- read.csv("testData123.csv")
data <- data[, -1]  

coordinates(data) <- ~x+y

summary(data)
ggplot(data.frame(data), aes(x=Co)) + geom_histogram(bins=20, fill="blue", alpha=0.7) + theme_minimal()
boxplot(data$Co, main="Boxplot of Co", col="lightblue")

vario <- variogram(Co ~ 1, data)
plot(vario, main="Empirical Variogram of Co")

vario_fit <- fit.variogram(vario, model=vgm("Sph"))
plot(vario, vario_fit, main="Fitted Variogram Model")

kriging_model <- krige(Co ~ 1, data, newdata=data, model=vario_fit)
spplot(kriging_model["var1.pred"], main="Kriging Prediction of Co")

cross_val <- krige.cv(Co ~ 1, data, model=vario_fit)
summary(cross_val$residual)
sd(cross_val$residual)

