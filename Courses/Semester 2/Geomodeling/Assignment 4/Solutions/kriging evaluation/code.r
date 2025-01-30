library(gstat)
library(sp)

testData123 <- read.csv("testData123.csv")
coordinates(testData123) <- ~x+y

variogram_exp <- variogram(Co ~ 1, testData123)

variogram_sph <- fit.variogram(variogram_exp, model = vgm("Sph"))
variogram_exp_model <- fit.variogram(variogram_exp, model = vgm("Exp"))
variogram_gauss <- fit.variogram(variogram_exp, model = vgm("Gau"))

plot(variogram_exp, variogram_sph, main="Spherical Model")
plot(variogram_exp, variogram_exp_model, main="Exponential Model")
plot(variogram_exp, variogram_gauss, main="Gaussian Model")

kriging_sph <- gstat(id="Co", formula=Co ~ 1, data=testData123, model=variogram_sph)
kriging_exp <- gstat(id="Co", formula=Co ~ 1, data=testData123, model=variogram_exp_model)
kriging_gauss <- gstat(id="Co", formula=Co ~ 1, data=testData123, model=variogram_gauss)

grid <- expand.grid(x = seq(min(testData123$x), max(testData123$x), length.out = 100),
                    y = seq(min(testData123$y), max(testData123$y), length.out = 100))
coordinates(grid) <- ~x+y

kriging_results_sph <- predict(kriging_sph, newdata=grid)
kriging_results_exp <- predict(kriging_exp, newdata=grid)
kriging_results_gauss <- predict(kriging_gauss, newdata=grid)

cv_sph <- gstat.cv(kriging_sph)
cv_exp <- gstat.cv(kriging_exp)
cv_gauss <- gstat.cv(kriging_gauss)


str(cv_sph)


std_dev_sph <- sd(cv_sph$residual)
std_dev_exp <- sd(cv_exp$residual)
std_dev_gauss <- sd(cv_gauss$residual)

print(std_dev_sph)
print(std_dev_exp)
print(std_dev_gauss)

summary(cv_sph)
summary(cv_exp)
summary(cv_gauss)

par(mfrow=c(3,1))
plot(cv_sph$residual, 
     xlab="Index", ylab="Residuals", main="Cross-Validation Residuals (Spherical)")

plot(cv_exp@data$observed, cv_exp@data$residual, 
     xlab="Observed", ylab="Residuals", main="Cross-Validation Residuals (Exponential)")

plot(cv_gauss@data$observed, cv_gauss@data$residual, 
     xlab="Observed", ylab="Residuals", main="Cross-Validation Residuals (Gaussian)")

