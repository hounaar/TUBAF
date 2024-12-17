library(gstat)
library(sp)

data - read.csv(testData123.csv)
coordinates(data) - ~ x + y

variogram_cloud - variogram(Co ~ 1, data, cloud = TRUE)
plot(variogram_cloud, main = Variogram Cloud for Co, xlab = Lag Distance, ylab = Semivariance)

variogram_empirical - variogram(Co ~ 1, data)
plot(variogram_empirical, main = Empirical Variogram for Co, xlab = Lag Distance, ylab = Semivariance)

variogram_model - fit.variogram(variogram_empirical, vgm(model = Sph))
plot(variogram_empirical, variogram_model, main = Fitted Spherical Variogram Model, xlab = Lag Distance, ylab = Semivariance)

data_left - data[data$x = median(data$x), ]
data_right - data[data$x  median(data$x), ]

variogram_left - variogram(Co ~ 1, data_left)
variogram_right - variogram(Co ~ 1, data_right)

plot(variogram_left, main = Variogram for Left Area, xlab = Lag Distance, ylab = Semivariance)
plot(variogram_right, main = Variogram for Right Area, xlab = Lag Distance, ylab = Semivariance)

variogram_directional - variogram(Co ~ 1, data, alpha = c(0, 45, 90, 135))
plot(variogram_directional, main = Directional Variograms, xlab = Lag Distance, ylab = Semivariance)

variogram_map - variogram(Co ~ 1, data, cutoff = max(variogram_empirical$dist))
plot(variogram_map, main = Variogram Map, xlab = X Coordinate, ylab = Y Coordinate)
