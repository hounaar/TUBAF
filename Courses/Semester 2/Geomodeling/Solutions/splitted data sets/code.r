# Load necessary libraries
library(gstat)
library(sp)
library(fields)

# Load datasets
df_links <- read.csv("testData_links123.csv", sep=" ", header=TRUE)
df_rechts <- read.csv("testData_rechts123.csv", sep=" ", header=TRUE)

# Convert columns to numeric
df_links[] <- lapply(df_links, as.numeric)
df_rechts[] <- lapply(df_rechts, as.numeric)

# Define spatial coordinates
coordinates(df_links) <- ~x+y
coordinates(df_rechts) <- ~x+y

# Compute experimental variograms
variogram_links <- variogram(Co ~ 1, df_links)
variogram_rechts <- variogram(Co ~ 1, df_rechts)

# Fit Variogram Models (Exponential for links, Spherical for rechts)
vario_exp_links <- fit.variogram(variogram_links, model = vgm(psill=7.83, "Exp", range=113.8, nugget=4.67))
vario_sph_rechts <- fit.variogram(variogram_rechts, model = vgm(psill=5.31, "Sph", range=337.5, nugget=6.99))

# Plot Experimental & Fitted Variograms
par(mfrow=c(1,2))
plot(variogram_links, vario_exp_links, main="Exponential Model - testData_links123")
plot(variogram_rechts, vario_sph_rechts, main="Spherical Model - testData_rechts123")

# Perform Kriging
kriging_exp_links <- gstat(id="Co", formula=Co ~ 1, data=df_links, model=vario_exp_links)
kriging_sph_rechts <- gstat(id="Co", formula=Co ~ 1, data=df_rechts, model=vario_sph_rechts)

# Define Extended Prediction Grid
grid_x <- seq(0, 3271, length.out = 50)  # Now covers full x-range
grid_y <- seq(0, 3000, length.out = 50)
grid <- expand.grid(x = grid_x, y = grid_y)
coordinates(grid) <- ~x+y

# Perform Kriging Interpolation
kriging_results_exp_links <- predict(kriging_exp_links, newdata=grid)
kriging_results_sph_rechts <- predict(kriging_sph_rechts, newdata=grid)

# Convert Kriging results to matrices (using Co.pred instead of var1.pred)
kriging_matrix_exp_links <- matrix(kriging_results_exp_links$Co.pred, 
                                   nrow=length(grid_y), ncol=length(grid_x), byrow=TRUE)

kriging_matrix_sph_rechts <- matrix(kriging_results_sph_rechts$Co.pred, 
                                    nrow=length(grid_y), ncol=length(grid_x), byrow=TRUE)

# Plot Kriging Interpolation Results
par(mfrow=c(1,2))

image.plot(grid_x, grid_y, kriging_matrix_exp_links, 
           main="Exponential Kriging - testData_links123", col=terrain.colors(100))
points(df_links$x, df_links$y, pch=20, col="black")

image.plot(grid_x, grid_y, kriging_matrix_sph_rechts, 
           main="Spherical Kriging - testData_rechts123", col=terrain.colors(100))
points(df_rechts$x, df_rechts$y, pch=20, col="black")
