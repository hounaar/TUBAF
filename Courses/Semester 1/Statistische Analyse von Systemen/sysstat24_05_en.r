################################################################################
# R script sysstat24_05_en.r for the practice "Statistical Analysis of Systems"
#####

####################
# First load the additional package geoR:
###
require(geoR) 


#############
# Task for Ordinary Kriging
###

# Load the dataset:
virt <- as.geodata(read.table("virt.txt"))

# Estimating and fitting a variogram:
vario.c <- variog(virt, uvec=seq(0, 0.9, len=15))
vario.ls1 <- variofit(vario.c, cov.model="matern", fix.nugget=T, nugget=0.0, ini.cov.pars=c(1,1)) # equals the exponential variogram
plot(vario.c)
lines(vario.ls1, col="red")

# Specification of the grid points:
x <- seq(0, 1, length=100)
y <- seq(0, 1, length=100)
xy.grid <- expand.grid(x=x, y=y)
# By changing the length parameter, you can change the resolution
# of the grid on which interpolation takes place.
# In particular, this also changes the number of interpolation points, 
# which might lengthen the Kriging procedure.

# Ordinary Kriging
# First define/save the Kriging type and the variogram model:
# "OK" is the basic setting (default).
kri.type1 <- krige.control(type.krige="OK", obj.model=vario.ls1)

# Calculation of Kriging estimates and Kriging variances:
kri.res1 <- krige.conv(virt, loc=xy.grid, krige=kri.type1)

# Kriging estimates in matrix form for the following illustration:
kri.res1.pre <- matrix(kri.res1$predict, nrow=length(x))
# Graphical illustration [S0]:
filled.contour(x, y, kri.res1.pre,
               asp=1, color.palette=topo.colors, plot.title = title(main = "Kriging estimates [S0]"),
               plot.axes={axis(1); axis(2); points(virt$coords[,1],virt$coords[,2],pch=19)})

# Kriging variances in matrix form for the following illustration:
kri.res1.var <- matrix(kri.res1$krige.var, nrow=length(x))
# Graphical illustration [V0]:
filled.contour(x, y, kri.res1.var,
               asp=1, color.palette=topo.colors, plot.title = title(main = "Kriging variances [V0]"),
               plot.axes={axis(1); axis(2); points(virt$coords[,1],virt$coords[,2],pch=19)})


############
# (a) larger prediction range
###
x2 <- seq(0,2,length=100)
y2 <- seq(0,2,length=100)
xy.grid2 <- expand.grid(x=x2, y=y2)

# Calculation of Kriging estimates and Kriging variances:
kri.res2 <- krige.conv(virt, loc=xy.grid2, krige=kri.type1)
kri.res2.pre <- matrix(kri.res2$predict, nrow=length(x2))
kri.res2.var <- matrix(kri.res2$krige.var, nrow=length(x2))

# Graphical illustration of the Kriging estimates:
filled.contour(x2, y2, kri.res2.pre,
               asp=1, color.palette=topo.colors, plot.title = title(main = "Kriging estimates a)"),
               plot.axes={axis(1); axis(2); points(virt$coords[,1],virt$coords[,2],pch=19)})
mean(virt$data)
# Conclusion: You can clearly see that outside the measurement area (or far away from the measured values) 
#             the Kriging estimates have values that roughly correspond to the general mean value.

# Graphical illustration of the Kriging variances:
filled.contour(x2, y2, kri.res2.var,
               asp=1, color.palette=topo.colors, plot.title = title(main = "Kriging variances a)"),
               plot.axes={axis(1); axis(2); points(virt$coords[,1],virt$coords[,2],pch=19)})
# Conclusion: Far away from the measured values, the Kriging variance becomes maximum again.

############ 
# (b) a smoother variogram
###
vario.ls2 <- variofit(vario.c, cov.model="matern", fix.kappa=FALSE, ini.cov.pars=c(1,1))
kri.type2 <- krige.control(type.krige="OK", obj.model=vario.ls2)

# Calculation of Kriging estimates and Kriging variances:
kri.res3 <- krige.conv(virt, loc=xy.grid, krige=kri.type2)
kri.res3.pre <- matrix(kri.res3$predict, nrow=length(x))
kri.res3.var<-matrix(kri.res3$krige.var, nrow=length(x))

# Graphical illustration of the Kriging estimates:
filled.contour(x, y, kri.res3.pre,
               asp=1, color.palette=topo.colors, plot.title = title(main = "Kriging estimates b)"),
               plot.axes={axis(1); axis(2); points(virt$coords[,1],virt$coords[,2],pch=19)})
# Conclusion: Compared to [S0], the contour lines are smoother and the range of values is smaller (see legend of color values).

# Graphical illustration of the Kriging variances:
filled.contour(x, y, kri.res3.var,
               asp=1, color.palette=topo.colors, plot.title = title(main = "Kriging variances b)"),
               plot.axes={axis(1); axis(2); points(virt$coords[,1],virt$coords[,2],pch=19)})
# Conclusion: The image of the Kriging variants is also smoother compared to [V0].

############ 
# (c) a larger nugget effect
###
vario.ls3 <- variofit(vario.c, cov.model="exponential", fix.nugget=TRUE, nugget=0.2, ini.cov.pars=c(1,1))
kri.type3 <- krige.control(type.krige="OK", obj.model=vario.ls3)

# Visual comparison of the variograms:
plot(vario.c)
lines(vario.ls1, col="red")
lines(vario.ls2, col="blue")
lines(vario.ls3, col=5)

# Calculation of Kriging estimates and Kriging variances:
kri.res4 <- krige.conv(virt, loc=xy.grid, krige=kri.type3)
kri.res4.pre <- matrix(kri.res4$predict, nrow=length(x))
kri.res4.var <- matrix(kri.res4$krige.var, nrow=length(x))

# Graphical illustration of the Kriging estimates:
filled.contour(x, y, kri.res4.pre,
               asp=1, color.palette=topo.colors, plot.title = title(main = "Kriging estimates c)"),
               plot.axes={axis(1); axis(2); points(virt$coords[,1],virt$coords[,2],pch=19)})
# Conclusion: Compared to [S0], the result looks smoother. However, it should be borne in mind that here you only see the values
#             of the Kriging estimate at the grid points and not at the measurement locations. At each measurement location, the 
#             Kriging estimate is the same as the measured value, so that there are actually abrupt changes at the measurement 
#             locations compared to the Kriging estimates of the surroundings (see also the investigations in the last exercise on Simple Kriging).

# Graphical illustration of the Kriging variances:
filled.contour(x, y, kri.res4.var,
               asp=1, color.palette=topo.colors, plot.title = title(main = "Kriging variances c)"),
               plot.axes={axis(1); axis(2); points(virt$coords[,1],virt$coords[,2],pch=19)})
# Conclusion: The main difference to [V0] is that even in the immediate vicinity of the measurement locations, larger
#             Kriging variances occur in the immediate vicinity of the measurement locations. These are at least as large 
#             as the nugget variance of 0.2. 
#             Spatial proximity to the measurement points is therefore not associated with a correspondingly small uncertainty in this case.

############
# (d) a smaller range
###
kri.type4 <- krige.control(type.krige="OK", cov.model="exponential", cov.pars=c(0.8628,0.1), nugget=0.0)

# Calculation of Kriging estimates and Kriging variances:
kri.res5 <- krige.conv(virt, loc=xy.grid, krige=kri.type4)
kri.res5.pre <- matrix(kri.res5$predict, nrow=length(x))
kri.res5.var <- matrix(kri.res5$krige.var, nrow=length(x))

# Graphical illustration of the Kriging estimates:
filled.contour(x, y, kri.res5.pre,
               asp=1, color.palette=topo.colors, plot.title = title(main = "Kriging estimatese d)"),
               plot.axes={axis(1); axis(2); points(virt$coords[,1],virt$coords[,2],pch=19)})
# Conclusion: Since a smaller range shortens the range of the effects of the measured values, there should be an overall faster 
#             change in the estimated values around the measured values compared to [S0].

# Graphical illustration of the Kriging variances:
filled.contour(x, y, kri.res5.var,
               asp=1, color.palette=topo.colors, plot.title = title(main = "Kriging variances d)"),
               plot.axes={axis(1); axis(2); points(virt$coords[,1],virt$coords[,2],pch=19)})
# Conclusion: Compared to [V0], the uncertainty in the form of the kriging variances around the measurement points 
#             increases faster than in [V0].


#############
# Tasks for Universal Kriging
###

##################
# Dataset jura #
##################

# Load data set, estimate empirical variogram 
# and fit a spherical variogram model:
jura <- read.table("juraset.txt", skip=17, header=T)
jura.Co <- as.geodata(jura,coords.col = c(1,2), data.col = 8)
jura.Co.sum <- summary(jura.Co)
max.dist <- jura.Co.sum$distances.summary[2]
jura.vario <- variog(jura.Co, breaks=seq(0,max.dist/2,length.out=50), bin.cloud=TRUE)
jura.variofit <- variofit(jura.vario, cov.model="spherical", fix.nugget=FALSE,nugget=0, ini.cov.pars=c(13,1.3))

# Specify locations at which to estimate:
xlim <- range(jura.Co$coords[,1])
ylim <- range(jura.Co$coords[,2])
bw <- 0.05
xs <- seq(xlim[1], xlim[2], by=bw)
ys <- seq(ylim[1], ylim[2], by=bw)
loc <- cbind(xs, rep(ys, each=NROW(xs)))

#####
## Constant trend (= Ordinary Kriging):
kc <- krige.control(type.krige="ok", obj.model=jura.variofit)
jura.krig <- krige.conv(jura.Co, loc=loc, krige=kc)

# Estimated trend parameters:
jura.krig$beta.est

# Graphical illustration of the Kriging estimates (with constant trend):
filled.contour(xs, ys, matrix(jura.krig$predict, nrow=length(xs)),
               asp=1, color.palette=topo.colors, plot.title = title(main = "Kriging estimates (with constant trend)"),
               plot.axes={axis(1); axis(2); points(jura.Co$coords[,1],jura.Co$coords[,2],pch=19)})


## Trend variants in geoR:
?trend.spatial # see in particular the sub-item "Details"

#####
## Linear trend (first order trend)
kc.1 <- kc
kc.1$trend.d <- "1st" # trend type for measurement locations
kc.1$trend.l <- "1st" # trend type for estimation locations

# Universal Kriging:
jura.krig.1 <- krige.conv(jura.Co, loc=loc, krige=kc.1)

# Estimated trend parameters:
jura.krig.1$beta.est 

# Graphical illustration of the Kriging estimates (with linear trend):
filled.contour(xs, ys, matrix(jura.krig.1$predict,nrow=length(xs)),
               asp=1,color.palette=topo.colors,plot.title = title(main = "Kriging estimates (with linear trend)"),
               plot.axes={axis(1);axis(2);points(jura.Co$coords[,1],jura.Co$coords[,2],pch=19)})

# Illustration of the linear trend:
b.1 <- jura.krig.1$beta.est
jura.trend.1 <- matrix(b.1[1]+b.1[2]*loc[,1]+b.1[3]*loc[,2], nrow=length(xs))
filled.contour(xs, ys, jura.trend.1,
               asp=1, color.palette=topo.colors, plot.title = title(main = "estimated linear trend"),
               plot.axes={axis(1); axis(2); points(jura.Co$coords[,1],jura.Co$coords[,2],pch=19)})

# Perspective display of the linear trend:
persp(xs, ys, jura.trend.1, xlab="x", ylab="y")

#####
## Quadratic trend (second order trend)
kc.2 <- kc
kc.2$trend.d <- "2nd" # trend type for measurement locations
kc.2$trend.l <- "2nd" # trend type for estimation locations

# Universal Kriging:
jura.krig.2 <- krige.conv(jura.Co, loc=loc, krige=kc.2)

# Estimated trend parameters:
jura.krig.2$beta.est 

# Graphical illustration of the Kriging estimates (with quadratic trend):
filled.contour(xs, ys, matrix(jura.krig.2$predict, nrow=length(xs)),
               asp=1, color.palette=topo.colors, plot.title = title(main = "Kriging estimates (with quadratic trend)"),
               plot.axes={axis(1); axis(2); points(jura.Co$coords[,1],jura.Co$coords[,2],pch=19)})

# Illustration of the quadratic:
b.2 <- jura.krig.2$beta.est
jura.trend.2 <- matrix(b.2[1]+b.2[2]*loc[,1]+b.2[3]*loc[,2]+b.2[4]*loc[,1]^2+b.2[5]*loc[,2]^2+b.2[6]*loc[,1]*loc[,2], nrow=length(xs))
filled.contour(xs, ys, jura.trend.2,
               asp=1, color.palette=topo.colors, plot.title = title(main = "estimated quadratic trend"),
               plot.axes={axis(1); axis(2); points(jura.Co$coords[,1],jura.Co$coords[,2],pch=19)})

# Perspective display of the quadratic trend:
persp(xs, ys, jura.trend.2, xlab="x", ylab="y")


##################
# Dataset virt #
##################

# Load data set:
virt <- as.geodata(read.table("virt.txt"))

# Variogram estimation and fitting:
vario.c <- variog(virt, uvec=seq(0, 0.9, len=15))
vario.ls1 <- variofit(vario.c, cov.model="matern", fix.nugget=T, nugget=0.0, ini.cov.pars=c(1,1)) # equals the exponential variogram

# Artificial generation of trended data (linear trend):
virt.trend <- virt
beta1 <- 5
beta2 <- 10
virt.trend$data <- virt.trend$data+beta1*virt.trend$coords[,1]+beta2*virt.trend$coords[,2]
plot(virt.trend) # 

# Variogram estimation and fitting taking into account a linear trend:
vario.trend.c <- variog(virt.trend, trend="1st", uvec=seq(0, 0.9, len=15))
vario.trend.ls1 <- variofit(vario.trend.c, cov.model="matern", fix.nugget=T, nugget=0.0, ini.cov.pars=c(1,1))

# visual comparison of the variograms:
plot(vario.c)
lines(vario.trend.c, col="red", lty=0)
lines(vario.ls1, col="blue")
lines(vario.trend.ls1, col="red")
# Conclusion: The empirical and adjusted variograms do not match exactly. The reason for this is that in the case of data 
#             that is not actually trended, a small trend is nevertheless included by chance, but this was not taken into account 
#             in the variogram estimation and fitting.

# Specification of the grid points:
x <- seq(0, 1, length=100)
y <- seq(0, 1, length=100)
xy.grid <- expand.grid(x=x, y=y)

## Calculation of Kriging estimates and Kriging variances:
kri.type.trend <- krige.control(type.krige="OK", obj.model=vario.trend.ls1, trend.d="1st", trend.l="1st")
kri.trend.res <- krige.conv(virt.trend, loc=xy.grid, krige=kri.type.trend)
kri.trend.res.pre <- matrix(kri.trend.res$predict, nrow=length(x))
kri.trend.res.var <- matrix(kri.trend.res$krige.var, nrow=length(x))

# Estimated trend parameters:
kri.trend.res$beta.est # Compare the estimated values for beta1 and beta2 with the values specified above!

# Graphical illustration of the Kriging estimates (with linear trend):
filled.contour(x, y, kri.trend.res.pre,
               asp=1, color.palette=topo.colors, plot.title = title(main = "Kriging estimates"),
               plot.axes={axis(1); axis(2); points(virt$coords[,1],virt$coords[,2],pch=19)})
# Conclusion: You can see the direction of the linear trend.

# Graphical illustration of the estimated linear trend:
b <- kri.trend.res$beta.est
filled.contour(x, y, matrix(b[1]+b[2]*xy.grid[,1]+b[3]*xy.grid[,2], nrow=length(x)),
               asp=1, color.palette=topo.colors, plot.title = title(main = "estimated trend"),
               plot.axes={axis(1); axis(2); points(virt$coords[,1],virt$coords[,2],pch=19)})

# Perspective display of the linear trend:
persp(x, y, matrix(b[1]+b[2]*xy.grid[,1]+b[3]*xy.grid[,2], nrow=length(x)), xlab="x", ylab="y", zlab="trend")

# Graphical illustration of the Kriging variances:
filled.contour(x, y, kri.trend.res.var,
               asp=1, color.palette=topo.colors, plot.title = title(main = "Kriging variances"),
               plot.axes={axis(1); axis(2); points(virt$coords[,1],virt$coords[,2],pch=19)})


#############
# Task for conditional simulation
###

# Geostatistical simulation with the difference methode
# Advantage: recourse to unconditional simulations

# Simulation of fields with "grf":
?grf

# Load dataset:
virt <- as.geodata(read.table("virt.txt"))
# Variogram estimation and fitting:
vario.c <- variog(virt, uvec=seq(0, 0.9, len=15))
vario.ls1 <- variofit(vario.c, cov.model="matern", fix.nugget=T, nugget=0.0, ini.cov.pars=c(1,1)) # equals the exponential variogram

# Number of measurement locations:
nv <- NROW(virt$coords)
# x- and y-coordinates of the estimation locations:
x <- seq(0, 1, length=100)
y <- seq(0, 1, length=100)
# Grid of estimation locations (as before):
xy.grid <- expand.grid(x,y)
colnames(xy.grid) <- colnames(virt$coords)

# In the following, we use the Kriging estimate for virt with a constant trend:
# = Step 1 of the algorithm:
kri.type <- krige.control(type.krige="OK", obj.model=vario.ls1)
kri.res <- krige.conv(virt, loc=xy.grid, krige=kri.type)

# Simulation of an (unconditional) field using the fitted variogram
# on measurement locations and estimation locations (Kriging grid) 
# = Step 2 of the algorithm:
simv <- grf(grid=rbind(virt$coords,xy.grid), cov.model=vario.ls1$cov.model, cov.pars=vario.ls1$cov.pars)

# Graphical illustration of the simulation:
filled.contour(x , y, matrix(simv$data[-(1:nv)], nrow=length(x)), asp=1, color.palette=topo.colors, plot.title = title(main = "Simulation"), plot.axes={axis(1);axis(2)})
# According to the 3-sigma rule, the values should be approximately in the range +/- 3*sqrt(vario.ls1$cov.pars[1]).
range(simv$data)                    # interval limits of the values
sqrt(vario.ls1$cov.pars[1])*c(-3,3) # 3-sigma interval

# Simple Kriging based on the simulated values at the measurement locations
# = Step 3 of the algorithm:
simv.geo <- as.geodata(cbind(simv$coords[1:nv,], simv$data[1:nv]))
simv.krig <- krige.conv(simv.geo, locations=xy.grid, krige=krige.control(type.krige="sk", obj.model=vario.ls1, beta=0))

# Final transformation of the conditional simulation
# = Step 4 of the algorith:
simv.res <- simv.krig
#     Z*        =          Z^        +         N         -         N^
simv.res$predict <- kri.res$predict + simv$data[-(1:nv)] - simv.krig$predict

## Graphical illustration
# Range of values:
zlimv <- c(min(c(kri.res$predict, simv.res$predict)), max(c(kri.res$predict, simv.res$predict)))

# Conditional simulation:
filled.contour(x, y, matrix(simv.res$predict, nrow=length(x)), zlim=zlimv,
               asp=1, color.palette=topo.colors, plot.title = title(main = "Bedingte Simulation"),
               plot.axes={axis(1); axis(2); points(virt$coords[,1],virt$coords[,2],pch=19)})
# Original Kriging result for comparison:
filled.contour(x, y, matrix(kri.res$predict, nrow=length(x)),zlim=zlimv,
               asp=1, color.palette=topo.colors, plot.title = title(main = "Kriging-Schätzwerte"),
               plot.axes={axis(1); axis(2); points(virt$coords[,1],virt$coords[,2],pch=19)})
# -> is much smoother, since the kriging estimates represent a kind of average of all 
# possible simulation results

# Repeat steps 2 to 4 (several times if necessary) to get an impression of the fluctuations:
# Step 2 of the algorithm:
simv2 <- grf(grid=rbind(virt$coords, xy.grid), cov.model=vario.ls1$cov.model, cov.pars=vario.ls1$cov.pars)
# Step 3 of the algorithm:
simv2.geo <- as.geodata(cbind(simv2$coords[1:nv,], simv2$data[1:nv]))
simv2.krig <- krige.conv(simv2.geo, locations=xy.grid, krige=krige.control(type.krige="sk", obj.model=vario.ls1, beta=0))
# Step 4 of the algorithm:
simv2.res <- simv2.krig
#     Z*        =          Z^        +         N         -         N^
simv2.res$predict <- kri.res$predict + simv2$data[-(1:nv)] - simv2.krig$predict
# Graphical illustration:
zlimv2 <- c(min(c(kri.res$predict, simv2.res$predict)), max(c(kri.res$predict, simv2.res$predict)))
filled.contour(x, y, matrix(simv2.res$predict, nrow=length(x)), zlim=zlimv2,
               asp=1, color.palette=topo.colors, plot.title = title(main = "Bedingte Simulation 2"),
               plot.axes={axis(1); axis(2); points(virt$coords[,1],virt$coords[,2],pch=19)})
