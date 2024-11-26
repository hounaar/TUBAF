################################################################################
# R script sysstat24_04_en.r for the practice "Statistical Analysis of Systems"
#####

####################
# First load the additional package geoR:
###
require(geoR)

####################
# Task 1
###

# The following command defines a function which performs a
# Simple Kriging estimation with respect to a given measurement point.
preSK <- function(s,s1,z1,r,a,cov.model="sph") z1*cov.spatial(abs(s1-s),cov.pars=c(a,r),cov.model=cov.model)/a
# The following command defines a function which computes the
# Simple Kriging variance with respect to a given measurement point.
varSK <- function(s,s1,z1,r,a,cov.model="sph") a-cov.spatial(abs(s1-s),cov.pars=c(a,r),cov.model=cov.model)^2/a

# Specification of the measurement location (on the real axis):
s1 <- 0
# Specification of the measured value:
z1 <- 1
# Set of locations at which to estimate:
svec <- seq(-3,3,by=0.1) # = (-3.0, -2.9, ..., 2.9, 3.0)

### 
# Simple Kriging with the spherical model, r is range, a is sill
###
# First generate Kriging estimates and Kriging variances for different r and a at the locations in svec!
# Estimates and Kriging variances for r=1 and a=1:
pre1 <- preSK(svec,s1=s1,z1=z1,r=1,a=1,cov.model="sph")
var1 <- varSK(svec,s1=s1,z1=z1,r=1,a=1,cov.model="sph")
# Estimates and Kriging variances for r=2 and a=1:
pre2 <- preSK(svec,s1=s1,z1=z1,r=2,a=1,cov.model="sph")
var2 <- varSK(svec,s1=s1,z1=z1,r=2,a=1,cov.model="sph")
# Estimates and Kriging variances for r=1 and a=2:
pre3 <- preSK(svec,s1=s1,z1=z1,r=1,a=2,cov.model="sph")
var3 <- varSK(svec,s1=s1,z1=z1,r=1,a=2,cov.model="sph")

# Compare the Kriging estimates and Kriging variances below!
# Comparison r=1,a=1 vs. r=2,a=1:
layout(matrix(1:4,2)) # 4 plots in the plot window
plot(svec,pre1,type="l",main="Kriging estimate, r=1, a=1",xlab="s",ylab=expression(hat(Z)(s)))
points(s1,z1,col=2) # measurement point
plot(svec,var1,type="l",main="Kriging variance, r=1, a=1",xlab="s",ylab=expression(paste(sigma[SK]^2,"(s)",sep="")))
plot(svec,pre2,type="l",main="Kriging estimate, r=2, a=1",xlab="s",ylab=expression(hat(Z)(s)))
points(s1,z1,col=2) # measurement point
plot(svec,var2,type="l",main="Kriging variance, r=2, a=1",xlab="s",ylab=expression(paste(sigma[SK]^2,"(s)",sep="")))
layout(1) # reset to one plot in the plot window
# Conclusion: At the measurement location, the Kriging estimate is equal to the measured value (red dot), which is why the Kriging variance is zero there.
#             The range parameter 'r' determines here how far away from the measurement location the measured value has an influence on the Kriging estimate.

# Comparison r=1,a=1 vs. r=1,a=2:
layout(matrix(1:4,2)) # 4 plots in the plot window
plot(svec,pre1,type="l",main="Kriging estimate, r=1, a=1",xlab="s",ylab=expression(hat(Z)(s)))
points(s1,z1,col=2) # measurement point
plot(svec,var1,type="l",main="Kriging variance, r=1, a=1",xlab="s",ylab=expression(paste(sigma[SK]^2,"(s)",sep="")),ylim=c(0,2))
plot(svec,pre3,type="l",main="Kriging estimate, r=1, a=2",xlab="s",ylab=expression(hat(Z)(s)))
points(s1,z1,col=2) # measurement point
plot(svec,var3,type="l",main="Kriging variance, r=1, a=2",xlab="s",ylab=expression(paste(sigma[SK]^2,"(s)",sep="")))
layout(1) # reset to one plot in the plot window
# Conclusion: The sill 'a' has no influence on the Kriging estimate here, but it does have an influence on the Kriging variance.

### 
# For comparison now Simple Kriging with the (smoother) Gaussian model, r is a scaling parameter similar to the range, a is the sill
###
# First generate Kriging estimates and Kriging variances for different r and a at the locations in svec!
# Estimates and Kriging variances for r=1 und a=1:
pre4 <- preSK(svec,s1=s1,z1=z1,r=1,a=1,cov.model="gau")
var4 <- varSK(svec,s1=s1,z1=z1,r=1,a=1,cov.model="gau")
# Estimates and Kriging variances for r=2 und a=1:
pre5 <- preSK(svec,s1=s1,z1=z1,r=2,a=1,cov.model="gau")
var5 <- varSK(svec,s1=s1,z1=z1,r=2,a=1,cov.model="gau")

# Compare the Kriging estimates and Kriging variances below!
# Comparison r=1,a=1 vs. r=2,a=1:
layout(matrix(1:4,2)) # 4 plots in the plot window
plot(svec,pre4,type="l",main="Kriging estimate, r=1, a=1",xlab="s",ylab=expression(hat(Z)(s)))
points(s1,z1,col=2) # measurement point
plot(svec,var4,type="l",main="Kriging variance, r=1, a=1",xlab="s",ylab=expression(paste(sigma[SK]^2,"(s)",sep="")))
plot(svec,pre5,type="l",main="Kriging estimate, r=2, a=1",xlab="s",ylab=expression(hat(Z)(s)))
points(s1,z1,col=2) # measurement point
plot(svec,var5,type="l",main="Kriging variance, r=2, a=1",xlab="s",ylab=expression(paste(sigma[SK]^2,"(s)",sep="")))
layout(1) # reset to one plot in the plot window
# Conclusion: Compared to the Kriging estimate with the spherical model, a smoother curve (parabola-like, no spike) results at the measurement location.
#             In the Gaussian model, the theoretical range is infinite. The parameter r is a scaling parameter here, which therefore
#             nevertheless controls how strongly the influence of the measured value on the Kriging estimate depends on the distance of the 
#             estimation location from the measurement location.

#############
# Task 2
###

# The following command defines a function that performs a
# Simple Kriging estimate with respect to two given measurement points (formula in the exercise sheet).
preSK2 <- function(s,s1,s2,z1,z2,cov.model="sph",...) {
  sig <- cov.spatial(0,cov.model=cov.model,...) # =sig00=sig11=sig22
  sig12 <- cov.spatial(abs(s1-s2),cov.model=cov.model,...) # =sig21
  sig10 <- cov.spatial(abs(s1-s),cov.model=cov.model,...)
  sig20 <- cov.spatial(abs(s2-s),cov.model=cov.model,...)
  ((sig*sig10-sig12*sig20)*z1+(sig*sig20-sig12*sig10)*z2)/(sig^2-sig12^2)
}
# The following command defines a function that determines the
# Simple Kriging variance with respect to two given measuring points (formula in the task sheet).
varSK2 <- function(s,s1,s2,z1,z2,cov.model="sph",...) {
  sig <- cov.spatial(0,cov.model=cov.model,...) # =sig00=sig11=sig22
  sig12 <- cov.spatial(abs(s1-s2),cov.model=cov.model,...) # =sig21
  sig10 <- cov.spatial(abs(s1-s),cov.model=cov.model,...)
  sig20 <- cov.spatial(abs(s2-s),cov.model=cov.model,...)
  sig-(sig*sig10^2+sig*sig20^2-2*sig12*sig10*sig20)/(sig^2-sig12^2)
}

# Specification of both measurement locations:
s1 <- -1
s2 <- 1
# Set of locations at which to estimate:
svec <- seq(-3,3,by=0.1) # = (-3.0, -2.9, ... 2.9, 3.0)

# Specification of both measured values:
z1 <- 1
z2 <- 0.8
# Determine Kriging estimate and Kriging variance:
p1 <- preSK2(svec,s1,s2,z1,z2,cov.model="sph",cov.pars=c(1,1))
v1 <- varSK2(svec,s1,s2,z1,z2,cov.model="sph",cov.pars=c(1,1))

###
#  Influence of changed measured values -> second measured value is changed
###
z2neu <- -0.5
# Determine Kriging estimate and Kriging variance:
p2 <- preSK2(svec,s1,s2,z1,z2neu,cov.model="sph",cov.pars=c(1,1))
v2 <- varSK2(svec,s1,s2,z1,z2neu,cov.model="sph",cov.pars=c(1,1))
# Graphical comparison of Kriging estimates and Kriging variances:
layout(matrix(1:4,2)) # 4 plots in the plot window
plot(svec,p1,type="l",main="Kriging estimate, z1=1, z2=0.8",xlab="s",ylab=expression(hat(Z)(s)),ylim=c(z2neu,z1))
points(c(s1,s2),c(z1,z2),col=2) # measurement points
plot(svec,v1,type="l",main="Kriging variance, z1=1, z2=0.8",xlab="s",ylab=expression(paste(sigma[SK]^2,"(s)",sep="")))
plot(svec,p2,type="l",main="Kriging estimate, z1=1, z2=-0.5",xlab="s",ylab=expression(hat(Z)(s)))
points(c(s1,s2),c(z1,z2neu),col=2) # measurement points
plot(svec,v2,type="l",main="Kriging variance, z1=1, z2=-0.5",xlab="s",ylab=expression(paste(sigma[SK]^2,"(s)",sep="")))
layout(1) # reset to one plot in the plot window
# Conclusion: At the measurement locations, the estimated values are equal to the measured values (this is always the case).
#             The course of the kriging estimates now also depends on the location of the measuring points and the size of 
#             the associated measured values.

###
# Influence of a smoother variogram (Gaussian variogram)
###
# Determine Kriging estimate and Kriging variance:
p3 <- preSK2(svec,s1,s2,z1,z2,cov.model="gau",cov.pars=c(1,1))
v3 <- varSK2(svec,s1,s2,z1,z2,cov.model="gau",cov.pars=c(1,1))
# Graphical comparison of Kriging estimates and Kriging variances:
layout(matrix(1:4,2)) # 4 plots in the plot window
plot(svec,p1,type="l",main="Kriging estimate, sph. variog.",xlab="s",ylab=expression(hat(Z)(s)),ylim=c(0,1))
points(c(s1,s2),c(z1,z2),col=2) # measurement points
plot(svec,v1,type="l",main="Kriging variance, sph. variog.",xlab="s",ylab=expression(paste(sigma[SK]^2,"(s)",sep="")))
plot(svec,p3,type="l",main="Kriging estimate, Gauss. variog.",xlab="s",ylab=expression(hat(Z)(s)))
points(c(s1,s2),c(z1,z2),col=2) # measurement points
plot(svec,v3,type="l",main="Kriging variance, Gauss. variog.",xlab="s",ylab=expression(paste(sigma[SK]^2,"(s)",sep="")))
layout(1) # reset to one plot in the plot window
# Conclusion: The choice of the variogram model (or model for the covariance function) has an influence on the result of the
#             Kriging estimation, e.g. in the form of smoothness at the measurement locations.

###
# Influence of the nugget effect on Kriging estimation and Kriging variance
###
# spherical variogram without and with nugget effect:
nug <- 0.3 # magnitude of the nugget effect
# set of locations at which to estimate: (The nugget effect makes it necessary to split them up for the display.)
svec1 <- seq(-3,-1.01,by=0.01)
svec2 <- seq(-0.99,0.99,by=0.01)
svec3 <- seq(1.01,3,by=0.01)
# Determine Kriging estimate and Kriging variance:
p41 <- preSK2(svec1,s1,s2,z1,z2,cov.model=c("sph","pure.nugget"),cov.pars=rbind(c(1-nug,1),c(nug,0)))
v41 <- varSK2(svec1,s1,s2,z1,z2,cov.model=c("sph","pure.nugget"),cov.pars=rbind(c(1-nug,1),c(nug,0)))
p42 <- preSK2(svec2,s1,s2,z1,z2,cov.model=c("sph","pure.nugget"),cov.pars=rbind(c(1-nug,1),c(nug,0)))
v42 <- varSK2(svec2,s1,s2,z1,z2,cov.model=c("sph","pure.nugget"),cov.pars=rbind(c(1-nug,1),c(nug,0)))
p43 <- preSK2(svec3,s1,s2,z1,z2,cov.model=c("sph","pure.nugget"),cov.pars=rbind(c(1-nug,1),c(nug,0)))
v43 <- varSK2(svec3,s1,s2,z1,z2,cov.model=c("sph","pure.nugget"),cov.pars=rbind(c(1-nug,1),c(nug,0)))
# graphical illustration:
layout(matrix(1:4,2)) # 4 plots in the plot window
plot(svec,p1,type="l",main="Kriging estimate, without nugget",xlab="s",ylab=expression(hat(Z)(s)),ylim=c(0,1))
points(c(s1,s2),c(z1,z2),col=2) # measurement locations
plot(svec,v1,type="l",main="Kriging variance, without nugget",xlab="s",ylab=expression(paste(sigma[SK]^2,"(s)",sep="")))
plot(c(svec1,svec2,svec3),c(p41,p42,p43),type="l",main="Kriging estimate, with nugget",xlab="s",ylab=expression(hat(Z)(s)),ylim=c(0,1))
points(c(s1,s2),c(z1,z2),col=2) # measurement locations
plot(c(svec1,svec2,svec3),c(v41,v42,v43),type="l",main="Kriging variance, with Nugget",xlab="s",ylab=expression(paste(sigma[SK]^2,"(s)",sep="")),ylim=c(0,1))
points(c(s1,s2),c(0,0)) # The Kriging variance at the measurement locations is of course 0.
layout(1) # reset to one plot in the plot window
# Conclusion: The nugget effect is primarily used to model measurement inaccuracies. The kriging estimate at the measurement locations is equal
#             to the measured values, but in the immediate vicinity of the measurement locations the measurement inaccuracy is smoothed out to a certain extent.


# You can try out the last block with a different value for 'nug' between 0 and 1 to practise.

# Gaussian variogram without and with nugget effect:
nug <- 0.3 # magnitude of the nugget effect
# set of locations at which to estimate: (The nugget effect makes it necessary to split them up for the display.)
svec1 <- seq(-3,-1.01,by=0.01)
svec2 <- seq(-0.99,0.99,by=0.01)
svec3 <- seq(1.01,3,by=0.01)
# Determine Kriging estimate and Kriging variance:
p51 <- preSK2(svec1,s1,s2,z1,z2,cov.model=c("gau","pure.nugget"),cov.pars=rbind(c(1-nug,1),c(nug,0)))
v51 <- varSK2(svec1,s1,s2,z1,z2,cov.model=c("gau","pure.nugget"),cov.pars=rbind(c(1-nug,1),c(nug,0)))
p52 <- preSK2(svec2,s1,s2,z1,z2,cov.model=c("gau","pure.nugget"),cov.pars=rbind(c(1-nug,1),c(nug,0)))
v52 <- varSK2(svec2,s1,s2,z1,z2,cov.model=c("gau","pure.nugget"),cov.pars=rbind(c(1-nug,1),c(nug,0)))
p53 <- preSK2(svec3,s1,s2,z1,z2,cov.model=c("gau","pure.nugget"),cov.pars=rbind(c(1-nug,1),c(nug,0)))
v53 <- varSK2(svec3,s1,s2,z1,z2,cov.model=c("gau","pure.nugget"),cov.pars=rbind(c(1-nug,1),c(nug,0)))
# graphical illustration:
layout(matrix(1:4,2)) # 4 plots in the plot window
plot(svec,p1,type="l",main="Kriging estimate, without nugget",xlab="s",ylab=expression(hat(Z)(s)),ylim=c(0,1))
points(c(s1,s2),c(z1,z2),col=2) # measurement locations
plot(svec,v1,type="l",main="Kriging variance, without Nugget",xlab="s",ylab=expression(paste(sigma[SK]^2,"(s)",sep="")))
plot(c(svec1,svec2,svec3),c(p51,p52,p53),type="l",main="Kriging estimate, with nugget",xlab="s",ylab=expression(hat(Z)(s)),ylim=c(0,1))
points(c(s1,s2),c(z1,z2),col=2) # measurement locations
plot(c(svec1,svec2,svec3),c(v51,v52,v53),type="l",main="Kriging variance, with nugget",xlab="s",ylab=expression(paste(sigma[SK]^2,"(s)",sep="")),ylim=c(0,1))
points(c(s1,s2),c(0,0)) # The Kriging variance at the measurement locations is of course 0.
layout(1) # reset to one plot in the plot window
# Conclusion: Except for the different type of smoothness, the result is the same as for the spherical model.

# Gaussian variogram without and with nugget effect and with z2neu:
nug <- 0.3
# set of locations at which to estimate: (The nugget effect makes it necessary to split them up for the display.)
svec1 <- seq(-3,-1.01,by=0.01)
svec2 <- seq(-0.99,0.99,by=0.01)
svec3 <- seq(1.01,3,by=0.01)
# Determine Kriging estimate and Kriging variance:
p61 <- preSK2(svec1,s1,s2,z1,z2neu,cov.model=c("gau","pure.nugget"),cov.pars=rbind(c(1-nug,1),c(nug,0)))
v61 <- varSK2(svec1,s1,s2,z1,z2neu,cov.model=c("gau","pure.nugget"),cov.pars=rbind(c(1-nug,1),c(nug,0)))
p62 <- preSK2(svec2,s1,s2,z1,z2neu,cov.model=c("gau","pure.nugget"),cov.pars=rbind(c(1-nug,1),c(nug,0)))
v62 <- varSK2(svec2,s1,s2,z1,z2neu,cov.model=c("gau","pure.nugget"),cov.pars=rbind(c(1-nug,1),c(nug,0)))
p63 <- preSK2(svec3,s1,s2,z1,z2neu,cov.model=c("gau","pure.nugget"),cov.pars=rbind(c(1-nug,1),c(nug,0)))
v63 <- varSK2(svec3,s1,s2,z1,z2neu,cov.model=c("gau","pure.nugget"),cov.pars=rbind(c(1-nug,1),c(nug,0)))
p7 <- preSK2(svec,s1,s2,z1,z2neu,cov.model="gau",cov.pars=c(1,1))
v7 <- varSK2(svec,s1,s2,z1,z2neu,cov.model="gau",cov.pars=c(1,1))
# graphical illustration:
layout(matrix(1:4,2)) # 4 plots in the plot window
plot(svec,p7,type="l",main="Kriging estimate, without nugget",xlab="s",ylab=expression(hat(Z)(s)),ylim=c(-0.5,1))
points(c(s1,s2),c(z1,z2neu),col=2) # measurement locations
plot(svec,v7,type="l",main="Kriging variance, without nugget",xlab="s",ylab=expression(paste(sigma[SK]^2,"(s)",sep="")))
plot(c(svec1,svec2,svec3),c(p61,p62,p63),type="l",main="Kriging estimate, with nugget",xlab="s",ylab=expression(hat(Z)(s)),ylim=c(-0.5,1))
points(c(s1,s2),c(z1,z2neu),col=2) # measurement locations
plot(c(svec1,svec2,svec3),c(v61,v62,v63),type="l",main="Kriging variance, with nugget",xlab="s",ylab=expression(paste(sigma[SK]^2,"(s)",sep="")),ylim=c(0,1))
points(c(s1,s2),c(0,0)) # The Kriging variance at the measurement locations is of course 0.
layout(1) # reset to one plot in the plot window
# Conclusion: Here it becomes clear that the nugget effect causes the Kriging estimate (except for the measurement locations) to behave similarly to a regression curve.
#             From the interpolation (connection) of the measurement points, the increasing nugget effect results in a
#             compensating curve running in between.

#############
# Task 3
##

# Load data set, estimate empirical variogram 
# and fit spherical variogram model:
jura <- read.table("juraset.txt",skip=17,header=T)
jura.Co <- as.geodata(jura,coords.col = c(1,2),data.col = 8)
jura.Co.sum <- summary(jura.Co)
max.dist <- jura.Co.sum$distances.summary[2]
jura.vario <- variog(jura.Co,breaks=seq(0,max.dist/2,length.out=50),bin.cloud=TRUE)
jura.variofit <- variofit(jura.vario, cov.model="spherical", fix.nugget=FALSE,nugget=0,ini.cov.pars=c(13,1.3))

# Ordinary Kriging
# First define/save the Kriging type and the variogram model:
# "OK" is the basic setting (default), so would not necessarily have to be specified explicitly.
kc <- krige.control(type.krige="ok",obj.model=jura.variofit)

# Create an equidistant point grid for the measurement data to be estimated:
# The corner points of the window are read out for better visualization.
# These data are included in the summary:
xlim <- jura.Co.sum$coords.summary[,1]
ylim <- jura.Co.sum$coords.summary[,2]

# We want to estimate several points on the map, 
# to create a map for the area.
# First create the locations:
bw <- 0.05 # grid spacing
xs <- seq(xlim[1],xlim[2],by=bw) # set of x-coordinates
ys <- seq(ylim[1],ylim[2],by=bw) # set of y-coordinates
loc <- expand.grid(xs,ys) # Coordinates of all locations at which the Kriging estimate is to be determined
# The "expand.grid" command generates the coordinates of a grid. 

# Display the measuring points and estimation points:
plot(jura.Co$coords,pch=20) # measurement points
points(loc,pch=".",col=2)   # locations (red) where values still need to be estimated

# Perform the Kriging at the desired locations:
jura.krig<-krige.conv(jura.Co,locations=loc,krige=kc)

# What does the object look like, what's inside?
names(jura.krig) 
# jura.krig$predict contains the estimated values.
# jura.krig$krige.var contains the Kriging variances.

# Viewing / displaying the kriging estimate (functions are supplied by the package)
# Display with color-coded values:
image(jura.krig)
# Display in the form of contour lines:
contour(jura.krig) 

# Different color palette:
image(jura.krig,col=terrain.colors(20))
contour(jura.krig,add=T) # additionally contour lines
points(jura.Co$coords,pch=20) # additionally the measuring locations

# Read out the estimated values and the Kriging variances:
jura.km <- matrix(jura.krig$predict,nrow=length(xs))
jura.var <- matrix(jura.krig$krige.var,nrow=length(xs))

# Preparations for displaying the deviations or (relative) estimation errors:
V1 <- 2*sqrt(jura.var)
V2 <- jura.var / (var(jura.Co$data))
V3.u <- jura.km - V1
V3.o <- jura.km + V1

# Illustration of the double kriging standard deviation:
filled.contour(xs,ys,V1,
               asp=1,color.palette=topo.colors,plot.title = title(main = "Uncertainty: double Kriging std.dev."),
               plot.axes={axis(1);axis(2);points(jura.Co$coords[,1],jura.Co$coords[,2],pch=19)})
# Conclusion: The Kriging error (i.e. the uncertainty of the Kriging estimate) is small in the vicinity of the measurement locations, 
#             far away from the measurement locations, however, it is large.


# Illustration of the relative kriging variance (i.e. relative to the variance of the random field):
filled.contour(xs,ys,V2,
               asp=1,color.palette=topo.colors,plot.title = title(main = "Uncertainty: relative Kriging variance"),
               plot.axes={axis(1);axis(2);points(jura.Co$coords[,1],jura.Co$coords[,2],pch=19)})
# Conclusion: The kriging variance is always smaller than the variance of the underlying random field.
#             The further away an estimation location is from the measurement locations, the closer the kriging variance is to the variance of the random field.

# Illustration of the estimated values and lower/upper limits within which 
# the unknown actual values are located with a very high probability: 
layout(matrix(1:4,2)) # 4 plots in the plot window
breaks <- seq(0,max(V3.o),length.out=20) # check color range
image(V3.u,col=topo.colors(length(breaks)-1),breaks=breaks,main="estimates-2*std.dev.")
contour(V3.u,add=T)
image(jura.km,col=topo.colors(length(breaks)-1),breaks=breaks,main="estimates")
contour(jura.km,add=T)
image(V3.o,col=topo.colors(length(breaks)-1),breaks=breaks,main="estimates+2*std.dev.")
contour(V3.o,add=T)
layout(1) # reset to one plot in the plot window
