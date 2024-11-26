################################################################################
# R script sysstat24_10_en.r for the practice "Statistical Analysis of Systems"
#####

# Loading some auxiliary functions and packages
if(!library(geoR,logical.return=TRUE)) {install.packages("geoR"); library(geoR)}
if(!library(sp,logical.return=TRUE)) {install.packages("sp"); library(sp)}
if(!library(gstat,logical.return=TRUE)) {install.packages("gstat"); library(gstat)}

# Then load the file “pathtools.r” from the OPAL course, save it in the same folder as sysstat24_10_en.r 
# and execute the following command:
source("pathtools.r")

#############
# Paths and path variograms
###

times <- seq(0,10,by=0.1)
### Directed movement
ax <- 1

ay <- 1
Xt <- ax*times
Yt <- ay*times
txy <- cbind(t=times, x=Xt, y=Yt)
# Illustration of the path
plot.path(txy)
# Development over time
plot(txy[,1],txy[,2],type="l",col=2,ylim=range(txy[,-1]),xlab="t",ylab="X(t) bzw. Y(t)",main="Zeitliche Entwicklung von X(t) (rot) und Y(t) (blau)")
lines(txy[,1],txy[,3],col=4)
# Path variograms
plot.pvg(txy)

# Changing a_y
ay <- 5
Xt <- ax*times
Yt <- ay*times
txy <- cbind(t=times, x=Xt, y=Yt)
# Illustration of the path
plot.path(txy)
# Development over time
plot(txy[,1],txy[,2],type="l",col=2,ylim=range(txy[,-1]),xlab="t",ylab="X(t) bzw. Y(t)",main="Zeitliche Entwicklung von X(t) (rot) und Y(t) (blau)")
lines(txy[,1],txy[,3],col=4)
# Path variograms
plot.pvg(txy)

ay <- -1
Xt <- ax*times
Yt <- ay*times
txy <- cbind(t=times, x=Xt, y=Yt)
# Illustration of the path
plot.path(txy)
# Path variograms
plot.pvg(txy)
# -> Whether the cross-path variogram gamma_xy rises or falls therefore indicates 
#    whether the coordinates X(t) and Y(t) are positively or negatively correlated.

ay <- 0
Xt <- ax*times
Yt <- ay*times
txy <- cbind(t=times, x=Xt, y=Yt)
# Illustration of the path
plot.path(txy)
# Path variograms
plot.pvg(txy)


### Long-term aimless/chaotic movement with the help of Brownian motion

# a single path
txy <- simBrown2D(times=times)
# Illustration of the path
plot.path(txy)
# Path variograms
plot.pvg(txy)

# another path
txy <- simBrown2D(times=times)
# Illustration of the path
plot.path(txy)
# Path variograms
plot.pvg(txy)

# several paths together (starting point is always (0,0))
txyi<-c()
for(i in 1:10) {
  txy<-simBrown2D(times=times)
  txyi<-rbind(txyi,cbind(txy,rep(i,nrow(txy))))
}
plot.path(txyi)

# -> High randomness leads to large fluctuations in the respective empirical path variograms.

# Prolonged observation
txy <- simBrown2D(times=seq(0,50,by=0.1))
# Illustration of the path
plot.path(txy)
# Path variograms
plot.pvg(txy,cutoff=10/3) # cutoff defines the range in which the path variograms are to be determined
plot.pvg(txy)

# In all cases, X(t) and Y(t) are independent one-dimensional Brownian motions:
plot(txy[,1],txy[,2],type="l",col=2,ylim=range(txy[,-1]),xlab="t",ylab="X(t) or Y(t)",main="Temporal development of X(t) (red) and Y(t) (blue)")
lines(txy[,1],txy[,3],col=4)
# On average, gamma_xy=0 should apply due to the independence of X and Y. This can be seen better in the model path variograms,
# which are determined here by averaging over many path realizations:
mod.pvg(sim=simBrown2D,corr=0.0,times=times,nrep=199)
# The linear shape of gamma_xx and gamma_yy (and thus also gamma_s) can be seen more clearly here.

## X(t) and Y(t) are no longer independent:

# In the following, we examine the effects on the path variograms when X(t) and Y(t) are correlated with each other.

# weak positive correlation
txy <- simBrown2D(times=times,corr=0.5)
plot(txy[,1],txy[,2],type="l",col=2,ylim=range(txy[,-1]),xlab="t",ylab="X(t) or Y(t)",main="Temporal development of X(t) (red) and Y(t) (blue)")
lines(txy[,1],txy[,3],col=4)
plot.pvg(txy)

# strong positive correlation
txy <- simBrown2D(times=times,corr=0.9)
plot(txy[,1],txy[,2],type="l",col=2,ylim=range(txy[,-1]),xlab="t",ylab="X(t) or Y(t)",main="Temporal development of X(t) (red) and Y(t) (blue)")
lines(txy[,1],txy[,3],col=4)
plot.pvg(txy)

txyi<-c()
for(i in 1:10) {
  txy<-simBrown2D(times=times,corr=0.9)
  txyi<-rbind(txyi,cbind(txy,rep(i,nrow(txy))))
}
plot.path(txyi)

# weak negative correlation
txy <- simBrown2D(times=times,corr=-0.5)
plot(txy[,1],txy[,2],type="l",col=2,ylim=range(txy[,-1]),xlab="t",ylab="X(t) or Y(t)",main="Temporal development of X(t) (red) and Y(t) (blue)")
lines(txy[,1],txy[,3],col=4)
plot.pvg(txy)

# strong negative correlation
txy <- simBrown2D(times=times,corr=-0.9)
plot(txy[,1],txy[,2],type="l",col=2,ylim=range(txy[,-1]),xlab="t",ylab="X(t) or Y(t)",main="Temporal development of X(t) (red) and Y(t) (blue)")
lines(txy[,1],txy[,3],col=4)
plot.pvg(txy)

## corresponding theoretical path variograms
# weak positive correlation
mod.pvg(sim=simBrown2D,corr=0.5,times=times,nrep=199)
# strong positive correlation
mod.pvg(sim=simBrown2D,corr=0.9,times=times,nrep=199)
# weak negative correlation
mod.pvg(sim=simBrown2D,corr=-0.5,times=times,nrep=199)
# strong negative correlation
mod.pvg(sim=simBrown2D,corr=-0.9,times=times,nrep=199)
# -> The corresponding cross-path variograms each show the tendency of the correlation between X and Y.


### Movement in restricted area/habitat

# a single path
txy <- simGauss2D(times=times,cov.model="matern",cov.pars=c(1,0.3))
# Illustration of the path
plot.path(txy)
# Path variograms
plot.pvg(txy)

# Several paths (here with different starting points)
txyi<-c()
for(i in 1:10) {
  txy<-simGauss2D(times=times,cov.model="matern",cov.pars=c(1,0.3))
  txyi<-rbind(txyi,cbind(txy,rep(i,nrow(txy))))
}
plot.path(txyi)

# Theoretical path variograms
mod.pvg(sim=simGauss2D,cov.model="matern",cov.pars=c(1,0.3),times=times,nrep=199)
# -> Here, a finite sill is obviously reached at gamma_xx and gamma_yy. 
# In this case, this is due to the fact that X(t) and Y(t) are nothing other than Gaussian random fields 
# on the one-dimensional (time) axis, whose variogram (cf. geostatistics) has a sill. 

# Here, too, X and Y can be correlated:
# a single path
txy <- simGauss2D(times=times,cov.model="matern",cov.pars=c(1,0.3),corr=0.9)
# Illustration of the path
plot.path(txy)
# Path variograms
plot.pvg(txy)

# Theoretical path variograms
mod.pvg(sim=simGauss2D,corr=0.9,cov.model="matern",cov.pars=c(1,0.3),times=times,nrep=199)
# -> The cross-path variogram also reaches a finite maximum value, which means that for all sufficiently
#    large time differences h, the coordinates X(t) and Y(t+h) practically no longer influence each other.


#############
# Uncertainties in the determination of empirical path variograms
###

### Uncertainties in the empirical path variogram based on the randomness of individual path realizations

# The path variograms of the path realizations show considerable differences in some cases.
plot.pvg(simBrown2D(times=seq(0,10,by=0.1)),cutoff=4)
plot.pvg(simBrown2D(times=seq(0,10,by=0.1)),cutoff=4)
plot.pvg(simBrown2D(times=seq(0,10,by=0.1)),cutoff=4)
# In addition to the model path variogram, you can see below the areas within which the respective path variograms
# run with a very high probability.
gB1 <- mod.pvg(sim=simBrown2D,corr=0.0,times=seq(0,10,by=0.1),nrep=199,env=TRUE,value=TRUE,cutoff=4,single=3)
# The range limits are dashed in black, the model curve is blue.
# In addition, 'single'-many empirical path variograms are shown in red.

# Look at the respective area of the y-axis below. 
# This should decrease with longer paths (= larger time interval).
gB2 <- mod.pvg(sim=simBrown2D,corr=0.0,times=seq(0,20,by=0.1),nrep=199,env=TRUE,value=TRUE,cutoff=4,single=3)
gB3 <- mod.pvg(sim=simBrown2D,corr=0.0,times=seq(0,30,by=0.1),nrep=199,env=TRUE,value=TRUE,cutoff=4,single=3)
gB4 <- mod.pvg(sim=simBrown2D,corr=0.0,times=seq(0,40,by=0.1),nrep=199,env=TRUE,value=TRUE,cutoff=4,single=3)
# Representation of the narrowing ranges for gamma_xx in a plot: 
plot(gB1[,1],gB1[,6],type="l",lty=2,main="Path variogram x vs. x",xlab=expression(paste(Delta,"t",sep="")),ylab=expression(paste("2",gamma[xx]),sep=""))
lines(gB1[,1],gB1[,5],lty=2)
lines(gB2[,1],gB2[,6],lty=2,col=2)
lines(gB2[,1],gB2[,5],lty=2,col=2)
lines(gB3[,1],gB3[,6],lty=2,col=3)
lines(gB3[,1],gB3[,5],lty=2,col=3)
lines(gB4[,1],gB4[,6],lty=2,col=4)
lines(gB4[,1],gB4[,5],lty=2,col=4)
# -> The fluctuation in the empirical path variogram can therefore be reduced with longer paths.

# another example
gG1 <- mod.pvg(sim=simGauss2D,cov.model="matern",cov.pars=c(1,0.3),times=seq(0,10,by=0.1),nrep=199,env=TRUE,value=TRUE,cutoff=4,single=3)
gG2 <- mod.pvg(sim=simGauss2D,cov.model="matern",cov.pars=c(1,0.3),times=seq(0,20,by=0.1),nrep=199,env=TRUE,value=TRUE,cutoff=4,single=3) # Wait ...
# Representation of the narrowing ranges for gamma_xx in a plot: 
plot(gG1[,1],gG1[,6],type="l",lty=2,main="Path variogram x vs. x",xlab=expression(paste(Delta,"t",sep="")),ylab=expression(paste("2",gamma[xx]),sep=""))
lines(gG1[,1],gG1[,5],lty=2)
lines(gG2[,1],gG2[,6],lty=2,col=2)
lines(gG2[,1],gG2[,5],lty=2,col=2)


### Uncertainty in the empirical path variogram based on the observation of only one point in a path
times <- seq(0,10,by=0.01)
txy <- simBrown2D(times=times)
plot.path(txy)
obstimes <- seq(0,10,by=0.1) # Observation times
ids <- (1:length(times))[is.element(times,obstimes)]
lines(txy[ids,2],txy[ids,3])
points(txy[ids,2],txy[ids,3],pch=20)
# If the red path represents the actual movement, we generally only observe it selectively,
# e.g. at the black points shown here.

# Fine vs. coarser observation
plot.pvg(txy,cutoff=4)
plot.pvg(txy[ids,],cutoff=4)
# Fine vs. coarser observation with small class width 'width' in the empirical path variogram
plot.pvg(txy,cutoff=4,width=0.1)
plot.pvg(txy[ids,],cutoff=4,width=0.1)

# The uncertainty decreases with increasing refinement of the observation:
gb1 <- mod.pvg(sim=simBrown2D,corr=0.0,times=seq(0,10,by=0.1),nrep=199,env=TRUE,value=TRUE,cutoff=4,width=0.1)
gb2 <- mod.pvg(sim=simBrown2D,corr=0.0,times=seq(0,10,by=0.01),nrep=199,env=TRUE,value=TRUE,cutoff=4,width=0.1)
# Representation of the narrowing ranges for gamma_xx in a plot: 
plot(gb1[,1],gb1[,6],type="l",lty=2,main="Path variogram x vs. x",xlab=expression(paste(Delta,"t",sep="")),ylab=expression(paste("2",gamma[xx]),sep=""))
lines(gb1[,1],gb1[,5],lty=2)
lines(gb2[,1],gb2[,6],lty=2,col=2)
lines(gb2[,1],gb2[,5],lty=2,col=2)

# another example
gg1 <- mod.pvg(sim=simGauss2D,cov.model="matern",cov.pars=c(1,0.3),times=seq(0,10,by=0.5),nrep=199,env=TRUE,value=TRUE,cutoff=4)
gg2 <- mod.pvg(sim=simGauss2D,cov.model="matern",cov.pars=c(1,0.3),times=seq(0,10,by=0.2),nrep=199,env=TRUE,value=TRUE,cutoff=4)
gg3 <- mod.pvg(sim=simGauss2D,cov.model="matern",cov.pars=c(1,0.3),times=seq(0,10,by=0.1),nrep=199,env=TRUE,value=TRUE,cutoff=4)
# Representation of the narrowing ranges for gamma_xx in a plot: 
plot(gg1[,1],gg1[,6],type="l",lty=2,main="Path variogram x vs. x",xlab=expression(paste(Delta,"t",sep="")),ylab=expression(paste("2",gamma[xx]),sep=""))
lines(gg1[,1],gg1[,5],lty=2)
lines(gg2[,1],gg2[,6],lty=2,col=2)
lines(gg2[,1],gg2[,5],lty=2,col=2)
lines(gg3[,1],gg3[,6],lty=2,col=3)
lines(gg3[,1],gg3[,5],lty=2,col=3)

# -> There is an effect of the fineness of the observation on the uncertainty in the estimation of the empirical path variogram.
#    However, this also depends on how finely the class subdivision is selected for the empirical path variogram.


#############
# Real path data
###

# Data from package 'adehabitatLT'
if(!library(adehabitatLT,logical.return=TRUE)) {install.packages("adehabitatLT"); library(adehabitatLT)}
# The package may still need to be installed.

# Movement data of a right whale
data(whale)
plot(whale)
?whale
head(whale[[1]]) # -> also contains time points without (x,y) values 
whale.raw <- cbind(t=cumsum(c(0,whale[[1]]$dt[-nrow(whale[[1]])])),x=whale[[1]]$x,y=whale[[1]]$y)
whale.txy <- na.omit(whale.raw)
plot.pvg(whale.txy) # -> directed

# Movement data of a female bear
data(bear)
plot(bear)
?bear
head(bear[[1]],n=14)
bear.raw <- cbind(t=cumsum(c(0,bear[[1]]$dt[-nrow(bear[[1]])])),x=bear[[1]]$x,y=bear[[1]]$y)
bear.txy <- na.omit(bear.raw)
plot.pvg(bear.txy) # -> rather a limited habitat

# Movement data of a mouflon over two weekends
data(mouflon)
plot(mouflon)
?mouflon
head(mouflon[[1]],n=25) # -> also contains time points without (x,y) values 
# on the first weekend:
mouflon1.raw <- cbind(t=cumsum(c(0,mouflon[[1]]$dt[-nrow(mouflon[[1]])])),x=mouflon[[1]]$x,y=mouflon[[1]]$y)
mouflon1.txy <- na.omit(mouflon1.raw)
plot.pvg(mouflon1.txy) # -> short-term directed, long-term aimless
# on the second weekend:
mouflon2.raw <- cbind(t=cumsum(c(0,mouflon[[2]]$dt[-nrow(mouflon[[2]])])),x=mouflon[[2]]$x,y=mouflon[[2]]$y)
mouflon2.txy <- na.omit(mouflon2.raw)
plot.pvg(mouflon2.txy) # -> aimless

# Movement data of six albatrosses
data(albatross)
plot(albatross)
?albatross
head(albatross[[3]],n=40)
albatross1.txy <- cbind(t=cumsum(c(0,albatross[[1]]$dt[-nrow(albatross[[1]])])),x=albatross[[1]]$x,y=albatross[[1]]$y)
plot.pvg(albatross1.txy)
albatross2.txy <- cbind(t=cumsum(c(0,albatross[[2]]$dt[-nrow(albatross[[2]])])),x=albatross[[2]]$x,y=albatross[[2]]$y)
plot.pvg(albatross2.txy)
albatross3.txy <- cbind(t=cumsum(c(0,albatross[[3]]$dt[-nrow(albatross[[3]])])),x=albatross[[3]]$x,y=albatross[[3]]$y)
plot.pvg(albatross3.txy)
albatross4.txy <- cbind(t=cumsum(c(0,albatross[[4]]$dt[-nrow(albatross[[4]])])),x=albatross[[4]]$x,y=albatross[[4]]$y)
plot.pvg(albatross4.txy)
albatross5.txy <- cbind(t=cumsum(c(0,albatross[[5]]$dt[-nrow(albatross[[5]])])),x=albatross[[5]]$x,y=albatross[[5]]$y)
plot.pvg(albatross5.txy)
albatross6.txy <- cbind(t=cumsum(c(0,albatross[[6]]$dt[-nrow(albatross[[6]])])),x=albatross[[6]]$x,y=albatross[[6]]$y)
plot.pvg(albatross6.txy)

# averaged path variograms for all six albatrosses
alba.pvg<-(est.pvg(albatross1.txy,boundaries=seq(40000,2000000,by=40000))
           +est.pvg(albatross2.txy,boundaries=seq(40000,2000000,by=40000))
           +est.pvg(albatross3.txy,boundaries=seq(40000,2000000,by=40000))
           +est.pvg(albatross4.txy,boundaries=seq(40000,2000000,by=40000))
           +est.pvg(albatross5.txy,boundaries=seq(40000,2000000,by=40000))
           +est.pvg(albatross6.txy,boundaries=seq(40000,2000000,by=40000)))/6
layout(matrix(1:4,2))
plot(alba.pvg[,1],alba.pvg[,3],type="l",col=4,main="Path variogram x vs. x",xlab=expression(paste(Delta,"t",sep="")),ylab=expression(paste("2",gamma[xx]),sep=""))
plot(alba.pvg[,1],alba.pvg[,4],type="l",col=4,main="Path variogram x vs. y",xlab=expression(paste(Delta,"t",sep="")),ylab=expression(paste("2",gamma[yx]),sep=""))
plot(alba.pvg[,1],alba.pvg[,4],type="l",col=4,main="Path variogram x vs. y",xlab=expression(paste(Delta,"t",sep="")),ylab=expression(paste("2",gamma[xy]),sep=""))
plot(alba.pvg[,1],alba.pvg[,5],type="l",col=4,main="Path variogram y vs. y",xlab=expression(paste(Delta,"t",sep="")),ylab=expression(paste("2",gamma[yy]),sep=""))
layout(1)
plot(alba.pvg[,1],alba.pvg[,2],type="l",col=4,main="Scalar path variogram",xlab=expression(paste(Delta,"t",sep="")),ylab=expression(paste("2",gamma[s]),sep=""))

# Movement data of four ibexes
data(ibex) 
plot(ibex)
?ibex
head(ibex[[1]]) # -> also contains time points without (x,y) values 

ibex1.txy <- na.omit(cbind(t=cumsum(c(0,ibex[[1]]$dt[-nrow(ibex[[1]])])),x=ibex[[1]]$x,y=ibex[[1]]$y))
plot.pvg(ibex1.txy)
ibex2.txy <- na.omit(cbind(t=cumsum(c(0,ibex[[2]]$dt[-nrow(ibex[[2]])])),x=ibex[[2]]$x,y=ibex[[2]]$y))
plot.pvg(ibex2.txy)
ibex3.txy <- na.omit(cbind(t=cumsum(c(0,ibex[[3]]$dt[-nrow(ibex[[3]])])),x=ibex[[3]]$x,y=ibex[[3]]$y))
plot.pvg(ibex3.txy)
ibex4.txy <- na.omit(cbind(t=cumsum(c(0,ibex[[4]]$dt[-nrow(ibex[[4]])])),x=ibex[[4]]$x,y=ibex[[4]]$y))
plot.pvg(ibex4.txy)

# Movement data of two wild boars (feeding at night)
data(puechcirc)
plot(puechcirc)
?puechcirc
head(puechcirc[[1]]) # -> also contains time points without (x,y) values 

puechcirc1.txy <- na.omit(cbind(t=cumsum(c(0,puechcirc[[1]]$dt[-nrow(puechcirc[[1]])])),x=puechcirc[[1]]$x,y=puechcirc[[1]]$y))
plot.pvg(puechcirc1.txy)
puechcirc2.txy <- na.omit(cbind(t=cumsum(c(0,puechcirc[[2]]$dt[-nrow(puechcirc[[2]])])),x=puechcirc[[2]]$x,y=puechcirc[[2]]$y))
plot.pvg(puechcirc2.txy)

