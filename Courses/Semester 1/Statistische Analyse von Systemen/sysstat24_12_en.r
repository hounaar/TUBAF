################################################################################
# R script sysstat24_12_en.r for the practice "Statistical Analysis of Systems"
#####

####################
# Additional package for differential equations (DGL) (provides the function 'lsoda' required in the following):
###
library(deSolve)

# The following function determines the solution of a system of differential equations.
# The system of differential equations (i.e. the form of the derivatives) is contained in the variable 'f'. 
# 'times' contains a sequence of times at which you want to have values, 
# 'param' contains all the parameters that 'f' requires,
# 'start' contains the initial values of the DGL system.
DGL.Lsg <- function(times, f, param, start) { 
  func <- function(t,y,parms) { list( do.call( f, as.list(c(y,parms)) ) , c()) } 
  val <- lsoda(y=start, times=times, func=func, parms=param) 
  return(val)
}

# Finally, the following function is used to display the solution.
plot.Lsg <- function( dgl=NULL, add=FALSE, xlim=NULL, ylim=NULL, col=NULL){
  dgl <- as.data.frame(dgl)	
  if(NCOL(dgl)<2) stop("Error in the variable dgl")
  if(NCOL(dgl)>3) stop("Display only works for one or two variables.")
  if(NCOL(dgl)==2) {
    if(is.null(ylim)){ ylim <- c(0, max(dgl$x)) }
    if(is.null(col)) col <- 4
    if(!add) plot(dgl, col=col, type="l",  main="Population size", xlab="t", ylab="B(t)", ylim=ylim)
    else lines(dgl, col=col)
  }
  if(NCOL(dgl)==3)
  { if( is.null(xlim) ){ xlim <- c(0, max(dgl$x)) }
    if( is.null(ylim) ){ ylim <- c(0, max(dgl$y)) } 
    if( !add ){ par(mfrow=c(2,2)) }
    plot(dgl$time, dgl$x ,col=4, ylim=xlim, type="l", main="Prey population", xlab="t", ylab="B(t)")
    plot(dgl$time, dgl$y, col=3, ylim=ylim, type="l", main="Predator population", xlab="t", ylab="R(t)")
    plot(1,1 ,type="n", xlim=range(dgl$time), ylim=c(0,max(c(xlim,ylim))), main="Prey & Predator", xlab="t", ylab="Population size")
    lines(dgl$time, dgl$x, col=4)
    lines(dgl$time, dgl$y, col=3)
    plot(dgl$x, dgl$y, col=2, xlim=xlim, ylim=ylim, type="l", main="Predator vs. prey", xlab="B", ylab="R")
    points(dgl$x[1], dgl$y[1], col="black",pch=16,cex=1.2)
    if( !add ){ par(mfrow=c(1,1)) }
  } 
  invisible()
}

# This function is used to display both data and curves of a model for estimated parameters.
plot.Vgl <- function( data=NULL, f=NULL, param=NULL, start=NULL, nt=1001) {
  data <- as.data.frame(data)	
  xlim <- c(0, 1.1*max(data$x))
  ylim <- c(0, 1.1*max(data$y))
  par(mfrow=c(2,2))
  times <- seq(min(data$time),max(data$time),length=nt)
  dgl <- as.data.frame(DGL.Lsg(times=times, f=f, param=param, start=start))
  plot(dgl$time, dgl$x ,col="orange", lwd=2, ylim=xlim, type="l", main="Prey population", xlab="t", ylab="B(t)")
  lines(data$time, data$x, col=4, lty=3)
  points(data$time, data$x, col=4, pch=16, cex=0.8)
  plot(dgl$time, dgl$y, col="magenta", lwd=2, ylim=ylim, type="l", main="Predator population", xlab="t", ylab="R(t)")
  lines(data$time, data$y, col=3, lty=3)
  points(data$time, data$y, col=3, pch=16, cex=0.8)
  plot(1,1 ,type="n", xlim=range(dgl$time), ylim=c(0,max(c(xlim,ylim))), main="Prey & Predator", xlab="t", ylab="Population size")
  lines(dgl$time, dgl$x, col="orange")
  lines(dgl$time, dgl$y, col="magenta")
  lines(data$time, data$x, col=4, lty=3)
  lines(data$time, data$y, col=3, lty=3)
  plot(dgl$x, dgl$y, col="black", xlim=xlim, ylim=ylim, type="l", main="Predator vs. prey", xlab="B", ylab="R")
  points(dgl$x[1], dgl$y[1], col="black",pch=16,cex=0.8)
  lines(data$x, data$y, col=2, lty=3)
  points(data$x[1], data$y[1], col="red",pch=2,cex=1.2)
  par(mfrow=c(1,1))
  invisible()
}


#############
# Task 1
###

# Function determines the quadratic distance between the data 'data' and the solution values of the model 'f' with parameters 'theta'
# There is the option via 'eststart' to take the start values of the curves from the data (FALSE) 
# or to transfer your own via 'theta' (TRUE).
getDist <- function(theta,f,data,eststart=FALSE,report=FALSE,bounds=NULL) {
  if(report) {cat("theta: ");cat(theta);}
  np <- length(theta)-NCOL(data)+1
  if(is.null(bounds)) bounds <- rep(Inf,np)
  if(eststart) start<-theta[-(1:np)] else start<-data[1,-1]
  if(min(theta)>0 && min(theta[1:np]<bounds)>0) {
    model <- DGL.Lsg(times=data[,1], f=f, param=theta[1:np], start=start)
    v<-sum((data[,-1]-model[,-1])^2)
  }
  else v<-Inf
  if(report) {cat(" value: ");cat(v);cat("\n")}
  return(v)
}

# Function determines the parameters using the least squares method for a start parameter 'parstart'
# There is an option via 'eststart' to take the start values of the curves from the data (FALSE) or to have them estimated (TRUE).
getOpt <- function(parstart,f,data,eststart=FALSE,report=FALSE,start=NULL,...) {
  if(is.null(start)) start <- data[1,-1]
  opt <- optim(par=c(parstart,start), fn=getDist, f=f, data=data, eststart=eststart,report=report,...)
  if(eststart) return(c(value=opt$value,opt$par)) else return(c(value=opt$value,opt$par[1:length(parstart)],data[1,-1]))
}

# Function determines the parameters using the least squares method for several start parameters 'parstart'
# There is an option via 'eststart' to take the start values of the curves from the data (FALSE) or to have them estimated (TRUE).
getOptMult <- function(parstart,f,data,eststart=FALSE,...) {
  optps <- t(apply(parstart,1,getOpt,f=f,data=data,eststart=eststart,...))
  return(optps[order(optps[,1])[1],])
}

# Lotka-Volterra model
f <- function(x,y,a,b,c,d) c(dx = (a - b*y)*x , dy= (-c + d*x)*y) # x = prey, y = predator, f describes the differential equation

# Times at which observations are made
obstimes<-seq(0,30,by=1)

# Parameters
param <- c(a=1,b=1.5,c=1,d=1)
# Start values
start <- c(x=2,y=1)
# Data generation according to the Lotka-Volterra model
data <- DGL.Lsg(times=obstimes, f=f, param=param, start=start)
# Illustration
plot.Vgl(data,f,param,start)
# Prey:     orange  = model curve for 'f' with parameters 'param' and start values 'start'
#           blue    = data values
# Predator: magenta = model curve for 'f' with parameters 'param' and start values 'start'
#           green   = data values
# R vs B:   black   = model curve for 'f' with parameters 'param' and start values 'start'
#           red     = data values

## now parameter estimation
# If you start with the actually unknown parameters as start parameters, then ...
opt1.F <- getOpt(parstart=param,f=f,data=data,eststart=FALSE)
opt1.F
# ... you get these back if you do not have the starting values estimated, or ...
opt1.T <- getOpt(parstart=param,f=f,data=data,eststart=TRUE)
opt1.T
# ... you get them back if you also estimate the starting values.
# The first entry 'value' in 'opt1.F' or 'opt1.T' is the minimized sum of the quadratic deviations.

# With other start parameters ...
opt2.F <- getOpt(parstart=c(a=1.5,b=1.5,c=1.5,d=1.5),f=f,data=data,eststart=FALSE)
opt2.F
# ... the optimization routine 'optim', which works in the background, does not always find the optimal parameters,
# since the optimization method used is not actually global.
opt2.T <- getOpt(parstart=c(a=1.5,b=1.5,c=1.5,d=1.5),f=f,data=data,eststart=TRUE)
opt2.T

# Therefore, you could, for example, search for the solution based on different starting parameters ("Multistart").
ps <- rbind(expand.grid(a=c(0.5,1.5),b=c(1,2),c=c(0.5,1.5),d=c(0.5,1.5)))
ps # 16 different variants
opt3.F <- getOptMult(parstart=ps,f=f,data=data,eststart=FALSE) # Wait ...
opt3.F
# Starting from several variants of start parameters that do not contain the actually unknown ones,
# you arrive at these pretty much exactly (compare with 'param').
opt3.T <- getOptMult(parstart=ps,f=f,data=data,eststart=TRUE) # Wait ...
opt3.T
# If the starting values are also estimated, the dimension of the search space is larger, 
# which in practice sometimes leads to a less accurate estimate, but from a theoretical point of view 
# should be the other way around, since the sum of the quadratic deviations can be made at least as small 
# with the free starting values as if the starting values had been selected from the data.

# As the multistart variant unfortunately takes a little longer, we always select the actually unknown parameters 
# as start parameters in the following. However, when incorporating measurement inaccuracies, these are not 
# necessarily the best start for the search for the optimum parameters.


## Inclusion of measurement inaccuracies
sigma <- 0.1 # Standard deviation of the measurement errors
# N(0,sigma^2)-distributed random numbers are added to the (exact) data.
dataz1 <- data + c(rep(0,times=NROW(data)), rnorm( n=(length(data)-NROW(data)), mean=0, sd=sigma)) # + measurement errors
# Illustration
plot.Vgl(dataz1,f,param,start)
# -> Points no longer lie exactly on the curves

# An estimator for 'sigma' could be obtained from the sum of the squared deviations in the following way if the exact 'data' is known:
sqrt(sum((dataz1[,-1]-data[,-1])^2)/(length(data[,-1])))
# In practice, however, only 'dataz1' is known.
# Estimation of the parameters
optz1.F <- getOpt(parstart=param,f=f,data=dataz1,eststart=FALSE) 
optz1.F
# Estimation of parameters and starting values
optz1.T <- getOpt(parstart=param,f=f,data=dataz1,eststart=TRUE) 
optz1.T
# Here it becomes clear why it makes sense and is better to estimate the initial values: the initial values from the data are also subject to measurement inaccuracies.
plot.Vgl(dataz1,f,optz1.F[2:5],dataz1[1,-1])
plot.Vgl(dataz1,f,optz1.T[2:5],optz1.T[6:7])
# Back estimation of 'sigma'
sqrt(optz1.F[1]/length(data[,-1])) # eststart=FALSE
sqrt(optz1.T[1]/length(data[,-1])) # eststart=TRUE

# larger sigma
sigma <- 0.2 # Standard deviation of the measurement errors
# N(0,sigma^2)-distributed random numbers are added to the (exact) data.
dataz2 <- data + c(rep(0,times=NROW(data)), rnorm( n=(length(data)-NROW(data)), mean=0, sd=sigma)) # + measurement errors
# Illustration
plot.Vgl(dataz2,f,param,start)
# Estimation of the parameters
optz2.F <- getOpt(parstart=param,f=f,data=dataz2,eststart=FALSE) 
optz2.F
# Estimation of parameters and starting values
optz2.T <- getOpt(parstart=param,f=f,data=dataz2,eststart=TRUE) 
optz2.T
# Variant 'eststart=TRUE' better again
plot.Vgl(dataz2,f,optz2.F[2:5],dataz2[1,-1])
plot.Vgl(dataz2,f,optz2.T[2:5],optz2.T[6:7])
# Back estimation of 'sigma'
sqrt(optz2.F[1]/length(data[,-1])) # eststart=FALSE
sqrt(optz2.T[1]/length(data[,-1])) # eststart=TRUE


## Small simulation study on estimation accuracy
param <- c(a=1,b=1.5,c=1,d=1) # model parameters
start <- c(x=2,y=1) # starting values
sigma <- 0.1 # Standard deviation of measurement uncertainty
optall.F <- c()
optall.T <- c()
for(i in 1:10) {
  data <- DGL.Lsg(times=obstimes, f=f, param=param, start=start)
  dataz <- data + c(rep(0,times=NROW(data)), rnorm( n=(length(data)-NROW(data)), mean=0, sd=sigma))
  optz.F <- getOpt(parstart=param,f=f,data=dataz,eststart=FALSE) 
  optz.T <- getOpt(parstart=param,f=f,data=dataz,eststart=TRUE) 
  optall.F <- rbind(optall.F,optz.F)
  optall.T <- rbind(optall.T,optz.T)
} # Wait ...

gnames <- c("without start value estimation","with start value estimation")
# Estimation accuracy of sigma
boxplot(sqrt(optall.F[,1]/length(data[,-1])),sqrt(optall.T[,1]/length(data[,-1])),names=gnames,main="Estimation accuracy of sigma")
abline(h=sigma,col=2)
# Estimation accuracy of a
boxplot(optall.F[,2],optall.T[,2],names=gnames,main="Estimation accuracy of a")
abline(h=param[1],col=2)
# Estimation accuracy of b
boxplot(optall.F[,3],optall.T[,3],names=gnames,main="Estimation accuracy of b")
abline(h=param[2],col=2)
# Estimation accuracy of c
boxplot(optall.F[,4],optall.T[,4],names=gnames,main="Estimation accuracy of c")
abline(h=param[3],col=2)
# Estimation accuracy of d
boxplot(optall.F[,5],optall.T[,5],names=gnames,main="Estimation accuracy of d")
abline(h=param[4],col=2)


# In principle, it is not to be expected that model parameters can be estimated well under an incorrect model assumption.
# In the following, the data will come from the Lotka-Volterra model known from the last exercise with random fluctuations 
# according to the Wiener process, but the parameters of the deterministic Lotka-Volterra model will be estimated.

# Own approximate numerical solution of the Lotka-Volterra model with random fluctuations:
DGL.Lsg.Zufall <- function(times,f,param,start,sig) {
  v <- matrix(start,nrow=1)
  for(i in 2:length(times)) v<-rbind(v,v[i-1,]+times[2]*f(v[i-1,1],v[i-1,2],param[1],param[2],param[3],param[4])+v[i-1,]*rnorm(2,sd=sig*sqrt(times[2])))
  return(cbind(time=times,x=v[,1],y=v[,2]))
}

times <- seq(0,40,by=0.001)
obstimes<-seq(0,40,by=1)
f <- function(x,y,a,b,c,d) c(dx = (a - b*y)*x , dy= (-c + d*x)*y) # x = prey, y = predator
param <- c(a=1,b=1.5,c=1,d=1 )
start <- c(x=2,y=1)
sig<-0.1

# In the following, three data sets are generated for the same initial parameters. Compare the estimated
# parameters and the associated illustrations.

# Data generation
dataz <- DGL.Lsg.Zufall(times=times, f=f, param=param, start=start, sig=sig)[is.element(times,obstimes),]
# Estimation of parameters and starting values
optz <- getOpt(parstart=param,f=f,data=dataz,eststart=TRUE) 
optz
# Illustration
plot.Vgl(dataz,f,optz[2:5],optz[6:7])

# Data generation
dataz <- DGL.Lsg.Zufall(times=times, f=f, param=param, start=start, sig=sig)[is.element(times,obstimes),]
# Estimation of parameters and starting values
optz <- getOpt(parstart=param,f=f,data=dataz,eststart=TRUE) 
optz
# Illustration
plot.Vgl(dataz,f,optz[2:5],optz[6:7])

# Data generation
dataz <- DGL.Lsg.Zufall(times=times, f=f, param=param, start=start, sig=sig)[is.element(times,obstimes),]
# Estimation of parameters and starting values
optz <- getOpt(parstart=param,f=f,data=dataz,eststart=TRUE) 
optz
# Illustration
plot.Vgl(dataz,f,optz[2:5],optz[6:7])


#############
# Task 2
###

# Unfortunately, the parameter estimation with 'getOpt' occasionally fails with the real data sets, as for some parameters then searched in 'optim'
# the routine 'lsoda', which solves the differential equation numerically, reports errors.
# In addition, 'optim' often runs into local optimum points.
# Therefore, approximately optimal parameters were found using a different, but more time-consuming method, 
# for which there is not enough time in the exercise. These serve as starting parameters in the following 
# (but are no guarantee that the global optimum will be found in the end).

#######
# (a)
###

## Data
Tag <- c(0,1,2,3,4,5,6,7,8,9,10,11,13,15,16,17,18,19)
Hefe <- c(160,40,21,9,31,59,118,109,50,21,10,22,70,130,130,50,20,22)
Pantoffel <- c(88,180,120,60,10,21,18,58,131,69,31,17,21,31,76,170,91,32)
# Combine data
Labor <- cbind(time=Tag,x=Hefe,y=Pantoffel)
# Illustration
plot.Lsg(Labor)

## Parameter estimation without estimation of the start values (i.e. start values B(0) and R(0) from the data)
# Start parameters (already very good preselection)
thetaL0.F <- c(a=0.55521057,b=0.01205360,c=1.53970425,d=0.02148968,x=160.0,y=88.0)
# Illustration with the start parameters
plot.Vgl(Labor,f,thetaL0.F[1:4],thetaL0.F[5:6])
# Parameter estimation (with the option 'report=TRUE' the entire parameter search is output)
optL.F <- getOpt(parstart=thetaL0.F[1:4],f=f,data=Labor,eststart=FALSE,report=FALSE,bounds=c(2,1,2,1),control=list(maxit=10000)) 
# -> actually an improvement on the starting parameters, because the sum of the quadratic deviations has become smaller:
getDist(theta=thetaL0.F,f=f,data=Labor,eststart=FALSE) > optL.F[1]
# Estimated parameters
optL.F[-1]
# Illustration with the estimated parameters
plot.Vgl(Labor,f,optL.F[2:5],optL.F[6:7])

## Parameter estimation with estimation of the start values (i.e. start values B(0) and R(0) are also estimated)
# Start parameters (already very good preselection)
thetaL0.T <- c(a=0.51774719,b=0.01006891,c=1.45434797,d=0.02311026,x=115.0,y=95.0)
# Illustration with the start parameters
plot.Vgl(Labor,f,thetaL0.T[1:4],thetaL0.T[5:6])
# Parameter estimation (with the option 'report=TRUE' the entire parameter search is output)
optL.T <- getOpt(parstart=thetaL0.T[1:4],start=thetaL0.T[5:6],f=f,data=Labor,eststart=TRUE,report=FALSE,control=list(maxit=10000)) 
# -> actually an improvement on the starting parameters, because the sum of the quadratic deviations has become smaller:
getDist(theta=thetaL0.T,f=f,data=Labor,eststart=TRUE) > optL.T[1]
# -> again improvement to the case in which the starting values are not also estimated
c(optL.F[1],optL.T[1])
# Estimated parameters
optL.T[-1]
# Illustration with the estimated parameters
plot.Vgl(Labor,f,optL.T[2:5],optL.T[6:7])


#######
# (b)
###

## Data
year <- 1845+2*(0:29)
hare <- c(20,20,52,83,64,68,83,12,36,150,110,60,7,10,70,100,92,70,10,11,137,137,18,22,52,83,18,10,9,65)
lynx <- c(32,50,12,10,13,36,15,12,6,6,65,70,40,9,20,34,45,40,15,15,60,80,26,18,37,50,35,12,12,25)
# a first illustration
plot(year,hare,type="l",col=4,ylab="Furs (in thousands)",xlab="Year",ylim=c(0,max(hare)))
points(year,hare,col=4,pch=16)
lines(year,lynx,col=3)
points(year,lynx,col=3,pch=16)
# Combine data
HaseLuchs<-cbind(time=year,x=hare,y=lynx)
# Illustration
plot.Lsg(HaseLuchs)

## Parameter estimation without estimation of the start values (i.e. start values from the data)
# Start parameters
thetaHL0.F <- c(a=0.5,b=0.02,c=0.5,d=0.01,x=20,y=32)
# Illustration with the start parameters
plot.Vgl(HaseLuchs,f,thetaHL0.F[1:4],thetaHL0.F[5:6])
# Parameter estimation (with the option 'report=TRUE' the entire parameter search is output)
optHL.F <- getOpt(parstart=thetaHL0.F[1:4],f=f,data=HaseLuchs,eststart=FALSE,report=FALSE,control=list(maxit=10000)) 
# -> actually an improvement on the starting parameters, because the sum of the quadratic deviations has become smaller:
getDist(theta=thetaHL0.F,f=f,data=HaseLuchs,eststart=FALSE) > optHL.F[1]
# Estimated parameters
optHL.F[-1]
# Illustration with the estimated parameters
plot.Vgl(HaseLuchs,f,optHL.F[2:5],optHL.F[6:7])

## Parameter estimation with estimation of the start values (i.e. start values B(0) and R(0) are also estimated)
# Start parameters (already very good preselection) 
thetaHL0.T <- c(a=0.734746642,b=0.024004644,c=0.505631552,d=0.008883166,x=57.990312346,y=49.496232173)
# Illustration with the start parameters
plot.Vgl(HaseLuchs,f,thetaHL0.T[1:4],thetaHL0.T[5:6])
# Parameter estimation (with the option 'report=TRUE' the entire parameter search is output)
optHL.T <- getOpt(parstart=thetaHL0.T[1:4],f=f,data=HaseLuchs,eststart=TRUE,report=FALSE,control=list(maxit=10000)) 
# -> actually an improvement on the starting parameters, because the sum of the quadratic deviations has become smaller:
getDist(theta=thetaHL0.T,f=f,data=HaseLuchs,eststart=TRUE) > optHL.T[1]
# -> again improvement to the case in which the starting values are not also estimated
c(optHL.F[1],optHL.T[1])
# Estimated parameters
optHL.T[-1]
# Illustration with the estimated parameters
plot.Vgl(HaseLuchs,f,optHL.T[2:5],optHL.T[6:7])


## Comparison between (a) and (b)
# From the perspective of the representations, the adjusted model in (a) fits better than the corresponding model in (b). 
# The deviation measure is just the sum of the squared deviations, which are normalized by the number of data for comparison purposes.
sqrt(optL.T[1]/length(Labor[,-1]))      # (a)
sqrt(optHL.T[1]/length(HaseLuchs[,-1])) # (b)
# -> no significant difference
# However, if one also takes into account that B(t) and R(t) generally have different scales, the picture changes.
# A measure of the magnitude of B(t) or R(t) are the respective time averages.
dglL <- DGL.Lsg(times=Labor[,1], f=f, param=optL.T[2:5], start=optL.T[6:7])        # model curve (a)
dglHL <- DGL.Lsg(times=HaseLuchs[,1], f=f, param=optHL.T[2:5], start=optHL.T[6:7]) # model curve (b)
# Quadratic distances weighted with the respective mean value over time
sqrt(sum((dglL[,2]-Labor[,2])^2)/mean(Labor[,2])+sum((dglL[,3]-Labor[,3])^2)/mean(Labor[,3]))                   # (a)
sqrt(sum((dglHL[,2]-HaseLuchs[,2])^2)/mean(HaseLuchs[,2])+sum((dglHL[,3]-HaseLuchs[,3])^2)/mean(HaseLuchs[,3])) # (b)


#######
# (c)
###

## Data
Jahr <- 1959:2011
Elch <- c(538,564,572,579,596,620,634,661,766,848,1041,1045,1183,1243,1215,1203,1139,1070,949,845,857,788,767,780,830,927,976,1014,
          1046,1116,1260,1315,1496,1697,1784,2017,2117,2398,900,925,997,1031,1120,1100,900,750,540,450,385,650,530,510,515)
Wolf <- c(20,22,22,23,20,26,28,26,22,22,17,18,20,23,24,31,41,44,34,40,43,50,30,14,23,24,22,20,16,12,12,15,12,12,13,17,16,22,24,14,
          25,29,19,17,19,29,30,30,21,23,24,19,16)
ElchWolf <- cbind(time=Jahr,x=Elch,y=Wolf)
# Illustration
plot.Lsg(ElchWolf)

# Scaling the population size of the wolves by a factor of 50 so that something can be recognized in comparison
ElchWolf.scaled <- cbind(time=Jahr,x=Elch,y=50*Wolf)
plot.Lsg(ElchWolf.scaled)

