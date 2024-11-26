################################################################################
# R script sysstat24_11_en.r for the practice "Statistical Analysis of Systems"
#####

####################
# Additional package for differential equations (DGL) (provides the function 'lsoda' required in the following):
###
if(!require(deSolve)) {install.packages("deSolve"); require(deSolve)} # load installed package and install beforehand if necessary

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

#############
# Task 1
###

###  Lotka-Voltera model
times <- seq(0,30,by=0.01) # Points in time at which the solution is to be determined
f <- function(x,y,a,b,c,d) c(dx = (a - b*y)*x , dy= (-c + d*x)*y) # x = prey, y = predator, f describes the differential equation
param <- c(a=1,b=1,c=1,d=1) # model parameters
start <- c(x=2,y=1) # initial values
dgl <- DGL.Lsg(times=times, f=f, param=param, start=start) # numerical solution of the differential equation
plot.Lsg(dgl) # Illustration of the solution
# The graph on the bottom right shows the progression of the vector (B(t),R(t)) over time, the starting point (B(0),R(0)) is marked as a 
# B(t) starts at 2 and then falls initially, R(t) starts at 1 and then grows initially,
# therefore, the red curve will run counterclockwise, similar to (cos(t),sin(t)).

## Change the starting values (previously at time t=0 value 2 for the prey and value 1 for the predators)
start <- c(x=4,y=1) # more prey
dgl <- DGL.Lsg(times=times, f=f, param=param, start=start)
plot.Lsg(dgl)

start <- c(x=2,y=2) # more predators
dgl <- DGL.Lsg(times=times, f=f, param=param, start=start)
plot.Lsg(dgl)

# Due to the cyclical behavior, (B(t),R(t)) always reaches the starting point (B(0),R(0)) (marked here by a black dot).
# B=4, R=1
dgl <- as.data.frame(DGL.Lsg(times=times, f=f, param=param, start=c(x=4,y=1)))
plot(dgl$x, dgl$y, col=2, xlim=c(0, max(dgl$x)), ylim=c(0, max(dgl$y)), type="l", main="Predator vs. prey", xlab="B", ylab="R")
points(dgl$x[1], dgl$y[1], col="black",pch=16,cex=1.2)
# B=2, R=1
dgl <- as.data.frame(DGL.Lsg(times=times, f=f, param=param, start=c(x=2,y=1)))
lines(dgl$x, dgl$y, col=2)
points(dgl$x[1], dgl$y[1], col="black",pch=16,cex=1.2)
# B=2, R=2
dgl <- as.data.frame(DGL.Lsg(times=times, f=f, param=param, start=c(x=2,y=2)))
lines(dgl$x, dgl$y, col=2)
points(dgl$x[1], dgl$y[1], col="black",pch=16,cex=1.2)
# B=1.2, R=1
dgl <- as.data.frame(DGL.Lsg(times=times, f=f, param=param, start=c(x=1.2,y=1)))
lines(dgl$x, dgl$y, col=2)
points(dgl$x[1], dgl$y[1], col="black",pch=16,cex=1.2)
# B=1, R=1
dgl <- as.data.frame(DGL.Lsg(times=times, f=f, param=param, start=c(x=1,y=1)))
lines(dgl$x, dgl$y, col=2)
points(dgl$x[1], dgl$y[1], col="black",pch=16,cex=1.2)
# The last “curve”, which consists only of the point (1,1), is just the case where R(t)=a/b and B(t)=c/d are constant over the
# are constant over the entire time, i.e. there is a state of equilibrium, see also the following diagram:
start <- c(x=1,y=1) # Gleichgewichtszustand
dgl <- DGL.Lsg(times=times, f=f, param=param, start=start)
plot.Lsg(dgl)
# Another (trivial) equilibrium state would be B(t)=0, R(t)=0, but this is extremely boring as there is neither prey nor predator.

## Other parameters
param <- c(a=1,b=2,c=1,d=1)
# B=1, R=2
start <- c(x=1,y=2) 
dgl <- DGL.Lsg(times=times, f=f, param=param, start=start)
plot.Lsg(dgl)
# B=1, R=1
start <- c(x=1,y=1) # more prey
dgl <- DGL.Lsg(times=times, f=f, param=param, start=start)
plot.Lsg(dgl)
# B=1, R=0.5
start <- c(x=1,y=0.5) # more predators
dgl <- DGL.Lsg(times=times, f=f, param=param, start=start)
plot.Lsg(dgl)

# B=1, R=2
dgl <- as.data.frame(DGL.Lsg(times=times, f=f, param=param, start=c(x=1,y=2)))
plot(dgl$x, dgl$y, col=2, xlim=c(0, max(dgl$x)), ylim=c(0, max(dgl$y)), type="l", main="Predator vs. prey", xlab="B", ylab="R")
points(dgl$x[1], dgl$y[1], col="black",pch=16,cex=1.2)
# B=1, R=1
dgl <- as.data.frame(DGL.Lsg(times=times, f=f, param=param, start=c(x=1,y=1)))
lines(dgl$x, dgl$y, col=2)
points(dgl$x[1], dgl$y[1], col="black",pch=16,cex=1.2)
# B=1, R=0.5
dgl <- as.data.frame(DGL.Lsg(times=times, f=f, param=param, start=c(x=1,y=0.5)))
lines(dgl$x, dgl$y, col=2)
points(dgl$x[1], dgl$y[1], col="black",pch=16,cex=1.2)

## Try out other parameter combinations by changing the vector 'param' and then using the above commands
## to create corresponding representations.

### The following situations are formally included in the Lotka-Volterra model, if one allows individual parameters to become zero.
## What happens if the prey does not reproduce?  (Parameter a=0)
param <- c(a=0,b=1,c=1,d=1)
start <- c(x=1,y=1) 
dgl <- DGL.Lsg(times=times, f=f, param=param, start=start)
plot.Lsg(dgl)
#
start <- c(x=2,y=1) 
dgl <- DGL.Lsg(times=times, f=f, param=param, start=start)
plot.Lsg(dgl)
#
start <- c(x=10,y=1) 
dgl <- DGL.Lsg(times=times, f=f, param=param, start=start)
plot.Lsg(dgl)

## What happens if the predators do not die?  (Parameter c=0)
param <- c(a=1,b=1,c=0,d=1)
start <- c(x=1,y=1) 
dgl <- DGL.Lsg(times=times, f=f, param=param, start=start)
plot.Lsg(dgl)

## What happens if there is no interaction at all? (Parameter b=d=0)
times <- seq(0,2,by=0.01)
param <- c(a=1,b=0,c=1,d=0)
start <- c(x=1,y=2) 
dgl <- DGL.Lsg(times=times, f=f, param=param, start=start)
plot.Lsg(dgl)
# Prey: exponential growth B(t)=B(0)*exp(a*t)
# Predators: exponential decline R(t)=R(0)*exp(-c*t)


#############
# Task 2
###

### Extended Lotka-Volterra model
times <- seq(0,40,by=0.01)
f2 <- function(x,y,a,b,c,d,e,f) c(dx = (a - b*y - e*x)*x, dy= (-c + d*x - f*y)*y) 
param <- c( a=1, b=1, c=1, d=1, e=0.1, f=0.1 )
start <- c(x=2,y=1) 
dgl <- DGL.Lsg(times=times, f=f2, param=param, start=start)
plot.Lsg(dgl)

start <- c(x=5,y=1) 
dgl <- DGL.Lsg(times=times, f=f2, param=param, start=start)
plot.Lsg(dgl)

start <- c(x=1,y=5) 
dgl <- DGL.Lsg(times=times, f=f2, param=param, start=start)
plot.Lsg(dgl)

start <- c(x=5,y=5) 
dgl <- DGL.Lsg(times=times, f=f2, param=param, start=start)
plot.Lsg(dgl)
# -> convergence to a known equilibrium state that is independent of the initial values
c(param[6]*param[1]+param[2]*param[3],param[4]*param[1]-param[5]*param[3])/(param[5]*param[6]+param[2]*param[4])


###  Schäfer model (based on the logistic equation)
times <- seq(0,10,by=0.01)
f3 <- function(x,r,K,s) dx = (r * ( 1 - x/K ) - s) * x
param <- c( r = 3, K = 4, s = 1 )
start <- c(x=0.01)
dgl <- DGL.Lsg(times=times, f=f3, param=param, start=start)
plot.Lsg(dgl,ylim=c(0,4))

start <- c(x=1)
dgl <- DGL.Lsg(times=times, f=f3, param=param, start=start)
plot.Lsg(dgl,add=T,col=2)

start <- c(x=8/3)
dgl <- DGL.Lsg(times=times, f=f3, param=param, start=start)
plot.Lsg(dgl,add=T,col=3)

start <- c(x=4)
dgl <- DGL.Lsg(times=times, f=f3, param=param, start=start)
plot.Lsg(dgl,add=T,col=5)

# The population size changes to a stable value of K*(1-s/r):
param[2]*(1-param[3]/param[1])
# If this value is selected as the starting value, the population size remains constant.

# s=0, i.e. without removal
param <- c( r = 3, K = 4, s = 0 )
start <- c(x=0.01)
dgl <- DGL.Lsg(times=times, f=f3, param=param, start=start)
plot.Lsg(dgl,add=T,col=1)

# Assume that B is the fish population of a body of water that can only tolerate a maximum size K
# is tolerated. The reproduction rate is r. If no fish were caught (s=0), the size of the fish population would be constant at K
# would be constant at K. One could ask how large the catch rate s must be in order to obtain a maximum yield s*B 
# can be obtained. The following plot shows the growth B' (without catch) for a given size B.
xx <- seq(0,param[2],length=101)
plot(xx,sapply(xx,f3,r=param[1],K=param[2],s=0),type="l",col=4,xlab="B",ylab="B'",ylim=c(0,3))
# So if you choose exactly the value B' for the catch s*B (points on the blue curve) for a given B, you get 
# exactly the increase. The maximum quantity results for B=K/2. The population size must therefore be kept at this stable value.
# size, which we know to be equal to K*(1-s/r) (see above). So K/2=K*(1-s/r) must apply,
# from which s=r/2 follows. The catch in this case is then s*B=r/2 * K/2= 0.25*r*K.
points(0.5*param[2],0.25*param[2]*param[1])
lines(c(0.5*param[2],0.5*param[2]),c(0,0.25*param[2]*param[1]),lty=2)
lines(c(-1,0.5*param[2]),c(0.25*param[2]*param[1],0.25*param[2]*param[1]),lty=2)

param <- c( r = 3, K = 4, s = 1.5 )
param[2]*(1-param[3]/param[1]) # Size of the stable population

# s=r/2, i.e. optimal removal
param <- c( r = 3, K = 4, s = 1.5 )
start <- param[2]*(1-param[3]/param[1])
names(start)<-"x"
dgl <- DGL.Lsg(times=times, f=f3, param=param, start=start)
plot.Lsg(dgl,col=4,ylim=c(0,4))

# However, the equilibrium would of course also occur if the body of water was previously unfished
# and the correct catch rate s=r/2 is selected.
start <- c(x=4)
dgl <- DGL.Lsg(times=times, f=f3, param=param, start=start)
plot.Lsg(dgl,add=T,col=2)


#############
# Task 3
###

### initially Schäfer model with random disturbance:
times <- seq(0,10,by=0.01)
param <- c( r = 3, K = 4, s = 1 )

## Comparison of solution variants:
start <- c(x=0.01)
# with R package
f3 <- function(x,r,K,s) dx = (r * ( 1 - x/K ) - s) * x
dgl <- DGL.Lsg(times=times, f=f3, param=param, start=start)
plot.Lsg(dgl)
# Theoretical solution (see task sheet):
f3th <- function(t, param, start) param[2]*(1-param[3]/param[1])/(1+exp(-(param[1]-param[3])*t)*(param[2]/start*(1-param[3]/param[1])-1))
lines(times,f3th(times,param,start),col=2)
# Simplified own numerical solution:
v <- start
for(i in 2:length(times)) v<-c(v,v[i-1]+times[2]*f3(v[i-1],param[1],param[2],param[3]))
lines(times,v,col=3)
# -> can trust their own simplified solution variant in the following and thus integrate the random disturbances
#    which is not possible with the R package

# Own numerical solution with small random disturbance, small initial value:
start <- c(x=0.01)
sig <- 0.1
plot(NA,NA,xlim=range(times),ylim=c(0,3),xlab="t",ylab="B(t)")
for(j in 1:5) {
  v <- start
  for(i in 2:length(times)) v<-c(v,v[i-1]+times[2]*f3(v[i-1],param[1],param[2],param[3])+v[i-1]*rnorm(1,sd=sig*sqrt(times[2])))
  lines(times,pmax(0,v),col=2+j)
}
# Theoretical solution without disturbance:
lines(times,f3th(times,param,start),col=2,lwd=2)

# Own numerical solution with small random disturbance, larger initial value:
start <- c(x=1)
sig <- 0.1
plot(NA,NA,xlim=range(times),ylim=c(0,3),xlab="t",ylab="B(t)")
for(j in 1:5) {
  v <- start
  for(i in 2:length(times)) v<-c(v,v[i-1]+times[2]*f3(v[i-1],param[1],param[2],param[3])+v[i-1]*rnorm(1,sd=sig*sqrt(times[2])))
  lines(times,pmax(0,v),col=2+j)
}
# Theoretical solution without disturbance:
lines(times,f3th(times,param,start),col=2,lwd=2)

# Own numerical solution with larger random disturbance, larger initial value:
start <- c(x=1)
sig <- 0.5
plot(NA,NA,xlim=range(times),ylim=c(0,4),xlab="t",ylab="B(t)")
for(j in 1:5) {
  v <- start
  for(i in 2:length(times)) v<-c(v,v[i-1]+times[2]*f3(v[i-1],param[1],param[2],param[3])+v[i-1]*rnorm(1,sd=sig*sqrt(times[2])))
  lines(times,pmax(0,v),col=2+j)
}
# Theoretical solution without disturbance:
lines(times,f3th(times,param,start),col=2,lwd=2)

# Own numerical solution with even greater random disturbance, larger initial value:
start <- c(x=1)
sig <- 1
plot(NA,NA,xlim=range(times),ylim=c(0,4),xlab="t",ylab="B(t)")
for(j in 1:5) {
  v <- start
  for(i in 2:length(times)) v<-c(v,v[i-1]+times[2]*f3(v[i-1],param[1],param[2],param[3])+v[i-1]*rnorm(1,sd=sig*sqrt(times[2])))
  lines(times,pmax(0,v),col=2+j)
}
# Theoretical solution without disturbance:
lines(times,f3th(times,param,start),col=2,lwd=2)


### Lotka-Volterra model with random fluctuations

# own approximate numerical solution
DGL.Lsg.Zufall <- function(times,f,param,start,sig) {
  v <- matrix(start,nrow=1)
  for(i in 2:length(times)) v<-rbind(v,v[i-1,]+times[2]*f(v[i-1,1],v[i-1,2],param[1],param[2],param[3],param[4])+v[i-1,]*rnorm(2,sd=sig*sqrt(times[2])))
  return(cbind(time=times,x=v[,1],y=v[,2]))
}

times <- seq(0,30,by=0.001)
f <- function(x,y,a,b,c,d) c(dx = (a - b*y)*x , dy= (-c + d*x)*y) # x = prey, y = predator
param <- c(a=1,b=1,c=1,d=1 )
start <- c(x=2,y=1) 

# for verification of 'DGL.Lsg.Zufall' solution with R package
dgl <- DGL.Lsg(times=times, f=f, param=param, start=start)
plot.Lsg(dgl)
# and for comparison own solution in the deterministic case, i.e. without random disturbance
sig<-0.0
dglz <- DGL.Lsg.Zufall(times=times, f=f, param=param, start=start, sig=sig)
plot.Lsg(dglz)
# -> can again apply the simplified solution variant for the case of random disturbances

## Now examples with random disturbance:
# Very small disturbance
sig<-0.01
# one realization
dglz <- DGL.Lsg.Zufall(times=times, f=f, param=param, start=start, sig=sig)
# Wait ...
plot.Lsg(dglz)
# another realization
dglz <- DGL.Lsg.Zufall(times=times, f=f, param=param, start=start, sig=sig)
# Wait ...
plot.Lsg(dglz)

# Small disturbance
sig<-0.1
# one realization
dglz <- DGL.Lsg.Zufall(times=times, f=f, param=param, start=start, sig=sig)
# Wait ...
plot.Lsg(dglz)
# another realization
dglz <- DGL.Lsg.Zufall(times=times, f=f, param=param, start=start, sig=sig)
# Wait ...
plot.Lsg(dglz)
# -> Results are no longer completely periodic, but presumably more realistic.


### Realizations of the Wiener process/the Brownian motion
times <- seq(0,10,by=0.001)
start <- c(x=0)
plot(NA,NA,xlim=range(times),ylim=c(-3,3)*sqrt(max(times)),xlab="t",ylab="W(t)")
for(j in 1:10) {
  v <- start
  for(i in 2:length(times)) v<-c(v,v[i-1]+rnorm(1,sd=sqrt(times[2])))
  lines(times,v,col=1+j)
}
