################################################################################
# R script sysstat24_07_en.r for the practice "Statistical Analysis of Systems"
#####

library(spatstat)    # Load the R package 'spatstat'

# Summary of several plot commands for the comparison of certain
# structural functions of a point pattern with those of the homogeneous Poisson process
plotDiagnostics <- function(X,Xname=deparse(substitute(X))) {
  oldpar <- par(no.readonly = TRUE)
  layout(matrix(1:4,2))
  par(mar=rep(2, 4))
  plot(X,main=Xname)
  par(mar=c(4,4,2,2))
  plot(Gest(X,correction="han"),main="G-function")
  plot(Fest(X,correction="km"),main="F-function")
  plot(Lest(X,correction="trans"),main="L-function")
  par(oldpar)
  layout(1)
}

#############
# Some point process models
###


##########
# Poisson cluster processes
##########

layout(matrix(1:4,2,byrow=T))
oldpar <- par(no.readonly = TRUE)
par(mar=rep(2, 4))		# reduces the white margin on the following plots

### Matern cluster process

# modified kappa = average number of clusters per unit area
plot(rMatClust(kappa=10, r=0.1, mu=10))
plot(rMatClust(kappa=20, r=0.1, mu=10))
plot(rMatClust(kappa=50, r=0.1, mu=10))
plot(rMatClust(kappa=100, r=0.1, mu=10))
# modified r = cluster radius
plot(rMatClust(kappa=10, r=0.1, mu=10))
plot(rMatClust(kappa=10, r=0.02, mu=10))
plot(rMatClust(kappa=10, r=0.05, mu=10))
plot(rMatClust(kappa=10, r=0.5, mu=10))
# modified mu = average number of points in a cluster
plot(rMatClust(kappa=10, r=0.1, mu=10))
plot(rMatClust(kappa=10, r=0.1, mu=20))
plot(rMatClust(kappa=10, r=0.1, mu=50))
plot(rMatClust(kappa=10, r=0.1, mu=2))

# Comparison with characteristics of the hom. Poisson process
X <- rMatClust(kappa=10, r=0.1, mu=10)
plotDiagnostics(X)

### Thomas process
layout(matrix(1:4,2,byrow=T))

# modified kappa = average number of clusters per unit area
plot(rThomas(kappa=10, sigma=0.2, mu=5))
plot(rThomas(kappa=20, sigma=0.2, mu=5))
plot(rThomas(kappa=50, sigma=0.2, mu=5))
plot(rThomas(kappa=100, sigma=0.2, mu=5))
# modified standard deviation of the positions in the cluster
plot(rThomas(kappa=10, sigma=0.2, mu=5))
plot(rThomas(kappa=10, sigma=0.05, mu=5))
plot(rThomas(kappa=10, sigma=0.01, mu=5))
plot(rThomas(kappa=10, sigma=0.5, mu=5))
# modified mu = average number of points in a cluster
plot(rThomas(kappa=20, sigma=0.05, mu=5))
plot(rThomas(kappa=20, sigma=0.05, mu=10))
plot(rThomas(kappa=20, sigma=0.05, mu=20))
plot(rThomas(kappa=20, sigma=0.05, mu=50))

# Comparison with characteristics of the hom. Poisson process
X <- rThomas(kappa=10, sigma=0.2, mu=5)
plotDiagnostics(X)

### Gauss-Poisson process
layout(matrix(1:4,2,byrow=T))

# modified r = distance between points in a cluster of two
plot(rGaussPoisson(kappa=30, r=0.01, p2=0.5))
plot(rGaussPoisson(kappa=30, r=0.05, p2=0.5))
plot(rGaussPoisson(kappa=30, r=0.15, p2=0.5))
plot(rGaussPoisson(kappa=30, r=0.3, p2=0.5))
# modified p2 = probability that parent point is replaced by a cluster of two
plot(rGaussPoisson(kappa=50, r=0.05, p2=0.1))
plot(rGaussPoisson(kappa=50, r=0.05, p2=0.3))
plot(rGaussPoisson(kappa=50, r=0.05, p2=0.7))
plot(rGaussPoisson(kappa=50, r=0.05, p2=0.9))

# Comparison with characteristics of the hom. Poisson process
X <- rGaussPoisson(kappa=30, r=0.01, p2=0.5)
plotDiagnostics(X)

##########
# Cox processes
##########

### Mixed Poisson process
layout(matrix(1:4,2))

# exponentially distributed random lambda
for(i in 1:4) plot(rpoispp(lambda=rexp(1, 1/100)),main=paste("Realization",i))

# discrete uniformly distributed random lambda
for(i in 1:4) plot(rpoispp(lambda=sample(c(50,100,150,200),1)),main=paste("Realization",i))


### Log-Gaussian Cox process
layout(1)
par(oldpar)
X <- rLGCP("gauss", mu=5, var=2, scale=0.1)

# Display of the random intensity function used
plot(log(attr(X, "Lambda")),col=terrain.colors(120),main="Points + logarithmized intensity function") # logarithmized intensity function
points(X)

plot(attr(X, "Lambda"),col=terrain.colors(120),main="Points + intensity function") # intensity function
points(X)

# Comparison with characteristics of the hom. Poisson process
plotDiagnostics(X)

### Random set-generated Cox process

# Function for simulation (of a specific example)
rRACSgenCox <- function(lam0,lam1,rho=20,R=0.1,win=owin(c(0,1),c(0,1))) {
  win0 <- owin(win$xrange+c(-R,R),win$yrange+c(-R,R))
  Y <- rpoispp(lambda=rho,win=win0)
  f <- function(x,y) {
    racs<-apply(outer(Y$x,x,"-")^2+outer(Y$y,y,"-")^2,2,min)<=R^2
    return(lam1*racs+lam0*(1-racs))
  }
  xx <- seq(win$xrange[1],win$xrange[2],len=201)
  yy <- seq(win$yrange[1],win$yrange[2],len=201)
  zz <- outer(xx,yy,f)
  X <- rpoispp(lambda=f,lmax=max(c(lam0,lam1)))
  res <- c()
  res$X <- X
  res$xx <- xx
  res$yy <- yy
  res$zz <- zz
  return(res)
}

Y <- rRACSgenCox(lam0=500,lam1=5000)
X <- Y$X
plot(X)

# Display of the random intensity function used
contour(Y$xx,Y$yy,Y$zz,xlab="x",ylab="y")
image(Y$xx,Y$yy,Y$zz,xlab="x",ylab="y",col=c(0,8))
plot(X,add=TRUE)

# Comparison with characteristics of the hom. Poisson process
plotDiagnostics(X)

##########
# Hard-core processes
##########

### Matern I process

# Point patterns for various kappa
par(mfcol=c(3,2),mar=rep(1, 4))
plot(rMaternI(kappa=10, r=0.05))
plot(rMaternI(kappa=50, r=0.05))
plot(rMaternI(kappa=100, r=0.05))
plot(rMaternI(kappa=200, r=0.05))
plot(rMaternI(kappa=500, r=0.05))
plot(rMaternI(kappa=1000, r=0.05))

# Dependence of the theoretical intensity of the Matern I process on kappa
par(mfrow=c(1,1),mar=rep(4, 4))
lamI <- function(kappa,r) kappa*exp(-kappa*pi*r^2)
kappa <- 0:1000
plot(kappa,lamI(kappa,r=0.05),type="l",xlab=expression(kappa),ylab=expression(lambda[I]),main="Intensity Matern I")

# Comparison with characteristics of the hom. Poisson process
X <- rMaternI(kappa=100, r=0.05)
plotDiagnostics(X)

### Matern II process

# Point patterns for various kappa
par(mfcol=c(3,2),mar=rep(1, 4))
plot(rMaternII(kappa=10, r=0.05))
plot(rMaternII(kappa=50, r=0.05))
plot(rMaternII(kappa=100, r=0.05))
plot(rMaternII(kappa=200, r=0.05))
plot(rMaternII(kappa=500, r=0.05))
plot(rMaternII(kappa=1000, r=0.05))

# Dependence of the theoretical intensity of the Matern I process on kappa
par(mfrow=c(1,1),mar=rep(4, 4))
lamII <- function(kappa,r) (1-exp(-kappa*pi*r^2))/pi/r^2
kappa <- 0:1000
plot(kappa,lamII(kappa,r=0.05),type="l",xlab=expression(kappa),ylab=expression(lambda[II]),main="Intensity Matern II")
lines(kappa,rep(1,length(kappa))/pi/0.05^2,lty=2,col=2) # upper limit

# Comparison with characteristics of the hom. Poisson process
X <- rMaternII(kappa=100, r=0.05)
plotDiagnostics(X)

### "Simple sequential inhibition" process (Matern III process)

# Point patterns for various n
par(mfcol=c(3,2),mar=rep(1, 4))
plot(rSSI(r=0.05, n=25))
plot(rSSI(r=0.05, n=50))
plot(rSSI(r=0.05, n=75))
plot(rSSI(r=0.05, n=100))
plot(rSSI(r=0.05, n=150))
plot(rSSI(r=0.05, n=200))

# Comparison with characteristics of the hom. Poisson process
X <- rSSI(r=0.05, n=100)
plotDiagnostics(X)



#############
# Prediction ranges/envelopes
###

par(oldpar)
layout(1)

# pointwise prediction envelope, scattering of empirical characteristics (CSR=complete spatial randomness=hom. Poisson process)
E<-envelope(cells,fun=Lest,nsim=39,nrank=1,savefuns=TRUE)  # determines from 39 realizations of a hom. Poisson process the emp. L-fct.
plot(E)                                                    # illustration of the pointwise prediction envelopes for alpha=0.05
# The black curve is the estimated L-function of the data set 'cells'.

# Contribution of the individual curves to the pointwise prediction envelope:
for(i in 1:39) lines(E$r,attributes(E)$simfuns[[i+1]],col=4) 

E<-envelope(cells,fun=Lest,nsim=199,nrank=5,savefuns=TRUE)  # determines from 199 realizations of a hom. Poisson process the emp. L-fct.
plot(E)                                                     # illustration of the pointwise prediction envelope for alpha=0.05
# Contribution of the individual curves to the pointwise prediction range:
for(i in 1:199) lines(E$r,attributes(E)$simfuns[[i+1]],col=4) # now some curves also outside the prediction envelope, 
# as the 5th smallest/largest value (nrank=5) is used for n=199.
lines(E$r,E$lo,col=2)                                         # the two envelopes again for comparison
lines(E$r,E$hi,col=2)

# global prediction envelope
EE<-envelope(cells,fun=Lest,nsim=199,nrank=5,global=TRUE)   # determines from 199 realizations of a hom. Poisson process the emp. L-fct.
plot(EE)                                                    # illustration of the global prediction envelopes for alpha=0.05
# The homogeneous Poisson process is therefore obviously not a good model for 'cells'.


# Simulation example
# possibly also for different realizations (i.e. several times)
X <- rpoispp(200) # These are our data (realization of a homogeneous Poisson process of intensity 200). 
# We test the null hypothesis that X originates from a hom. Poisson process with ...

# ... the G-function
plot(envelope(X,fun=Gest,nsim=199,nrank=5,global=FALSE)) # pointwise prediction envelope
plot(envelope(X,fun=Gest,nsim=199,nrank=5,global=TRUE))  # global prediction envelope

# ... the L-function
plot(envelope(X,fun=Lest,nsim=199,nrank=5,global=FALSE)) # pointwise prediction envelope
plot(envelope(X,fun=Lest,nsim=199,nrank=5,global=TRUE))  # global prediction envelope

# ... the F-function
plot(envelope(X,fun=Fest,nsim=199,nrank=5,global=FALSE)) # pointwise prediction envelope
plot(envelope(X,fun=Fest,nsim=199,nrank=5,global=TRUE))  # global prediction envelope

# back to the data set cells:
minnndist(cells) # smallest distance between two points

# Dataset cells, null hypothesis: cells can be explained by the Matern II process
# for selected parameter values kappa=116.6, r=0.0836 
# G-function
plot(envelope(cells,fun=Gest,simulate=expression(rMaternII(kappa=116.6,r=0.0836,win=cells$win)),nsim=199,nrank=5,global=FALSE)) # pointwise prediction envelope
plot(envelope(cells,fun=Gest,simulate=expression(rMaternII(kappa=116.6,r=0.0836,win=cells$win)),nsim=199,nrank=5,global=TRUE))  # global prediction envelope

# L-function
plot(envelope(cells,fun=Lest,simulate=expression(rMaternII(kappa=116.6,r=0.0836,win=cells$win)),nsim=199,nrank=5,global=FALSE)) # pointwise prediction envelope
plot(envelope(cells,fun=Lest,simulate=expression(rMaternII(kappa=116.6,r=0.0836,win=cells$win)),nsim=199,nrank=5,global=TRUE))  # global prediction envelope

# F-function
plot(envelope(cells,fun=Fest,simulate=expression(rMaternII(kappa=116.6,r=0.0836,win=cells$win)),nsim=199,nrank=5,global=FALSE)) # pointwise prediction envelope
plot(envelope(cells,fun=Fest,simulate=expression(rMaternII(kappa=116.6,r=0.0836,win=cells$win)),nsim=199,nrank=5,global=TRUE))  # global prediction envelope


# Dataset cells, null hypothesis: cells can be explained by the SSI Process/Matern III process
# selected parameter values: cells contains 42 points, minimum distance like for Matern II
# G-function
plot(envelope(cells,fun=Gest,simulate=expression(rSSI(r=0.0836,n=42,win=cells$win)),nsim=199,nrank=5,global=FALSE)) # pointwise prediction envelope
plot(envelope(cells,fun=Gest,simulate=expression(rSSI(r=0.0836,n=42,win=cells$win)),nsim=199,nrank=5,global=TRUE))  # global prediction envelope

# L-function
plot(envelope(cells,fun=Lest,simulate=expression(rSSI(r=0.0836,n=42,win=cells$win)),nsim=199,nrank=5,global=FALSE)) # pointwise prediction envelope
plot(envelope(cells,fun=Lest,simulate=expression(rSSI(r=0.0836,n=42,win=cells$win)),nsim=199,nrank=5,global=TRUE))  # global prediction envelope

# F-function
plot(envelope(cells,fun=Fest,simulate=expression(rSSI(r=0.0836,n=42,win=cells$win)),nsim=199,nrank=5,global=FALSE)) # pointwise prediction envelope
plot(envelope(cells,fun=Fest,simulate=expression(rSSI(r=0.0836,n=42,win=cells$win)),nsim=199,nrank=5,global=TRUE))  # global prediction envelope

# Conclusion: The point pattern cells is obviously based on a different random mechanism than that given by a homogeneous Poisson process, 
# a Matern II process or an SSI process.