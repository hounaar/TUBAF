################################################################################
# R script sysstat24_07_en.r for the practice "Statistical Analysis of Systems"
#####

require(spatstat)     # Load the R package 'spatstat'

#############
# Task 1
###

###
# several point patterns of the same intensity
###

oldpar <- par()                  # Save old graphics parameters
par(mar=rep(2.0, 4))		         # reduces the white border on the following plots

w <- owin(c(0,2),c(0,2))
Z1<-rMatClust(5, 0.1, 20,win=w)  # strongly clustered 
plot(Z1)
Z2<-rMatClust(20, 0.1, 5,win=w)  # weakly clustered
plot(Z2)
Z3<-rpoispp(100,win=w)           # homogeneous Poisson process
plot(Z3)
Z4<-rSSI(400,r=0.06,win=w)       # repulsive, 'hardcore'
plot(Z4)
par(mar=oldpar$mar)              # Restore old graphics parameters

###
# Distribution function of the distance to the nearest neighbor/ G-function
###
r <- seq(0,0.2,by=0.001)            # arguments for G(r)
lambda <- 100                       # intensity

X <- rpoispp(lambda)                # a realization of a Poisson process
G0 <- Gest(X,r=r,correction="none") # naive estimator (without edge correction)
G0$raw                              # values of the naive estimator
G0$theo                             # values of the theoretical function of the Poisson process
# Representation of the estimate ("raw") together with the theoretical fct. of a Poisson process of the same estimated intensity
plot(G0,main="Nearest-neighbor-distance distribution function")                           

### Simulation study on the mean behavior of the naive estimator
G0all <- c()
for(i in 1:200) {
  X <- rpoispp(lambda)                # Generate a realization of the hom. Poisson process,
  G0 <- Gest(X,r=r,correction="none") # estimate the G-function from this
  G0all <- rbind(G0all,G0$raw)        # and add this as line G0all.
}
G0$raw <- apply(G0all,2,mean)         # Determine the mean value for each column (=2)
# Instead of a single estimate, G0$raw now contains an average value over 200 realizations
plot(G0, xlim=c(0,max(r)),main="Nearest-neighbor-distance distribution function") # Plot

GG <- Gest(X,r=r,correction=c("none","rs","km","han"))	# with edge corrections
plot(GG, xlim=c(0,max(r)),main="Nearest-neighbor-distance distribution function") # Display with and without edge correction

### Illustration of the need for edge correction
X <- rpoispp(100)
plot(X,pch=19,main="Distances to the nearest neighbor")
# Representation of the distances to the nearest neighbor in the form of circles
apply(cbind(X$x,X$y,nndist(X)),1,function(point)symbols(x = point[1],y = point[2],circles = point[3],fg=2,add=T,inches = F))
# -> Points with circles that intersect the edge could also have a smaller distance
# to the nearest neighbor, as there could be an unobserved but closer point outside the window.

### Help for 'Gest'
?Gest

### Simulation study on the mean behavior of all estimators
Graw <- c()
Gkm <- c()
Ghan <- c()
Grs <- c()
for(i in 1:200) {
  X <- rpoispp(lambda)           # Generate a realization of the hom. Poisson process,
  GG <- Gest(X,r=r,correction=c("none","rs","km","han")) # estimate from this the G-function for several types of edge correction.
  Graw <- rbind(Graw,GG$raw)
  Gkm <- rbind(Gkm, GG$km)
  Ghan <- rbind(Ghan, GG$han)
  Grs <- rbind(Grs, GG$rs)
}
GG$raw <- apply(Graw,2,mean)     # GG$raw now contains average values over 200 simulations
GG$km <- apply(Gkm,2,mean)       # likewise
GG$han <- apply(Ghan,2,mean)     # likewise
GG$rs <- apply(Grs,2,mean)       # likewise
plot(GG, xlim=c(0,max(r)),main="Nearest-neighbor-distance distribution function") # Plot of all averaged estimators 
# Conclusion: Although the curves of the averaged estimators deviate from the theoretical curve (G_pois),  
#             but much more so for the estimator without boundary correction.

###
# Spherical contact distribution function/ F-function
###
r <- seq(0,0.2,by=0.001)        # arguments for F(r)
lambda <- 100                   # intensity

X <- rpoispp(lambda)            # a realization of a Poisson process
F0 <- Fest(X,r=r,correction="none") # naive estimator (without edge correction)
F0$raw                          # values of the naive estimator
F0$theo                         # values of the theoretical function of the Poisson process
# Representation of the estimate ("raw") together with the theoretical fct. of a Poisson process of the same estimated intensity
plot(F0,main="Spherical contact distribution function")                        

### Simulation study on the mean behavior of the naive estimator
F0all <- c()
for(i in 1:200) {
  X <- rpoispp(lambda)
  F0 <- Fest(X,r=r,correction="none")
  F0all <- rbind(F0all,G0$raw)
}
F0$raw <- apply(F0all,2,mean)   # Instead of a single estimate
# F0$raw now contains an average value over 200 realizations
plot(F0, xlim=c(0,max(r)),main="Spherical contact distribution function") # Plot

FF <- Fest(X,r=r,correction=c("none","rs","km","cs"))	# with edge corrections
plot(FF, xlim=c(0,max(r)),main="Spherical contact distribution function") # Display with and without edge correction

### Help for 'Fest'
?Fest

### Simulation study on the mean behavior of all estimators
Fraw <- c()
Fkm <- c()
Fcs <- c()
Frs <- c()
for(i in 1:1000) {        # 10000 simulations show the effect better,
  X <- rpoispp(lambda)    # but take too long.
  FF <- Fest(X,r=r,correction=c("none","rs","km","cs"))
  Fraw <- rbind(Fraw,FF$raw)
  Fkm <- rbind(Fkm, FF$km)
  Fcs <- rbind(Fcs, FF$cs)
  Frs <- rbind(Frs, FF$rs)
} # wait ...
FF$raw <- apply(Fraw,2,mean)    # as above ...
FF$km <- apply(Fkm,2,mean)
FF$cs <- apply(Fcs,2,mean)
FF$rs <- apply(Frs,2,mean)
plot(FF, xlim=c(0,max(r)),main="Spherical contact distribution function") # Plot of all averaged estimators
# Conclusion: Although the curves of the averaged estimators deviate from the theoretical curve (F_pois), 
#             this deviation is much stronger for the estimator without boundary correction. 
#             (It is possible that because of the only 1000 simulations, this statement may not apply in some cases). 

#############
# Task 2
###

###
# L-function
###

r <- seq(0,0.25,by=0.001)  # arguments for L(r)
lambda <- 100              # intensity

X <- rpoispp(lambda)       # a realization of a Poisson process
L0 <- Lest(X,r=r,correction="none") # naive estimator (without edge correction)
plot(L0,main="L-function") # Representation of the estimate ("un")
# together with the theoretical fct. of a Poisson process of the same estimated intensity

LL <- Lest(X,r=r,correction=c("none","border","trans","iso"))	
plot(LL,main="L-function")

# Execute the next three commands several times in succession to see
# how the estimate of L scatters for different realizations.
X <- rpoispp(lambda)
LL <- Lest(X,r=r)	
plot(LL,main="L-function")

###
# Pair correlation function
###

r <- seq(0,0.2,by=0.001)   # arguments for g(r)
lambda <- 100              # intensity
X <- rpoispp(lambda)	

# with rectangular kernel, bandwidth 0.01
gg <- pcf(X, kernel="rectangular", bw=0.01)
plot(gg,main="Pair correlation function")
# The pole at r=0 can occur with certain point patterns, but here it is a
# so-called "artifact", as g(r)=1 applies to the homogeneous Poisson process.

# with Gaussian kernel, bandwidth 0.01 (generally somewhat smoother result
# than with rectangular kernel)
gg <- pcf(X, kernel="gaussian", bw=0.01)
plot(gg,main="Pair correlation function")

# with Epanechnikov kernel, bandwidth 0.01 (generally somewhat smoother result
# than with rectangular kernel)
gg <- pcf(X, kernel="epanechnikov", bw=0.01)
plot(gg,main="Pair correlation function")

# with Epanechnikov kernel, too small bandwidth 0.001
gg <- pcf(X, kernel="epanechnikov", bw=0.001)
plot(gg,main="Pair correlation function")

# with Epanechnikov kernel, bandwidth 0.1
gg <- pcf(X, kernel="epanechnikov", bw=0.1)
plot(gg,main="Pair correlation function")
# bandwidth almost too large

# another point pattern
gs <- pcf(simdat) # Epanechnikov kernel preset,
# bandwidth determined automatically
plot(gs,main="Pair correlation function")

# another bandwidth
gs <- pcf(simdat, bw=0.1)
plot(gs,main="Pair correlation function")	


#############
# Task 3
###

layout(matrix(1:4,2,2,byrow=T))

### an artificial clustered point process
w <- owin(c(0,2),c(0,2))
Z1<-rMatClust(5, 0.1, 20,win=w)  # strongly clustered 
plot(Z1,main="Point pattern")
GZ1 <- Gest(Z1,correction=c("none","rs","km","han"))	
plot(GZ1, main="G-function") # display with and without edge correction
FZ1 <- Fest(Z1,correction=c("none","rs","km","cs"))	
plot(FZ1, main="F-function") # display with and without edge correction
LZ1 <- Lest(Z1,correction=c("none","border","trans","iso"))	
plot(LZ1, main="L-function") # display with and without edge correction

### another artificial clustered point process
Z2<-rMatClust(20, 0.1, 5,win=w)  # weakly clustered
plot(Z2,main="Point pattern")
GZ2 <- Gest(Z2,correction=c("none","rs","km","han"))	
plot(GZ2, main="G-function") # display with and without edge correction
FZ2 <- Fest(Z2,correction=c("none","rs","km","cs"))	
plot(FZ2, main="F-function") # display with and without edge correction
LZ2 <- Lest(Z2,correction=c("none","border","trans","iso"))	
plot(LZ2, main="L-function") # display with and without edge correction

### an artificial, more regular point process
Z4<-rSSI(400,r=0.06,win=w)       # repulsive, 'hardcore'
plot(Z4,main="Point pattern")
GZ4 <- Gest(Z4,correction=c("none","rs","km","han"))	
plot(GZ4, main="G-function") # display with and without edge correction
FZ4 <- Fest(Z4,correction=c("none","rs","km","cs"))	
plot(FZ4, main="F-function") # display with and without edge correction
LZ4 <- Lest(Z4,correction=c("none","border","trans","iso"))	
plot(LZ4, main="L-function") # display with and without edge correction

### a real point pattern
P1<-swedishpines
plot(swedishpines)
GP1 <- Gest(P1,correction=c("none","rs","km","han"))	
plot(GP1, main="G-function") # display with and without edge correction
FP1 <- Fest(P1,correction=c("none","rs","km","cs"))	
plot(FP1, main="F-function") # display with and without edge correction
LP1 <- Lest(P1,correction=c("none","border","trans","iso"))	
plot(LP1, main="L-function") # display with and without edge correction
# So what can we say about 'swedishpines'?

### another real point pattern
P2<-longleaf
plot(longleaf)
GP2 <- Gest(P2,correction=c("none","rs","km","han"))	
plot(GP2, main="G-function") # display with and without edge correction
FP2 <- Fest(P2,correction=c("none","rs","km","cs"))	
plot(FP2, main="F-function") # display with and without edge correction
LP2 <- Lest(P2,correction=c("none","border","trans","iso"))	
plot(LP2, main="L-function") # display with and without edge correction
# So what can we say about 'longleaf'?

layout(1)
