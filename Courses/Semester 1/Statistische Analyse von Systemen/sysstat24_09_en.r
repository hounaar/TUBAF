################################################################################
# R script sysstat24_09_en.r for the practice "Statistical Analysis of Systems"
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
# Gibbs processes
###

# Some examples of the Strauss process with interaction radius R=0.1
# and different interaction parameters gamma starting with gamma=1 (Poisson process)
# up to gamma=0 (Gibbs hardcore process)
X<-rStrauss(beta=100, gamma=1, R=0.1, W=owin(c(0,2),c(0,2)))
plotDiagnostics(X)
min(nndist(X)) # minimum pair distance

X<-rStrauss(beta=100, gamma=0.5, R=0.1, W=owin(c(0,2),c(0,2)))
plotDiagnostics(X)
min(nndist(X)) # minimum pair distance

X<-rStrauss(beta=100, gamma=0.1, R=0.1, W=owin(c(0,2),c(0,2)))
plotDiagnostics(X)
min(nndist(X)) # minimum pair distance

X<-rStrauss(beta=100, gamma=0.0, R=0.1, W=owin(c(0,2),c(0,2)))
plotDiagnostics(X)
min(nndist(X)) # minimum pair distance, should be larger than R=0.1

# Illustration of the Strauss pair potential
bStrauss <- function(r,gamma,R) log(gamma)*(r<=R)*(r>=0)
rseq <- seq(0,0.15,by=0.001)
plot(rseq,bStrauss(rseq,gamma=1,R=0.1),type="l",main="Strauss pair potential",xlab="r",ylab="b",ylim=c(-2.5,0.5),xlim=c(0,0.15),xaxs="i")
plot(stepfun(rseq[-1],bStrauss(rseq,gamma=0.5,R=0.1)),col=2,add=T,do.points=F)
plot(stepfun(rseq[-1],bStrauss(rseq,gamma=0.1,R=0.1)),col=4,add=T,do.points=F)
legend(0.105,-0.5,legend=c("gamma=1","gamma=0.5","gamma=0.1"),col=c(1,2,4),lty=c(1,1,1))

# and the corresponding factor exp(Strauss pair potential) in p(X)
plot(rseq,exp(bStrauss(rseq,gamma=1,R=0.1)),type="l",main="exp(Strauss pair potential)",xlab="r",ylab="exp(b)",ylim=c(0,1.1),xlim=c(0,0.15),xaxs="i")
plot(stepfun(rseq[-1],exp(bStrauss(rseq,gamma=0.5,R=0.1))),col=2,add=T,do.points=F)
plot(stepfun(rseq[-1],exp(bStrauss(rseq,gamma=0.1,R=0.1))),col=4,add=T,do.points=F)
plot(stepfun(rseq[-1],exp(bStrauss(rseq,gamma=0.0000001,R=0.1))),col=3,add=T,do.points=F)
legend(0.105,0.8,legend=c("gamma=1","gamma=0.5","gamma=0.1","gamma=0"),col=c(1,2,4,3),lty=c(1,1,1,1))

# Illustration of the area interaction pair potential
bPenttinen <- function(r,gamma,R) log(gamma)*(r<=2*R)*(r>=0)*(2*R^2*acos(0.5*r*(r<=2*R)/R)-0.5*r*sqrt((4*R^2-r^2)*(r<=2*R)))/(pi*R^2)
rseq <- seq(0,0.15,by=0.001)
plot(rseq,bPenttinen(rseq,gamma=1,R=0.05),type="l",main="Area interaction pair potential",xlab="r",ylab="b",ylim=c(-2.5,0.5),xlim=c(0,0.15),xaxs="i")
lines(rseq,bPenttinen(rseq,gamma=0.5,R=0.05),col=2)
lines(rseq,bPenttinen(rseq,gamma=0.1,R=0.05),col=4)
legend(0.105,-0.5,legend=c("gamma=1","gamma=0.5","gamma=0.1"),col=c(1,2,4),lty=c(1,1,1))

# Example for the area interaction process
X <- rPenttinen(beta=100, gamma=0.5, R=0.05, W=owin(c(0,2),c(0,2)))
plotDiagnostics(X)

# The pair correlation function is particularly suitable for investigating Gibbs processes.
# However, there is usually no analytical formula for this, but you have to rely on simulations:

rr<-seq(0,0.5,length=501)
pcf1<-c();pcf2<-c();pcf3<-c();pcf4<-c();
for(i in 1:40) {
  Y1<-rStrauss(beta=100,gamma=0.5,R=0.1,W=owin(c(0,2),c(0,2)))
  Y2<-rStrauss(beta=100,gamma=0.1,R=0.1,W=owin(c(0,2),c(0,2)))
  Y3<-rStrauss(beta=100,gamma=0,R=0.1,W=owin(c(0,2),c(0,2)))
  Y4<-rPenttinen(beta=100, gamma=0.5, R=0.05, W=owin(c(0,2),c(0,2)))
  pcf1<-rbind(pcf1,pcf(Y1,r=rr,divisor="d")$iso)
  pcf2<-rbind(pcf2,pcf(Y2,r=rr,divisor="d")$iso)
  pcf3<-rbind(pcf3,pcf(Y3,r=rr,divisor="d")$iso)
  pcf4<-rbind(pcf4,pcf(Y4,r=rr,divisor="d")$iso)
} # Wait ...
plot(rr,apply(pcf1,2,mean),type="l",ylim=c(0,1.3),main="Pair correlation function Strauss process",xlab="r",ylab="g(r)")
lines(rr,apply(pcf2,2,mean),col=2)
lines(rr,apply(pcf3,2,mean),col=4)
lines(rr,apply(pcf4,2,mean),col=5)
lines(rr,rep(1,length(rr)),col=8)
legend(0.3,0.6,legend=c("gamma=1","gamma=0.5","gamma=0.1","gamma=0","area interaction"),col=c(8,1,2,4,5),lty=c(1,1,1,1,1))
# Actually, in the case gamma=0, which leads to the Gibbs hardcore process with hardcore distance R=0.1, the (blue) curve of the
# pair correlation function in the interval [0,R]=[0,0.1] should be zero, as these pair distances cannot occur. That this is not
# completely the case is due to the fact that the pair correlation is estimated via smoothing (kernel density estimation) and thus also 
# a little mass on a small area to the left of R=0.1.


#############
# Marked point processes
##

### Quantitative marks

### Dataset 'spruces'
plot(spruces)
?spruces
mean(marks(spruces)) # mean of marks (in meters)
var(marks(spruces)) # variance of marks
plot(markvario(spruces,correction="iso"),main="") #  mark variogram
# For comparison, the red line shows the (constant) mark variogram for the case that the marks are independent of the positions.
# Value of the constant: var(marks(spruces))
plot(markcorr(spruces,correction="iso"),main="") # k_mm function
# For comparison, the red line shows the (constant) k_mm function for the case that the marks are independent of the positions.
# Value of the constant: 1
plot(markcorr(spruces,correction="iso",normalise=FALSE,f=function(m1,m2){m1}),ylab="E(r)",main="")
# For comparison, the red line shows the (constant) E-function for the case that the marks are independent of the positions.
# Value of the constant: mean(marks(spruces))
# Conclusion: There is a correlation of breast height diameters for small tree distances. From k_mm(r) and E(r) you can see that for
#             trees standing close to each other, the product of the marks or the mark of the first tree is smaller than the average.
#             There is therefore a mutual negative influence on the width growth of the trees.

### Dataset 'longleaf'
plot(longleaf)
?longleaf
mean(marks(longleaf)) # mean of marks (in meters)
var(marks(longleaf)) # variance of marks
plot(markvario(longleaf,correction="iso"),main="") #  mark variogram
# For comparison, the red line shows the (constant) mark variogram for the case that the marks are independent of the positions.
# Value of the constant: var(marks(longleaf))
plot(markcorr(longleaf,correction="iso"),main="") # k_mm function
# For comparison, the red line shows the (constant) k_mm function for the case that the marks are independent of the positions.
# Value of the constant: 1
plot(markcorr(longleaf,correction="iso",normalise=FALSE,f=function(m1,m2){m1}),ylab="E(r)",main="")
# For comparison, the red line shows the (constant) E-function for the case that the marks are independent of the positions.
# Value of the constant: mean(marks(longleaf))
# Conclusion: see 'spruces'

### Dataset 'anemones'
plot(anemones)
?anemones
mean(marks(anemones)) # mean of marks (w.r.t. some unknown unit length)
var(marks(anemones)) # variance of marks
plot(markvario(anemones,correction="iso"),main="") #  mark variogram
# For comparison, the red line shows the (constant) mark variogram for the case that the marks are independent of the positions.
# Value of the constant: var(marks(anemones))
plot(markcorr(anemones,correction="iso"),main="") # k_mm function
# For comparison, the red line shows the (constant) k_mm function for the case that the marks are independent of the positions.
# Value of the constant: 1
plot(markcorr(anemones,correction="iso",normalise=FALSE,f=function(m1,m2){m1}),ylab="E(r)",main="")
# For comparison, the red line shows the (constant) E-function for the case that the marks are independent of the positions.
# Value of the constant: mean(marks(anemones))
# Conclusion: similar to 'spruces'

### Dataset 'finpines'
plot(finpines, which.marks="height", main="heights")
?finpines
fp<-unmark(finpines)
marks(fp)<-finpines[[6]]$height # only want to look at the tree height
mean(marks(fp)) # mean of marks (in meters)
var(marks(fp)) # variance of marks
plot(markvario(fp,correction="iso"),main="") #  mark variogram
# For comparison, the red line shows the (constant) mark variogram for the case that the marks are independent of the positions.
# Value of the constant: var(marks(fp))
plot(markcorr(fp,correction="iso"),main="") # k_mm function
# For comparison, the red line shows the (constant) k_mm function for the case that the marks are independent of the positions.
# Value of the constant: 1
plot(markcorr(fp,correction="iso",normalise=FALSE,f=function(m1,m2){m1}),ylab="E(r)",main="")
# # For comparison, the red line shows the (constant) E-function for the case that the marks are independent of the positions.
# Value of the constant: mean(marks(fp))
# Conclusion: In contrast to the previous examples, in which the mark was a width, there is a tendency here for trees 
#             that are close together to be slightly larger on average. However, as these are young trees, this tendency could 
#             also just be coincidental and not necessarily due to competition for light.


### Qualitative marks

### Dataset 'urkiola'
plot(urkiola)
?urkiola
summary(urkiola)
summary(urkiola)$marks # contains under 'proportion' the proportions p_i
plot(markconnect(urkiola,"birch","birch",correction="iso")) # p_11
plot(markconnect(urkiola,"oak","oak",correction="iso"))     # p_22
plot(markconnect(urkiola,"birch","oak",correction="iso"))   # p_12=p_21
# Conclusion: Trees of the same species “like” each other, trees of different species are not so happy to be direct neighbors.

### Dataset 'lansing'
plot(lansing)
?lansing
summary(lansing)
summary(lansing)$marks
species<-rownames(summary(lansing)$marks)
for(i in species) plot(markconnect(lansing,i,i,correction="iso"))
for(i in 2:6) for(j in 1:(i-1)) plot(markconnect(lansing,species[i],species[j],correction="iso"))
# Conclusion: similar to dataset 'urkiola'

