################################################################################
# R script sysstat24_02_en.r for the practice "Statistical Analysis of Systems"
#####

###########################################
# Additional package geoR and dataset jura
###

# The following command loads the 'geoR' package into memory and 
# installs it first, if this has not already been done:
if(!require(geoR)) {install.packages("geoR"); require(geoR)} 

# Dataset: This is soil data from the canton of Jura (Switzerland).
# Import the data from a text file (must be in the current working directory):
jura <- read.table("juraset.txt", skip=17, header=T)

class(jura) # jura is of type 'data.frame', a matrix in which entire columns 
# can also have a type other than the type number.

# Data overview:
names(jura) # names of columns
NROW(jura)  # number of data records

# Plot of the measuring points; both lines produce the same display:
plot(jura$X, jura$Y)
plot(jura[,1:2])

# Distribution of the measurement data:
hist(jura$Cd) 
# Other variable names can also be used instead of "Cd":
# "Cu"   "Pb"   "Co"   "Cr"   "Ni"   "Zn"

# Table for the categories:
table(jura$Land)
# "Rock" and "Land"

# geoR works with the 'geodata' data type, into which the data can be converted
# with the command 'as.geodata'.
# In this we pass 
# - the data.frame to be converted
# - the positions of the coordinates (here columns 1 and 2)
# - the column number of the measured variable to be considered, here initially Co (cobalt)
jura.Co <- as.geodata(jura, coords.col = c(1,2), data.col = 8)
class(jura.Co)
names(jura.Co)

# Summary of the data:
jura.Co.sum <- summary(jura.Co)
names(jura.Co.sum)
jura.Co.sum$distances.summary
max.dist <- jura.Co.sum$distances.summary[2] # maximum distance between two measuring points

################
# h-scatterplot
###

# The 'source' command processes an entire .r file. 
# The hscatter.r file contains the definition of the R function 
# 'hscatter<-function(geoobj,h,eps,which.col = 1)',
# which returns the data for an h-scatter plot for the geodata object geoobj at the 
# distance h in the form of a two-column matrix.
# Executing the next line makes R aware of the hscatter function.
source("hscatter.r")
# The entire definition of the function hscatter is obtained with:
hscatter

hh <- c(0.1,1,3,5) # distances for which an h-scatterplot is to be created
pp <- vector("list", length(hh)) # an empty list
for(i in 1:length(hh)) pp[[i]] <- hscatter(jura.Co, h=hh[i], eps=0.05) 
# runs through the distances in hh and stores the data for an h-scatterplot 
# for the respective distance in pp[[i]]

layout(matrix(1:4,2)) # several plots in the same graphics window
xlim <- c(0,max(jura.Co$data)) # range of values
for(i in 1:length(hh)) {
  plot(pp[[i]], pch=20, xlim=xlim, ylim=xlim, 
       main=paste("Distance: h=",hh[i],sep=""), xlab="z_i", ylab="z_j")
}
layout(1) # reset to only one plot in the graphics window

# Create your own display, i.e. replace ?? with values of your choice:
h <- ??
eps <- ??
p <- hscatter(jura.Co, h=h, eps=eps)
plot(p, pch=20, xlim=xlim, ylim=xlim)

# h-scatterplots for different measured variables (e.g. "Cd" and "Pb") and the same h: 
jura.Cd <- as.geodata(jura, coords.col = c(1,2), data.col = 5)
jura.Pb <- as.geodata(jura, coords.col = c(1,2), data.col = 7)

hsp.Cd <- hscatter(jura.Cd, h=0.1, eps=0.05)
hsp.Pb <- hscatter(jura.Pb, h=0.1, eps=0.05)

plot(hsp.Cd)
plot(hsp.Pb)


##############################################
# Empirical covariance function / covariogram
###

# Estimator for the mean value of all measured values:
mu <- mean(jura.Co$data)

# Tolerance range for a distance:
eps <- 0.1           
# Distances to be considered:
hseq <- seq(0.1, 1.5, by=2*eps)

# Function definition for estimating the covariance function:
empCov <- function(geoobj, h, eps) {
  p <- hscatter(geoobj, h, eps)       # value pairs approx. h apart
  mu <- mean(geoobj$data)             # mean value of the measured values
  return(mean((p[,1]-mu)*(p[,2]-mu)))
}
# 'sapply' applies a function to an entire vector. 
Cov.Co <- sapply(hseq, empCov, geoobj=jura.Co, eps=eps)

# Plot of the individual pairs ( dij,(zi-mu)*(zj-mu) ):
plot(NA, NA, xlim=c(0,max(hseq)+eps), ylim=c(-70,70), xlab="h", ylab="C(h)")
n <- length(jura.Co$data)
invisible(sapply(1:n,function(i){
  dij <- sqrt( (jura.Co$coords[i,1]-jura.Co$coords[i:n,1])^2
               +(jura.Co$coords[i,2]-jura.Co$coords[i:n,2])^2 )
  zij <- (jura.Co$data[i]-mu)*(jura.Co$data[i:n]-mu)
  points(dij, zij, pch=".")
}))

# Plot of the individual distance classes and associated value of the covariance function C(h):
for(i in 1:length(hseq)){
  abline(v = hseq[i]+eps, col=2)
  points(hseq[i], Cov.Co[i], col=2, pch=20)
}
# The height of a red dot is calculated as the mean value of the heights of the small 
# black dots within the respective distance class.

# Connected curve with smaller eps and a more finely resolved sequence of h-values:
v <- var(jura.Co$data) # value for h=0 is the sample variance
lines(c(0,seq(0.05,1.75,by=0.05)), 
      c(v,sapply(seq(0.05,1.75,by=0.05),empCov,geoobj=jura.Co,eps=0.05)), 
      col=4, lwd=2)


#################################################
# Empirical (semi-)variogram and variogram cloud
###

# The function 'variog' from 'geoR' estimates the semivariogram.
jura.vario <- variog(jura.Co,                              # dataset
                     breaks=seq(0,max.dist,length.out=50)) # class division
# Plot of the empirical semivariogram:
plot(jura.vario,pch=20,cex=1)                              
# Number of pairs in a class:
jura.vario$n                                               

# Obviously, only a few data pairs are available for large distances, which is
# why the estimates of the covariance function or the (semi-)variogram based on
# this (semi-)variogram are uncertain and, purely by chance, show something in 
# this area that is not actually present (so-called artifacts).

# It therefore makes sense (later) to continue working with a smaller range of 
# distances, e.g. up to half the maximum distance.
jura.vario <- variog(jura.Co,breaks=seq(0,max.dist/2,length.out=50))
plot(jura.vario, pch=20, cex=1)

# The values of the empirical variogram are obtained in a similar way to the procedure 
# for the empirical covariance function:
hseq2 <- seq(0.1, 2.9, by=2*eps)
# Plot of the individual pairs ( dij, 0.5*(zi-zj)^2 ):
plot(NA, NA, xlim=c(0,max(hseq2)+eps), ylim=c(0,180), xlab="h", ylab="gamma(h)")
n <- length(jura.Co$data)
invisible(sapply(1:n,function(i){
  dij <- sqrt( (jura.Co$coords[i,1]-jura.Co$coords[i:n,1])^2
               +(jura.Co$coords[i,2]-jura.Co$coords[i:n,2])^2 )
  gij <- 0.5*(jura.Co$data[i]-jura.Co$data[i:n])^2
  points(dij, gij, pch=".")
}))
# The small black dots that can be seen in the plot form the so-called 
# variogram cloud.
# For the following plot, the range on the y-axis is restricted:
plot(NA, NA, xlim=c(0,max(hseq2)+eps), ylim=c(0,40), xlab="h", ylab="gamma(h)")
n <- length(jura.Co$data)
invisible(sapply(1:n,function(i){
  dij <- sqrt( (jura.Co$coords[i,1]-jura.Co$coords[i:n,1])^2
               +(jura.Co$coords[i,2]-jura.Co$coords[i:n,2])^2 )
  gij <- 0.5*(jura.Co$data[i]-jura.Co$data[i:n])^2
  points(dij, gij, pch=".")
}))
for(i in 1:length(hseq2)) abline(v = hseq2[i]+eps, col=2)
uv <- variog(jura.Co, breaks=hseq2+eps)
points(uv$u, uv$v, col=2, pch=20)
# The height of a red point (belonging to the empirical semivariogram) results 
# from the mean value of the heights of the small black dots (belonging to the
# variogram cloud) within the respective distance class.

# The individual points are often connected to form a curve, which is then 
# also called an empirical variogram:
lines(uv$u,uv$v,col=4)

########################################
# Further tasks

# Data:
virt<-as.geodata(read.table("virt.txt"))

# a)
vario.class <- variog(virt, max.dist=1)
vario.modul <- variog(virt, max.dist=1, est="modulus")
layout(matrix(1:2,1))
plot(vario.class, main="Classical estimator")
plot(vario.modul, main="Robust estimator")
layout(1)
# -> There are differences in the two variants of the empirical variogram.

# b)
?variog # in particular details on the max.dist argument

vario.class.08 <- variog(virt, max.dist=0.8)
vario.class.12 <- variog(virt, max.dist=1.2)

plot(vario.class.08, main="max.dist = 0.8")
plot(vario.class, main="max.dist = 1.0")
plot(vario.class.12, main="max.dist = 1.2")

vario.class.08$n
vario.class$n
vario.class.12$n
# -> As the number of classes remains the same (13 is preset), the numbers 
#    in the classes change and thus, of course, the values of the 
#    empirical variograms.

# c)
layout(matrix(1:4,2))
plot(variog(virt, uvec=seq(0.0,1,len=10)))
plot(variog(virt, uvec=seq(0.1,1,len=10)))
plot(variog(virt, uvec=seq(0.0,1,len=20)))
plot(variog(virt, uvec=seq(0.1,1,len=20)))

plot(variog(virt, uvec=seq(0.0,1,len=10)))
plot(variog(virt, uvec=seq(0.1,1,len=10)))
plot(variog(virt, breaks=seq(0.0,1,len=10)))
plot(variog(virt, breaks=seq(0.1,1,len=10)))
layout(1)

plot(variog(virt, uvec=seq(0.0,1,len=100)))
# -> Due to the high number of classes, the numbers within the individual 
#    distance classes are so low that the fluctuation of the class averages 
#    is very large.

# Numbers within the individual distance classes:
variog(virt, uvec=seq(0.0,1,len=100))$n 

# d)
layout(matrix(1:4,2))
plot(variog(virt, max.dist=1, direction=0))
plot(variog(virt, max.dist=1, direction=pi/4))
plot(variog(virt, max.dist=1, direction=pi/2))
plot(variog(virt, max.dist=1, direction=3*pi/4))
layout(1)
# -> First of all, you can recognize differences for the four different 
#    directions. One question would then be whether these differences are 
#    significant, i.e. whether they are not just random.

# e)
virt.with.trend <- virt
# artificial generation of trended data:
virt.with.trend$data <- virt$data + 5*virt$coords[,1] 
plot(variog(virt))
plot(variog(virt.with.trend))
# -> For the trended data, the empirical variogram does not approach a sill, 
#    but grows indefinitely with increasing distance.

# f)
plot(virt)
plot(virt.with.trend)

# Estimation of the coefficients for linear regression with respect to the location coordinates:
virt.lm <- lm(virt$data~virt$coords[,1]+virt$coords[,2], data=virt) 
summary(virt.lm)                                                   
# Are the p-values (last column) of the coefficients (table 'Coefficients') 
# of virt$coords[,1] or virt$coords[,2] less than 0.05?
