################################################################################
#  R script sysstat24_06_en.r for the practice "Statistical Analysis of Systems"
#####

if(!require(spatstat)) {install.packages("spatstat"); require(spatstat)}      # Download the R package 'spatstat' and install it beforehand if necessary.

#############
# Task 1  
###

Xa <- swedishpines    # Save point pattern 'swedishpines' in Xa
plot(Xa)              # Plot point pattern (same as plot(swedishpines))
Xa                    # Short info on the point pattern
summary(Xa)           # Basic statistcs of the point pattern
summary(Xa)$intensity # Show estimated intensity (= mean number of points per unit area)

Xb <- japanesepines   # Save point pattern 'japanesepines' in Xb
plot(Xb)              # Plot point pattern
Xb                    # Short info on the point pattern
summary(Xb)           # Basic statistcs of the point pattern
summary(Xb)$intensity # Show estimated intensity

Xc <- longleaf        # Save point pattern 'longleaf' in Xc
plot(Xc)              # Plot point pattern -> This is a marked point pattern. The size of the mark is shown as a circle.
?longleaf             # What are the marks?
Xc                    # Short info on the point pattern
summary(Xc)           # Basic statistcs of the point pattern
summary(Xc)$intensity # Show estimated intensity

Xd <- bei             # Save point pattern 'bei' in Xd
plot(Xd)              # Plot point pattern
# What is the visual difference to 'swedishpines' and 'japanesepines' in particular?
Xd                    # Short info on the point pattern
summary(Xd)           # Basic statistcs of the point pattern
summary(Xd)$intensity # Show estimated intensity

# Marked point patterns (point positions and related additional information = marks)

# categorical/type
plot(lansing)
?lansing

plot(sporophores)
?sporophores

plot(urkiola)
?urkiola

# real-valued
plot(spruces)
?spruces

plot(anemones)
?anemones

##############
# Task 2
###

# Generate a realization of the homogeneous Poisson process
# with intensity 80 in the window [0,2]x[0,2]:
X1 <- rpoispp(lambda=80,win=owin(c(0,2),c(0,2)))
summary(X1)
plot(X1)

# Generate another realization:
X2 <- rpoispp(lambda=80,win=owin(c(0,2),c(0,2)))
summary(X2)
plot(X2)

# yet another:
X3 <- rpoispp(lambda=80,win=owin(c(0,2),c(0,2)))
summary(X3)
plot(X3)

# Conclusion: As with a random number (think of throwing dice), the realizations (as results of a random experiment) are different.
#             Nevertheless, under certain conditions and a sufficiently large observation window, spatial 'processes' 
#             such as random point patterns (in contrast to random numbers), certain regularities can already be read from individual realizations.

# Estimated intensities:
summary(X1)$intensity
summary(X2)$intensity
summary(X3)$intensity
# Conclusion: Although the estimated intensities differ, as they are each based on a single realization, they also
#             not deviate too much from the specified theoretical intensity 80.

### 
# Simulation study 
###

# The following function first generates a realization of a homogeneous Poisson process
# of intensity lambda in the observation window [0,a]x[0,a] and then returns the estimated intensity.
getInt<-function(i,a,lambda) summary(rpoispp(lambda,win=owin(c(0,a),c(0,a))))$intensity

# Using the function 'getInt', we now generate realizations for three values each for the intensity lambda
# and for the side length a of the observation window N=200 and estimate the intensity.
N<-200
lambda.all<-c()
for(lambda in c(80,100,120)) for(a in 1:3) lambda.all<-cbind(lambda.all,sapply(1:N,getInt,a=a,lambda=lambda))

# Now we use these simulation results to determine how the estimated values behave on average
# and how they scatter. We also look at the theoretical values for comparison.
title <- c("a","lambda","mean.theor","mean.empir","var.theor","var.empir")
result <- matrix(NA,9,6)
colnames(result) <- title
i<-0
for(lambda in c(80,100,120)) for(a in 1:3) {
  i<-i+1
  mlambda <- mean(lambda.all[,i])
  vlambda <- var(lambda.all[,i])
  result[i,] <- c(a,lambda,lambda,mlambda,lambda/a^2,vlambda)
}
result     
# Conclusion: The larger the window (or a), the smaller the variances, i.e. the smaller the fluctuations
#             in the estimated intensities.
#             This can also be visualized well with the help of box plots.
# Graphical illustration:
boxplot(lambda.all[,1:3],xlab="a",main="lambda=80")
boxplot(lambda.all[,4:6],xlab="a",main="lambda=100")
boxplot(lambda.all[,7:9],xlab="a",main="lambda=120")


#############
# Task 3
###

# Two different intensity functions in the plane:
lambda1 <- function(x,y) 500*(x+y)^2
lambda2 <- function(x,y) 500 * exp(-10*((x-0.5)^2 + (y-0.5)^2))

x <- seq(0,1,by=0.01) # Vector of x-coordinates
y <- seq(0,1,by=0.01) # Vector of y-coordinates

# Illustration of these intensity functions:
z1 <- outer(x,y,lambda1) # generates the values of lambda1(x,y) for each coordinate pair of x and y
# Three different representations:
contour(x,y,z1)
persp(x,y,z1)
plot(im(z1,x,y))

z2 <- outer(x,y,lambda2) # generates the values of lambda2(x,y) for each coordinate pair of x and y
# Three different representations:
contour(x,y,z2)
persp(x,y,z2)
plot(im(z2,x,y))

# Realization of an inhom. Poisson process with intensity function lambda1 (in window [0,1]x[0,1], which is preset):
X1 <- rpoispp(lambda=lambda1, lmax=2000) # In the best case, lmax is chosen to be exactly equal to the maximum value of the intensity function in the area to be simulated.
plot(X1)

plot(im(z1,x,y))				# intensity function together with
plot(X1, add=TRUE)			# a realization of the point process

# Kernel density estimation in 'spatstat':
?density.ppp

# Estimation of the intensity function lambda1 based on a realization (of the Poisson process or a random point pattern in general):
h <- 0.05					# bandwidth h here ccertainly too small
plot(density(X1,h))
contour(density(X1,h))
persp(density(X1,h),theta=20)

h <- 0.1
plot(density(X1,h))
contour(density(X1,h))
persp(density(X1,h),theta=20)

h <- 0.5
plot(density(X1,h))
contour(density(X1,h))
persp(density(X1,h),theta=20)


# Realization of an inhom. Poisson process with intensity function lambda2:
X2 <- rpoispp(lambda=lambda2, lmax=500)
plot(X2)

plot(im(z2,x,y))				# intensity function together with
plot(X2, add=TRUE)			# a realization of the point process

# Estimation of the intensity function lambda2
h <- 0.05					# bandwidth h here ccertainly too small
plot(density(X2,h))
contour(density(X2,h))
persp(density(X2,h),theta=20)

h <- 0.1
plot(density(X2,h))
contour(density(X2,h))
persp(density(X2,h),theta=20)

h <- 0.5
plot(density(X2,h))
contour(density(X2,h))
persp(density(X2,h),theta=20)

# Kernel density estimation of the intensity function for the data set 'bei':
h <- 1
plot(density(bei,h))
contour(density(bei,h))
plot(bei, add=TRUE, col=2)
persp(density(bei,h),theta=20)

h <- 10
plot(density(bei,h))
contour(density(bei,h))
plot(bei, add=TRUE, col=2)
persp(density(bei,h),theta=20)

h <- 30
plot(density(bei,h))
contour(density(bei,h))
plot(bei, add=TRUE, col=2)
persp(density(bei,h),theta=20)

h <- 50
plot(density(bei,h))
contour(density(bei,h))
plot(bei, add=TRUE, col=2)
persp(density(bei,h),theta=20)

