################################################################################
# R script sysstat24_03_en.r for the practice "Statistical Analysis of Systems"
#####

###
# First load again the additional package geoR:
require(geoR)

###
# Now load again the soil data for the canton of Jura (Switzerland) and estimate the semivariogram for the measured variable cobalt:
jura <- read.table("juraset.txt",skip=17,header=T)
jura.Co <- as.geodata(jura,coords.col = c(1,2),data.col = 8)
jura.vario <- variog(jura.Co,breaks=seq(0,3,length.out=40))

###################################
# Fitting a (semi-)variogram model
###

# In order to continue working with a variogram, a suitable variogram model 
# must be found for the empirical variogram.
# geoR provides the 'variofit' function for this purpose, which fits a variogram
# model using a least squares approach. This should be used first.
# (A suitable variogram model can also be found using the 'likfit' function,
# which searches for the model parameters using a maximum likelihood approach).

# The help for 'variofit'
?variofit
# shows that, in addition to the empirical semivariogram (input variable vario), at least
# the model to be fitted (input variable cov.model) must be specified. In addition, e.g. 
# initial values (ini.cov.pars) and the type of weights in the least squares approach (weights) can be specified.

# Fitting a spherical semivariogram:
jura.variofit <- variofit(jura.vario, cov.model="spherical", ini.cov.pars=c(13,1.3))
# What type of weights were used?

# Visual comparison of the fitted semivariogram with the empirical semivariogram:
plot(jura.vario)
lines(jura.variofit, col=2)

# Characteristics of the fitted semivariogram:
jura.variofit$nugget                              # nugget
jura.variofit$practicalRange                      # range
jura.variofit$cov.pars[1]+jura.variofit$nugget    # sill

# The following command draws a horizontal line at the height of the sill.
abline(h=jura.variofit$cov.pars[1]+jura.variofit$nugget, lty=2)

# With
jura.variofit
# you get a display of the essential values of the fitting. In particular
# you get the value of the minimized sum of weighted squares. This value is in
jura.variofit$value
# and could be used to compare the fit of different models with each other.

# The choice of weights in the least squares approach naturally has a certain influence on the fitting result.
jura.vf.equal <- variofit(jura.vario, cov.model="spherical", ini.cov.pars=c(13,1.3), weights="equal")
jura.vf.cressie <- variofit(jura.vario, cov.model="spherical", ini.cov.pars=c(13,1.3), weights="cressie")
plot(jura.vario)
lines(jura.variofit, col=2)
lines(jura.vf.equal, col=3)
lines(jura.vf.cressie, col=4)
# In this example, however, the difference for the spherical semivariogram model is obviously very small.


########################################
# Task 1
# -> try out several semivariogram models

## Fitting an exponential semivariogram:
jura.vf.exp <- variofit(jura.vario, cov.model="exponential", ini.cov.pars=c(13,1.3))
# Comparison of the fitted semivariogram with the empirical semivariogram:
plot(jura.vario)
lines(jura.vf.exp, col=2)
# Minimized square distance ...
jura.vf.exp$value # ... for the exponential model
jura.variofit$value # ... for the (previously fitted) spherical model

## Fitting a Gaussian semivariogram:
jura.vf.gau <- variofit(jura.vario, cov.model="gaussian", ini.cov.pars=c(13,0.8))
# Comparison of the fitted semivariogram with the empirical semivariogram:
plot(jura.vario)
lines(jura.vf.gau, col=2)
# Minimized square distance ...
jura.vf.gau$value # ... for the Gaussian model

## Fitting of Matérn semivariograms (parameter kappa with different values specified in each case):
?cov.spatial # -> details for the type "matern"

jura.vf.mat0.5 <- variofit(jura.vario, cov.model="matern", ini.cov.pars=c(13,1.3), fix.kappa=TRUE, kappa=0.5)
# Comparison of the fitted semivariogram with the empirical semivariogram:
plot(jura.vario)
lines(jura.vf.mat0.5, col=2)
# Minimized square distance ...
jura.vf.mat0.5$value # for this Matérn model

jura.vf.mat1.5 <- variofit(jura.vario, cov.model="matern", ini.cov.pars=c(13,1.3), fix.kappa=TRUE, kappa=1.5)
# Comparison of the fitted semivariogram with the empirical semivariogram:
plot(jura.vario)
lines(jura.vf.mat1.5, col=2)
# Minimized square distance ...
jura.vf.mat1.5$value # for this Matérn model

jura.vf.mat2.5 <- variofit(jura.vario,cov.model="matern", ini.cov.pars=c(13,0.8), fix.kappa=TRUE, kappa=2.5)
# Comparison of the fitted semivariogram with the empirical semivariogram:
plot(jura.vario)
lines(jura.vf.mat2.5, col=2)
# Minimized square distance ...
jura.vf.mat2.5$value # for this Matérn model

jura.vf.mat2.1 <- variofit(jura.vario,cov.model="matern", ini.cov.pars=c(13,1.3), fix.kappa=TRUE, kappa=2.1)
# Comparison of the fitted semivariogram with the empirical semivariogram:
plot(jura.vario)
lines(jura.vf.mat2.1, col=2)
# Minimized square distance ...
jura.vf.mat2.1$value # for this Matérn model

# Which model fits best due to the minimized square distance?
jura.variofit$value  # spherical model
jura.vf.exp$value    # exponential model
jura.vf.gau$value    # Gaussian model
jura.vf.mat0.5$value # Matérn model, kappa=0.5
jura.vf.mat1.5$value # Matérn model, kappa=1.5
jura.vf.mat2.5$value # Matérn model, kappa=2.5
jura.vf.mat2.1$value # Matérn model, kappa=2.1

########################################
# Task 2

# The following function generates values of theoretical semivariograms.
v.f <- function(x, cov.pars, ...){cov.pars[1]-cov.spatial(x, cov.pars=cov.pars, ...)} 

# Illustration of theoretical semivariograms:
curve(v.f(x,cov.pars=c(1, 0.2)), from=0, to=1, xlab = "h", ylab = expression(gamma(h)),
      main = "Semivariograms with the same \"practical range\"", lwd=2)
curve(v.f(x, cov.pars = c(1, 0.6), cov.model = "sph"), 0, 1, add=TRUE, col=2, lwd=2)
curve(v.f(x, cov.pars = c(1, 0.6/sqrt(3)), cov.model = "gau"), 0, 1, add=TRUE, col=4, lwd=2)
legend("topleft", c("exponential", "spherical", "gaussian"),col=c(1,2,4), lwd=c(2,2,2))

# Values for cov.pars and cov.model can now be changed individually here:
curve(v.f(x, cov.pars = c(2, 1.6), cov.model = "sph"), from=0, to=2, xlab = "h", ylab = expression(gamma(h)), col=2, lwd=2)
# ...

########################################
# Task 3

virt<-as.geodata(read.table("virt.txt"))

# a) Fitting by eye:
vario.emp<-variog(virt, uvec=seq(0,0.9,len=15)) # estimates empirical semivariogram
# The following command opens a window in which you can select models and
# set various parameters. The corresponding theoretical curve is 
# displayed together with the empirical semivariogram in the graphics window.
vario.eye<-eyefit(vario.emp)                    

# b) Least squares fitting:
vario.ls1<-variofit(vario.emp, cov.model="matern", ini.cov.pars=c(1.0,1.0)) # Here kappa=0.5 is fixed.
vario.ls2<-variofit(vario.emp, cov.model="matern",fix.kappa=FALSE, ini.cov.pars=c(1.0,1.0)) # Here kappa is also estimated.
vario.ls3<-variofit(vario.emp, cov.model="exponential", ini.cov.pars=c(1.0,1.0))
vario.ls4<-variofit(vario.emp, cov.model="gaussian", ini.cov.pars=c(1.0,1.0))
vario.ls5<-variofit(vario.emp, cov.model="spherical", ini.cov.pars=c(1.0,1.0))

# Illustration of the empirical semivariogram and the fitted model curves:
plot(vario.emp)
lines(vario.ls1)
lines(vario.ls2, col=2)
lines(vario.ls3, col=3)
lines(vario.ls4, col=4)
lines(vario.ls5, col=5)

# Displaying the estimated model parameters:
vario.ls1
vario.ls2
vario.ls3
vario.ls4
vario.ls5

# Minimized square distances: (the smaller, the better)
c(vario.ls1$value, vario.ls2$value, vario.ls3$value, vario.ls4$value, vario.ls5$value)

# c) Maximum likelihood fitting:
vario.ml1 <- likfit(virt, ini=c(0.5, 0.5), cov.model="matern") # coincides with type "exponential"
vario.ml2 <- likfit(virt, ini=c(0.5, 0.5), cov.model="matern", fix.kappa = FALSE)
vario.ml3 <- likfit(virt, ini=c(0.5, 0.5), cov.model="spherical")

# Illustration:
plot(vario.emp)
lines(vario.ml1)
lines(vario.ml2, col=2)
lines(vario.ml3, col=3)

# Model parameters:
vario.ml1
vario.ml2
vario.ml3

# Maximized log-likelihoods: (je larger, the better)
c(vario.ml1$loglik, vario.ml2$loglik, vario.ml3$loglik)
