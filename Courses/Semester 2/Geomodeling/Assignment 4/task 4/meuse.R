

install.packages("sp")
install.packages("gstat")
install.packages("MASS")
install.packages('rgdal')
install.packages("sf")
install.packages("")
install.packages("fields")
library(sf)

library(rgdal)
st_crs(meuse)  # If using sf
proj4string(meuse)  # If using sp

st_crs(meuse.grid)  # If using sf
proj4string(meuse.grid)  # If using sp

st_crs(meuse) <- 28992
st_crs(meuse.grid) <- 28992

proj4string(meuse) <- CRS("+init=epsg:28992")
proj4string(meuse.grid) <- CRS("+init=epsg:28992")


# Assign the same CRS as meuse
proj4string(meuse.grid) <- CRS("+init=epsg:28992")

# If meuse.grid still has a different CRS, transform it:
meuse.grid <- spTransform(meuse.grid, CRS(proj4string(meuse)))



library(sp)
library(gstat)
library(MASS)
  # Required for eqscplot()
data(meuse)
head(meuse)
summary(meuse)

plot(meuse)



coordinates(meuse) <- ~x+y  # Convert to SpatialPointsDataFrame

# Set projection (required for spatial operations)
proj4string(meuse) <- CRS("+init=epsg:28992")  


bubble(meuse,"zinc")
#distance to the river meuse
bubble(meuse,"dist.m")
data(meuse.grid)
coordinates(meuse) = ~x+y
coordinates(meuse.grid) = ~x+y
gridded(meuse.grid) = TRUE

image(meuse.grid["dist"])



cf = function(x) {
  crange = range(x)
  hsv(0.7*(crange[2]-x)/(crange[2]-crange[1]),0.7,0.7)
}
require("MASS")
#here you should see that we have measured the big zinc values near the river 
#and with increasing distance to the river the zinc values decrease quickly
eqscplot(meuse$x,meuse$y,col=cf(meuse$zinc),pch=19)
legend("right",title="Zink",
       legend=round(seq(min(meuse$zinc),max(meuse$zinc),length.out=10),digits=2),fill=cf(round(seq(min(meuse$zinc),max(meuse$zinc),length.out=10),digits=2)))

summary(meuse$zinc)
sqrt(var(meuse$zinc))
#distribution + outliers
hist(meuse$zinc,breaks=10)
hist(log(meuse$zinc),breaks=10)
boxplot(meuse$zinc)
logzinc<-log(meuse$zinc)
boxplot(logzinc)
summary(log(meuse$zinc))
sqrt(log(var(meuse$zinc)))

plot(log(zinc)~sqrt(dist),data=meuse)
abline(lm(log(zinc)~sqrt(dist),data=meuse))


#variogram
vMeuse<-variogram((log(zinc))~sqrt(dist),meuse)
vMeuse
plot(vMeuse,main="experimentelles Semivariogramm")
plot(vMeuse,main="experimentelles Semivariogramm - zinc",vgm(0.16,"Sph",800,0.07))
vMeuse.fit<- fit.variogram(vMeuse,model=vgm(0.16,"Sph",800,0.07))
vMeuse.fit
plot(vMeuse,vMeuse.fit)


# Check CRS of meuse dataset
proj4string(meuse)

# Check CRS of meuse.grid dataset
proj4string(meuse.grid)
proj4string(meuse) <- CRS("+init=epsg:28992")
proj4string(meuse)

library(rgdal)

# Assign the same CRS as meuse
proj4string(meuse.grid) <- CRS("+init=epsg:28992")

# If meuse.grid still has a different CRS, transform it:
meuse.grid <- spTransform(meuse.grid, CRS(proj4string(meuse)))

mk<-krige(log(zinc)~sqrt(dist),meuse,meuse.grid,model=vMeuse.fit,nmax=30)
spplot(mk)

#cross validation (leave-one-out)
?krige.cv
mcv <- krige.cv(log(zinc)~sqrt(dist), meuse, vMeuse.fit, nmax = 40)
mcv
summary(mcv)
bubble(mcv, "residual", main = "log(zinc): leave-one-out cross validation")
plot(mcv$var1.pred,mcv$observed,main="log(zinc): leave-one-out cross validation",sub="estimated vs observed values")
abline(0,1)
