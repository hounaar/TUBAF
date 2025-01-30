##5. Kriging
#create a grid for the kriging estimation
#if necessary load the libraries sp and gstat and the jura data set
objects()
library(sp)
library(gstat)
require(MASS)
#inking
cf = function(x) {
  crange = range(x)
  hsv(0.7*(crange[2]-x)/(crange[2]-crange[1]),0.7,0.7)
}

#if necessary clean up with 
#rm(list=ls())

#Testdataset
##1. load data
#You need to set the working directory first, menu Session -> Set Working Directory -> To SOurce FIle Location
testdata<-read.table("dataset.csv",header=T)
sapply(testdata,class)
testdata
##2. Explorative Data analysis
eqscplot(testdata$x,testdata$y,col=cf(testdata$Cu),pch=19,cex=1.7,xlab="x [m]",ylab="y [m]")
legend("right",title="Cu",legend=round(seq(min(testdata$Cu),max(testdata$Cu),length.out=10),digits=2),fill=cf(round(seq(min(testdata$Cu),max(testdata$Cu),length.out=10),digits=2)))

eqscplot(testdata$x,testdata$y,col=cf(testdata$Zn),pch=19,cex=1.7,xlab="x [m]",ylab="y [m]")
legend("right",title="Zn",legend=round(seq(min(testdata$Zn),max(testdata$Zn),length.out=10),digits=2),fill=cf(round(seq(min(testdata$Zn),max(testdata$Zn),length.out=10),digits=2)))

summary(testdata[,-c(1:3)])
par(mfrow=c(2,3))		# tile display
hist(testdata$Cu)
qqnorm(testdata$Cu)
boxplot(testdata$Cu)
hist(testdata$Zn)
qqnorm(testdata$Zn)
boxplot(testdata$Zn)
par(mfrow=c(1,1))
cor(testdata[,4:5])
### convert to spatial dataset (ds) 
ds = SpatialPointsDataFrame(testdata[,c("x","y")],testdata)
##3. Empirical Variogram
?variogram
plot(variogram((Cu)~1, data=ds, cloud=TRUE),main="Variogram Cloud -Cu",pch=".")
plot(variogram((Zn)~1, data=ds, cloud=TRUE),main="Variogram Cloud -Zn",pch=".")
##3. Variography
###unidirectional Variogram
vCu <- variogram((Cu)~1, ds)
vCu
plot(vCu,main="experimental Semivariogram - Cu")

vZn <- variogram((Zn)~1, ds)
vZn
plot(vZn,main="experimental Semivariogram - Zn")
###Check for Anisothrophy
vCu_map <- variogram((Cu)~1,ds,cutoff=450,width=60,map=TRUE)
vZn_map <- variogram((Zn)~1,ds,cutoff=450,width=60,map=TRUE)
plot(vCu_map,main="Variogram Map - Cu")
plot(vZn_map,main="Variogram Map - Zn")
##4. Variogrammodel

mCu_sph<-fit.variogram(vCu, vgm(40, "Sph", 350,0)) #vgm(sill,model,range,nugget)
mCu_sph
plot(vCu,mCu_sph,main="Variogrammodell - Cu")

mCu_exp<-fit.variogram(vCu, vgm(40, "Exp", 200,0)) #vgm(sill,model,range,nugget)
mCu_exp
plot(vCu,mCu_exp,main="Variogrammodell - Cu")

plot(vCu$dist,vCu$gamma,main="comparison of unidirectional variogrammmodels - Cu",xlab="distance",ylab="gamma")
mCu_exp_L<-variogramLine(mCu_exp,maxdist=700,n=20)
lines(mCu_exp_L$dist,mCu_exp_L$gamma,col="blue")
mCu_sph_L<-variogramLine(mCu_sph,maxdist=800,n=20)
lines(mCu_sph_L$dist,mCu_sph_L$gamma,col="green")

mZn_sph<-fit.variogram(vZn, vgm(60, "Sph", 300,0)) #vgm(sill,model,range,nugget)
mZn_sph
plot(vZn,mZn_sph,main="Variogrammodell - Zn")
##5.Kriging
###Create a grid for the kriging estimation
min(testdata$x)
max(testdata$x)
x.grid<-seq(min(testdata$x)-50,max(testdata$x+50),by=10)
x.grid
y.grid<-seq(min(testdata$x)-50,max(testdata$y)+50,by=10)
y.grid
testdata.grid<-expand.grid(x=x.grid,y=y.grid)
testdata.grid
gridded(testdata.grid) = ~x+y
?krige
tdk_Cu <- krige(formula = Cu~1,ds, testdata.grid, model = mCu_sph,nmax=10)
plot(tdk_Cu)
plot(tdk_Cu["var1.var"],main="Krigingvarianz")
spplot(tdk_Cu)
summary(tdk_Cu)

tdk_Zn <- krige(formula = Zn~1,ds, testdata.grid, model = mZn_sph,nmax=10)
plot(tdk_Zn)
plot(tdk_Zn["var1.var"],main="Krigingvarianz")
spplot(tdk_Zn)
summary(tdk_Zn)
#
#Jura dataset
jura<-read.table("jura.dat",header=T)
### convert to spatial dataset (ds) 
dsJ = SpatialPointsDataFrame(jura[,c("X","Y")],jura)

x.grid<-seq(min(jura$X),max(jura$X),by=0.05)
x.grid
y.grid<-seq(min(jura$Y),max(jura$Y),by=0.05)
y.grid
jura.grid<-expand.grid(x=x.grid,y=y.grid)
jura.grid
gridded(jura.grid) = ~x+y
plot(jura.grid)
#create a border polygon for the jura data set
jura.hull <- chull(jura$X,jura$Y)
jura.hull
juraPolygon = Polygon(coordinates(dsJ)[jura.hull,])
juraPolygon
lines(juraPolygon)

#experimentel variogram
###unidirektional variogram
vCo <- variogram((Co)~1, dsJ)
vCo
plot(vCo,main="experimentel semivariogram - Co")

###Variogram map - check for anisotropy
vCo_map <- variogram((Co)~1,dsJ,cutoff=1.5,width=0.3,map=TRUE)
plot(vCo_map,main="variogram map - Co")

###directional variogram
vCo_dir <- variogram((Co)~1, dsJ,alpha=seq(0,180,by=45),tol.hor=22.5,cutoff=1.5)
plot(vCo_dir)

#variogram model
###unidirectional variogram model
plot(vCo)
mCo_exp<-fit.variogram(vCo, vgm(14, "Exp", 1,2)) #vgm(sill,model,range,nugget)
mCo_exp
plot(vCo,mCo_exp,main="exponential variogram - Co")

mCo_sph<-fit.variogram(vCo, vgm(14, "Sph", 1,2)) #vgm(sill,model,range,nugget)
mCo_sph
plot(vCo,mCo_sph,main="sphaerical variogram - Co")

plot(vCo$dist,vCo$gamma,main="comparison of unidirectional variogram models - Co",xlab="distance",ylab="gamma")
mCo_exp_L<-variogramLine(mCo_exp,maxdist=2,n=20)
lines(mCo_exp_L$dist,mCo_exp_L$gamma,col="blue")
mCo_sph_L<-variogramLine(mCo_sph,maxdist=2,n=20)
lines(mCo_sph_L$dist,mCo_sph_L$gamma,col="green")
legend("bottomright",lty=1,col=c("blue","green"),legend=c("Exponentielles Mod.: Nug=1.53, sill=12.97, range=0.55","Sphaerisches Mod.: Nug=2.13, sill=11.39, range=1.14"))

###direktionales Variogrammemodell
plot(vCo_dir,mCo_sph)
mCo_dirS<-fit.variogram(vCo, vgm(14, "Sph", 1,2,anis=c(45,0.5))) #vgm(sill,model,range,nugget)
plot(vCo_dir,mCo_dirS,main="direktionales sphaerische Variogrammodell - Co",sub=mCo_dirS)
mCo_dirS

plot(vCo_dir,mCo_exp)
mCo_dirE<-fit.variogram(vCo, vgm(14, "Exp", 1,2,anis=c(45,0.5))) #vgm(sill,model,range,nugget)
plot(vCo_dir,mCo_dirE)
mCo_dirE

?krige
#ordinary kriging with directional variogrammodel
jk <- krige(formula = Co~1, dsJ, jura.grid, model = mCo_dirE,nmax=10)
plot(jk,main="estimated Co values")
points(jura$X,jura$Y)
plot(jk["var1.var"],main="kriging variance")
points(jura$X,jura$Y)
lines(juraPolygon)
spplot(jk)



#where we will pass the critical value of: 25ppm Co
jk$co2sigma<-jk$var1.pred+sqrt(jk$var1.var)*2
plot(jk["co2sigma"])
jk$co2sigma>=25
mean(jk$co2sigma>=25)
range(jk$co2sigma)

#execute simple kriging for Co, which value you would use for the known
#constant trend
#What is changing in contrary to ordinary kriging

#create a variable containing only the points inside the jura polygon
pip <- 0!=point.in.polygon(point.x=jura.grid$x,point.y=jura.grid$y,pol.x=coordinates(juraPolygon)[,1],pol.y=coordinates(juraPolygon)[,2])
pip
#dev.new()
#par(mar=c(5, 4, 4, 2) + 0.1)
plot(jura.grid[pip,])
polygon(jura$X[jura.hull],jura$Y[jura.hull],dens=0)
axis(c(1,2))


jk <- krige(formula = Co~1, dsJ, jura.grid[pip], model = mCo_dirE,nmax=10)
spplot(jk)
jk

#you can clean up the graphic plot area with the broom,
#if it looks strange


###################
#Indicator Kriging#
###################
#threshold value for Cd=0.8ppm
require("MASS")
eqscplot(jura$X,jura$Y,pch=as.numeric(jura$Cd<=0.8),xlab="X [km]",ylab="Y [km]",main="Cadmium concentration",sub="critical value=0.8ppm")
legend("right",pch=0:1,legend=c("Cd>0.8","Cd<=0.8"))

test<-jura[jura$"Cd">=0.8,]
test
nrow(test)

#Cd indicator,0>threshold value,1<=threshold value
Cd_Ind<-as.numeric(jura$Cd<=0.8)

#Indicator variogram
vCd_Ind<-variogram(Cd_Ind~1,dsJ)
vCd_Ind
plot(vCd_Ind, main="indicator variogram for Cd")
vCd_mapI <- variogram((Cd_Ind)~1,dsJ,cutoff=1.5,width=0.3,map=TRUE)
plot(vCd_mapI, main="variogram map for the Cd indicator")

#fit a variogrammodel
mCd_Ind<-fit.variogram(vCd_Ind, vgm(0.23, "Sph", 0.7,0.1))
mCd_Ind
plot(vCd_Ind,mCd_Ind)

mCd_Ind2<-fit.variogram(vCd_Ind, vgm(0.23, "Exp", 0.7,0.1))
mCd_Ind2
plot(vCd_Ind,mCd_Ind2)

#Kriging
jk_CdInd <- krige(formula = Cd_Ind~1, dsJ, jura.grid[pip], model = mCd_Ind2,nmax=10)
spplot(jk_CdInd)
jk_CdInd
plot(jk_CdInd["var1.var"],main="Kriging variance")
points(jura$X,jura$Y,pch=4)
plot(jk_CdInd["var1.pred"],main="Estimated values")
points(jura$X,jura$Y,pch=4)


##################
#cross validation#
##################
require("MASS")
eqscplot(jura$X,jura$Y,xlab="X [km]",ylab="Y [km]",main="Jura data set - mesurment points")
#create a data set for cross validation 
#select 20 points with the identify function
##
#Tools -> Global Options... -> Appearance -> Zoom: 100%
##
newXY<-identify(x=jura$X,y=jura$Y,labels=1:nrow(jura))
newXY
length(newXY)
juraNew<-jura[newXY,]
juraNew
juraKlein<-jura[-newXY,]
juraKlein
eqscplot(juraKlein$X,juraKlein$Y,pch=18,col="green",main="all 359 data locations")
points(juraNew$X,juraNew$Y,pch=18,col="red")

#experimentel Variogram for cross validation
###unidirectional Variogram
dsKlein = SpatialPointsDataFrame(juraKlein[,c("X","Y")],juraKlein)
vCo_new <- variogram((Co)~1, dsKlein)
plot(vCo_new,main="experimentel Semivariogram - Co",sub="cross validation")
###fitting of the vriogram model
mCo_exp_N<-fit.variogram(vCo, vgm(14, "Exp", 1,2)) #vgm(sill,model,range,nugget)
mCo_exp_N
plot(vCo_new,mCo_exp_N,main="exponental Variogram - Co")


?krige
jk_new<-krige(formula = Co~1, dsKlein, jura.grid[pip], model = mCo_exp_N,nmax=10)
#or so 
juraNewCoords<-juraNew[,1:2]  
dsNew = SpatialPointsDataFrame(juraNewCoords[,c("X","Y")],juraNewCoords)
jk_new<-krige(formula = Co~1, dsKlein, dsNew, model = mCo_exp_N,nmax=10)
spplot(jk_new)
plot(jk_new["var1.pred"],main="Vorhergesagte Werte")
jk_new
predCo<-jk_new$"var1.pred"
eqscplot(juraNew$Co,predCo,main="Kreuzvalidierung",xlab="wahre Co-Werte",ylab="vorhergesagte Co-Werte")
#lines(0:16,0:16)
abline(0,1)
plot(sqrt(jk_new$var1.var),(juraNew$Co-jk_new$var1.pred)/sqrt(jk_new$var1.var))
plot(sqrt(jk_new$var1.var),(juraNew$Co-jk_new$var1.pred),main="true error vs. krige standard error of Co values",xlab="Kriging error")
abline(h=0)
###################
#Universel Kriging#
###################
data(meuse)
meuse
coordinates(meuse)=~x+y
data(meuse.grid)
gridded(meuse.grid) = ~x+y
#try to understand which data are stored in the data set
?meuse
#have a look at the data set, how would you proceed, 
#please concentrate in the first attemp on zinc
#if you have no ideas or not enough ideas check meuse.R, 
#but first try it by your own

#############
#3D Data Set#
#############
library(rgl) #install.packages("rgl", dependencies = TRUE)
#options(rgl.printRglwidget = TRUE)
##1. load data
borehole<-read.table(file="borehole.csv",header=TRUE,sep="\t",dec=",")
borehole
colnames(borehole)
colnames(borehole)[2]<-"X"
colnames(borehole)[3]<-"Y"
colnames(borehole)[4]<-"Z"
colnames(borehole)[5]<-"Cu"
colnames(borehole)[6]<-"Mo"
levels(borehole$BL_ID)
summary(borehole$BL_ID)

### convert to spatial dataset (ds) 
dsBL = SpatialPointsDataFrame(borehole[,c("X","Y","Z")],borehole)


##2. Explorative data analyse


###spatial presentation
#borehole starting point
plot(Y~X,data=borehole,type="n",xlim=c(1000,5000))
tapply(1:nrow(borehole),borehole$BL_ID,function(i){
  x<-borehole[i,]
  min=which.max(x$Z)
  points(x = x[min,"X"],y=x[min,"Y"],main="borehole starting points")
  isNord = regexpr("N",x[min,"BL_ID"])>0
  text(x = x[min,"X"],y=x[min,"Y"],label=x[min,"BL_ID"],pos=ifelse(isNord,3,1),cex=0.75)
})

#inking/colorize
cf = function(x) {
  crange = range(x)
  hsv(0.7*(crange[2]-x)/(crange[2]-crange[1]),0.7,0.7)
}

#3D Plot of the boreholes, inking with the copper values as points
plot3d(x = borehole$X, y = borehole$Y, z =
         borehole$Z,col=cf(borehole$"Cu"),size=10)
legend3d("right",title="Cu in %",
         legend=round(seq(min(borehole$"Cu"),max(borehole$"Cu"),length.out=10),digits=2),fill=cf(round(seq(min(borehole$"Cu"),max(borehole$"Cu"),length.out=10),digits=2)))

hist(borehole$Cu)
hist(log(borehole$Cu))
##3. empirical variogram
###unidirekcional variogram
vCu <- variogram(log(Cu)~1, dsBL)
plot(vCu,main="experimental Semivariogram - Cu")

vCu


##4. Variogram model
###unidirectional variogram model
plot(vCu,main="experimentelles Semivariogramm - Cu")
plot(vCu,main="experimentelles Semivariogramm - Cu",vgm(0.01,"Exp",300,0.002))
mCu_exp<-fit.variogram(vCu, vgm(0.01, "Exp", 300,0.002))
mCu_exp
plot(vCu,mCu_exp,main="Variogrammodell - Cu")

##5. Kriging
summary(borehole$X)
summary(borehole$Y)
summary(borehole$Z)

#create a 3d grid
gridKoor<-GridTopology(cellcentre.offset=c(min(borehole$X),min(borehole$Y),min(borehole$Z)),cellsize=c(100,100,100), cells.dim=c((max(borehole$X)-min(borehole$X))/100,(max(borehole$Y)-min(borehole$Y))/100,(max(borehole$Z)-min(borehole$Z))/100))
class(gridKoor)
summary(gridKoor)
xyz<-coordinates(gridKoor)
bl.grid<-SpatialGrid(grid =gridKoor)
plot3d(xyz[,1],xyz[,2],xyz[,3])

blkrige<-krige(formula=Cu~1,dsBL,bl.grid,model=mCu_exp) 


selectSlice <- function(x,...) {
  xdim <-  x@grid@cells.dim
  remainingDims <- !sapply(as.list(substitute(c(...))[-1]),is.numeric)
  data2 = unclass(x@data)
  for(i in 1:length(data2) ) {
    a = data2[[i]]
    dim(a)=xdim
    data2[[i]]=c(a[...])
  }
  data=as.data.frame(data2)
  og <- x@grid
  gridTopoly <- GridTopology(cellcentre.offset=og@cellcentre.offset[remainingDims],
                             cellsize=og@cellsize[remainingDims],
                             cells.dim=og@cells.dim[remainingDims])
  SpatialGridDataFrame(gridTopoly,data,proj4string = x@proj4string)  
}

data <- selectSlice(blkrige["var1.pred"],,,1)

image(selectSlice(blkrige,,,1)["var1.pred"])
image(selectSlice(blkrige["var1.pred"],,,2))
image(selectSlice(blkrige["var1.pred"],,,3))
image(selectSlice(blkrige["var1.pred"],,,4))

image(selectSlice(blkrige["var1.pred"],,1,))

spplot(selectSlice(blkrige["var1.pred"],,1,))
spplot(selectSlice(blkrige["var1.var"],,1,))

