#Applying this to the URCHIN DATASET
# Cokriging multivariate data
#
# Multivariate plots using lattice library
#
rm(list = ls(all = TRUE))

library(lattice)
library(sp)

setwd("~/Desktop/Urchin Data")
urchin = read.csv("urchin.csv") #this one has NAs
summary(urchin)
View(urchin)
coordinates(urchin) = c("longitude","latitude")

plot(urchin$longitude,urchin$latitude,type='n')
points(urchin)
View(urchin)
#
#
#Can this be changed in the for-loop to make the variograms?
## How can we change this dynamically?
# create a new dataframe
urchin.count = urchin$mean_urchincount2009
crust = urchin$mean_crustpercent2009
under = urchin$mean_understorypercent2009
canopy = urchin$mean_canopypercent2009

x = urchin$longitude
y = urchin$latitude
df = data.frame(urchin.count, crust, under, canopy, x, y)

df2 <- df[complete.cases(df), ] #NAs
df2 <- na.omit(df)

coordinates(df2) = c("x","y")

#View(df2)
# Histograms of original data
#
par(mfrow=c(2,2))
hist(df2$urchin.count,main="Urchin")
hist(df2$crust,main="Crustaceans")
hist(df2$under,main="Understory")
hist(df2$canopy,main="Canopy")
par(mfrow=c(1,1))
#
# Histograms of log data
#
par(mfrow=c(2,2))
hist(log(df2$urchin.count),main="log(Urchin)")
hist(log(df2$crust),main="log(Crustacean)")
hist(log(df2$under),main="log(Understory)")
hist(log(df2$canopy),main="log(Canopy)")
par(mfrow=c(1,1))
#
# Add log versions to meuse data object
#
df2$log.urchin.count = log(df2$urchin.count)
df2$log.crust = log(df2$crust)
df2$log.under = log(df2$under)
df2$log.canopy = log(df2$canopy)
#
# Take a look at the datasets together
#
library(RColorBrewer)
library(classInt)
#
pal = brewer.pal(5,"Greens")
par(mfrow=c(2,2),mar=c(0,0,0,0)+.5)
#
#par(mfrow=c(2,2))
q5 = classIntervals(df2$log.urchin.count, n=5, style="quantile")
q5Colors = findColours(q5,pal)
plot(df2,col=q5Colors,pch=19, cex=0.75)
title("Log(Urchin)")
#
q5 = classIntervals(df2$crust, n=5, style="quantile")
q5Colors = findColours(q5,pal)
plot(df2,col=q5Colors,pch=19, cex=0.75)
title("Crustacean")
#
q5 = classIntervals(df2$under, n=5, style="quantile")
q5Colors = findColours(q5,pal)
plot(df2,col=q5Colors,pch=19, cex=0.75)
title("Understory")
#
q5 = classIntervals(df2$canopy, n=5, style="quantile")
q5Colors = findColours(q5,pal)
plot(df2,col=q5Colors,pch=19, cex=0.75)
title("Canopy")
#
par(mfrow=c(1,1),mar=c(5, 4, 4, 2) + 0.1)
#

# Create multivariate variograms
#
library(gstat)
#
# Append the information together
#
#View(df2)

#
g = gstat(NULL,"L.Urchin",log.urchin.count~1,df2)
g = gstat(g,"Crust",crust~1,df2)
g = gstat(g,"Under",under~1,df2)
g = gstat(g,"Canopy",canopy~1,df2)
#
vm = variogram(g)
#
vm.fit = fit.lmc(vm, g, vgm(1,"Sph",0.8,1))
#
plot(vm,vm.fit)
#
#Get the summary!
summary(vm.fit)
#
# Now we do cokriging
#
data(meuse.grid)
coordinates(meuse.grid)=c("x","y")
#
cok.maps = predict(vm.fit, meuse.grid)
#
# Examine what is contained in prediction object
#
names(cok.maps)
#
# Plot just the predictions
#
spplot(cok.maps,c("LogCd.pred","LogCu.pred","LogPb.pred","LogZn.pred"))
#
# I don't like the scaling of the responses
# So I do each individually here
#
spplot(cok.maps,c("LogCd.pred"),pch=15,
       scale=list(draw=T),key.space=list(x=0.1,y=.95,corner=c(0,1)),
       main="LogCd")
#
spplot(cok.maps,c("LogCu.pred"),pch=15,
       scale=list(draw=T),key.space=list(x=0.1,y=.95,corner=c(0,1)),
       main="LogCu")
#
spplot(cok.maps,c("LogPb.pred"),pch=15,
       scale=list(draw=T),key.space=list(x=0.1,y=.95,corner=c(0,1)),
       main="LogPb")
#
spplot(cok.maps,c("LogZn.pred"),pch=15,
       scale=list(draw=T),key.space=list(x=0.1,y=.95,corner=c(0,1)),
       main="LogZn")



