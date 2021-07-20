# Preliminary data analysis 
# November 16, 2015
# Data from St. George Island  beach transects

#this reads data from buffer
newdata.all = read.table(pipe("pbpaste"), header = T)  # here is a problem.  MG did 5 transects one year, then went back again and added more.  Plus, he may have added more plots further up the dunes, even for plots 1-5.  But, he only has elevations for 1:5. 
newdata.all=read.csv("plot.csv")
# lets go ahead and fix the inverse elevation problem caused by using the laser level.
elevation = max(newdata.all$laser.ht) - newdata.all$laser.ht
new.again = cbind(newdata.all,elevation)

head(new.again)
newdata = new.again
library(vegan)


par(mar = c(4,5,1,1))
# make plot for sesuvium
avs.sesuvium = aggregate(newdata$Sesuvium, list(newdata$distance), mean)
plot(avs.sesuvium$x~avs.sesuvium$Group.1, typ = "b", ylim = c(0, 22), lwd = 2, xlab = "Distance from shore", ylab = "Average plant cover")

# add uniola to plot
avs.uniola = aggregate(newdata$Uniola, list(newdata$distance), mean)
lines(avs.uniola$x~avs.uniola$Group.1, type = "b", col = "red", lwd = 2)

# add cakile to plot
avs.cakile = aggregate(newdata$Cakile, list(newdata$distance), mean)
lines(avs.cakile$x~avs.cakile$Group.1, type = "b", col = "blue", lwd = 2)

# add chameasyce to plot
avs.chameasyce = aggregate(newdata$Chameasyce, list(newdata$distance), mean)
lines(avs.chameasyce$x~avs.chameasyce$Group.1, type = "b", col = "orange", lwd = 2)

# add iva to plot
avs.iva = aggregate(newdata$Iva, list(newdata$distance), mean)
lines(avs.iva$x~avs.iva$Group.1, type = "b", col = "green", lwd = 2)

# add ipomea to plot
avs.ipomea= aggregate(newdata$Ipomea, list(newdata$distance), mean)
lines(avs.ipomea$x~avs.ipomea$Group.1, type = "b" , col= "purple" , lwd= 2)

#plot averaged over longer distances

# make plot for sesuvium
avs.sesuvium = aggregate(newdata$Sesuvium, list(newdata$dist.group), mean)
plot(avs.sesuvium$x~avs.sesuvium$Group.1, typ = "b", ylim = c(0, 20), lwd = 2, xlab = "Distance from Beach", col = "orange", lty = 2)
sesuvium.spl = smooth.spline(avs.sesuvium$Group.1, avs.sesuvium$x)
lines(sesuvium.spl)

# add cakile to plot
avs.cakile  = aggregate(newdata$Cakile , list(newdata$dist.group), mean)
lines(avs.cakile  $x~avs.cakile  $Group.1, type = "b", col = "red", lwd = 2)

# add uniola to plot
avs.uniola = aggregate(newdata$Uniola, list(newdata$dist.group), mean)
lines(avs.uniola$x~avs.uniola$Group.1, type = "b", col = "black", lwd = 2)

# add chameasyce to plot  Can't do with Chameasyce, as it is in only one plot
#avs.chameasyce = aggregate(newdata$Chamesyce, list(newdata$dist.group), mean)
#lines(avs.chameasyce $x~avs.chameasyce $Group.1, type = "b", col = "purple", lwd = 2)

# add iva to plot
avs.iva = aggregate(newdata$Iva, list(newdata$dist.group), mean)
lines(avs.iva$x~avs.iva$Group.1, type = "b", col = "blue", lwd = 2)

# add ipomea to plot
avs.ipomea = aggregate(newdata$Ipomea, list(newdata$dist.group), mean)
lines(avs.ipomea$x~avs.ipomea$Group.1, type = "b", col = "green", lwd = 2)
#ipomea.spl = smooth.spline(avs.ipomea$Group.1, avs.ipomea$x)
#lines(ipomea.spl)


newdata=new.again

dim(newdata)
head(newdata)
mds.data = newdata[rowSums(newdata[,4:9]) > 0,]
dim(mds.data)


newdata.mds = metaMDS(mds.data[,4:9], try = 100)
par(mar = c(4,5,1,1), mfrow = c(1,1))
plot(newdata.mds)
plot(newdata.mds,typ= "n", xlab = "NMDS Axis 1", ylab = "NMDS Axis 2")
text(newdata.mds$species[,1], newdata.mds$species[,2], rownames(newdata.mds$species), cex=0.7, col ="black")
points(newdata.mds$points[,1], newdata.mds$points[,2],  pch = 1) #I think it looks better with black dots and cant invert elevation so maybe you can run it on your end
ordisurf(newdata.mds, mds.data[,2], prioirty=,labcex=0.9, add = T,col="forestgreen")

env.data = cbind(mds.data$distance, mds.data$elevation)
mds.data.envfit = envfit(newdata.mds, env.data)

plot(mds.data.envfit, col = "black", labels = c("Distance", "Elevation"), lwd = 2)

orditorp(newdata.mds, disp="sp")
orditorp(mds.data.envfit, col = "black", labels = c("Distance", "Elevation"), lwd = 2)
#PERMANOVA ->NEW!!!!!!!!!!!!!!!!!!!
adonis(mds.data[,4:9]~distance*elevation, data=mds.data,  method="bray")

#PERMANOVA with Environemntal Variables->NEW!!!!!!!!!!!!!!!!!!!
adonis(mds.data[,4:9]~distance*elevation*percent.Moisture*salinity.ppt.*percent.ash, data=mds.data,  method="bray")


dist<-dist()


par(mfrow = c(2,3), mar = c(4,5,1,1))
plot(mds.data$percent.moisture ~ newdata.mds$points[,1], xlab = "NMDS 1", ylab = "Soil Moisture")
line1 = lm(mds.data$percent.moisture ~ newdata.mds$points[,1])
abline(line1)
summary(line1)
plot(mds.data$percent.ash ~ newdata.mds$points[,1], xlab = "NMDS 1", ylab = "Percent Ash")
line2 = lm(mds.data$percent.ash ~ newdata.mds$points[,1])
abline(line2)
summary(line2)
plot(mds.data$salinity ~ newdata.mds$points[,1], xlab = "NMDS 1", ylab = "Salinity")
line1 = lm(mds.data$salinity ~ newdata.mds$points[,1])
abline(line1)
summary(line1)
plot(mds.data$percent.moisture ~ newdata.mds$points[,2], xlab = "NMDS 2", ylab = "Soil Moisture")
line1 = lm(mds.data$percent.moisture ~ newdata.mds$points[,2])
abline(line1)
summary(line1)
plot(mds.data$percent.ash ~ newdata.mds$points[,2], xlab = "NMDS 2", ylab = "Percent Ash")
line2 = lm(mds.data$percent.ash ~ newdata.mds$points[,2])
abline(line2)
summary(line2)
plot(mds.data$salinity ~ newdata.mds$points[,2], xlab = "NMDS 2", ylab = "Salinity")
line2 = lm(mds.data$salinity ~ newdata.mds$points[,2])
abline(line2)
summary(line2)

#  Let's try the whole thing with PCA and adonis for the environmental variables

par(mar = c(4,5,1,1))
newdata.pca = rda(log(mds.data[,5:10]+1))
plot(newdata.pca, typ = "n")
newdata.pca.scores = scores(newdata.pca)
text(newdata.pca.scores$species[,1], newdata.pca.scores$species[,2], rownames(newdata.pca.scores$species))
pca.data.envfit = envfit(newdata.pca.scores, mds.data[,2:3])
plot(pca.data.envfit)

#  it appears that the nmds gives a better fit.  
