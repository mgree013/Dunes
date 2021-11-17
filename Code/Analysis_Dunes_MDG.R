# Dunes Data analysis 
# December 2016
# Data from St. George Island  beach transects

#this reads data from buffer
newdata.all = read.table(pipe("pbpaste"), header = T)
head(newdata.all)
newdata = newdata.all[,1:10]
library(vegan)
par(mar = c(4,5,1,1))

# make plot for sesuvium
avs.sesuvium = aggregate(newdata$sesuvium, list(newdata$distance), mean)
plot(avs.sesuvium$x~avs.sesuvium$Group.1, typ = "b", ylim = c(0, 22), lwd = 2, xlab = "Distance from shore", ylab = "Average plant cover")

# add uniola to plot
avs.uniola = aggregate(newdata$uniola, list(newdata$distance), mean)
lines(avs.uniola$x~avs.uniola$Group.1, type = "b", col = "red", lwd = 2)

# add heterotheca to plot
#avs.heterotheca = aggregate(newdata$heterotheca, list(newdata$distance), mean)
#lines(avs.heterotheca $x~avs.heterotheca $Group.1, type = "b", col = "blue", lwd = 2)

# add chameasyce to plot
avs.chameasyce = aggregate(newdata$chameasyce, list(newdata$distance), mean)
lines(avs.chameasyce$x~avs.chameasyce$Group.1, type = "b", col = "orange", lwd = 2)

# add iva to plot
avs.iva = aggregate(newdata$iva, list(newdata$distance), mean)
lines(avs.iva$x~avs.iva$Group.1, type = "b", col = "green", lwd = 2)

# add ipomea to plot
avs.ipomea= aggregate(newdata$ipomea, list(newdata$distance), mean)
lines(avs.ipomea$x~avs.ipomea$Group.1, type = "b" , col= "purple" , lwd= 2)

# add oenothera to plot
#avs.oenothera=aggregate(newdata$oenothera, list(newdata$distance), mean)
#lines(avs.oenothera$x~avs.oenothera$Group.1, type= "b" , col= "yellow" , lwd= 2)

# add cakile to plot
avs.cakile= aggregate(newdata$cakile, list(newdata$distance), mean)
lines(avs.cakile$x~avs.cakile$Group.1, type = "b" , col= "yellow" , lwd= 2)

#plot averaged over longer distances
par(mar = c(4,5,1,1), mfrow = c(1,2))

# make plot for sesuvium
avs.sesuvium = aggregate(newdata$sesuvium, list(newdata$dist.group), mean)
plot(avs.sesuvium$x~avs.sesuvium$Group.1, typ = "b", ylim = c(0, 20), lwd = 2, xlab = "Distance from Beach", col = "orange", lty = 2)
sesuvium.spl = smooth.spline(avs.sesuvium$Group.1, avs.sesuvium$x)
lines(sesuvium.spl)

# add cakile to plot
avs.cakile  = aggregate(newdata$cakile , list(newdata$dist.group), mean)
lines(avs.cakile$x~avs.cakile  $Group.1, type = "b", col = "red", lwd = 2)

# add uniola to plot
avs.uniola = aggregate(newdata$uniola, list(newdata$dist.group), mean)
lines(avs.uniola$x~avs.uniola$Group.1, type = "b", col = "black", lwd = 2)

# add chameasyce to plot
avs.chameasyce = aggregate(newdata$chamesyce, list(newdata$dist.group), mean)
lines(avs.chameasyce $x~avs.chameasyce $Group.1, type = "b", col = "purple", lwd = 2)

# add iva to plot
avs.iva = aggregate(newdata$iva, list(newdata$dist.group), mean)
lines(avs.iva$x~avs.iva$Group.1, type = "b", col = "blue", lwd = 2)

# add ipomea to plot
avs.ipomea = aggregate(newdata$ipomea, list(newdata$dist.group), mean)
lines(avs.ipomea$x~avs.ipomea$Group.1, type = "b", col = "green", lwd = 2)
ipomea.spl = smooth.spline(avs.ipomea$Group.1, avs.ipomea$x)
lines(ipomea.spl)

#NMDS
dim(newdata.all)
mds.data = newdata.all[rowSums(newdata[,4:9]) > 0,]
dim(mds.data)
head(mds.data)

newdata.mds = metaMDS(mds.data[,4:9])
par(mar = c(4,5,1,1), mfrow = c(1,2))
stressplot(newdata.mds)

#Stressplots tells us that should be easy to oridnate

#Plotting Distiribution Data NMDS and Regression

gveg <-read.table(pipe("pbpaste"), header = T)
summary(gveg)

# gets rid of rows with all zeros
kveg = gveg[rowSums(gveg[,4:9]) >0,]

# shows the number of rows and columns in kveg
dim(kveg)

# does the NMDS analysis
bob = metaMDS(kveg[,4:9])

#  cool plot of NMDS analyses

###MAke grpahs more legible clear, bigger font etc...
# make our own window for nmds
quartz("NMDS of St. George Transects", 4.5, 9)
par(mar= c(4,5,1,1), mfrow = c(2,1))
plot(bob, type="t")


bobby.plot <- plot(bob, type = "n") 
points(bobby.plot, col="black", pch =16)
ordilabel(bobby.plot, display ="species", cex=1, font =4)

#orditorp(bob, display="species",cex= 1,font =2, pcol= "black")
ordisurf(bobby.plot, kveg[,2], add = T, labcex= 0.5,col="forestgreen")

bob.plot= plot(bob, type = "n")
points(bob, col="black", pch =8)
ordilabel(bob, display ="species", cex=1, font =4)


#orditorp(bob, display="species",cex=0.7, pcex =2, pcol= "black")
ordisurf(bob, kveg[,10], labcex=0.9, add = T,col="forestgreen")

dev.copy(png,"Figure1b.png",width=8, height=8, units="in",res=800) 
dev.off() 
#Regression against NMDS 1 values

quartz("Regressions for elevation and distance", 4.5, 9)
par(mar= c(4,5,1,1), mfrow = c(2,1))

plot(bob.plot$sites[,1] ~ kveg[,2], xlab = "Distance from shore", ylab = "NMDS 1 Score")
text(20,1.2,expression(y==0.03044*x -1.28225))
text(20,1,expression(R^2==0.3897))
bob.lr = lm(bob.plot$sites[,1] ~ kveg[,2])      #(NMDS values~ elevation/distance)
anova(bob.lr)
abline(bob.lr) 
summary(bob.lr)





plot(bob.plot$sites[,1] ~ kveg[,10], xlab = "Inverse Elevation", ylab = "NMDS 1 Score")
text(2.6,-1.3,expression(y==-0.9273 *x +3.0936))
text(2.6,-1.5,expression(R^2==0.08541))
bob.lr.elev = lm(bob.plot$sites[,1] ~ kveg[,10])
anova(bob.lr.elev)
abline(bob.lr.elev)
summary(bob.lr.elev)

big.anova = aov(bob.plot$sites[,1] ~ kveg[,10] + kveg[,2])
summary(big.anova)



adonis(bob.plot$sites[,1] ~ gveg[,10] * gveg[,2] , data = gveg, perm=1000)
#Environmental Variables vs NMDS axis


par(mfrow = c(2,3), mar = c(4,5,1,1))
plot(mds.data$percent.Moisture ~ newdata.mds$points[,1], xlab = "NMDS 1", ylab = "Soil Moisture")
line1 = lm(mds.data$percent.Moisture ~ newdata.mds$points[,1])
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
plot(mds.data$percent.Moisture ~ newdata.mds$points[,2], xlab = "NMDS 2", ylab = "Soil Moisture")
line1 = lm(mds.data$percent.Moisture ~ newdata.mds$points[,2])
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

#Nothing in here appears to be signifcant, probably need to take env data at every site where veg/elev data was gathered