#Germination Figures 3 and 4:
#Read in Germ.Percent file
germs = read.table(pipe("pbpaste"), header = T)
library(ggplot2)
library(cowplot)
library(multcomp)


u.germs = germs[germs$Species == "Uniola",]
ip.germs = germs[germs$Species == "Ipomea",]
iv.germs = germs[germs$Species == "Iva",]

dog<-ggplot(iv.germs, aes(x=as.factor(iv.germs[,2]), y= iv.germs[,11], fill=as.factor(iv.germs[,2])))+geom_boxplot() +labs(x="Salinity (ppt)",y=expression(paste(italic("Iva imbricata"), " Germination (%)"))) +guides(fill=FALSE)
cat<-ggplot(ip.germs, aes(x=as.factor(ip.germs[,2]), y= ip.germs[,11], fill=as.factor(ip.germs[,2])))+geom_boxplot() +labs(x="Salinity (ppt)",y=expression(paste(italic("Ipomea imperati"), " Germination (%)"))) +guides(fill=FALSE)
bat<-ggplot(u.germs, aes(x=as.factor(u.germs[,2]), y= u.germs[,11], fill=as.factor(u.germs[,2])))+geom_boxplot()+labs(x="Salinity (ppt)",y=expression(paste(italic("Uniola paniculata"), " Germination (%)"))) +guides(fill=FALSE)

star<-ggplot(iv.germs, aes(x=as.factor(iv.germs[,3]), y= iv.germs[,11], fill=as.factor(iv.germs[,3])))+geom_boxplot() +labs(x="Organic Matter (%)",y=expression(paste(italic("Iva imbricata"), " Germination  (%)"))) +guides(fill=FALSE)
bar<-ggplot(ip.germs, aes(x=as.factor(ip.germs[,3]), y= ip.germs[,11], fill=as.factor(ip.germs[,3])))+geom_boxplot() +labs(x="Organic Matter (%)",y=expression(paste(italic("Ipomea imperati"), " Germination (%)"))) +guides(fill=FALSE)
tar<-ggplot(u.germs, aes(x=as.factor(u.germs[,3]), y= u.germs[,11], fill=as.factor(u.germs[,3])))+geom_boxplot()+labs(x="Organic Matter (%)",y=expression(paste(italic("Uniola paniculata"), " Germination (%)"))) +guides(fill=FALSE)

plot_grid(star,bar,tar, ncol=3 ,labels = c('A', 'B', 'C'))
plot_grid(dog,cat,bat, ncol=3 ,labels = c('A', 'B', 'C'))

#Figure 2
#paste field data
newdata.all = read.table(pipe("pbpaste"), header = T, sep = "\t")
head(newdata.all)
elevation = max(newdata.all$laser.ht) - newdata.all$laser.ht
new.again = cbind(newdata.all,elevation)

head(new.again)
newdata = new.again
#newdata = newdata.all[,1:10]
library(vegan)

dim(newdata)
head(newdata)
mds.data = newdata[rowSums(newdata[,4:9]) > 0,]
md.data=decostand(mds.data[,4:9], method="hellinger")
dim(md.data)
stressplot(md.data)
set.seed(201)
newdata.mds = metaMDS(md.data, try = 100)
plot(newdata.mds,typ= "n", xlab = "NMDS Axis 1", ylab = "NMDS Axis 2")
text(newdata.mds$species[,1], newdata.mds$species[,2], rownames(newdata.mds$species), cex=0.7, col ="black")
points(newdata.mds$points[,1], newdata.mds$points[,2],  pch = 1) #I think it looks better with black dots and cant invert elevation so maybe you can run it on your end
ordisurf(newdata.mds, mds.data[,2], prioirty=,labcex=0.9, add = T,col="forestgreen")

env.data = cbind(mds.data$distance, mds.data$elevation)
mds.data.envfit = envfit(newdata.mds, env.data)

plot(mds.data.envfit, col = "black", labels = c("Distance", "Elevation"), lwd = 2)

#elevation
plot(newdata.mds,typ= "n", xlab = "NMDS Axis 1", ylab = "NMDS Axis 2")
text(newdata.mds$species[,1], newdata.mds$species[,2], rownames(newdata.mds$species), cex=0.7, col ="black")
points(newdata.mds$points[,1], newdata.mds$points[,2],  pch = 1) #I think it looks better with black dots and cant invert elevation so maybe you can run it on your end
ordisurf(newdata.mds, mds.data[,11], prioirty=,labcex=0.9, add = T,col="forestgreen")

env.data = cbind(mds.data$distance, mds.data$elevation)
mds.data.envfit = envfit(newdata.mds, env.data)

plot(mds.data.envfit, col = "black", labels = c("Distance", "Elevation"), lwd = 2)

#Table1
#PERMANOVA Analysis

adonis(md.data~distance
       *elevation, data=mds.data,  method="bray")

betad <- betadiver(md.data, "z")
adonis(betad ~ distance*elevation, data=mds.data, perm=200)


#Table II and III
###Analayzing germiantion data GLM
germs1 = read.table(pipe("pbpaste"), header = T)

uniola = subset(germs1, Species=="Uniola")
ipomea = subset(germs1, Species=="Ipomea")
iva = subset(germs1, Species=="Iva")

library(bbmle)
library(MuMIn)

#Ipomea
dog1<-glm(ipomea$day.28~salinity*organic.matter, family=binomial(), data=ipomea)
dog2<-glm(ipomea$day.28~organic.matter, family=binomial(link="logit"), data=ipomea)
dog3<-glm(ipomea$day.28~salinity, family=binomial(link="logit"), data=ipomea)
dog4<-glm(ipomea$day.28~1 , family=binomial(link="logit"), data=ipomea)

reported.table2 <- bbmle::AICtab(dog1, dog2,dog3,dog4, weights = TRUE, sort = FALSE)
pseudoR1 <- ((dog1$null.deviance-dog1$deviance)/dog1$null.deviance)
pseudoR2 <- ((dog2$null.deviance-dog2$deviance)/dog2$null.deviance)
pseudoR3 <- ((dog3$null.deviance-dog3$deviance)/dog3$null.deviance)
pseudoR4 <- ((dog4$null.deviance-dog4$deviance)/dog4$null.deviance)

#Iva
dog1<-glm(iva$day.28~salinity*organic.matter, family=binomial(link="logit"), data=iva)
dog2<-glm(iva$day.28~organic.matter, family=binomial(link="logit"), data=iva)
dog3<-glm(iva$day.28~salinity, family=binomial(link="logit"), data=iva)
dog4<-glm(iva$day.28~1, family=binomial(link="logit"), data=iva)
reported.table2 <- bbmle::AICtab(dog1, dog2,dog3,dog4, weights = TRUE, sort = FALSE)

pseudoR1 <- ((dog1$null.deviance-dog1$deviance)/dog1$null.deviance)
pseudoR2 <- ((dog2$null.deviance-dog2$deviance)/dog2$null.deviance)
pseudoR3 <- ((dog3$null.deviance-dog3$deviance)/dog3$null.deviance)
pseudoR4 <- ((dog4$null.deviance-dog4$deviance)/dog4$null.deviance)

#Uniola
dog1<-glm(uniola$day.28~salinity*organic.matter, family=binomial(link="logit"), data=uniola)
dog2<-glm(uniola$day.28~salinity, family=binomial(link="logit"), data=uniola)
dog3<-glm(uniola$day.28~organic.matter, family=binomial(link="logit"), data=uniola)
dog4<-glm(uniola$day.28~1, family=binomial(link="logit"), data=uniola)

reported.table2 <- bbmle::AICtab(dog1, dog2,dog3,dog4, weights = TRUE, sort = FALSE)
pseudoR1 <- ((dog1$null.deviance-dog1$deviance)/dog1$null.deviance)
pseudoR2 <- ((dog2$null.deviance-dog2$deviance)/dog2$null.deviance)
pseudoR3 <- ((dog3$null.deviance-dog3$deviance)/dog3$null.deviance)
pseudoR4 <- ((dog4$null.deviance-dog4$deviance)/dog4$null.deviance)

#All Species
dog2<-glm(germs1$day.28~Species*salinity*organic.matter, family=binomial(link = "logit"), data=germs1)
dog3<-glm(germs1$day.28~Species+salinity*organic.matter, family=binomial(link = "logit"), data=germs1)
dog4<-glm(germs1$day.28~Species*salinity+organic.matter, family=binomial(link = "logit"), data=germs1)
dog5<-glm(germs1$day.28~Species*organic.matter+salinity, family=binomial(link = "logit"), data=germs1)
dog6<-glm(germs1$day.28~Species+salinity+organic.matter, family=binomial(link = "logit"), data=germs1)
dog7<-glm(germs1$day.28~Species*salinity, family=binomial(link = "logit"), data=germs1)
dog8<-glm(germs1$day.28~Species+salinity,family=binomial(link = "logit"), data=germs1)
dog9<-glm(germs1$day.28~Species+organic.matter, family=binomial(link = "logit"), data=germs1)
dog10<-glm(germs1$day.28~Species*organic.matter, family=binomial(link = "logit"), data=germs1)
dog11<-glm(germs1$day.28~salinity+organic.matter, family=binomial(link = "logit"), data=germs1)
dog12<-glm(germs1$day.28~salinity*organic.matter, family=binomial(link = "logit"), data=germs1)
dog13<-glm(germs1$day.28~Species, family=binomial(link = "logit"), data=germs1)
dog14<-glm(germs1$day.28~salinity, family=binomial(link = "logit"), data=germs1)
dog15<-glm(germs1$day.28~organic.matter, family=binomial(link = "logit"), data=germs1)
dog16<-glm(germs1$day.28~1, family=binomial(link = "logit"), data=germs1)
reported.table2 <- bbmle::AICtab(dog6,dog8,dog9,dog11,dog13,dog14,dog15,dog16, weights = TRUE, sort = FALSE)

reported.table2 <- bbmle::AICtab(dog2,dog3,dog4,dog5,dog6,dog7,dog8,dog9,dog10,dog11,dog12,dog13,dog14,dog15,dog16, weights = TRUE, sort = FALSE)
pseudoR16 <- ((dog15$null.deviance-dog15$deviance)/dog15$null.deviance)

lme4.models <- r.squaredGLMM((dog2))
lme4.models
#TABLE 4 Tukey pos hoc test
dog3<-glm(germs1$day.28~Species +salinity + organic.matter, family=binomial(link="logit"), data=germs1)
dogg<-glht(dog3, linfct=mcp(Species = "Tukey"))




###########################################################################################################################################################################
