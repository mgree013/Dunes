germs = read.table(pipe("pbpaste"), header = T)

#Analysis 1- ANOVA
#Uniola
u.germs = germs[germs$Species == "Uniola",]
par(mar = c(4,5,1,1), mfrow = c(2,2))
boxplot(u.germs$day.28 ~ (u.germs$salinity), las = 1, ylab = "Uniola paniculata Germination (%)", ylim = c(0,100), xlab = "Salinity (ppt)")
u.sums = aggregate(u.germs$day.28, list(u.germs$salinity), mean)
lines((u.sums$Group.1), u.sums$x, typ = "l")

ggplot(u.germs, aes(x=as.factor(u.germs[,2]), y= u.germs[,11]), fill=as.factor(u.germs[,2]))+geom_boxplot() + geom_smooth(method = "lm", se=FALSE, aes(group=1))


ip.germs = germs[germs$Species == "Ipomea",]
boxplot(ip.germs$day.28 ~ (ip.germs$salinity), las = 1, ylab = "Ipomea imperati Germination (%)", ylim = c(0,100), xlab = "Salinity (ppt)")
ip.sums = aggregate(ip.germs$day.28, list(ip.germs$salinity), mean)
lines((ip.sums$Group.1), ip.sums$x, typ = "l")

ggplot(ip.germs, aes(x=as.factor(ip.germs[,2]), y= ip.germs[,11]), fill=as.factor(ip.germs[,2]))+geom_boxplot() + geom_smooth(method = "lm", se=FALSE, aes(group=1))


iv.germs = germs[germs$Species == "Iva",]
boxplot(iv.germs$day.28 ~ (iv.germs$salinity), las = 1, ylab = "Iva imbricata Germination (%)", ylim = c(0,100), xlab = "Salinity (ppt)")
iv.sums = aggregate(iv.germs$day.28, list(iv.germs$salinity), mean)
lines((iv.sums$Group.1), iv.sums$x, typ = "l")

dog<-ggplot(iv.germs, aes(x=as.factor(iv.germs[,2]), y= iv.germs[,11], fill=as.factor(iv.germs[,2])))+geom_boxplot() +labs(x="Salinity (ppt)",y=expression(paste(italic("Iva imbricata"), " Germination (%)"))) +guides(fill=FALSE)
cat<-ggplot(ip.germs, aes(x=as.factor(ip.germs[,2]), y= ip.germs[,11], fill=as.factor(ip.germs[,2])))+geom_boxplot() +labs(x="Salinity (ppt)",y=expression(paste(italic("Ipomea imperati"), " Germination (%)"))) +guides(fill=FALSE)
bat<-ggplot(u.germs, aes(x=as.factor(u.germs[,2]), y= u.germs[,11], fill=as.factor(u.germs[,2])))+geom_boxplot()+labs(x="Salinity (ppt)",y=expression(paste(italic("Uniola paniculata"), " Germination (%)"))) +guides(fill=FALSE)

star<-ggplot(iv.germs, aes(x=as.factor(iv.germs[,3]), y= iv.germs[,11], fill=as.factor(iv.germs[,3])))+geom_boxplot() +labs(x="Organic Matter (%)",y=expression(paste(italic("Iva imbricata"), " Germination  (%)"))) +guides(fill=FALSE)
bar<-ggplot(ip.germs, aes(x=as.factor(ip.germs[,3]), y= ip.germs[,11], fill=as.factor(ip.germs[,3])))+geom_boxplot() +labs(x="Organic Matter (%)",y=expression(paste(italic("Ipomea imperati"), " Germination (%)"))) +guides(fill=FALSE)
tar<-ggplot(u.germs, aes(x=as.factor(u.germs[,3]), y= u.germs[,11], fill=as.factor(u.germs[,3])))+geom_boxplot()+labs(x="Organic Matter (%)",y=expression(paste(italic("Uniola paniculata"), " Germination (%)"))) +guides(fill=FALSE)


ggplot(germs, aes(x=Species, y=germs[,11]))+geom_boxplot()

library(gridExtra)
grid.arrange(star,bar,tar, ncol=2)
grid.arrange(dog,cat,bat, ncol=2)
#OMMMM
u.germs = germs[germs$Species == "Uniola",]
par(mar = c(4,5,1,1), mfrow = c(2,2))
boxplot(u.germs$day.28 ~ (u.germs$organic.matter), las = 1, ylab = "Uniola paniculata Germination (%)", ylim = c(0,100), xlab = "Organic Matter (%)")
u.sums = aggregate(u.germs$day.28, list(u.germs$organic.matter), mean)
lines((u.sums$Group.1), u.sums$x, typ = "l")



ip.germs = germs[germs$Species == "Ipomea",]
boxplot(ip.germs$day.28 ~ (ip.germs$organic.matter), las = 1, ylab = "Ipomea imperati Germination (%)", ylim = c(0,100), xlab = "Organic Matter (%)")
ip.sums = aggregate(ip.germs$day.28, list(ip.germs$organic.matter), mean)
lines((ip.sums$Group.1), ip.sums$x, typ = "l")

iv.germs = germs[germs$Species == "Iva",]
boxplot(iv.germs$day.28 ~ (iv.germs$organic.matter), las = 1, ylab = "Iva imbricata Germination (%)", ylim = c(0,100), xlab = "Organic Matter (%)")
iv.sums = aggregate(iv.germs$day.28, list(iv.germs$organic.matter), mean)
lines((iv.sums$Group.1), iv.sums$x, typ = "l")

#  Let's look at just salinity, with  boxplot, line, and anova

boxplot(u.germs$day.28 ~ (u.germs$salinity), las = 1, ylab = "Uniola Germination (%)", ylim = c(0,1), xlab = "Salinity")
u.sums = aggregate(u.germs$day.28, list(u.germs$salinity), mean)
lines((u.sums$Group.1), u.sums$x, typ = "l")
u.salinity=lm(u.germs$day.28~u.germs$salinity)
u.salinity.anova = aov(u.germs$day.28~u.germs$salinity)
summary(u.salinity.anova)




#  now, lets try looking at both salinity and organic matter together

boxplot(day.28 ~ organic.matter* salinity, data = u.germs, col = c(2,2,2,3,3,3,4,4,4,5,5,5,6,6,6))
u.tot.anova = aov(day.28 ~ salinity * organic.matter, data = u.germs)
summary(u.tot.anova)
u.organic=lm(u.germs$day.28~u.germs$organic.matter)

ip.interaction=lm(ip.germs$day.28~ip.germs$organic.matter*ip.germs$salinity)
u.interaction=lm(u.germs$day.28~u.germs$organic.matter*u.germs$salinity)
iv.interaction=lm(iv.germs$day.28~iv.germs$organic.matter*iv.germs$salinity)
#Iva
iv.germs = germs[germs$Species == "Iva",]
boxplot(iv.germs$day.28 ~ (iv.germs$salinity), las = 1, ylab = "Iva Germination", ylim = c(0,5), xlab = "Salinity")
iv.sums = aggregate(iv.germs$day.28, list(iv.germs$salinity), mean)
lines((iv.sums$Group.1), iv.sums$x, typ = "l")
iv.salinity=lm(iv.germs$day.28~iv.germs$salinity)
iv.salinity.anova = aov(iv.germs$day.28~iv.germs$salinity*iv.germs$organic.matter)
summary(iv.salinity.anova)
boxplot(day.28 ~ organic.matter* salinity, data = iv.germs, col = c(2,2,2,3,3,3,4,4,4,5,5,5,6,6,6))
iv.tot.anova = aov(day.28 ~ salinity * organic.matter, data = iv.germs)
summary(iv.tot.anova)

#Ipomea
ip.germs = germs[germs$Species == "Ipomea",]
boxplot(ip.germs$day.28 ~ (ip.germs$salinity), las = 1, ylab = "Ipomea Germination", ylim = c(0,5), xlab = "Salinity")
ip.sums = aggregate(ip.germs$day.28, list(ip.germs$salinity), mean)
lines((ip.sums$Group.1), ip.sums$x, typ = "l")
ip.salinity.anova = aov(ip.germs$day.28~ip.germs$salinity*ip.germs$organic.matter)
summary(ip.salinity.anova)
ip.salinity=lm(ip.germs$day.28~ip.germs$salinity)
boxplot(day.28 ~ organic.matter* salinity, data = ip.germs, col = c(2,2,2,3,3,3,4,4,4,5,5,5,6,6,6))
ip.tot.anova = aov(day.28 ~ salinity * organic.matter, data = ip.germs)
summary(ip.tot.anova)

#From ANOVA organic matter doesnt appear to be signifacnt as well as the interaction, so focus on salinity value
#ANOVA values in Table 2

#Analysis 2-Tukey Test differnces among species (Table 3)
#Now lets look at differences in final germianiton among species, salinity, and organic matter
#Re-paste "Germination" tab from St.George_Alldata
germs1 = read.table(pipe("pbpaste"), header = T)
attach(germs1)
germs1.aov=lm(germs1$day.28~germs1$Species+germs1$salinity+germs1$organic.matter+germs1$Species*germs1$salinity+germs1$Species*germs1$organic.matter+germs1$salinity*germs1$organic+germs1$Species*germs1$salinity*germs1$organic.matter)
par(mfrow=c(2,2))
plot(germs1.aov)

anova(germs1.aov)

germs2.aov=lm(germs1$day.28~germs1$Species+germs1$salinity+germs1$organic)
anova(germs2.aov)
library(multcomp)
summary(glht(lm(germs1$day.28~germs1$Species+germs1$salinity+germs1$organic), linfct=mcp(germs1$Species = "Tukey")))

#Tukeys comparison values in Table 3

#Analyzing Germiantion rate Linear Models

#Within species and among salinity treatments

Day=germ$Day
library(multcomp)
germ.aov=lm(Day~germ$ipomea.0+germ$ipomea.4+germ$ipomea.8+germ$ipomea.12)
anova(germ.aov)

Day=germ$Day
library(multcomp)
germ.aov=lm(Day~germ$uniola.0+germ$uniola.4+germ$uniola.8+germ$uniola.12)
anova(germ.aov)

Day=germ$Day
library(multcomp)
germ.aov=lm(Day~germ$iva.0+germ$iva.4+germ$iva.8+germ$iva.12)
anova(germ.aov)

#Within salinity treamtnet and among species
Day=germ$Day
library(multcomp)
germ.aov=lm(Day~germ$iva.0+germ$uniola.0+germ$ipomea.0)
anova(germ.aov)

Day=germ$Day
library(multcomp)
germ.aov=lm(Day~germ$iva.4+germ$uniola.4+germ$ipomea.4)
anova(germ.aov)

Day=germ$Day
library(multcomp)
germ.aov=lm(Day~germ$iva.8+germ$uniola.8+germ$ipomea.8)
anova(germ.aov)

Day=germ$Day
library(multcomp)
germ.aov=lm(Day~germ$iva.12+germ$uniola.12+germ$ipomea.12)
anova(germ.aov)

#Analyzing Final Germiantion: Generalized Linear Models

#Within species and among salinity treatments
germ = read.table(pipe("pbpaste"), header = T)

u.germ = germ[germ$Species == "Uniola",]
iv.germ = germ[germ$Species == "Iva",]
ip.germ = germ[germ$Species == "Ipomea",]

u.lm <-lm(germinate ~ salinity + day + organic.matter + salinity*day,family=binomial(link="logit"), data = u.germ)
iv.lm <-lm(germinate ~ salinity + day + organic.matter + salinity*day,family=binomial(link="logit"), data = iv.germ)
ip.lm <-lm(germinate ~ salinity + day + organic.matter + salinity*day,family=binomial(link="logit"), data = ip.germ)


u.glm <-glm(germinate ~ salinity + day + organic.matter + salinity*day,family=binomial(link="logit"), data = u.germ)
iv.glm <-glm(germinate ~ salinity + day + organic.matter + salinity*day,family=binomial(link="logit"), data = iv.germ)
ip.glm <-glm(germinate ~ salinity + day + organic.matter + salinity*day,family=binomial(link="logit"), data = ip.germ)

fit.u <- predict(u.glm, data=u.germ,type="response")
plot( u.germ$day,u.germ$germinate, data=u.germ)
lines(fit.u, type="l")


fit.iv <- predict(iv.glm, data=iv.germ,type="response")
plot( iv.germ$day,iv.germ$germinate, data=iv.germ)
lines(fit.iv, type="l")


fit.ip <- predict(ip.glm, data=ip.germ,type="response")
plot(ip.germ$day,ip.germ$germinate, data=ip.germ)
lines(fit.ip, type="l")


u.germ.0 = u.germ[u.germ$salinity == "0",]
u.germ.4 = u.germ[u.germ$salinity == "4",]
u.germ.8 = u.germ[u.germ$salinity == "8",]
u.germ.12 = u.germ[u.germ$salinity == "12",]

u.glm.0 <-glm(germinate ~ salinity + day + organic.matter + salinity*day,family=binomial(link="logit"), data = u.germ.0)
u.glm.4 <-glm(germinate ~ salinity + day + organic.matter + salinity*day,family=binomial(link="logit"), data = u.germ.4)
u.glm.8 <-glm(germinate ~ salinity + day + organic.matter + salinity*day,family=binomial(link="logit"), data = u.germ.8)
u.glm.12 <-glm(germinate ~ salinity + day + organic.matter + salinity*day,family=binomial(link="logit"), data = u.germ.12)

fit.u.0 <- predict(u.glm.0, data=u.germ.0,type="response")
plot( u.germ.0$day,u.germ.0$germinate, data=u.germ.0)
lines(fit.u.0, type="l")





#Figure 3- Germination rate through time for Species and for Salinity on Species
#Averaged germiainton rate for the 9 replicates of each salinity treament in excel
#paste "PLotting" tab from St. george ALL data
germ = read.table(pipe("pbpaste"), header = T)
layout(matrix(c(3,2,4,4), ncol=, byrow=TRUE), heights=c(5, 1))
par(mai=rep(.2,1))

Uniola.all <-subset(germ, Species =="Uniola")
Ipomea.all <-subset(germ, Species =="Ipomea")
Iva.all <-subset(germ, Species =="Iva")
Salinity.0<-subset(germ, Salinity =="0")
Salinity.4<-subset(germ, Salinity =="4")
Salinity.8<-subset(germ, Salinity =="8")
Salinity.12<-subset(germ, Salinity =="12")


Rad<-ggplot(data=Uniola.all, aes(x=Day, y= germination)) +geom_point(aes(shape=factor(Salinity))) + geom_line(aes(colour=factor(Salinity))) + scale_x_continuous(name="Time (Day)", limits=c(0, 28)) +scale_y_continuous(name="Germination Percentage", limits=c(0, 1)) +ggtitle("Uniola paniculata Germination")+ labs(shape="Salinity", colour="Salinity")
Bad<-ggplot(data=Ipomea.all, aes(x=Day, y= germination)) +geom_point(aes(shape=factor(Salinity)))+geom_line(aes(colour=factor(Salinity)))+ scale_x_continuous(name="Time (Day)", limits=c(0, 28)) +scale_y_continuous(name="Germination Percentage", limits=c(0, 1))+ ggtitle("Ipomea imperati Germination")+labs(shape="Salinity", colour="Salinity")
Tad<-ggplot(data=Iva.all, aes(x=Day, y= germination))+geom_point(aes(shape=factor(Salinity))) +geom_line(aes(colour=factor(Salinity)))+ scale_x_continuous(name="Time (Day)", limits=c(0, 28)) +scale_y_continuous(name="Germination Percentage", limits=c(0, 1))+ ggtitle("Iva Imbricata Germination") +labs(shape="Salinity", colour="Salinity")
ggarrange(Rad, Bad, Tad, ncol=2, nrow=2, common.legend = TRUE, legend="bottom")


Dill<-ggplot(data=Salinity.0, aes(x=Day,y=germination)) +geom_point(aes(shape=factor(Species)))+geom_line(aes(colour=factor(Species)))+ scale_x_continuous(name="Time (Day)", limits=c(0, 28)) +scale_y_continuous(name="Germination Percentage", limits=c(0, 1)) +ggtitle("Germination for Salinity 0 ppt")+ labs(shape="Species", colour="Species")
Bill<-ggplot(data=Salinity.4, aes(x=Day,y=germination)) +geom_point(aes(shape=factor(Species)))+geom_line(aes(colour=factor(Species)))+ scale_x_continuous(name="Time (Day)", limits=c(0, 28)) +scale_y_continuous(name="Germination Percentage", limits=c(0, 1)) +ggtitle("Germination for Salinity 4 ppt ")+ labs(shape="Species", colour="Species")
Phill<-ggplot(data=Salinity.8, aes(x=Day,y=germination))+geom_point(aes(shape=factor(Species))) +geom_line(aes(colour=factor(Species)))+ scale_x_continuous(name="Time (Day)", limits=c(0, 28)) +scale_y_continuous(name="Germination Percentage", limits=c(0, 1)) +ggtitle("Germination for Salinity 8 ppt")+ labs(shape="Species", colour="Species")
Nill<-ggplot(data=Salinity.12, aes(x=Day,y=germination)) +geom_point(aes(shape=factor(Species)))+geom_line(aes(colour=factor(Species)))+ scale_x_continuous(name="Time (Day)", limits=c(0, 28)) +scale_y_continuous(name="Germination Percentage", limits=c(0, 1)) +ggtitle("Germination for Salinity 12 ppt")+ labs(shape="Species", colour="Species")
ggarrange(Dill, Phill, Bill,Nill, ncol=2, nrow=2, common.legend = TRUE, legend="bottom")

par(mai=c(0,0,0,0))
plot.new()
legend(x="center", c("O","4","8","12"),xpd = TRUE, horiz=TRUE, inset=c(0,0), lty=c(0,2,0,0), pch=c(4,0,19,15), title="Legend")





#Germination rate <- Survival Analysis

#Read data
library(survival)
germsrate = read.table(pipe("pbpaste"), header = T)
summary(germsrate)

Uniola <- subset(germsrate, Species == "Uniola")
Uniola.0 <-subset(Uniola, Salinity=="0")
Uniola.4 <-subset(Uniola, Salinity=="4")
Uniola.8 <-subset(Uniola, Salinity=="8")
Uniola.12 <-subset(Uniola, Salinity=="12")
Iva <- subset(germsrate, Species == "Iva")
Iva.0 <-subset(Iva, Salinity=="0")
Iva.4 <-subset(Iva, Salinity=="4")
Iva.8 <-subset(Iva, Salinity=="8")
Iva.12 <-subset(Iva, Salinity=="12")
Ipomea <- subset(germsrate, Species == "Pomea")
Ipomea.0 <-subset(Ipomea, Salinity=="0")
Ipomea.4 <-subset(Ipomea, Salinity=="4")
Ipomea.8 <-subset(Ipomea, Salinity=="8")
Ipomea.12 <-subset(Ipomea, Salinity=="12")
S.12 <-subset(germsrate, Salinity == (12))
S.8 <- subset(germsrate, Salinity == (8))
S.4 <- subset(germsrate, Salinity == (4))
S.0 <- subset(germsrate, Salinity == (0))


#K-M survival curve
S<- survfit(Surv(Time, Event) ~ 1, data=germsrate, type='kaplan-meier')
print(summary(S))
plot(S)

#Add Treatments (diff)
S1<- survfit(Surv(Time, Event, type="right")~  Species * Salinity * OM, data =  germsrate )
S2<- survdiff(Surv(Time, Event) ~ Salinity * Species , data =  germsrate)
summary(S1) #p-value signif reject null that curves are same
print(S2)
plot(S1, xlim=c(0,28), ylim=c(0,1), xlab= "Time (days)", ylab= "Proportion seeds not germinated", col=c(1:20))

#Cox Proportional Hazard for All Data. Doesnt violate non zero slpoe
coxmod<-coxph(formula=Surv(Time, Event, type="right")~ Species  + Salinity + OM+ Species*Salinity  + Species*OM + Salinity*OM +Salinity*OM*Species , data =  germsrate)
coxmod<-coxph(formula=Surv(Time, Event, type="right")~ strata(Species) + strata(Salinity) + strata(OM) , data =  germsrate)

summary(coxmod)
modzph<-cox.zph(coxmod)
modzph
plot(modzph)






#Since S2 p-value sig; reject null H0 that curves are the same

#PLotting Survival Graphs
library(ggplot2)
library(GGally)
library(survival)
library(ggpubr)



S1<- survfit(Surv(Time, Event, type="right")~  Species , data =  S.0)
S1.1<-ggsurv(S1, CI="TRUE", surv.col = "gg.def", cens.col = "gg.def", xlab= "Time (days)", ylab= "Proportion seeds not germinated", main=" Salininty 0/32") 

S2<- survfit(Surv(Time, Event, type="right")~  Species , data =  S.4)
S2.2<-ggsurv(S2, CI="TRUE", surv.col = "gg.def", cens.col = "gg.def", xlab= "Time (days)", ylab= "Proportion seeds not germinated", main=" Salininty 4/32")

S3<- survfit(Surv(Time, Event, type="right")~  Species , data =  S.8)
S3.3<-ggsurv(S3, CI="TRUE", surv.col = "gg.def", cens.col = "gg.def", xlab= "Time (days)", ylab= "Proportion seeds not germinated", main=" Salininty 8/32")

S4<- survfit(Surv(Time, Event, type="right")~  Species , data =  S.12)
S4.4<-ggsurv(S4, CI="TRUE", surv.col = "gg.def", cens.col = "gg.def", xlab= "Time (days)", ylab= "Proportion seeds not germinated", main=" Salininty 12/32")

ggarrange(S1.1,S2.2,S3.3,S4.4, ncol=2, nrow=2, common.legend = TRUE, legend="bottom")



S1<- survfit(Surv(Time, Event)~  Salinity  , data =  Ipomea)
S1.1<-ggsurv(S1, CI="TRUE", surv.col = "gg.def", cens.col = "gg.def", xlab= "Time (days)", ylab= "Proportion seeds not germinated", main= "Ipomea Survival")

S2<- survfit(Surv(Time, Event)~  Salinity  , data =  Iva)
S2.1<-ggsurv(S2, CI="TRUE", surv.col = "gg.def", cens.col = "gg.def", xlab= "Time (days)", ylab= "Proportion seeds not germinated", main= "Iva Survival")

S3<- survfit(Surv(Time, Event)~  Salinity  , data =  Uniola)
S3.1<-ggsurv(S3, CI="TRUE", surv.col = "gg.def", cens.col = "gg.def", xlab= "Time (days)", ylab= "Proportion seeds not germinated", main= "Uniola Survival")

ggarrange(S1.1,S2.1,S3.1, ncol=3, nrow=1, common.legend = TRUE, legend="bottom")



