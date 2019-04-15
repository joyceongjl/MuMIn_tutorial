#packages to install: summarytools and MuMIn
library(MuMIn)
library(summarytools)
library(Hmisc)
library(corrplot)
library(ggplot2)
library(nlme)
library(dplyr)

view(dfSummary(iris))
str(iris)
plot(iris)
iris$Insects<-runif(150,0,25)
cormat<-rcorr(as.matrix(iris[,c(1, 3:4, 6)]))#from package hmisc
corrplot(cormat$r, type="lower", diag=FALSE, p.mat=cormat$P, sig.level=0.01, insig="blank")#from pkg corrplot

m0<-lm(Sepal.Width ~ 1, data=iris)
m1<-lm(Sepal.Width ~ Sepal.Length, data=iris)
m2<-lm(Sepal.Width ~ Sepal.Length + Species, data=iris)
m3<-lm(Sepal.Width ~ Petal.Width, data=iris)
m4<-lm(Sepal.Width ~ Petal.Width + Species, data=iris)
m.out<-model.sel(m0,m1,m2,m3,m4)

options(na.action="na.fail")
globm<-lm(Sepal.Width ~ Sepal.Length + Petal.Length + Petal.Width + Species + Insects, data=iris)
dd.globm<-dredge(globm, rank="AICc", m.lim=c(0,3), subset=!(Petal.Length & Sepal.Length) & !(Petal.Width & Sepal.Length) & !(Petal.Width & Petal.Length))
summary(get.models(dd.globm, 1)[[1]])
importance(dd.globm)

#by default, intercept is always fixed. Can also add other variables that should always be in the models
dd2.globm<-dredge(globm, rank="AICc", m.lim=c(0,3), fixed="Species", subset=!(Petal.Length & Sepal.Length) & !(Petal.Width & Sepal.Length) & !(Petal.Width & Petal.Length))

getAllTerms(globm)#helpful sometimes.

#trial with polynomial
globm2<-lm(Sepal.Width ~ Sepal.Length + Petal.Length + Species + Petal.Width + I(Petal.Width^2) + I(Petal.Width^3), data=iris)
dd.globm2<-dredge(globm2, rank="AICc", m.lim=c(0,3), subset=!(Petal.Length & Sepal.Length) & 
                    !(Petal.Width & Sepal.Length) & !(Petal.Width & Petal.Length))
summary(get.models(dd.globm2, 1)[[1]])

#trial with linear mixed effects
data("ChickWeight") #if does not appear
str(ChickWeight)
view(dfSummary(ChickWeight))
ggplot(ChickWeight, aes(x = Time, y = weight, col=Chick)) +geom_line ()+ geom_point() + facet_wrap(~Diet)

#add in random variable
cloudscore<-as.data.frame(ChickWeight[1:12,2])
colnames(cloudscore)<-"Time"
cloudscore$cloud<-runif(12,0,9)
ChickWeight2<-as.data.frame(ChickWeight)
ChickWeight2<-ChickWeight2 %>% left_join(cloudscore, by="Time")
str(ChickWeight2)

chickglobm<-lme(weight ~ Diet + poly(Time,2) + cloud, random= ~ Time|Chick, data=ChickWeight2, method="ML")
dd.chick<-dredge(chickglobm, rank="AICc", m.lim=c(0,4))
summary(get.models(dd.chick, 1)[[1]])#note that final model parameters should be done with REML
#Note: for list of supported models, pg 38-39 from pdf on CRAN and in Github