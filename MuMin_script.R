#packages to install: summarytools and MuMIn
library(MuMIn)
library(summarytools)
library(Hmisc)
library(corrplot)

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

dd2.globm<-dredge(globm, rank="AICc", m.lim=c(0,3), fixed="Species", subset=!(Petal.Length & Sepal.Length) & !(Petal.Width & Sepal.Length) & !(Petal.Width & Petal.Length))

getAllTerms(globm)#helpful sometimes.

#try to do a polynomial? And also add in gamm to show the mixed effects.