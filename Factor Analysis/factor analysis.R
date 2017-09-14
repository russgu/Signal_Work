setwd('/Users/guzi/Desktop/SignalDataScience/day10')
library(dplyr)
library(corrplot)
library(DAAG)
library(psych)


#PCA
set.seed(1); factors=data.frame(x=rnorm(n=100),y=rnorm(n=100),z=rnorm(n=100))

noisyProxies=function(feature,k,correlation){
  if(sum(is.na(feature))>0) stop ("There are NAs")
  df=matrix(data=NA, ncol=k,nrow=length(feature))
  set.seed(4)
  for(i in 1:k){
    for(j in 1:length(feature)){
      error=rnorm(1)*sqrt(1-correlation^2)
      df[j,i]=sum(correlation*feature[j])+error 
    }
  }
  return(data.frame(df))
}
x.noisy=noisyProxies(factors$x,4,0.9)
y.noisy=noisyProxies(factors$y,3,0.9)
noises=cbind(x.noisy,y.noisy)
colnames(noises)[5:7]=c('Y1','Y2','Y3')

corrplot(as.matrix(cor(noises)),is.corr=FALSE)

#principal component
p.factors=prcomp(factors)
p.compare=cbind(noises,p.factors$x)
corrplot(as.matrix(cor(p.compare)),is.corr=FALSE)

#orthogonal factor analysis
fa.factors=fa(noises,nfactors=2,rotate="varimax")
fa.compare=cbind(noises,fa.factors$scores)
corrplot(cor(as.matrix(fa.compare)),is.corr=FALSE)

factors2=matrix(NA, ncol=50,nrow=100)
set.seed(1)
for(i in 1:50){
  factors2[,i]=factors$x*runif(1)+factors$y*runif(1)+factors$z*runif(1)+0.5*rnorm(nrow(factors))
}
factors2=data.frame(factors2)

pc.50=prcomp(factors2)
p.compare=cbind(factors,pc.50$x[,1:3])
corrplot(as.matrix(cor(p.compare)),is.corr=FALSE)

fa.factors2=fa(factors2,nfactors=3,rotate="varimax")
fa.compare2=cbind(factors,fa.factors2$scores)
corrplot(cor(as.matrix(fa.compare2)),is.corr=FALSE)


#oblique factor
w=0.5*factors$x+factors$y
w.y=cbind(w,factors$y)
corrplot(w.y,is.corr=FALSE)

noise.x=noisyProxies(factors$x,10,0.8)
noise.w=noisyProxies(w,4,0.8)

corrplot(as.matrix(noise.x),is.corr=FALSE)
corrplot(as.matrix(noise.w),is.corr=FALSE)

w.x=cbind(noise.w,noise.x)

fa.factors3=fa(w.x,nfactors=2,rotate="varimax")
fa.compare3=cbind(factors,fa.factors3$scores)
corrplot(cor(as.matrix(fa.compare3)),is.corr=FALSE)

fa.factors4=fa(w.x,nfactors=2,rotate="oblimin")
fa.compare4=cbind(factors,fa.factors4$scores)
corrplot(cor(as.matrix(fa.compare4)),is.corr=FALSE)


# Speed Dating ------------------------------------------------------------
df=read.csv("speed-dating-aggregated.csv")

activities=select(df,sports:yoga)
for(i in 1:4){
  varimax.activities=fa(activities,nfactors=i,rotate="varimax")
  oblimin.activities=fa(activities,nfactors=i,rotate="oblimin")
  p1=corrplot(varimax.activities$loadings, title="varimax")
  p2=corrplot(oblimin.activities$loadings, title="oblimin")
}


# Big Five ----------------------------------------------------------------

df2=read.table("data.csv",sep="\t",header=TRUE)
df2.50=select(df2,E1:O10)
varimax.bf=fa(df2.50,nfactors=5,rotate="varimax")
oblimin.bf=fa(df2.50,nfactors=5,rotate="oblimin")
PCA.bf=prcomp(df2.50,scale.=TRUE)
total=cbind(df2.50,PCA.bf$x,varimax.bf$scores,oblimin.bf$scores)
corrplot(cor(total),is.corr = FALSE)

#logistic regression
View(df2)
genders=df2$gender-1
gender.df=cbind(genders,df2.50)
gender.df=filter(gender.df,genders!=-1 & genders!=2)
variables.model=glm(genders ~.,data=gender.df,family="binomial")

gender.varimax=cbind(genders,varimax.bf$scores)
gender.varimax=data.frame(gender.varimax)
gender.varimax=filter(gender.varimax, genders!=-1 & genders!=2)
gender.oblimin=cbind(genders,oblimin.bf$scores)
gender.oblimin=data.frame(gender.oblimin)
gender.oblimin=filter(gender.oblimin,genders!=-1 & genders!=2)

varimax.model=glm(genders ~.,data=gender.varimax,family="binomial")
oblimin.model=glm(genders ~.,data=gender.varimax,family="binomial")

coef.varimax=coef(varimax.model)
coef.oblimin=coef(oblimin.model)
coef.variables=coef(variables.model)

coef.varimax
coef.oblimin
coef.variables
