setwd('/Users/guzi/Desktop/SignalDataScience/day9')
library(psych)
library(dplyr)
library(corrplot)
library(DAAG)
library(pROC)

df=msq
View(df)
features=select(df,active:scornful)
features2=select(df,active:scornful,Neuroticism,Extraversion)

colSums(is.na(features))
throw=names(rev(sort(colSums(is.na(features))))[1:10])

features=select(features, -one_of(throw))
features=na.omit(features)

features2=select(features2, -one_of(throw))
features2=na.omit(features2)

View(features2)

extraversion=select(features2,Extraversion)
neuroticism=select(features2,Neuroticism)

# features2=select(features2,-Neuroticism,-Extraversion)


top=function(n){
  PCs=prcomp(scale(features2),scale.=TRUE)
  PC=PCs$rotation[,n]
  rev(sort(abs(PC)))[1:10]
}

top(2)


PCs=prcomp(scale(features2),scale.=TRUE)
View(PCs$rotation)

PC5.10=PCs$rotation[,5:10]
corrplot(PC5.10,is.corr=FALSE)

p=PCs$x
p
qplot(x=1:ncol(p),y=PCs$sdev)
View(PCs$x)


# Principle Component Regression ------------------------------------------
features3=select(features2,-Extraversion,-Neuroticism)
pscores=prcomp(scale(features3),scale.=TRUE)$x
ncol(pscores)
head(features2)

rmseExtras=vector("list",65)
rmseNeuro=vector("list",65)
for(i in 1:ncol(pscores)){
  pc.extra=cbind(features2["Extraversion"],data.frame(pscores)[1:i])
  pc.neuro=cbind(features2["Neuroticism"],data.frame(pscores)[1:i])
  f.extra=formula(lm(Extraversion ~ .,pc.extra))
  f.neuro=formula(lm(Neuroticism ~ .,pc.neuro))
  
  model.extra=CVlm(data = pc.extra, form.lm=f.extra,m=10,seed=1)
  predict.extra=model.extra$cvpred
  actual.extra=model.extra$Extraversion
  rmseExtras[i]=rmse(predict.extra,actual.extra)
  
  model.neuro=CVlm(data = pc.neuro, form.lm=f.neuro,m=10,seed=1)
  predict.neuro=model.neuro$cvpred
  actual.neuro=model.neuro$Neuroticism
  rmseNeuro[i]=rmse(predict.neuro,actual.neuro)
}

rmse=function(x,y){
  sum=0
  for(i in 1:length(x)){
    sum=sum+((y[i]-x[i])**2)
  }
  mean_result=sum/length(x)
  return (sqrt(mean_result))
}


qplot(x=1:65,y=unlist(rmseNeuro))
qplot(x=1:65, y=unlist(rmseExtras))



# PCA on speed dating -----------------------------------------------------

df2=read.csv("speed-dating-aggregated.csv")
features=select(df2,sports:yoga)
features=na.omit(features)

#Corrplot
PCs.activities=prcomp(scale(features),scale.=TRUE)
p.activities=PCs.activities$rotation
p.scores=PCs.activities$x
p.eigen=PCs.activities$sdev
p.eigen
corrplot(p.activities,is.corr=FALSE)
p.total=cbind(p.scores,features)
View(p.total)
corrplot(cor(p.total),is.corr=TRUE)


#Predicting Race, Gender, and Career
df2=na.omit(df2)
df2.race=filter(df2,race==2 | race==4)
df2.career=filter(df2, career_c==2 | career_c==7)

#race(2)=White
#race(4)=Asian
#career(2)=Academic
#career(7)=Business

table()
df2.career$career_c=ifelse(df2.career$career_c == 2, 0, 1)
df2.race$race=ifelse(df2.race$race == 2,0,1)

features.gender=select(df2,-(attr_o:career_c))
features.race=select(df2.race,-(gender:amb_o),-career_c)
features.career=select(df2.career,-(gender:race))


s.feat.g=select(features.gender,sports:yoga)
PCs.activities.g=prcomp(scale(s.feat.g),scale.=TRUE)
p.scores.gender=PCs.activities.g$x

s.feat.r=select(features.race,sports:yoga)
PCs.activities.r=prcomp(scale(s.feat.r),scale.=TRUE)
p.scores.race=PCs.activities.r$x

s.feat.c=select(features.career,sports:yoga)
PCs.activities.c=prcomp(scale(s.feat.c),scale.=TRUE)
p.scores.career=PCs.activities.c$x

# coef.gender=vector("list",ncol(p.scores.gender))
# coef.race=vector("list",ncol(p.scores.race))
# coef.career=vector("list",ncol(p.scores.career))


actual.gender=features.gender$gender
actual.race=features.race$race
actual.career=features.career$career_c

for(i in 1:ncol(p.scores.gender)){
  pc.gender=cbind(features.gender["gender"],data.frame(p.scores.gender)[1:i])
  pc.race=cbind(features.race["race"],data.frame(p.scores.race)[1:i])
  pc.career=cbind(features.career["career_c"],data.frame(p.scores.career)[1:i])
  
  model.gender=glm(gender ~ .,data=pc.gender,family="binomial")
  model.race=glm(race ~ .,data=pc.race,family="binomial")
  model.career=glm(career_c ~ .,data=pc.career,family="binomial")
  
  co.gender=data.frame(summary(model.gender)$coefficients)
  probs.gender=predict(model.gender,type="response")
  r.gender=roc(actual.gender,prob.gender)
  
  co.race=data.frame(summary(model.race)$coefficients)
  probs.race=predict(model.race,type="response")
  r.race=roc(actual.race,prob.race)
  
  co.career=data.frame(summary(model.career)$coefficients)
  probs.career=predict(model.career,type="response")
  r.career=roc(actual.career,prob.career)
  
}

r.gender$auc
r.race$auc
r.career$auc

#pROC

##gender

plot(r.gender)

##race
plot(r.race)

##career
plot(r.career)

#multinomial prediction
View(df2)
common=sort(table(unlist(df2$career_c)),decreasing=TRUE)[1:4]
common=as.numeric(names(common))
df2.common=filter(df2,df2$career_c %in% common)
feats=select(df2.common,sports:yoga,attr_o)
multi.career=glmnet(as.matrix(feats),df2.common$career_c,family="multinomial")
predict.multi=predict(multi.career,as.matrix(feats),s=0)
predict.multi=data.frame(predict.multi)

probabilities=function(preds,rownum){
  sum.logs=sum(exp(preds[rownum,]))
  sapply(preds[rownum,],function(m) exp(m)/sum.logs)
}
probs=probabilities(predict.multi,2)
probs
sum(probs)

names(predict.multi)=c("Lawyer","Academic","Creative","Business")

View(predict.multi)

PC=prcomp(scale(predict.multi),scale.=TRUE)
pc.rotate=PC$rotation
corrplot(pc.rotate,is.corr=FALSE)
