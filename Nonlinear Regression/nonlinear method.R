setwd('/Users/guzi/Desktop/SignalDataScience/day13')
install.packages("gbm")

library(dplyr)
library(glmnet)
library(ggplot2)
library(caret)
library(gbm)
library(caretEnsemble)

# Getting Started ---------------------------------------------------------

df=read.csv("winequality-white.csv",sep=";")
View(df)

features=select(df,fixed.acidity:alcohol)
response=df$quality

qplot(y=df$quality,x=features[,1])+geom_smooth()
qplot(y=df$quality,x=features[,2])+geom_smooth()
qplot(y=df$quality,x=features[,3])+geom_smooth()
qplot(y=df$quality,x=features[,4])+geom_smooth()
qplot(y=df$quality,x=features[,5])+geom_smooth()


#baseline model useing elastic net
##caret supports a very large number of different models
caret_reg = function(x, y, method, grid, ...) { 
  set.seed(1)
  control = trainControl(method="repeatedcv", repeats=1,number=3, verboseIter=TRUE) 
  train(x=x, y=y, method=method, tuneGrid=grid,trControl=control, metric="RMSE", preProcess=c("center", "scale"), ...)
}

alphas=seq(0, 1, 0.1)
lambdas=2^seq(-4,1,length.out=20)
grids=expand.grid(alphas,lambdas)
str(grids)
colnames(grids)=c("alpha","lambda")

##fit elastic net for wine quality
results=data.frame(matrix(ncol=2))
names(results)=c("method","rmse")
fit=caret_reg(x=scale(features),y=response,method="glmnet",grid=grids)
best.alpha=fit$bestTune$alpha
best.lambda=fit$bestTune$lambda
best.rmse=min(fit$results$RMSE)
fit$results
results$rmse=best.rmse
results$method="glmnet"
View(results)

head(results)

# K-Nearest Neighbors -----------------------------------------------------
k=expand.grid(seq(1, 20,1))
str(k)
colnames(k)=c("k")
knn=caret_reg(x=scale(features),y=response,method="knn",grid=k)
rmse.k=min(knn$results$RMSE)
results=rbind(results, c("knn", rmse.k))
View(results)



# multivariate adaptive regression spline ---------------------------------

degree=1:5
nprune=10:30
mars=expand.grid(nprune,degree)
names(mars)=c("nprune","degree")
fit.mars=caret_reg(x=scale(features),y=response,method="earth",grid=mars)
rmse.mars=min(fit.mars$results$RMSE)
results=rbind(results, c("mars", rmse.mars))
View(results)



# Decision Tree  ----------------------------------------------------------

cp=10^seq(-3, 0, length.out=10)
cp.grid=expand.grid(cp)
names(cp.grid)=c("cp")
fit.tree=caret_reg(x=scale(features),y=response,method="rpart",grid=cp.grid)
fit.tree
rmse.tree=min(fit.tree$results$RMSE)
results=rbind(results,c("tree",rmse.tree))
View(results)


# Random forests ----------------------------------------------------------

mtry=2:6
mtry.grid=expand.grid(mtry)
names(mtry.grid)=c("mtry")
fit.forest=caret_reg(x=scale(features),y=response,method="ranger",grid=mtry.grid,importance="impurity")
rmse.forest=min(fit.forest$results$RMSE)

#variable importance(gini impurity)
fit.forest$finalModel$variable.importance
#out of bag error
fit.forest$finalModel$prediction.error

################# Why out of bag error is smaller than rmse?

results=rbind(results,c("forest",rmse.forest))


# Gradient boosted trees --------------------------------------------------

n.trees=500
shrinkage=seq(0.01, 0.1, 0.03)
interaction.depth=c(1, 5, 10, 20, 40, 60)
n.minobsinnode=1:3
gbm.grid=expand.grid(n.trees,interaction.depth,shrinkage,n.minobsinnode)
names(gbm.grid)=c("n.trees","interaction.depth","shrinkage","n.minobsinnode")
fit.gbm=caret_reg(x=scale(features),y=response,method="gbm",grid=gbm.grid)

rmse.na=na.omit(fit.gbm$results$RMSE)

rmse.gbm=min(rmse.na)
rmse.gbm
results=rbind(results,c("gbm",rmse.gbm))



# Cubist ------------------------------------------------------------------

committees=seq(30,50,5)
neighbors=5:9
cubist.grid=expand.grid(committees,neighbors)
names(cubist.grid)=c("committees","neighbors")
fit.cubist=caret_reg(x=scale(features),y=response,method="cubist",grid=cubist.grid)
rmse.cubist=min(fit.cubist$results$RMSE)
results=rbind(results,c("cubist",rmse.cubist))


# Stacking ----------------------------------------------------------------
ensemble_methods = c('glmnet', 'earth', 'rpart')
ensemble_control = trainControl(method="repeatedcv", repeats=1,number=5, verboseIter=TRUE,savePredictions="final")
ensemble_tunes = list(glmnet=caretModelSpec(method='glmnet', tuneGrid=grids), earth=caretModelSpec(method='earth', tuneGrid=mars), rpart=caretModelSpec(method='rpart', tuneGrid=cp.grid))

ensemble_fits = caretList(features, response, trControl=ensemble_control,methodList=ensemble_methods, tuneList=ensemble_tunes, preProcess=c("center", "scale"))
fit_ensemble=caretEnsemble(ensemble_fits)
print(fit_ensemble)
summary(fit_ensemble)

