setwd('/Users/guzi/Desktop/SignalDataScience/')
library(dplyr)
library(glmnet)
library(caret)


# Exploration -------------------------------------------------------------

set.seed(1); j = 50; a = 0.25; x = rnorm(j)
error = sqrt(1 - a^2)*rnorm(j); y = a*x + error
summary(lm(y ~ x - 1))
qplot(x, y) + geom_smooth(method = "lm")


cost =  function(x, y, aEst, lambda, p){
  penalty = lambda*abs(aEst)^p
  sqrt(mean((y - aEst*x)^2)) + penalty
}

cost(1,2,3,4,2) #cost equals 37

power=-2:7
lambdas=2^power
aEsts=seq(-.1,.3,by=.001)

grid=expand.grid(aEsts,lambdas)

head(grid)
grid = cbind(grid,matrix(nrow=nrow(grid),ncol=2))

for(i in 1:nrow(grid)){
  lambda=grid[[i,1]]
  alpha=grid[[i,2]]
  grid[[i,3]]=cost(1,2,lambda,alpha,1)
  grid[[i,4]]=cost(1,2,lambda,alpha,2)
}
names(grid)=c("aEsts","lambdas","costL1","costL2")
head(grid)
View(grid)

get_plot=function(lambda,p){
  df=dplyr::filter(grid,lambdas==lambda)
  if(p==1){
    qplot(df[1],df[3],geom="point")
  }else{
    qplot(df[1],df[4],geom="point")
  }
}
get_plot(0.75,1)

#lapply with unique lambdas
lambda.unique=unique(grid$lambdas)
lists=lapply(lambda.unique,function(x) return(list(get_plot(x,1),get_plot(x,2))))
 #multiplot, do this later



# comparing regularization and stepwise -----------------------------------

df=read.csv("speed-dating-simple.csv")
df=dplyr::filter(df,gender==1)
df=select(df, -gender, -intel_o, -fun_o, -amb_o, -sinc_o)
head(df)
scaling=select(df,-attr_o)
head(scaling)
activities_scaled=scale(scaling)
View(activities_scaled)
activities_scaled = cbind(activities_scaled,df$attr_o)

names(activities_scaled)

scaled_matrix=matrix(unlist(activities_scaled[,1:ncol(activities_scaled)-1]),ncol=17)
response=unlist(activities_scaled[,ncol(activities_scaled)])
lasso=glmnet(scaled_matrix,response,alpha=1)
ridge=glmnet(scaled_matrix,response,alpha=0)

min(lasso$lambda)
min(ridge$lambda)


#get rmse
get_rmse=function(fit,df,target){
  rmse.list=c()
  lambs=fit$lambda
  for(i in lambs){
    predicted=predict(fit,df,s=i)
    rmse.list=c(rmse.list,rmse(predicted,target))
  }
  return(rmse.list)
}
l2.rmse=get_rmse(ridge,scaled_matrix,response)
l1.rmse=get_rmse(lasso,scaled_matrix,response)

rmse=function(x,y){
  sum=0
  for(i in 1:length(x)){
    sum=sum+((y[i]-x[i])**2)
  }
  mean_result=sum/length(x)
  return (sqrt(mean_result))
}


#get plot
lambs.l1=lasso$lambda
lambs.l2=ridge$lambda


qplot(lambs.l1,l1.rmse,geom="point")
qplot(lambs.l2,l2.rmse,geom="point")

#make cross validated RMSE
l2.cv=cv.glmnet(scaled_matrix,response,nfolds=10,alpha=0)
l1.cv=cv.glmnet(scaled_matrix,response,nfolds=10,alpha=1)

l1.cv$lambda.min
l2.cv$lambda.min

qplot(l1.cv$lambda,l1.cv$cvm,geom="point")
qplot(l2.cv$lambda,l2.cv$cvm,geom="point")




#stepwise vs.regularization

tenfold=function(df){
  
  results=c()
  
  #creating folds
  set.seed(1)
  shuffle=df[sample(nrow(df)), ]
  vec=1:nrow(shuffle)
  fold_size=nrow(shuffle)/10
  indexes=split(vec,ceiling(seq_along(vec)/fold_size))

  #within each fold
  for(i in 1:length(indexes)){
    testIdx=indexes[[i]]
    test = shuffle[testIdx, ]
    train = shuffle[-testIdx,]

    #step prediction
    model_init = lm(attr_o ~ ., train)
    model = formula(lm(attr_o ~ ., train))
    step_reg = step(model_init, model, direction="backward")
    predicted.step=predict(step_reg,test)
    
    #cv.glmnet
    
    ##scaling
    scaling.train=select(train,-attr_o)
    scaling.test=select(test,-attr_o)
    activities_scaled.train=scale(scaling.train)
    
    #activities_scaled.train
    #activities_scaled.test=scale(scaling.test)
    
    #However, when making predictions on the held-out data, you don???t want to simply call scale() 
    #on that data directly; instead, you want to apply the same transformations which were applied to 
    #the training set. To do so, (1) extract the scaling parameters from the scaled training data 
    #(stored as attributes) and (2) pass them in as additional parameters to scale() when calling it on the test set.
    
    
    activities_scaled.test=scale(scaling.test,attr(activities_scaled.train,"scaled:center"),attr(activities_scaled.train,"scaled:scale"))
    response.train=unlist(activities_scaled.train[,ncol(activities_scaled.train)])
    response.test=unlist(activities_scaled.test[,ncol(activities_scaled.test)])
     
    
    l1.cv=cv.glmnet(activities_scaled.train,response.train,alpha=1)
    l2.cv=cv.glmnet(activities_scaled.train,response.train,alpha=0)
    
    
    predicted.l1=predict(l1.cv,scaled_test,s=l1.cv$lambda.min)
    predicted.l2=predict(l2.cv,scaled_test,s=l2.cv$lambda.min)
  }
  
  rmse.step=rmse(predicted.step,response.test)
  rmse.l1=rmse(predicted.l1,response.test)
  rmse.l2=rmse(predicted.l2,response.test)
  
  
  results=c(rmse.step,rmse.l1,rmse.l2)
  return (results)
}

tenfold(df)



# elastic net regression --------------------------------------------------


# Set grid of parameter values to search over
param_grid = expand.grid(.alpha = 1:10 *0.1, .lambda = 10^seq(-4,0,length.out=10))

# Set 10-fold cross validation repeated 3x
control = trainControl(method="repeatedcv", number=10, repeats=3, verboseIter=TRUE)

# Search over the grid

scaling.elastic=select(df,-attr_o)
activities_scaled.elastic=scale(scaling.elastic)
features=activities_scaled.elastic
response.elastic=df$attr_o
target=response.elastic

caret_fit = train(x=features, y=target, method="glmnet", tuneGrid=param_grid, trControl=control)

caret_fit$bestTune
min(caret_fit$results$RMSE)
# the result is 1.100995





















