setwd('/Users/guzi/Desktop/SignalDataScience/day12')
library(dplyr)
library(softImpute)
library(DAAG)
library(corrplot)
library(glmnet)
library(dummies)

df=read.csv("ratings.dat",sep=":",header=FALSE)
df=select(df,-V2,-V4,-V6,-V7)
colnames(df)=c("userID","movieID","ratings")
userIDs=unique(df$userID)
movieIDs=unique(df$movieID)
View(df)

#average movie ratings
movie.mean=mean(df$ratings)

#shuffle data and create train and test sets
set.seed(3); shuffle_Idx=sample(nrow(df))
df.shuffle=df[shuffle_Idx,]
train.80=floor(80*(nrow(df.shuffle)/100))
trainIdx=1:train.80
train=df.shuffle[trainIdx,]
test=df.shuffle[-trainIdx,]

##fake movie and fake users
#creating fake.movie
fake.movie=data.frame(userIDs,rep(max(movieIDs)+1,max(userIDs)))
head(fake.movie)
fake.movie$ratings = rep(movie.mean, nrow(fake.movie)) 
names(fake.movie)=c("userID","movieID","ratings")

#creating fake.user
fake.user=data.frame(rep(max(userIDs)+1, 3952), c(1:max(movieIDs)))
fake.user$ratings = rep(movie.mean, 3952)
names(fake.user)=c("userID","movieID","ratings")

#Perturbing the ratings
set.seed(3)
fake.user$ratings= fake.user$ratings+ rnorm(3952, sd = 0.01)
fake.movie$ratings= fake.movie$ratings+ rnorm(6040, sd = 0.01)
train2 = train
train2=rbind(train2,fake.user,fake.movie)
real.train = train2
View(real.train)

#creating a sparse matrix
movie.matrix=Incomplete(real.train$userID, real.train$movieID, real.train$ratings)

##Collaborative filtering
#biScale
movie.scale.mat = biScale(movie.matrix,maxit=5,trace=TRUE)

#lambda
lam0=lambda0(movie.scale.mat)
lambdas=seq(log(lam0),log(1),length.out = 20)
exp.lambdas=exp(lambdas)

#create data frame
rank.list=rep(NA, length(exp.lambdas))
rmse.list=rep(NA, length(exp.lambdas))
results=data.frame(lambda = exp.lambdas, rank=rank.list, rmse=rmse.list)


# Imputation via alternating least squares --------------------------------

rank=function(d){
  rounded.matrix=round(d,4)
  return(sum((rounded.matrix)!=0))
}

rmse=function(x,y){
  sum=0
  for(i in 1:length(x)){
    sum=sum+((y[i]-x[i])**2)
  }
  mean_result=sum/length(x)
  return (sqrt(mean_result))
}

i=1
prev=0
fits=vector("list",length(exp.lambdas))
for(l in exp.lambdas){
  if(i==1){
    prev=softImpute(movie.scale.mat,lambda=l,rank.max = 30, maxit = 1000)
    fits[[i]]=prev
  }else{
    prev=softImpute(movie.scale.mat,lambda=l,rank.max = 30, maxit = 1000, warm.start = prev )
    fits[[i]]=prev
  }
  
  results$rank[i]=rank(fits[[i]]$d)
  predict=impute(fits[[i]],test$userID,test$movieID)
  results$rmse[i]=rmse(predict,test$ratings)
  i=i+1
}


View(results)
best_svd=fits[[7]]


# MAE(Mean Absolute Error) ------------------------------------------------

preds=vector("list",20)

results$mae
l=1

for(i in fits){
  pred=impute(i,test$userID,test$movieID)
  mae=mean(abs(pred-test$ratings))
  results$mae[l]=mae
  l=l+1
}

#precision and recall
for(i in 1:20)
{
  imp=impute(fits[[i]],test$userID,test$movieID)
  actual.pos=sum(test$ratings>=movie.mean)
  predicted.pos=sum(imp>=movie.mean)
  true.pos=sum((test$ratings>=movie.mean) & (imp>=movie.mean))
  results$precision[i]=true.pos/predicted.pos
  results$recall[i]=true.pos/actual.pos
}


results$precision
results$recall

#ASYM

L=matrix(c(0,0,0,3,4,0,0,0,2,3,0,0,0,1,2,7.5,4,1.5,0,0,10,6,3,0,0),nrow=5,ncol=5)

for(i in 1:20){
  penalty=0
  imp=impute(fits[[i]],test$userID,test$movieID)
  imp[imp>5]=5
  imp[imp<1]=1
  for(j in 1:length(imp)){
    t=test$ratings[j]
    p=round(imp[j])
    penalty=penalty+L[t,p]
  }
  results$asym[i]=penalty
}
results$asym


# Analyzing the results ---------------------------------------------------

movies=read.csv("movies.dat",sep="~",header=FALSE)

names(movies)=c("number","name","genre")
nrow(movies)
genres=unique(movies$genre)
genres=as.character(genres)
genres
genre.list=list()
for(i in genres){
  genre.list=list(genre.list,strsplit(x=i,split="|",fixed=TRUE))
}
genre.list=unique(unlist(genre.list))


for(i in genre.list){
  print(i)
  movies[[i]] = 0
}

for(i in 1:nrow(movies)){
  character.genre=as.character(movies$genre[i])
  str.split = strsplit(character.genre, split = "|", fixed= TRUE)
  str.split = unlist(str.split)
  for (j in str.split){
    movies[i, j] = 1
  }
}
View(movies)
movies = cbind(movies, best_svd$v[as.numeric(as.character(movies$number)),])
colnames(movies)
colnames(movies)[-21:0] = sapply(colnames(movies)[-21:0], function(name){paste("X", name, sep = "")})
cor(movies$Drama, select(movies, X1:X30))
model=glm(Crime ~ .,select(movies, Crime, X1:X30),family='binomial')
model
predictions=CVbinary(model)$cvhat
predictions

drama.roc=roc(movies$Drama,predictions)
plot(drama.roc)

drama.probability=cbind(as.character(movies$name),movies$Crime)
drama.probability=cbind(drama.probability,predictions)
colnames(drama.probability)=c("titles","crime","probability")
drama.probability=data.frame(drama.probability)
drama.probability=drama.probability[order(drama.probability$probability,decreasing = TRUE),]
View(drama.probability)


#users.dat
raters=read.csv("users.dat",sep="~",header=FALSE)
names(raters)=c("userID","gender","age","occupation","zipcode")
raters.mod=filter(raters,age >= 35)
raters.mod=filter(raters.mod,occupation!=0 & occupation!=16)
View(raters.mod)
occup=sort(table(raters.mod$occupation),decreasing=TRUE)[1:4]
occup=data.frame(occup)
occup
raters.mod2=filter(raters.mod,occupation %in% unlist(occup$Var1))
View(raters.mod2)

##unregularized multinomial logistic regression
U=data.frame(best_svd$u[-nrow(best_svd$u),])
age=raters$age
occupation=raters$occupation
userid=raters$userID
U=cbind(age,U)
U=cbind(occupation,U)
U=cbind(userid,U)
View(U)
U=filter(U,age >= 35)
U=filter(U,occupation!=0 & occupation!=16)
U=filter(U,occupation %in% unlist(occup$Var1))
View(U)
model.u=glmnet(as.matrix(select(U,X1:X30)),U$occupation,family="multinomial",lambda=0)
logodds=predict(model.u,as.matrix(select(U,X1:X30)))
pcs=prcomp(as.matrix(data.frame(logodds)))
pcs$rotation
corrplot(pcs$rotation,is.corr=FALSE)


# Estimating different career??s genre -------------------------------------
View(movies)
movies.genre=select(movies,Animation:X30)

View(movies.genre)
colnames(movies.genre)[12]="SciFi"
colnames(movies.genre)[17]="FilmNoir"
log.list=vector("list",18)
for(i in 1:18){
  data.movies=cbind(movies.genre[i],select(movies.genre,X1:X30))
  model=glm(paste(colnames(movies.genre[i]),"~."),data.movies,family='binomial')
  set.seed(1)
  predictions=CVbinary(model)$cvhat
  logodds=log(predictions/(1-predictions))
  logodds
  log.list[[i]]=logodds
} #18 genres,30 factors,3883 movies, log.list as for (3883,18) logodds and fact.scores as 
#(3883,30) factor scores



#linear combination
ncol(data.frame(log.list))
fact.scores=select(movies.genre,X1:X30)
ncol(fact.scores)
fact.matrix=as.matrix(fact.scores)
nrow(fact.matrix)
linear.combination=data.frame(nrow=3883)
for(i in 1:18){
  linear.combination=cbind(linear.combination,colSums(log.list[[i]]*fact.matrix))
}
linear.combination=linear.combination[,-1]
names(linear.combination)=names(select(movies,Animation:Western))
genre_scores=linear.combination

#Dummy user
users=read.csv("users.dat",sep="~",header=FALSE)
names(users)=c("userID","gender","age","occupation","zipcode")
users$occupation=factor(users$occupation)
users$zipcode=as.numeric(users$zipcode)
users$gender=as.numeric(users$gender)
users=dummy.data.frame(users)

##career scores
nrow(users)
users = cbind(users, best_svd$u[as.numeric(as.character(users$userID)),])
colnames(users)[26:ncol(users)]=sapply(colnames(users)[26:ncol(users)], function(i){
  paste("X",i,sep="")
})
View(users)

loguser.list=vector("list",21)
for(i in 4:24){
  data.users=cbind(users[i],select(users,X1:X30))
  model=glm(paste(colnames(users[i]),"~."),data.users,family='binomial')
  set.seed(1)
  predictions=CVbinary(model)$cvhat
  logodds=log(predictions/(1-predictions))
  loguser.list[[i-3]]=logodds
} 

fact.scores.user=select(users,X1:X30)
fact.matrix.user=as.matrix(fact.scores.user)

linear=data.frame(nrow=6040)

loguser.list[[2]]

for(i in 1:21){
  linear=cbind(linear,colSums(loguser.list[[i]]*fact.matrix.user))
}

linear=linear[,-1]
names(linear)=names(select(users,occupation0:occupation20))
career_scores=linear
View(career_scores)

#pairings
pairings=matrix(ncol=18,nrow=21)
dim(career_scores)
dim(genre_scores)
dim(as.matrix(best_svd$d))

r=vector("list",30)
ncol(career_scores)
for(i in 1:21){
  r=cbind(r,career_scores[[i]]*best_svd$d)
}
r=r[,-1]
dim(r)
class(genre_scores)
#r=dim(30,21), genre_scores=dim(30,18)
head(r)
dim(r)


r2=as.matrix(data.frame(r))
typeof(r2)
dim(career_scores)
dim(genre_scores)
for(i in 1:21){
  for( j in 1:18){
  sum=sum(career_scores[,i]*genre_scores[,j]*best_svd$d)
  pairings[i,j]=sum
  }
}

corrplot(pairings,is.corr = FALSE)

#biscale





