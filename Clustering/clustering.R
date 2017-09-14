setwd('/Users/guzi/Desktop/SignalDataScience/day11')
library(dplyr)
library(pvclust)
library(cluster)
library(fpc)
library(ggplot2)
library(mixtools)
library(mclust)

df=read.table("protein.txt",sep="\t",header=TRUE)
df.selected=select(df,-Country)
View(df.selected)

scaled.df=scale(df.selected)
country=unlist(select(df,Country))
rownames(scaled.df)=country
View(scaled.df)

#Creating euclidean distance matrix
distance.matrix=dist(scaled.df,method="euclidean")
head(distance.matrix)

#hclust
hclust.ward=hclust(distance.matrix,method="ward")
hclust.D=hclust(distance.matrix,method="ward.D")
hclust.D2=hclust(distance.matrix,method="ward.D2")

plot(hclust.ward)
plot(hclust.D)
plot(hclust.D2)


#k clutsters using hierachical clustering

##convenience function
print_clusters = function(labels, k) {
  for(i in 1:k) {
    print(paste("cluster", i))
    print(df[labels == i, c("Country", "RedMeat",
                                 "Fish", "Fr.Veg")])
  } 
}

treecut=cutree(hclust.ward,5)

#different clusterings of geographic difference, east, west, middle, north, south
for(i in 2:5){
  print_clusters(treecut,i)
}

#validating clusters

#transposed matrix
t.df=t(scaled.df)
View(t.df)
pv.clust=pvclust(method.hclust = "ward.D2", method.dist = "euclidean",t.df)
pvrect(pv.clust)
for(al in seq(0.95,0.55,by=-0.1)){
  pvrect(pv.clust,alpha=al)
}

#K-means
kmean.protein=kmeans(scaled.df,centers=5)
kmean.protein$cluster

##plotting kmeans

row=seq(1,9,by=1)
ncol(scaled.df)
df.copy=rbind(scaled.df,row)

kmeans_plot=function(data,k){
  kmean.clusters=kmeans(data,centers=k)
  clusplot(x=data,kmean.clusters$cluster,label=2, shade = TRUE, color = TRUE)
}
kmeans_plot(scaled.df,5)
kmeans_plot(df.copy,4)


#validating kmeans
k=1:10
kruns=kmeansruns(data=scaled.df,krange=k,criterion = "ch")
plot(k,kruns$crit)

#clusterboot

boot=clusterboot(scaled.df,clustermethod = kmeansCBI, runs=100, iter.max=100, krange=5)
original.cluster=boot$result$partition
stability=boot$bootmean
dissolve=boot$bootbrd

original.cluster
stability
dissolve

#Mixture Models--Gaussian
df.faithful=faithful
View(df.faithful)
ggplot(df.faithful,aes(waiting))+geom_histogram()

normalmix=normalmixEM(df.faithful$waiting)
plot(normalmix,density = TRUE,which=2)
summary(normalmix)

semi=spEMsymloc(df.faithful$waiting,mu0=2,bw=3)
plot(semi)

waiting.copy=c(df.faithful$waiting,1000)
semi2=spEMsymloc(waiting.copy,mu0=2,bw=3)
plot(semi2)
normalmix2=normalmixEM(waiting.copy)
plot(normalmix2,density=TRUE,which=2)

#Multivariate Mixture
ggplot(df.faithful,aes(waiting,eruptions))+geom_point()
#2 clusters
mcl=Mclust(scale(df.faithful))
plot(mcl)

#mclust on protein
protein=Mclust(scaled.df)
plot(protein)
