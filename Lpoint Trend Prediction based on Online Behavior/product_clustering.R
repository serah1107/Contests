#data manipulation----------------------------------------------------------

library(readr)
library(tidyverse)
library(data.table)
memory.limit(size=56000) #memory increased  to 7GB
data = read.csv("C:\\Users\\user\\Desktop\\lpoint\\dmass.csv", stringsAsFactors = F)
head(data)

#product clustering (sales, date, product(class2))

dmass = data.table(data)
product =dmass[,list(buy_total = sum(PD_BUY_CT*PD_BUY_AM)), by = c("CLAC2_NM","SESS_DT")]
head(product)
product2 =dmass[,list(buy_total = sum(PD_BUY_CT*PD_BUY_AM)), by = c("CLAC2_NM")]
head(product2)
product3 =dmass[,list(buy_total = sum(PD_BUY_CT*PD_BUY_AM)), by = c("CLAC2_NM","SESS_WW")]
head(product3)

length(unique(product$SESS_DT))#183
library(reshape)
product.wide <- reshape(product, direction = "wide", idvar = "CLAC2_NM", timevar="SESS_DT")
product.wide <- product.wide[order(product.wide$CLAC2_NM),]
# View(product.wide)

product.wide2=data.frame(product.wide)
head(product.wide2)
write.csv(product.wide2,"C:\\Users\\user\\Desktop\\lpoint\\ngram\\product.wide.csv")

for (i in 2:128){
  for (j in 2:184){
    if(is.na(product.wide2[i,j]==TRUE))
    {
      product.wide2[i,j]=0
    }
  }
}





for (i in 2:128){
  for (j in 2:184){
    if(is.na(product.wide2[i,j]==TRUE))
       {
         product.wide2[i,j]=0
    }
  }
}

for (i in 2:184){
  colnames(product.wide2)[i]=substr(colnames(product.wide2)[i],11,20)
  colnames(product.wide2)[i]=as.character(colnames(product.wide2)[i])
  substr(colnames(product.wide2)[i],5,5)<-"-"
  substr(colnames(product.wide2)[i],8,8)<-"-"
}

date=sort(unique(product$SESS_DT))
product.wide2=data.table(product.wide2)
setcolorder(product.wide2, date)
head(product.wide2)

product.wide3=cbind(product.wide2[,184],product.wide2[,c(1:183)])
head(product.wide3)
p=data.frame(product.wide3)
colnames(p)=colnames(product.wide3)
pt=t(p); pt=pt[-1,]
for (i in 1:183){
  for (j in 1:128){
    if(is.na(pt[i,j]==TRUE))
    {
      pt[i,j]=0
    }
  }
}
pt2=data.frame(0)

for (i in 1:183){
  for (j in 1:128){
      pt2[i,j]=as.numeric(pt[i,j])
  }
}
colnames(pt2)=product.wide$CLAC2_NM
rownames(pt2)=date
pt2=t(pt2)
pt2
write.csv(pt2,"product_wide.csv")

#clustering---------------------------------------------------------------------------
#군집수를 지정해주는 clustering방법들
#1.nbcluster
library(NbClust)
pt2[is.na(pt2)==TRUE]=0
product.scaled=scale(pt2) 
pp=data.frame(product2$buy_total)
rownames(pp)=product2$CLAC2_NM
pp=scale(pp)#SUM OF PRODOCTS SELLED BY PRODUCT
nb=NbClust(pp,distance="euclidean",min.nc=4,max.nc=60,method="average")
nb2=NbClust(product.scaled[,1],distance="euclidean",min.nc=4,max.nc=60,method="average")
#JUST ONE MOMENT OF PRODUCT
#nb1---------------------------------------------------------------------------------
str(nb)
nb$Best.partition
table(nb$Best.nc)
result2=data.table(nb$Best.partition)
n=max(result2)
group2=list()
for(i in 1:n){
  group2[[i]]=pd_nm[which(result2$V1==i)]
}
View(group2)

barplot(table(nb$Best.nc[1,]), xlab="# of clusters", ylab="# of criteria", main="Number of clusters chosen by 26 criteria")
#8IS THE BEST?
#nb2-----------------------------------------------------------------------------------
result2_2=data.table(nb2$Best.partition)
n=max(result2_2)
group2_2=list()
for(i in 1:n){
  group2_2[[i]]=pd_nm[which(result2_2$V1==i)]
}
View(group2_2)

barplot(table(nb$Best.nc[1,]), xlab="# of clusters", ylab="# of criteria", main="Number of clusters chosen by 26 criteria")
#60isthebest?

pd_group_nb2=data.frame(0)

for (i in 1:60){
  for (j in 1:length(group2_2[[i]])){
    pd_group_nb2[i,j]=group2_2[[i]][j]
  }
}
rownames(pd_group_nb2)=paste(1:60,"cluster")
View(pd_group_nb2)

write.csv(pd_group_nb2,"pd_grouping_nb2.csv")

#2. mclust------------------------------------------------------
library(mclust)
mclust <- Mclust(pt2, G=20:59)
mclust$BIC
plot(mclust$classification)

mclust$modelName #EEI
summary(mclust) #38개
result_mclust=data.frame(mclust$classification)
group=list()
n=38
for(i in 1:n ){
  group[[i]]=pd_nm[which(result_mclust$mclust.classification==i)]
}
pd_group=data.frame(0)

for (i in 1:38){
  for (j in 1:length(group[[i]])){
    pd_group[i,j]=group[[i]][j]
  }
}
rownames(pd_group)=paste(1:38,"cluster")
View(pd_group)
write.csv(pd_group,"mcslut_clssf.csv")

View(mclust$classification)

##군집화! 
#hclust------------------------------------------------------------------------------------
pt2=data.frame(pt2)
hclust=hclust(dist(pt2),method="average")
plot(hclust)
hclust$height

n=30#50?
clusters=cutree(hclust,k=n)
clusters
table(clusters)
result = data.table(clusters)
pd_nm = product.wide$CLAC2_NM

group=list()
for(i in 1:n ){
  group[[i]]=pd_nm[which(result$clusters==i)]
}


pd_group=data.frame(0)

for (i in 1:30){
  for (j in 1:length(group[[i]])){
    pd_group[i,j]=group[[i]][j]
  }
}
rownames(pd_group)=paste(1:30,"cluster")
View(pd_group)

write.csv(pd_group,"pd_grouping.csv")

View(group)
rect.hclust(hclust,k=30) #50?

#SOM-------------------------------------------------------------------------------------
library(SOMbrero)
library(kohonen)
library(RCurl)
library(scales)
set.seed(593)
ncol(pt2)/10*7
# run the SOM algorithm with 10 intermediate backups and 2000 iterations
#SCALING 필수!
pt3=pt2
for(i in 1:ncol(pt2)){
pt3[,i]=scales::rescale(pt2[,i],to=c(0,1))
}
my.som <- trainSOM(pt3, dimension=c(6,6), nb.save=6, maxit=2000, 
                   scaling="none", radius.type="letremy")
plot(my.som, what="energy")
table(my.som$clustering)
summary(my.som)
plot(my.som, what="obs", type="color", variable=1, print.title=TRUE, 
     main="2018-04-01")
plot(my.som, what="prototypes", type="lines", print.title=TRUE)
plot(my.som, what="obs", type="boxplot", print.title=TRUE)
plot(my.som, what="obs", type="names", print.title=TRUE, scale=c(0.9,0.5))
plot(superClass(my.som))
my.som$clustering



group=list()
for(i in 1:36 ){
  group[[i]]=pd_nm[which(my.som$clustering==i)]
}
group[[30]]=0
group[[26]]=0
group[[23]]=0
group[[18]]=0
group[[11]]=0
pd_group=data.frame(0)

for (i in 1:36){
  for (j in 1:length(group[[i]])){
    pd_group[i,j]=group[[i]][j]
  }
}
rownames(pd_group)=paste(1:36,"cluster")
View(pd_group)

write.csv(pd_group,"pd_grouping_som.csv")

#dtw (time series clustering)------------------------------------------
library(dtwclust) 
product.wide[is.na(product.wide)==TRUE]=0
N = 128 

X = list() 
for (i in 1:N) { 
  x = product.wide[i,-1]
  X = append(X, list(x)) 
} 

cluster = tsclust(X, k=31L, distance="dtw_basic", type="hierarchical") 
cl = slot(cluster, "cluster")

plot(cluster, type="sc")
par(mfrow=c(3,5))
plot(cluster, type="series",clus=c(3L:4L))
slot(cluster, "distmat")

hc.clust <- data.frame(CLAC2_NM=product2$CLAC2_NM, dtwclust = cluster@cluster)
View(hc.clust)

group=list()
for(i in 1:31 ){
  group[[i]]=pd_nm[which(hc.clust$dtwclust==i)]
}

pd_group=data.frame(0)

for (i in 1:31){
  for (j in 1:length(group[[i]])){
    pd_group[i,j]=group[[i]][j]
  }
}
rownames(pd_group)=paste(1:31,"cluster")
View(pd_group)

write.csv(pd_group,"pd_grouping_dtw.csv")



#FUZZY CLUSTERING----------------------------------------
library(dtwclust) 
acf_ftn<-function(series){
  lapply(series,function(x){
    as.numeric(acf(x,lag.max=50,plot=FALSE)$acf)
  })
}

fuzzy=tsclust(X,type="f",k=30L,preproc=acf_ftn,distance = "L2",seed=50)
fuzzy@fcluster
plot(fc,data,type="series")




#--------------------------------------------------------------------------------------
##주차 별로 다시 acf 사용
product3$SESS_WW=product3$SESS_WW+1
product.wide_week=reshape(product3, direction = "wide", idvar = "CLAC2_NM", timevar="SESS_WW")
product.wide_week <- product.wide_week[order(product.wide_week$CLAC2_NM),]
product.wide2_week=data.frame(product.wide_week)
head(product.wide2_week)

for (i in 2:28){
  colnames(product.wide2_week)[i]=substr(colnames(product.wide2_week)[i],11,12)
}
for (i in 2:28){
  colnames(product.wide2_week)[i]=as.numeric(colnames(product.wide2_week)[i])
}
product.wide2_week=data.table(product.wide2_week)
date=unique(product3$SESS_WW)
date=sort(date) ; date=as.character(date)
setcolorder(product.wide2_week, date)
head(product.wide2_week)
product.wide2_week=cbind(product.wide2_week$CLAC2_NM,product.wide2_week[,-28])


product.wide3_week=data.frame(product.wide2_week)

library(scales)

for(i in 2:ncol(product.wide3_week)){
  product.wide3_week[,i]=scales::rescale(product.wide3_week[,i],to=c(0,1))
}
head(product.wide3_week)

library(forecast)


par(mfrow=c(3,3))
acf(t(product.wide3_week[,c(2:28)]),lag.max=20,na.action = na.pass)
acf(t(product.wide3_week[11,c(2:28)]),lag.max=20,na.action = na.pass)
acf(t(product.wide3_week[12,c(2:28)]),lag.max=20,na.action = na.pass)
acf(t(product.wide3_week[13,c(2:28)]),lag.max=20,na.action = na.pass)
acf(t(product.wide3_week[14,c(2:28)]),lag.max=20,na.action = na.pass)
acf(t(product.wide3_week[15,c(2:28)]),lag.max=20,na.action = na.pass)
acf(t(product.wide3_week[16,c(2:28)]),lag.max=20,na.action = na.pass)
acf(t(product.wide3_week[17,c(2:28)]),lag.max=20,na.action = na.pass)
acf(t(product.wide3_week[18,c(2:28)]),lag.max=20,na.action = na.pass)


auto.arima(t(product.wide3_week[1,c(2:28)]))



acf(t(product.wide3_week),lag.max=5,na.action = na.pass)


