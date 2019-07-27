#customer clustering ----------------------------------------
##session month 
month_list = c()

for (i in 1:nrow(dmass)){
  month_list[i]=month(dmass$SESS_DT[i])
}

dmass2=cbind(dmass,month_list)
colnames(dmass2)[20]="SESS_M"
customer <- dmass2[,list(Recency= max(SESS_M, na.rm = T), Frequency = sum(PD_BUY_CT), Monetary = sum(PD_BUY_AM*PD_BUY_CT)), by = c("CLNT_ID", "SESS_M")]

##reshaping of R,F,M
library(reshape)

customer2 <- as.data.frame(customer)
customer.wide <- reshape(customer2, direction = "wide", idvar = "CLNT_ID", timevar="SESS_M")
customer.wide <- customer.wide[order(customer.wide$CLNT_ID),]
head(customer.wide)

#col 조정 
month=sort(colnames(customer.wide)[-1])
setcolorder(customer.wide, month)
customer.wide = cbind(customer.wide$CLNT_ID,customer.wide[,c(1:18)])
customer.wide

##방법1: for quantile 하위 25 상위 25 
#1522152
customer2[is.na(customer2)==TRUE]=0
summary(customer2['Recency'][-c(339452,339450),])#5미만 하위, 8이상 상위
summary(customer2['Frequency'][-c(339452,339450),]) # 1미만 하위, 4이상 상위 
summary(customer2['Monetary'][-c(339452,339450),])#40000미만 하위, 179000이상 상위 
#이상치 
which(customer2['Monetary']==208996400)#339452
which(customer2['Monetary']==178286400)#339450
##producing a, b,c
a.r=as.numeric(summary(customer2['Recency'][-c(339452,339450),])[5]); b.r= as.numeric(summary(customer2['Recency'][-c(339452,339450),])[2] )
a.f=as.numeric(summary(customer2['Frequency'][-c(339452,339450),])[5]); b.f= as.numeric(summary(customer2['Frequency'][-c(339452,339450),])[2])
a.m=as.numeric(summary(customer2['Monetary'][-c(339452,339450),])[5]); b.m= as.numeric(summary(customer2['Monetary'][-c(339452,339450),])[2])

##grouping function
group_R<-function(recency.m,a,b){
  if(recency.m>=a){
    return(1)
  }
  else if (recency.m<a & recency.m>=b){
    return(2)
    
  }
  else {
    return(3)
  }
}

group_F<-function(frequency,a,b){
  if(frequency>=a){
    return(3)
  }
  else if (frequency<a & frequency>=b){
    return(2)
    
  }
  else {
    return(1)
  }
}
group_M<-function(monetary,a,b){
  if(monetary>=a){
    return(3)
  }
  else if (monetary<a & monetary>=b){
    return(2)
    
  }
  else {
    return(1)
  }
}

##grouping----------------------------------------------------------------------
group.R=c()
for(i in 1:nrow(customer2)){
  group.R[i]=group_R(customer2$Recency[i],a.r,b.r)
}

group.F=c()
for(i in 1:nrow(customer2)){
  group.F[i]=group_F(customer2$Frequency[i],a.f,c.f)
}

group.M=c()
for(i in 1:nrow(customer2)){
  group.M[i]=group_M(customer2$Monetary[i],a.m,b.m)
}

customer2_2=cbind(customer2,group.R,group.F,group.M)
head(customer2_2)

##가중치 a,b,c 여기서 frequency는 rfm의 f가 아니라 그냥 빈도수 ----------------------
customer2_2=data.table(customer2_2)
Recency <- customer2_2[,list(Frequency=.N,Monetary2 = sum(Monetary)), by = c("group.R")]
Frequency <- customer2_2[,list(Frequency=.N, Monetary2 = sum(Monetary)), by = c("group.F")]
Monetary <- customer2_2[,list(Frequency=.N,Monetary2 = sum(Monetary)), by = c("group.M")]

head(Recency)
head(Frequency)
head(Monetary)
#6개월 간 가중치 ----------------------------------------------------------------------
m.plus=c()
m.plus=Recency$Monetary2/sum(Recency$Monetary2)
f.plus=c()
f.plus=Recency$Frequency/sum(Recency$Frequency)
t.plus=c()
t.plus=m.plus/f.plus
a=sum(t.plus) # R 가중치

m.plus=c(); f.plus=c(); t.plus=c()
m.plus=Recency$Monetary2/sum(Recency$Monetary2)
f.plus=Recency$Frequency/sum(Recency$Frequency)
t.plus=m.plus/f.plus
b=sum(t.plus)

m.plus=c(); f.plus=c(); t.plus=c()
m.plus=Monetary$Monetary2/sum(Monetary$Monetary2)
f.plus=Monetary$Frequency/sum(Monetary$Frequency)
t.plus=m.plus/f.plus
c=sum(t.plus)
a; b; c
#가중치 곱한 RFM지수값----------------------------------------------------------------
total = customer2_2$group.R*a+customer2_2$group.F*b+customer2_2$group.M*c
customer2_3=cbind(customer2_2,total)
head(customer2_3)
#달 별 가중치 
cust_month4=customer2_2[which(customer2_2$SESS_M==4),]
cust_month5=customer2_2[which(customer2_2$SESS_M==5),]
cust_month6=customer2_2[which(customer2_2$SESS_M==6),]
cust_month7=customer2_2[which(customer2_2$SESS_M==7),]
cust_month8=customer2_2[which(customer2_2$SESS_M==8),]
cust_month9=customer2_2[which(customer2_2$SESS_M==9),]
month.RFM <- month.R[,list(Recency2=sum(Recency),Frequency2=sum(Frequency),Monetary2 = sum(Monetary)), by = c("group.R")]
##방법2: 가중치 다르게 --------------------------------------------------------------------------------------------------
#recency ; 14일 , 7일 
#frequency ; 1,2,3
#monetary ; 하위, 상위 25 
summary(customer2['Monetary'][-c(339452,339450),])#40000미만 하위, 179000이상 상위 
#이상치 
which(customer2['Monetary']==208996400)#339452
which(customer2['Monetary']==178286400)#339450
##producing a, b,c
a.r = 14; b.r = 7;
b.f = 2 ; c.f = 1;
a.m=as.numeric(summary(customer2['Monetary'][-c(339452,339450),])[5]); b.m= as.numeric(summary(customer2['Monetary'][-c(339452,339450),])[2])
group_F2<-function(frequency,b,c){
  if (frequency==b){
    return(2)
    
  }
  else if(frequency==c){
    return(1)
  }
  else {
    return(3)
  }
}
##grouping
group.R=c()
for(i in 1:nrow(customer2)){
  group.R[i]=group_R(customer2$Recency[i],a.r,b.r)
}

group.F=c()
for(i in 1:nrow(customer2)){
  group.F[i]=group_F2(customer2$Frequency[i],b.f,c.f)
}

group.M=c()
for(i in 1:nrow(customer2)){
  group.M[i]=group_M(customer2$Monetary[i],a.m,b.m)
}

customer2_4=cbind(customer2,group.R,group.F,group.M)
head(customer2_4)

write.csv(customer2_4,"customer_RFM.csv")

#구매주기 계산 -------------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(scales)

#WEEK --------------------------------------------------------------------------------------------------------------------------
week_list = c()
for (i in 1979064:nrow(dmass)){
  week_list[i]=week(dmass$SESS_DT[i])
}

dmass3=cbind(dmass,week_list)
colnames(dmass3)[20]="SESS_W"
customer_week <- dmass3[,list(buy_count = sum(PD_BUY_CT)),by = c("CLNT_ID", "SESS_W")]
head(customer_week)

# make dataset--------------------MAX WEEK GAP--------------------------------------------------------
customer_week2 <- as.data.frame(customer_week)
customer_wwide <- reshape(customer_week2, direction = "wide", idvar = "CLNT_ID", timevar="SESS_W")
customer_wwide <- customer_wwide[order(customer_wwide$CLNT_ID),]
colnames(customer_wwide)[-1]=substr(colnames(customer_wwide)[-1],11,12)
setcolorder(customer_wwide, sort(colnames(customer_wwide)[-1]))
customer_wwide = cbind(customer_wwide$CLNT_ID,customer_wwide[,-28])
colnames(customer_wwide)[1]="CLNT_ID"
head(customer_wwide)
write.csv(customer_wwide,"customer_week.csv")

customer_wwide=read.csv("customer_week.csv")
customer_wwide=customer_wwide[,-1]
colnames(customer_wwide)[-1]=seq(14,40,1)
head(customer_wwide)

week.gap=list()
for(i in 859492:nrow(customer_wwide)){
  week.gap[[i]]=list(as.numeric(colnames(customer_wwide[i,which(is.na(customer_wwide[i,])==FALSE)][-1])))
}
week.gap[[4]]


diff.week=function(list){
   k=c()
  for(i in 2:length(list)){
    k[i-1]=list[i]-list[i-1]
  }
   return(max(k))
}

max.gap=c()
for(i in 1:nrow(customer_wwide)){
if(length(week.gap[[i]][[1]])==1)
{
  max.gap[i]=0
}
  else
  {
    max.gap[i]=diff.week(week.gap[[i]][[1]])
  }
}
customer.maxw=cbind(customer_wwide,max.gap)
write.csv(customer.maxw,"customer_maxgap.csv")

#FOR PLOT---------------------------------------------------------------------------
library(dplyr)
library(ggplot2)
# table
customer.maxw2 <-
  customer.maxw %>%
  group_by(max.gap) %>%
  summarise(user.count = n())
head(customer.maxw2)

# charts
customer.maxw2 %>%
  ggplot(aes(x=max.gap, y=user.count)) + 
  geom_line() + 
  geom_text(aes(y=user.count, label = max.gap))

customer.maxw2  %>%
  filter(max.gap > 0) %>%
  ggplot(aes(x=max.gap, y=user.count)) + 
  geom_line() + 
  geom_text(aes(y=user.count, label = max.gap)) +
  scale_y_continuous(label = comma)

customer.week.ratio <-
  customer.maxw2 %>% 
  mutate(cumsum.user = cumsum(user.count)) 
View(customer.week.ratio)