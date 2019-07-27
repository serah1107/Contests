library(ggplot2)
library(readr)
library(tidyverse)
library(dplyr) 
library(data.table)
library(gridExtra)
options("scipen" = 100)
memory.limit(size=56000) #memory increased  to 7GB

dtotal = read_csv(file.choose(),col_names = T)
head(dtotal)

data = read.csv("C:\\Users\\user\\Desktop\\lpoint\\dmass.csv", stringsAsFactors = F)
dmass = read.csv("C:\\Users\\user\\Desktop\\lpoint\\dmass_add_dt.csv", stringsAsFactors = F)
head(data)


data=data[which(is.na(data["PD_BUY_CT"])==FALSE),]
data=data.table(data)
data[977,]
data$PD_NM=='이롬황성주1일1생식 8주  쉐이커'
data[which(data$CLNT_ID== 145260),]
View(data[which(data$PD_NM=='이롬황성주1일1생식 8주  쉐이커'),])


DD =data[,list(buy_total = sum(PD_BUY_CT)), by = "CLNT_GENDER"]
head(DD)
DD=data.frame(DD)

DD2=data[,list(buy_total = sum(PD_BUY_CT)), by = "ZON_NM"]
head(DD2)
DD2=data.frame(DD2)

DD3=data[,list(buy_total = sum(PD_BUY_CT)), by = "DVC_CTG_NM"]
head(DD3)
DD3=data.frame(DD3)

DD4=data[,list(buy_total = sum(PD_BUY_CT)), by = "CLNT_AGE"]
head(DD4)
DD4=data.frame(DD4)

DD5=data[,list(buy_total = sum(PD_BUY_CT)), by = "SESS_DT"]
head(DD5)
DD5=data.frame(DD5)


DD=DD[-3,]
group = c("M","F")
DD$CLNT_GENDER=as.factor(DD$CLNT_GENDER)
p=ggplot(DD, aes(x=CLNT_GENDER , y=buy_total,fill=group))+
  geom_bar(stat = "identity")
require(scales)
p + scale_y_continuous(labels = comma)+labs(x = "성별", 
                                           y = "구매량", 
                                           title = "성별 간 구매량 비교 ")+theme_light() 

group1 = c("Capital","Yeongnam","Hoseo","Gangwon-Jeju","Honam")
DD2$ZON_NM=as.factor(DD2$ZON_NM)
p=ggplot(DD2, aes(x=ZON_NM , y=buy_total,fill=group1))+
  geom_bar(stat = "identity")
require(scales)
p + scale_y_continuous(labels = comma) +labs(x = "지역", 
                                             y = "구매량", 
                                             title = "지역 간 구매량 비교 ")+theme_light() 


group = c("mobile","desktop","tablet")
DD3$DVC_CTG_NM=as.factor(DD3$DVC_CTG_NM)
p=ggplot(DD3, aes(x=DVC_CTG_NM , y=buy_total,fill=group))+
  geom_bar(stat = "identity")
require(scales)
p + scale_y_continuous(labels = comma) +labs(x = "기기", 
                                             y = "구매량", 
                                             title = "기기 간 구매량 비교 ")+theme_light() 


group = DD4$CLNT_AGE[-3]
DD4=DD4[-3,]
DD4$CLNT_AGE=as.factor(DD4$CLNT_AGE)
p=ggplot(DD4, aes(x=CLNT_AGE , y=buy_total,fill=group))+
  geom_bar(stat = "identity")
require(scales)
p + scale_y_continuous(labels = comma) +labs(x = "연령", 
                                             y = "구매량", 
                                             title = "연령 간 구매량 비교 ")+theme_light()


ggplot(DD5,aes(x = SESS_DT, y =buy_total)) +
  geom_line(size = 1, alpha = 0.8) +
  guides(color = FALSE)

ggplot(DD5,aes(x = SESS_DT, y =buy_total)) +
  geom_line( size = 1, alpha = 0.8) +
  labs(title = "일 별 총 구매량",
       x = "구매일",
       y = "총 구매량",
       color = "")+ scale_y_continuous(labels = comma) 

plot(DD5)

DD5$SESS_DT=substr(DD5$SESS_DT,6,10)

ggplot(DD5,aes(x = SESS_DT, y =buy_total, group = 1)) + 
  geom_line(color = "#00AFBB", size = 1)+
  labs(title = "일 별 총 구매량",
       x = "구매일",
       y = "총 구매량",
       color = "")+ scale_y_continuous(labels = comma)+theme( axis.text.x=element_blank(),
                                                              axis.ticks.x=element_blank())


dmass=data.table(dmass)

D=dmass[,list(buy_total = sum(SALES_MEAN)), by = c("pd_group","C_GROUP1","SESS_DT")]
head(D)
pc10=D[which(D$pd_group==1),]
pc20=D[which(D$pd_group==2),]
pc30=D[which(D$pd_group==3 ),]
pc40=D[which(D$pd_group==4),]
pc50=D[which(D$pd_group==5),]
pc60=D[which(D$pd_group==6 ),]

pc10$SESS_DT=as.Date(pc10$SESS_DT)
pc10[which(pc10$C_GROUP1==0),2]="Customer group1"
pc10[which(pc10$C_GROUP1==1),2]="Customer group2"
pc10[which(pc10$C_GROUP1==2),2]="Customer group3"

ggplot(pc10,aes(x = SESS_DT, y =buy_total, coloar = C_GROUP1)) + 
   facet_wrap(~ C_GROUP1)+
  geom_line() +
  labs(x = "Month", y = "Total Purchase Amount", 
       title = "Total Purchase Amount of product group 1")+theme_light()


pc20$SESS_DT=as.Date(pc20$SESS_DT)
pc20[which(pc20$C_GROUP1==0),2]="Customer group1"
pc20[which(pc20$C_GROUP1==1),2]="Customer group2"
pc20[which(pc20$C_GROUP1==2),2]="Customer group3"

ggplot(pc20,aes(x = SESS_DT, y =buy_total, coloar = C_GROUP1)) + 
  facet_wrap(~ C_GROUP1)+
  geom_line() +
  labs(x = "Month", y = "Total Purchase Amount", 
       title = "Total Purchase Amount of product group2")+theme_light()


pc30$SESS_DT=as.Date(pc30$SESS_DT)
pc30[which(pc30$C_GROUP1==0),2]="Customer group1"
pc30[which(pc30$C_GROUP1==1),2]="Customer group2"
pc30[which(pc30$C_GROUP1==2),2]="Customer group3"

ggplot(pc30,aes(x = SESS_DT, y =buy_total, coloar = C_GROUP1)) + 
  facet_wrap(~ C_GROUP1)+
  geom_line() +
  labs(x = "Month", y = "Total Purchase Amount", 
       title = "Total Purchase Amount of product group 3")+theme_light()


pc40$SESS_DT=as.Date(pc40$SESS_DT)
pc40[which(pc40$C_GROUP1==0),2]="Customer group1"
pc40[which(pc40$C_GROUP1==1),2]="Customer group2"
pc40[which(pc40$C_GROUP1==2),2]="Customer group3"

ggplot(pc10,aes(x = SESS_DT, y =buy_total, coloar = C_GROUP1)) + 
  facet_wrap(~ C_GROUP1)+
  geom_line() +
  labs(x = "Month", y = "Total Purchase Amount", 
       title = "Total Purchase Amount of product group 4")+theme_light()


pc50$SESS_DT=as.Date(pc50$SESS_DT)
pc50[which(pc50$C_GROUP1==0),2]="Customer group1"
pc50[which(pc50$C_GROUP1==1),2]="Customer group2"
pc50[which(pc50$C_GROUP1==2),2]="Customer group3"

ggplot(pc50,aes(x = SESS_DT, y =buy_total, coloar = C_GROUP1)) + 
  facet_wrap(~ C_GROUP1)+
  geom_line() +
  labs(x = "Month", y = "Total Purchase Amount", 
       title = "Total Purchase Amount of product group 5")+theme_light()


pc60$SESS_DT=as.Date(pc60$SESS_DT)
pc60[which(pc60$C_GROUP1==0),2]="Customer group1"
pc60[which(pc60$C_GROUP1==1),2]="Customer group2"
pc60[which(pc60$C_GROUP1==2),2]="Customer group3"

ggplot(pc60,aes(x = SESS_DT, y =buy_total, coloar = C_GROUP1)) + 
  facet_wrap(~ C_GROUP1)+
  geom_line() +
  labs(x = "Month", y = "Total Purchase Amount", 
       title = "Total Purchase Amount of product group 6")+theme_light()




DDD =dmass[,list(brand=sum(IDX_SB_B)), by = c("C_GROUP1","pd_group")]
head(DDD)
d=data.frame(DDD)


d[which(d$C_GROUP1==0),1]="Customer group1"
d[which(d$C_GROUP1==1),1]="Customer group2"
d[which(d$C_GROUP1==2),1]="Customer group3"

d$brand=scale(d$brand)
d$pd_group=d$pd_group-1
ggplot(d,aes(x = pd_group, y =brand, coloar = C_GROUP1)) + 
  facet_wrap(~ C_GROUP1)+
  geom_bar(stat = "identity") +
  labs(x = "상품 그룹", y = "전체 브랜드 지수", 
       title = "상품 그룹 / 고객 그룹 별 브랜드 중요도")+theme_light()


DDD =dmass[,list(brand=sum(IDX_SB_T)), by = c("C_GROUP1","pd_group")]
head(DDD)
d=data.frame(DDD)


d[which(d$C_GROUP1==0),1]="Customer group1"
d[which(d$C_GROUP1==1),1]="Customer group2"
d[which(d$C_GROUP1==2),1]="Customer group3"

d$brand=scale(d$brand)
d$pd_group=d$pd_group-1
ggplot(d,aes(x = pd_group, y =brand, coloar = C_GROUP1)) + 
  facet_wrap(~ C_GROUP1)+
  geom_bar(stat = "identity") +
  labs(x = "상품 그룹", y = "전체 선호 지수", 
       title = "상품 그룹 / 고객 그룹 별 브랜드 중요도")+theme_light()

