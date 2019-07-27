#data manipulation----------------------------------------------------------
library(tidyverse)
library(data.table)
library(reshape)
library(dplyr)
memory.limit(size=56000) #memory increased  to 7GB
#선호지수 추가 후 고객 별 테이블 ====================================================================
#c_group1 rfm 가중치 비율 c_group2회귀로 한거 ========================================================
#==================================================================================================
dmass = read.csv("C:\\Users\\user\\Desktop\\lpoint\\final data\\dmass_add.csv",stringsAsFactors = F)
weather = read.csv("C:\\Users\\user\\Desktop\\lpoint\\final data\\weather.csv",stringsAsFactors = F)
holiday = read.csv("C:\\Users\\user\\Desktop\\lpoint\\final data\\holiday.csv",stringsAsFactors = F)
#------------------------------------------------------------------------------------------------------------------------
colnames(holiday)=c("SESS_DT","holiday","special","weekend")
colnames(weather)=c("Zone","SESS_DT","temp","rain","wind","sun","x","PM25")
weather = weather[,-7]
head(dmass)
holi = c() ; spe=c() ; week=c()
for(i in 1:nrow(holiday)){
  for(j in 1:nrow(dmass)){
  if(holiday$SESS_DT[i]==dmass$SESS_DT[j])
  {
    holi[j]=holiday$holiday[i]
  }
    }
}
for(i in 1:nrow(holiday)){
  for(j in 1:nrow(dmass)){
    if(holiday$SESS_DT[i]==dmass$SESS_DT[j])
    {
      spe[j]=holiday$special[i]
    }
  }
}
for(i in 1:nrow(holiday)){
  for(j in 1:nrow(dmass)){
    if(holiday$SESS_DT[i]==dmass$SESS_DT[j])
    {
      week[j]=holiday$weekend[i]
    }
  }
}




#datatable===================================================================================================
zone <- unique(dmass$ZON_NM)
gender <- setdiff(unique(dmass$CLNT_GENDER), 'U') %>% sort
age <- setdiff(unique(dmass$CLNT_AGE), 'U') %>% sort
dvc <- unique(dmass$DVC_CTG_NM) %>% sort
dfinal <- dmass %>% 
  group_by(C_GROUP1, CLAC2_NM, SESS_WW) %>% 
  summarise(SALES_MEAN = mean(PD_BUY_CT*PD_BUY_AM), 
            IDX_SB_B = sum(IDX_SB_B), 
            IDX_SB_T = sum(IDX_SB_T),
            # IDX_BO = sum(IDX_BO),
            PROP_FEMALE = sum(CLNT_GENDER=='F')/sum(CLNT_GENDER%in%gender), 
            PROP_ZON1 = sum(ZON_NM=='Capital')/sum(ZON_NM%in%zone), 
            PROP_ZON2 = sum(ZON_NM=='Yeongnam')/sum(ZON_NM%in%zone),
            PROP_AGE_Y = sum(CLNT_AGE%in%c('10','20'))/sum(CLNT_AGE%in%age), 
            PROP_AGE_M1 = sum(CLNT_AGE=='30')/sum(CLNT_AGE%in%age), 
            PROP_AGE_M2 = sum(CLNT_AGE=='40')/sum(CLNT_AGE%in%age),
            PROP_MOBILE = sum(DVC_CTG_NM%in%c('mobile','tablet'))/sum(DVC_CTG_NM%in%dvc)
)


head(dfinal) ; View(dfinal) #그룹별 상품별 주차별 데이터 

###################GROUP1 ##############################################################
dfinal2 <- dfinal %>% filter(C_GROUP1==1)
head(dfinal2) ; View(dfinal2)
dfinal2_2 <- dfinal %>% filter(C_GROUP1==2)
dpd=read.csv("C:\\Users\\user\\Desktop\\lpoint\\final data\\dpd.csv",stringsAsFactors = F)
View(dpd)
head(dpd)

group=c()
for(i in 1:nrow(dpd)){
  for(j in 1:nrow(dfinal2_2)){
    if(dpd$CLAC2_NM[i]==dfinal$CLAC2_NM[j])
      group[j]=dpd$GROUP[i]
  }
}


group=data.frame(group)
dfinal2=data.frame(dfinal2)
dfinal2=cbind(dfinal2,group$group)
ncol(dfinal2)
colnames(dfinal2)[14]="pd_group"


write.csv(dfinal2,"group1_add.csv")

dfinal2 = read.csv("C:\\Users\\user\\Desktop\\lpoint\\group1_add.csv",stringsAsFactors = F)
View(dfinal2)

#----------------------------일별 -------------------------------

dfinal3 <- dmass %>% 
  group_by(C_GROUP1, CLAC2_NM, SESS_DT) %>% 
  summarise(SALES_MEAN = mean(PD_BUY_CT*PD_BUY_AM), 
            IDX_SB_B = sum(IDX_SB_B), 
            IDX_SB_T = sum(IDX_SB_T),
            # IDX_BO = sum(IDX_BO),
            PROP_FEMALE = sum(CLNT_GENDER=='F')/sum(CLNT_GENDER%in%gender), 
            PROP_ZON1 = sum(ZON_NM=='Capital')/sum(ZON_NM%in%zone), 
            PROP_ZON2 = sum(ZON_NM=='Yeongnam')/sum(ZON_NM%in%zone),
            PROP_AGE_Y = sum(CLNT_AGE%in%c('10','20'))/sum(CLNT_AGE%in%age), 
            PROP_AGE_M1 = sum(CLNT_AGE=='30')/sum(CLNT_AGE%in%age), 
            PROP_AGE_M2 = sum(CLNT_AGE=='40')/sum(CLNT_AGE%in%age),
            PROP_MOBILE = sum(DVC_CTG_NM%in%c('mobile','tablet'))/sum(DVC_CTG_NM%in%dvc)
  )

write.csv(dfinal3,"dmass_add_day.csv")

rm(dfinal2)
group=c()
for(i in 1:nrow(dpd)){
  for(j in 1:nrow(dfinal3)){
    if(dpd$CLAC2_NM[i]==dfinal3$CLAC2_NM[j])
      group[j]=dpd$GROUP[i]
  }
}

group=data.frame(group)
dfinal3=data.frame(dfinal3)
dfinal3=cbind(dfinal3,group$group)
colnames(dfinal3)[14]="pd_group"
write.csv(dfinal3,"dmass_add_dt.csv")

dfinal4 <- dfinal3 %>% filter(C_GROUP1==1)
write.csv(dfinal4,"group1_dt.csv")

#xgboost-------------------------------------------------------------------------------------------------------------------------------------------
dgroup1=read.csv("C:\\Users\\user\\Desktop\\lpoint\\group1_dt.csv",stringsAsFactors = F)
head(dgroup1)
dgroup1=dgroup1[,-1]
ncol(dgroup1)
head(dgroup1)
dgroup1_1=dgroup1[,-c(1:3)]
head(dgroup1_1)

dgroup1_1$pd_group=as.numeric(dgroup1_1$pd_group)

library(xgboost)
nrow(dgroup1_1)/10*8
train = dgroup1_1[c(1:12148),]
test = dgroup1_1[c(12148:15185),]
head(train)
unique(dgroup1$pd_group)

xgb <- xgboost(data = data.matrix(train[,-11]), 
               label = train$pd_group, 
               eta = 0.1,
               max_depth = 15, 
               nround=25, 
               subsample = 0.5,
               colsample_bytree = 0.5,
               seed = 1,
               eval_metric = "merror",
               objective = "multi:softprob",
               num_class = 7,
               nthread = 3
)

y_pred <- predict(xgb, data.matrix(test[,-11]))
head(y_pred)


# Create a training and validation sets
trainObs <- sample(nrow(dgroup1_1), .8 * nrow(dgroup1_1), replace = FALSE)
valObs <- sample(nrow(dgroup1_1), .2 * nrow(dgroup1_1), replace = FALSE)

train_dat <- dgroup1_1[trainObs,]
val_dat <- dgroup1_1[valObs,]

# Create numeric labels with one-hot encoding
train_labs <- as.numeric(train_dat$pd_group) - 1
val_labs <- as.numeric(val_dat$pd_group) - 1

new_train <- model.matrix(~ . + 0, data = train_dat[,-11])
new_val <- model.matrix(~ . + 0, data = dgroup1_1[valObs, -11])

library(Xgboost)
# Prepare matrices
xgb_train <- xgb.DMatrix(data = new_train, label = train_labs)
xgb_val <- xgb.DMatrix(data = new_val, label = val_labs)







#SOM----------------------------------------------------------------------------------------------------------

dmass$SESS_DT <- as.Date(dmass$SESS_DT)

dpd <- dmass %>% group_by(CLAC2_NM, SESS_DT) %>% summarise(BUY_TOT = sum(PD_BUY_AM*PD_BUY_CT))
dpd <- dpd %>% mutate(SESS_WW = floor(difftime(SESS_DT, as.Date('2018-04-01'), units = 'weeks')))

dpd.week <- dpd %>% group_by(CLAC2_NM, SESS_WW) %>% summarise(BUY_MEAN = mean(BUY_TOT, na.rm = T))
dpd.wide.week <- dpd.week %>% spread(SESS_WW, BUY_MEAN)
sum(is.na(dpd.wide.week))

dpd.wide <- dpd %>% select(-SESS_WW) %>% spread(SESS_DT, BUY_TOT)

mins <- apply(dpd.wide.week[,-1], 1, function(x) min(x[x > 0]))
maxs <- apply(dpd.wide.week[,-1], 1, max)
dpd.wide.week.scaled <- dpd.wide.week
for (i in 1:128) dpd.wide.week.scaled[i,-1] <- (dpd.wide.week.scaled[i,-1] - mins[i]) / (maxs[i] - mins[i])

mat <- as.data.frame(dpd.wide.week.scaled[,-1])
rownames(mat) <- unlist(dpd.wide.week[,1])

# mat <- as.data.frame(dpd.wide[,-1])
# missdate <- apply(mat, 1, function(x) sum(is.na(x))) %>% as.numeric
# mat <- mat[(missdate == 0),]

set.seed(1)
dpd.som <- trainSOM(x.data = mat, nb.save = 5, verbose = T, dimension = c(1,6), maxit = 1000)
plot(dpd.som, what = "energy")
summary(dpd.som)

plot(dpd.som, what = "prototypes", type = "lines", print.title = T)
plot(dpd.som, what = "obs", type = "names", print.title = T, scale = c(0.9, 0.5))
plot(dpd.som, what = 'obs', type = 'barplot', print.title = T)
output <- data.frame(CLAC2_NM = dpd.wide.week[,1], GROUP = dpd.som$clustering)


#---------------------------------------------------------------------------------------------------------










