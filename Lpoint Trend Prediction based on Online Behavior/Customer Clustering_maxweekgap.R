#구매주기 계산 -------------------------------------------------------------------------
##library-----------------------------------------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(scales)

##data----------------------------------------------------------------------------------------------------------
dmass = read.csv("C:\\Users\\user\\Desktop\\lpoint\\final data\\dmass_add.csv",stringsAsFactors = F)

##WEEK ---------------------------------------------------------------------------------------------------------
##주별 컬럼 생성 
week_list = c()
for (i in 1:nrow(dmass)){
  week_list[i]=week(dmass$SESS_DT[i])
}

dmass3=cbind(dmass,week_list)
colnames(dmass3)[20]="SESS_W"
customer_week <- dmass3[,list(buy_count = sum(PD_BUY_CT)),by = c("CLNT_ID", "SESS_W")]#주별, 고객 별 구매량 
head(customer_week)

# make dataset--------------------MAX WEEK GAP--------------------------------------------------------
##고객 별 시계열 데이터 생성 
customer_week2 <- as.data.frame(customer_week)
customer_wwide <- reshape(customer_week2, direction = "wide", idvar = "CLNT_ID", timevar="SESS_W") #row :고객, col : 주별 , 값 :구매량 
customer_wwide <- customer_wwide[order(customer_wwide$CLNT_ID),]

##컬럼 재조정
colnames(customer_wwide)[-1]=substr(colnames(customer_wwide)[-1],11,12)
setcolorder(customer_wwide, sort(colnames(customer_wwide)[-1]))
customer_wwide = cbind(customer_wwide$CLNT_ID,customer_wwide[,-28])
colnames(customer_wwide)[1]="CLNT_ID"
head(customer_wwide)
customer_wwide=customer_wwide[,-1]
colnames(customer_wwide)[-1]=seq(14,40,1)
head(customer_wwide)

##각 고객 별 주 별  구매시기 gap 계산 
week.gap=list()
for(i in 1:nrow(customer_wwide)){
  week.gap[[i]]=list(as.numeric(colnames(customer_wwide[i,which(is.na(customer_wwide[i,])==FALSE)][-1])))
}

##구매시기 gap의 가장 최대 값을 출력하는 함수 
diff.week=function(list){
  k=c()
  for(i in 2:length(list)){
    k[i-1]=list[i]-list[i-1]
  }
  return(max(k))
}

##max.week.gap의 계산 
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