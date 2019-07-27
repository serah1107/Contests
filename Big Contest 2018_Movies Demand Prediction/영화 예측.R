library(ggplot2)
library(SignifReg)
library(olsrr)
library(modelr)
library(leaps)
library(SignifReg)


dat<-read.csv(file.choose(),header=TRUE)
View(dat)
dat<-dat[,-c(1)]
colnames(dat)<-c("x1","x2","x3","x4","x5","x6","x7","x8","x9","x10","x11","x12","x13","x14","y")


lm1<-lm(y~factor(x1)+factor(x2)+factor(x3)+x4+factor(x6)+factor(x7)+x8+x9+x10+x11+x12+x13+x14,data=dat)
summary(lm1)
anova(lm1)
plot(lm1$residuals)
sig1_for<-SignifReg(y~factor(x1)+factor(x2)+factor(x3)+x4+factor(x6)+factor(x7)+x8+x9+x10+x11+x12+x13+x14,data=dat, alpha=0.1, direction="forward",criterion="p-value",correction="None")
summary(sig1_for)
sig1_back<-SignifReg(y~factor(x1)+factor(x2)+factor(x3)+x4+factor(x6)+factor(x7)+x8+x9+x10+x11+x12+x13+x14,data=dat, direction="step_full",criterion="BIC",correction="None")
summary(sig1_back)
ols_step_both_p (lm1,details = TRUE)


lm2<-lm(y~x4+factor(x6)+x8+x9+x10+x12,data=dat)
summary(lm2)


lm3<-lm(y~x4+x8+x9+x12,data=dat)
summary(lm3)
plot(lm3$residuals)

# 
# anova(lm2)
# plot(lm2$residuals)
# sig1_back<-SignifReg(y~factor(x1)+factor(x3)+x4+factor(x6)+factor(x7)+x8+x9+x10+x11+x12+x13+x14,data=dat, direction="step_full",criterion="BIC",correction="None")
# summary(sig1_back)
# ols_step_both_p (lm2,details = TRUE)
# 
# 
# 
# 
# lm3<-lm(y~x4+factor(x5)+factor(x6)+factor(x7)+x8+x9+x10+x11+x12+x13+x14,data=dat)
# summary(lm3)
# plot(lm3$residuals)


intercept<-summary(lm3)$coefficients[1]
coef_x4<-summary(lm3)$coefficients[2]
coef_x8<-summary(lm3)$coefficients[3]
coef_x9<-summary(lm3)$coefficients[4]
coef_x12<-summary(lm3)$coefficients[5]




# 
# lm1<-lm(y~factor(x3)+factor(x5)+x6+x7+factor(x8)+factor(x9)+x10+x11+x12+x13+x14+x15+x16,data=dat2)
# summary(lm1)
# anova(lm1)
# mean(lm1$residuals^2)
# summary(lm1)$coefficients
# plot(lm1$residuals)
# sig1_for<-SignifReg(y~factor(x3)+factor(x5)+x6+x7+factor(x8)+factor(x9)+x10+x11+x12+x13+x14+x15+x16,data=dat2, alpha=0.1, direction="forward",criterion="p-value",correction="None")
# summary(sig1_for)
# sig1_back<-SignifReg(y~x3+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14+x15+x16,data=dat2, direction="step_full",criterion="BIC",correction="None")
# summary(sig1_back)
# ols_step_both_p (lm1,details = TRUE)
# 
# 
# lm2<-lm(y~factor(x3)+factor(x5)+x6+x7+factor(x9)+x13+x14+x15+x16,data=dat2)
# mean(lm2$residuals^2)
# summary(lm2)$coefficients
# plot(lm2$residuals)
# summary(lm2)
# sig2_back<-SignifReg(y~factor(x3)+factor(x5)+x6+x7+factor(x9)+x13+x14+x15+x16,data=dat2, direction="step_full",criterion="BIC",correction="None")
# summary(sig2_back)
# ols_step_both_p (lm2,details = TRUE)
# 
# View(dat2)
# 
# 
# lm3<-lm(y~factor(x5)+x6+x7+factor(x9)+x13+x14+x15+x16,data=dat2)
# mean(lm3$residuals^2)
# summary(lm3)$coefficients
# plot(lm3$residuals)
# summary(lm3)
# ols_step_both_p (lm3,details = TRUE)


test_data<-read.csv(file.choose(),header=TRUE)
View(test_data)
test_data<-test_data[,-c(1,4,18:21)]

colnames(test_data)<-c("x1","x2","x3","x4","x5","x6","x7","x8","x9","x10","x11","x12","x13","y")
test_y<-coef_x4*test_data$x3 +coef_x8*test_data$x11+ coef_x9*test_data$x13+coef_x12*test_data$x7


####???䷮ ???? ####
library(readxl)

dat1_2<-read_excel("C:\\Users\\user\\Desktop\\???䷮_3??.xlsx",sheet=2)
View(dat1_2)
dat1_3<-read_excel("C:\\Users\\user\\Desktop\\???䷮_3??.xlsx",sheet=3)
View(dat1_3)
ggplot

ols_bass <- function(dataset, n) {
  data = dataset %>% mutate(Yt_1 = lag(Yt))
  model <- nls(St ~ a + b*Yt_1 + c*Yt_1^2, data = data[1:n,], start = list(a=0, b=0, c=0))
  a = coef(model)[1] ; b = coef(model)[2] ; c = coef(model)[3]
  m = (-b-sqrt((b^2-4*c*a)))/(2*c) ; q = -c*m ; p = q-b 
  total = dataset[dim(dataset)[1],3]
  se = 100*(m-total)/total
  return(data.frame(real_m = total, est_m = m, p, q, se))
}


qq_rsq_bass <- function(theta, dataset, n) {
  m = theta[1] ; p = theta[2] ; q = theta[3]
  model <- lm(tt ~ 0 + I((1/(p+q))*log((1+(q/p)*(Yt/(m+1)))/(1-(Yt/(m+1))))), data = dataset[1:n,], na.action = na.omit)
  rsq = summary(model)$r.squared
  return(rsq)
}

ols_logis <- function(dataset, n) {
  data <- dataset %>% mutate(Yt_1 = lag(Yt))
  model <- nls(St ~ a*Yt_1 + b*Yt_1^2, data = data[1:n,], start = list(a=0, b=0))
  a = coef(model)[1] ; b = coef(model)[2]
  q = a ; m = -q/b
  total = dataset[dim(dataset)[1],4]
  se = 100*(m-total)/total
  return(data.frame(real_m = total, est_m = m, q, se))
}

est = ols_logis(dat1_3, dim(dat1_3)[1])





qq_rsq_logis <- function(theta, dataset, n) {
  m = theta[1] ; q = theta[2]
  model <- lm(tt ~ I(log(Yt/(m+1)/(1-(Yt/(m+1))))), data = dataset[1:n,], na.action = na.omit)
  rsq = summary(model)$r.squared
  return(rsq)
}


library(gridExtra)
mySt <- ggplot(dat1_2) +
  geom_line(aes(x = tt, y = St), color = "blue", size = 1) + 
  ggtitle("Time Series of Sales at t") + theme_classic()
myYt <- ggplot(dat1_2) +
  geom_line(aes(tt, Yt), color = "darkblue", size = 1) + 
  ggtitle("Time Series of Cumulative Sales at t") + theme_classic()
return(grid.arrange(mySt, myYt, ncol = 2))

library(gridExtra)
mySt <- ggplot(dat1_3) +
  geom_line(aes(x = tt, y = St), color = "blue", size = 1) + 
  ggtitle("Time Series of Sales at t") + theme_classic()
myYt <- ggplot(dat1_3) +
  geom_line(aes(tt, Yt), color = "darkblue", size = 1) + 
  ggtitle("Time Series of Cumulative Sales at t") + theme_classic()
return(grid.arrange(mySt, myYt, ncol = 2))



est = ols_logis(dat1_2, dim(dat1_2)[1])
est
qqtable2<- data.frame(tt=dat1_2$tt,Ur = dat1_2$Yt/(est$est_m+1)) %>%
  mutate(quantile = (1/est$q)*log(Ur/(1-Ur)))
qqtable2
qqtable2 <- qqtable2[-c(1),]
lmqq <- lm(tt ~ quantile, data = qqtable2)
plot(lmqq)

init = ols_logis(dat1_2, 35)
m0 = init$est_m ; q0 = init$q
optiminfo <- optim(par = c(m0, p0, q0), qq_rsq_logis, dataset = dat1_2[2:35], n = 50,
                   control = list(fnscale = -1))
mm = optiminfo$par[1]
model <- nls(tt ~ mu + (1/q)*log((Yt/(mm+1))/(1-(Yt/(mm+1)))), data = dat1_2[1:n,], 
             start = list(q = q0, mu = 0), na.action = na.omit)
qq = 0.3689
qqtable <- data.frame(tt = dat1_2$tt, Ur = dat1_2$Yt/(mm+1)) %>% 
  mutate(quantile = (1/qq)*log(Ur)/(1-Ur))
lmqq <- lm(tt ~ 0 + quantile, data = qqtable)
lmqq

gqq <- ggplot(qqtable) + 
  geom_point(aes(quantile, tt)) + 
  geom_smooth(aes(quantile, tt), color = "blue", size = 1, method = "lm", se = F) +
  ggtitle(paste(" Q-Q plot : R2 = ", round(summary(lmqq)$r.squared, 4)*100, "%", sep = "")) + 
  theme_classic()
gqq 
mm





est = ols_bass(dat1_2, dim(dat1_2)[1])
est
qqtable2<- data.frame(tt=dat1_2$tt,Ur = dat1_2$Yt/(est$est_m+1)) %>%
  mutate(quantile = (1/est$q)*log(Ur/(1-Ur)))
qqtable2
qqtable2 <- qqtable3[-c(1),]
lmqq <- lm(tt ~ quantile, data = qqtable2)
plot(lmqq)

init = ols_bass(dat1_2, 35)
m0 = init$est_m ; q0 = init$q
optiminfo <- optim(par = c(m0, p0, q0), qq_rsq_bass, dataset = dat1_2[2:35], n = 50,
                   control = list(fnscale = -1))
mm = optiminfo$par[1]
model <- nls(tt ~ mu + (1/q)*log((Yt/(mm+1))/(1-(Yt/(mm+1)))), data = dat1_2[1:n,], 
             start = list(q = q0, mu = 0), na.action = na.omit)
qq = 0.3689
qqtable <- data.frame(tt = dat1_2$tt, Ur = dat1_2$Yt/(mm+1)) %>% 
  mutate(quantile = (1/qq)*log(Ur)/(1-Ur))
lmqq <- lm(tt ~ 0 + quantile, data = qqtable)
lmqq

gqq <- ggplot(qqtable) + 
  geom_point(aes(quantile, tt)) + 
  geom_smooth(aes(quantile, tt), color = "blue", size = 1, method = "lm", se = F) +
  ggtitle(paste(" Q-Q plot : R2 = ", round(summary(lmqq)$r.squared, 4)*100, "%", sep = "")) + 
  theme_classic()
gqq 
mm





