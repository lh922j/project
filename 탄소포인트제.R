install.packages("dplyr")
library(dplyr)

data1 = read.csv("C:/Users/Leedonghoon/Desktop/학교/4-2/월123회귀분석및실습2/탄소포인트/한국환경공단_탄소포인트제_전국 참여자 현황_20200831.csv",header=T, fileEncoding = "euc-kr")
str(data1)
summary(data1)
data1 = data1[,-c(2:12)]
data1
#colnames(data1)[c(1)] = c('지자체'),
names(data1)[1] <- c("지자체")
str(data1)

data2 = read.csv("C:/Users/Leedonghoon/Desktop/학교/4-2/월123회귀분석및실습2/탄소포인트/한국환경공단_탄소포인트제 전기 사용량_20200910.csv",header=T, fileEncoding = "euc-kr")
data2
data2 = data2 %>% filter(연도 == 2018)
data2 = data2[,-1]
str(data2)

data3 = read.csv("C:/Users/Leedonghoon/Desktop/학교/4-2/월123회귀분석및실습2/탄소포인트/한국환경공단_탄소포인트제 도시가스 사용량_20200910.csv",header=T, fileEncoding = "euc-kr")
summary(data3)
data3 = data3 %>% filter(연도 == 2018)
data3 = data3[,-1]
str(data3)

data4 = read.csv("C:/Users/Leedonghoon/Desktop/학교/4-2/월123회귀분석및실습2/탄소포인트/한국환경공단_탄소포인트제 상수도 사용량_20200910.csv",header=T, fileEncoding = "euc-kr")
str(data4)
data4 = data4 %>% filter(연도 == 2018)
data4 = data4[,-1]
str(data4)

data5 = read.csv("C:/Users/Leedonghoon/Desktop/학교/4-2/월123회귀분석및실습2/탄소포인트/공공부문 배출량 통계_20221122205157440.csv",header=T, fileEncoding = "euc-kr")
summary(data5)
str(data5)
data5[,3]
names(data5)[3] <- c("지자체")
data5 = data5[,-c(1,2,10,11)]
str(data5)

tot = inner_join(data1,data2, by="지자체")
tot1 = inner_join(tot,data3,by="지자체")
tot2 = left_join(tot1,data4,by="지자체")
tot3 = left_join(tot2,data5, by="지자체")
tot3
summary(tot3)

tot3 = tot3 %>% filter(!is.na(tot3$기준배출량.tonCO..eq.))
tot3 = tot3 %>% filter(!is.na(tot3$상수도.사용량))
summary(tot3)
nrow(tot3)
str(tot3)

# 분석
tot4 = tot3[,-c(1,3,9)]
tot4

#################################################### 
#################################################
# 변수들의 상관관계를 전체적으로 파악하기 위해 산점도를 사용
plot(tot4)

# 다중공선성 있는지 판단. 존재. 변수 제거가 필요하다. 
library(olsrr)
reg = lm(탄소포인트.감축량.tonCO..eq.~합계..가입자수.+전기.사용량+도시가스.사용량+상수도.사용량+기준배출량.tonCO..eq.+온실가스.배출량.tonCO..eq.+온실가스.감축량.tonCO..eq.+온실가스.감축률...,data=tot4)
ols_vif_tol(reg) # 다중공선성 존재 

# 변수선택 단계적 선택 이유는? 
library(MASS)
stepAIC(lm(탄소포인트.감축량.tonCO..eq.~1,data=tot4),direction = "both")  # 온실가스.감축량, 온실가스 배출량,기준배출량, 상수도, 온실가스 감축률 
colnames(tot4) = c("X1","X2","X3","X4","X5","X6","Y","X7","X8")
reg1 = lm(Y~X4+X5+X6+X7+X8,data=tot4) # X8의 유의확률이 가장 큼, 그래서 제거
summary(reg1)
reg2 = lm(Y~X4+X5+X6+X7,data=tot4)# 유의 x
summary(reg2)
reg3 = lm(Y~X5+X6+X7+X8,data=tot4) #유의 x
summary(reg3)
reg2 = lm(Y~X5+X6+X7,data=tot4) # 최종 모델 선정 
summary(reg2)
ols_regress(Y~X5+X6+X7,data=tot4)
ols_step_both_aic(reg2,progress = F,details=F) # aic = 2265.115 ############### 
##########
library(olsrr)
ols_regress(Y~X7+X6+X5+X4+X8,data=tot4)

########### 단계적 방법, 결과
regF = lm(Y~.,data=tot4)
ols_step_both_aic(regF,progress = T,details=T)  # aic = 2263.948  
ols_step_both_p(regF,prem=0.1)
ols_regress(reg2)


reg.F = lm(Y~X7+X6+X5+X4+X8,data=tot4)
summary(reg.F) # 모형은 유의하고 수정된 결정계수는 0.8747로 설명력은 높지만 X4,X8 변수가 유의하지 않음.

reg1 = lm(Y~1,data=tot4)
reg7 = lm(Y~X7,data=tot4)
anova(reg1,reg7)

reg76 = lm(Y~X7+X6,data=tot4)
anova(reg7,reg76)

reg765 = lm(Y~X7+X6+X5,data=tot4)
anova(reg76,reg765)

reg7654 = lm(Y~X7+X6+X5+X4,data=tot4)
anova(reg765,reg7654) # 유의하지 않음

reg7658 = lm(Y~X7+X6+X5+X8,data=tot4)
anova(reg765,reg7658) # 유의하지 않음

reg76548 = lm(Y~X7+X6+X5+X4+X8,data=tot4)
anova(reg765,reg76548)
################## 전진선택
reg = lm(Y~X1+X2+X3+X4+X5+X6+X7+X8, data=tot4)
ols_step_forward_aic(reg, progress=T,details=T) # aic = 2263.948 0.87468
regFo = lm(Y~X7+X6+X5+X4+X8,data=tot4)
summary(regFo)
############### 후진제거 # 너무 안좋음
ols_step_backward_aic(reg, progress = T, details=T) # aic = 2263.948  0.87468
regBack = lm(Y~X1+X2+X3,data=tot4)
summary(regBack)
##########################################################################
############# 교차검증 ############################################## 모형 평가
######################################################################
# Loo 사용
n = dim(tot4)[1] 
pred.res = rep(NA, n)
for(i in 1:n){
  test.data = tot4[i,]
  train.data = tot4[-i,]
  regA = lm(Y~X4+X5+X6+X7, data=train.data)
  pred.res[i] = test.data[,1] - predict(regA, newdata = test.data)
}

press1 = sum(pred.res^2)

n = dim(tot4)[1] 
pred.res = rep(NA, n)
for(i in 1:n){
  test.data = tot4[i,]
  train.data = tot4[-i,]
  regA = lm(Y~., data=train.data)
  pred.res[i] = test.data[,1] - predict(regA, newdata = test.data)
}
press2 = sum(pred.res^2)

c(press1,press2) # 예측력은 press1 처음 모형이 더 좋음
###############
a = rstandard(reg2,type="pred")^2
b = rstandard(reg,type="pred")^2

AIC(reg,reg2)
BIC(reg,reg2)
###################
out = ols_step_all_possible(reg)
out

