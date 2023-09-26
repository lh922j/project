ex_rate.data <- read.table("C:/Users/Leedonghoon/Desktop/학교/4-1학기/금123 시계열자료분석/과제/ECOS_TABLE_20220316_165406.txt",header=FALSE, sep = "\t",fileEncoding = "euc-kr")
ex_rate <- ts(data = ex_rate.data[,2], start=c(2007,1), end=c(2021,4), frequency = 4)
ex_rate1 <-  ts(data = ex_rate.data[,2], start=c(2007,1), end=c(2021,4), frequency = 12)
ex_rate1 
#분해법

library(tseries)
library(forecast)
library(TTR)
plot(decompose(ex_rate1, type = "multiplicative"))
plot(decompose(ex_rate))

#추세성분  보통 주기의 배수 중 작은값 손실값을 줄이기 위해 order값 설정 
 # 중심화 이동평균
ex_rate1 <-  ts(data = ex_rate.data[,2], start=c(2007,1), end=c(2021,4), frequency = 12)
plot(ex_rate1)
lines(ma(ex_rate,order=4), col="blue")
lines(ma(ex_rate1,order=12), col="red")
 # 회귀도 가능 둘중 아무거나 해  추세파악 
trd <- 1:length(ex_rate1)
lm(ex_rate1~trd)  #y절편 기울기
abline(lm(ex_rate1~trd), col="red")
plot(trd,ex_rate1, type="l")
abline(lm(ex_rate1~trd), col="red")

#추세조정(detrending) Dt = 계절 + 불규칙 
 











par(mfrow=c(2,1))
dev.off()

# 지수평활 20개 예측
ex_rate.data <- read.table("C:/Users/user/Desktop/4-1학기/금123 시계열자료분석/과제/ECOS_TABLE_20220316_165406.txt",header=FALSE,sep="\t")
ex_rate <- ts(data = ex_rate.data[,2], start=c(2007,1), end=c(2021,4), frequency = 4)
ex_rate


library(tseries)
library(forecast)
library(TTR)

es1 <- HoltWinters(ex_rate, beta=FALSE, gamma=FALSE)
es1.p <- predict(es1,20,prediction.interval = TRUE)
plot(es1,es1.p)

es.t <- HoltWinters(ex_rate,gamma=FALSE) # 추세, 계절 고려x
es.t.p <- predict(es.t, 20, prediction.interval = TRUE)
es.t.p
plot(es.t,es.t.p)

 #계절 성분 고려
es.s <- HoltWinters(ex_rate)
es.s.p <- predict(es.s, 20,prediction.interval = TRUE)
es.s
es.s.p
plot(es.s, es.s.p)













es1 <- HoltWinters(ex_rate1, beta=FALSE, gamma=FALSE)
es1.p <- predict(es1,20,prediction.interval = TRUE)
plot(es1,es1.p)

es.t <- HoltWinters(ex_rate1,gamma=FALSE) # 추세, 계절 고려x
es.t.p <- predict(es.t, 20, prediction.interval = TRUE)
es.t.p
plot(es.t,es.t.p)

#계절 성분 고려
es.s <- HoltWinters(ex_rate1)
es.s.p <- predict(es.s, 20,prediction.interval = TRUE)
es.s
es.s.p
plot(es.s, es.s.p)
