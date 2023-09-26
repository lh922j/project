install.packages("dplyr")
library(dplyr)
ex_rate.data<- read.table("C:/Users/Leedonghoon/Desktop/학교/4-1학기/금123 시계열자료분석/과제/ECOS_TABLE_20220316_165406.txt",header=FALSE, sep = "\t",fileEncoding = "euc-kr")
ex_rate.data
ex_rate <- ts(data = ex_rate.data[,2], start=c(2007,1), end=c(2021,4), frequency = 4)
ex_rate
plot(ex_rate)
dev.off()
# 모형의 식별 (Model identification)
ex_rate
library(tseries)
tseries::kpss.test(ex_rate, null="Level") # 유의수준 < 유의확률 차분필요
tseries::kpss.test(diff(ex_rate), null="Level") # d=1 #정상시계열
plot(ex_rate); plot(diff(ex_rate))

par(mfrow=c(1,2))

?pacf
# model selection
acf(diff(ex_rate)); pacf(diff(ex_rate)) #AR(1) SPACF 1시차 부분부터 0 #Estimation 추정
out1 = arima(ex_rate, order = c(1,1,0),include.mean = TRUE)
out1
forecast::Arima(ex_rate,order=c(1,1,0),include.drift = TRUE)
forecast::Arima(ex_rate,order=c(1,0,0),include.mean = TRUE)

fit = arima(ex_rate, order=c(1,1,0))
fit


#모형의 진단
#잔차분석
fit = arima(ex_rate, order=c(1,1,0))
fit


par(mfrow=c(1,3))
plot(fit$residuals) ; acf(fit$residuals);pacf(fit$residuals) # 0을 중심으로 수평선 띠 모양이 보임. 잔차시계열에 백색잡음 존재함이 보임.
Box.test(fit$residuals, lag=1 , type = c("Box-Pierce"))


#과다적합 = 현재 고려중인 모형보다 한 차수씩 더 높은 모형을 적합시켜 새롭게 추가된 항이 유의한가 살펴보는 방법 
fit1 = arima(ex_rate, order=c(1,1,1))
fit1
fit2 = arima(ex_rate, order=c(2,1,0))
fit2
fit3 = arima(ex_rate, order=c(2,1,1))
fit3
AIC(fit,fit1,fit2,fit3) ##모형 비교
BIC(fit,fit1,fit2,fit3)

#모형진단
tsdiag(fit)  # 로우1 로우2 0인지 p-value판단













