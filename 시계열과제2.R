dev.off()
ex_rate.data<- read.table("C:/Users/Leedonghoon/Desktop/학교/4-1학기/금123 시계열자료분석/과제/ECOS_TABLE_20220316_165406.txt",header=FALSE, sep = "\t",fileEncoding = "euc-kr")
ex_rate.data
ex_rate <- ts(data = ex_rate.data[,2], start=c(2007,1), end=c(2021,4), frequency = 4)
ex_rate
plot(ex_rate)

# 중심화 이동평균 m=3 m=4 일때 비교
library(forecast)
library(tseries)
library(TTR)
ma.cen <- ma(ex_rate, order=3, centre=TRUE)
ma.noncen <- ma(ex_rate, order=4, centre=FALSE)
ma.noncen
n <- length(ex_rate)
ma.P <- c(NA,ma.noncen[-n]); ma.F <- ma.noncen
ma.t <- (ma.P+ma.F)/2  
ma.mat <- cbind(ma.cen, ma.noncen, ma.P, ma.F, ma.t)
ma.mat
plot(ma.t)
lines(ma.cen,col="red")




plot(ex_rate)
lines(SMA(ex_rate, n=3), col="blue")
lines(SMA(ex_rate, n=4), col="red")

sum((ma.cen-ex_rate)^2,na.rm=TRUE)
sum((ma.t-ex_rate)^2,na.rm=TRUE)

plot(co2)
lines(SMA(co2, n=12), col="blue")
lines(SMA(co2, n=6), col="red")


#lowess 국소회귀 window + wls 
plot(ex_rate)
lines(lowess(ex_rate, f=2/3), col="blue")
lines(lowess(ex_rate, f=0.1), col="red")



#
ex_rate.fore <- c(ex_rate, rep(NA,20))
ex_rate.fore.ts <- ts(ex_rate.fore, start=c(2007,1), frequency=4)
plot(ex_rate.fore.ts)
abline(v=2022, col="red", lty=2)
sma.4 <- SMA(ex_rate, n=4)
sma.fore.ts <- ts(c(NA, sma.4, rep(sma.12[n],19)), start=c(2007,1), frequency=4)
abline(h=mean(ex_rate), col="green")
lines(sma.fore.ts, col="blue")

dma.4 <- SMA(sma.4, n=4)
#lines(dma.4, col="green")
ma.trend <- c(NA, (2*sma.4-dma.4)+2/3*(sma.4-dma.4), (2*sma.4[n]-dma.4[n])+2/3*(sma.4[n]-dma.4[n])*(2:20))
lines(ts(ma.trend, start=c(2007,1), frequency=4), col="red")
list(ts(ma.trend, start=c(2007,1), frequency=4), col="red")



#
plot(ex_rate)
fit1 <- HoltWinters(ex_rate, alpha=0.1, beta=FALSE, gamma=FALSE)
lines(fit1$fitted[,1], col="red")
fit5 <- HoltWinters(ex_rate, alpha=0.5, beta=FALSE, gamma=FALSE)
lines(fit5$fitted[,1], col="blue")
fit.auto <- HoltWinters(ex_rate, beta=FALSE, gamma=FALSE)
lines(fit.auto$fitted[,1],col="green")
plot(forecast(fit.auto))
forecast(fit.auto)

ex_rate
plot(ex_rate)

w.vec <- seq(0.1, 0.9, by=0.1)
p <- 1
es.sse <- c()
for(w in w.vec){
  es <- HoltWinters(ex_rate, alpha=w, beta=FALSE, gamma=FALSE)
  lines(es$fitted[,1], col=p)
  es.sse <- c(es.sse, es$SSE)  # es$SSE = sum((co2[-1]-es$fitted[,1])^2)
  p <- p+1
}
w.vec[which.min(es.sse)]

es.auto <- HoltWinters(ex_rate, beta=FALSE, gamma=FALSE)
plot(forecast(es.auto))
forecast(es.auto,20)

#
ex_rate.fore <- c(ex_rate, rep(NA,20))
ex_rate.fore.ts <- ts(ex_rate.fore, start=c(2007,1), frequency=4)
plot(ex_rate.fore.ts)
abline(v=2022, col="red", lty=2)
sma.4 <- SMA(ex_rate, n=4)
sma.fore.ts <- ts(c(NA, sma.4, rep(sma.12[n],19)), start=c(2007,1), frequency=4)
abline(h=mean(ex_rate), col="green")
lines(sma.fore.ts, col="blue")
list(sma.fore.ts)
