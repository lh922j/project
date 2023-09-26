ex_rate.data<- read.table("C:/Users/user/Desktop/4-1학기/금123 시계열자료분석/과제/ECOS_TABLE_20220316_165406.txt",header=FALSE, sep = "\t")
ex_rate.data
ex_rate <- ts(data = ex_rate.data[,2], start=c(2007,1), end=c(2021,4), frequency = 4)
ex_rate
tt <- 1:length(ex_rate)
#정상성 확인   하락하는 추세를 보이면서 분산도 일정하지 않음을 볼 수 있습니다. 그림의 형태가 수평선 모양이 아니기 때문에 육안으로는 비정상 시계열로 볼 수 있습니다.
plot(ex_rate)
#표본자기상관함수 그림
acf(ex_rate)  # 정상성 만족 못함 -> 비정상 시계열
# 정상시계열의 SACF 그림처럼 지수적으로 빠르게 0으로 수렴하는 형태를 가지는 것을 볼 수 있습니다. 그러나 시계열의 그래프를 보면 일정하지 않는 분산과 하락하는 추세를 보면 정상시계열로 판단하기 어렵습니다다. 그래서 정상화 절차를 진행하였습니다.


?boxcox
# 정상화 
#분산 안정화
library(MASS)
boxcox(ex_rate~time(ex_rate),lambda=seq(-10,10,1/10))
boxcox(ex_rate~tt)
time(ex_rate)
boxcox(ex_rate~time(ex_rate),lambda=seq(-10,10,1/10), plotit=FALSE)
bc = boxcox(ex_rate~time(ex_rate),lambda=seq(-10,10,1/10), plotit=FALSE)
which.max(bc$y)
lam = bc$x[which.max(bc$y)]
lam

par(mfrow = c(1,2)) ;dev.off()
BC1 = (ex_rate^lam-1)/lam; plot(BC1)
BC2 = ex_rate^lam ; plot(BC2)

plot(BC2)
Ln = log(ex_rate) ; plot(Ln)

reg = lm(BC1~time(ex_rate))
plot(BC1)
abline(reg, col= "blue")
E = ts(reg$residuals, start=c(2007,1), frequency = 4)
plot(E)
abline(h=0, col="blue", lty =2)

dec = decompose(E)
plot(dec)

season = dec$x -dec$seasonal
plot(season)

#차분
par(mfrow = c(1,2))
D1 = diff(BC1) ; plot(D1) # 1차 차분, 선형추세 제거
D4 = diff(BC1,lag = 4) ; plot(D4) # 주기4로 반복되는 패턴 제거
dev.off()
plot(D4)











