rm(list=ls()); dev.off()

# 파일 불러오기-유로 환율 
ex_rate.file <- readline()
C:/Users/Leedonghoon/Desktop/학교/4-1학기/금123 시계열자료분석/과제/ECOS_TABLE_20220316_165406.txt
ex_rate.file <- gsub("\\\\","/",ex_rate.file)
ex_rate.data <- read.csv(file=ex_rate.file, header = FALSE, sep = "\t",fileEncoding = "euc-kr")
head(ex_rate.data)
ex_rate.data
ex_rate.ts <- as.ts(ex_rate.data)
ex_rate.ts

ex_rate.data
ex_rate <- ts(data = ex_rate.data[,2], start=c(2007,1), end=c(2021,4), frequency = 4)
ex_rate

plot(ex_rate)
length(ex_rate)
tt <- 1:length(ex_rate)
plot(tt,ex_rate)

reg1 <- lm(ex_rate~tt)
summary(reg1)
lines(reg1$fitted.values, col=1)

reg2 <- lm(ex_rate~tt+I(tt^2))
summary(reg2)
lines(reg2$fitted.values, col = 2)

reg3 <- lm(ex_rate~tt+I(tt^2)+I(tt^3))
summary(reg3)
lines(reg3$fitted.values, col=3)

reg4 <- lm(ex_rate~tt+I(tt^2)+I(tt^3)+I(tt^4))
summary(reg4)
lines(reg4$fitted.values, col=4)

reg5 <- lm(ex_rate~tt+I(tt^2)+I(tt^3)+I(tt^4)+I(tt^5))
summary(reg5)
lines(reg5$fitted.values,col=5)

reg6 <- lm(ex_rate~tt+I(tt^2)+I(tt^3)+I(tt^4)+I(tt^5)+I(tt^6))
summary(reg6)
lines(reg6$fitted.values, col=6)

reg7 <- lm(ex_rate~tt+I(tt^2)+I(tt^3)+I(tt^4)+I(tt^5)+I(tt^6)+I(tt^7))
summary(reg7)
lines(reg7$fitted.values, col=7)

reg8 <- lm(ex_rate~tt+I(tt^2)+I(tt^3)+I(tt^4)+I(tt^5)+I(tt^6)+I(tt^7)+I(tt^8))
summary(reg8)
lines(reg8$fitted.values, col=8)

reg9 <- lm(ex_rate~tt+I(tt^2)+I(tt^3)+I(tt^4)+I(tt^5)+I(tt^6)+I(tt^7)+I(tt^8)+I(tt^9))
summary(reg9)
lines(reg9$fitted.values, col=9)

reg10 <- lm(ex_rate~tt+I(tt^2)+I(tt^3)+I(tt^4)+I(tt^5)+I(tt^6)+I(tt^7)+I(tt^8)+I(tt^9)+I(tt^10))
summary(reg10)
lines(reg10$fitted.values, col=10)


LE <- log(ex_rate) 
plot(tt,LE)
reg11 <- lm(LE~tt)
summary(reg11)
lines(reg11$fitted.values, col = 11)

par(mfrow=c(2,2))

plot(tt,ex_rate,type="l", main = '분기별')
lines(reg1$fitted.values,col=1)
lines(reg2$fitted.values,col=2)
lines(reg3$fitted.values,col=3)
lines(reg4$fitted.values,col=4)
lines(reg5$fitted.values,col=5)
lines(reg6$fitted.values,col=6)
lines(reg7$fitted.values,col=7)
lines(reg8$fitted.values,col=8)
lines(reg9$fitted.values,col=9)
lines(reg10$fitted.values,col=10)
lines(exp(reg11$fitted.values),col=11)

sse1 <- sum((ex_rate-reg1$fitted.values)^2)
sse2 <- sum((ex_rate-reg2$fitted.values)^2)
sse3 <- sum((ex_rate-reg3$fitted.values)^2)
sse4 <- sum((ex_rate-reg4$fitted.values)^2)
sse5 <- sum((ex_rate-reg5$fitted.values)^2)
sse6 <- sum((ex_rate-reg6$fitted.values)^2)
sse7 <- sum((ex_rate-reg7$fitted.values)^2)
sse8 <- sum((ex_rate-reg8$fitted.values)^2)
sse9 <- sum((ex_rate-reg9$fitted.values)^2)
sse10 <- sum((ex_rate-reg10$fitted.values)^2)
sse11 <- sum((ex_rate-exp(reg11$fitted.values))^2)
c(sse1,sse2,sse3,sse4,sse5,sse6,sse7,sse8,sse9,sse10,sse11)  # 가장 낮은값 10차 추세값

#패턴파악하기 위해 분기별로 회귀분석 
Q1 <- (tt%%4)==1
Q2 <- (tt%%4)==2
Q3 <- (tt%%4)==3

reg20 <- lm(ex_rate~tt+I(tt^2)+I(tt^3)+I(tt^4)+I(tt^5)+I(tt^6)+I(tt^7)+I(tt^8)+I(tt^9)+I(tt^10)+Q1+Q2+Q3)
summary(reg20)
lines(reg20$fitted.values, col=20)
sse20 <- sum((ex_rate-reg20$fitted.values)^2)
c(sse1,sse2,sse3,sse4,sse5,sse6,sse7,sse8,sse9,sse11,sse20)  #가장 낮은값 10차 추세값

reg30 <- lm(ex_rate~tt+Q1+Q2+Q3)
summary(reg30) # 유의하지 않음

