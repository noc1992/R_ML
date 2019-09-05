library("dplyr")
library("ggplot2")
data <- read.csv("Data/일반탄산음료_5조.csv",encoding = "euc-kr")
sparkle <- group_by(data, CATEGORY="일반탄산음료")
sparkle = sparkle %>%
  select( -X,-X.1,-X.2)
str(sparkle)
head(sparkle)

shapiro.test(sparkle$QTY)
shapiro.test(log10(sparkle$QTY))
# p 2.2 이상

attach(sparkle)

head(sparkle)
ggplot(sparkle, aes(x=MAXTEMP, y=QTY)) +
  geom_point()
#회귀 모델
m1=lm(MAXTEMP~QTY,data = sparkle)
m2=lm(PRICE~QTY,data = sparkle)
m3=lm(SALEDAY~QTY,data = sparkle)
m4=lm(RAIN_DAY~QTY,data = sparkle)

par(mfrow=c(2,2))
plot(MAXTEMP~QTY)
abline(m1, col='blue') # 온도 판매량 m1
plot(PRICE~QTY)
abline(m2, col='blue') # 가격 판매량  m2
plot(SALEDAY~QTY)
abline(m3, col='blue') # 판매일 판매량  m3
plot(RAIN_DAY~QTY)
abline(m4, col='blue') # 강수일수 판매량 m4

summary(m1)
summary(m2)
summary(m3) 
summary(m4)

#회귀계수
coef(m1)
coef(m2)
coef(m3)
coef(m4)

#회귀계수 신뢰구간
confint(m1)
confint(m2)
confint(m3)
confint(m4)
head(sparkle)

#상관분석&시각화
cor(sparkle[,3:8]) # 판매량 ~ 쉬는날
pairs(sparkle[,3:8])

#잔차분석 시각(가우시안) 
par(mfrow=c(2,2))
plot(m1)
plot(m2)
plot(m3)
plot(m4)

