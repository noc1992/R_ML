library("dplyr")
library("ggplot2")
library("caret")
library(rpart)
library(tree)
library(arules)
library(arulesViz)

data <- read.csv("Data/sparkle.csv",encoding = "euc-kr")
str(sparkle)
head(sparkle)

QTY_CNT <- data$QTY / data$ITEM_CNT

spk <- cbind(sparkle, QTY_CNT)
spk
subset(spk, select = -CATEGORY)
subset(spk, select = -QTY)
spk <- subset(spk, select = -c(CATEGORY, QTY, ITEM_CNT, YM.1, 판매년월))


x1 = sample(spk$QTY,100,replace = F)     
x2 = sample(spk$QTY,50,replace = F)
shapiro.test(spk$QTY)
shapiro.test(log10(spk$QTY))
shapiro.test(x1)
shapiro.test(x2)

#P-value 값이 매우 작으므로 귀무가설
#(H0 : 수입은 정규분포를 따른다)을 기각하고,
#대립가설 (H1: 수입은 정규분포를 따르지 않는다)를 채택하게 되겠습니다.

#그러나 통계량만을 가지고 정규성 여부를 검정하는 것은 부족하며,
#반드시 그래프를 통해서 정규성 여부를 병행해서 확인할 필요가 있습니다.

qqnorm(spk$QTY)
qqline(spk$QTY)
qqnorm(x1)
qqline(x1)
qqnorm(x2)
qqline(x2)

# 정규성을 따른다.!!

attach(spk)

head(spk)
ggplot(spk, aes(x=MAXTEMP, y=QTY_CNT)) +
  geom_point()


#회귀 모델
m1=lm(MAXTEMP~QTY,data = spk)
m2=lm(PRICE~QTY,data = spk)
m3=lm(SALEDAY~QTY,data = spk)
m4=lm(RAIN_DAY~QTY,data = spk)
m1
m2
m3
m4

par(mfrow=c(2,2))
plot(MAXTEMP~QTY)
abline(m1, col='blue') # 온도 판매량 m1
plot(PRICE~QTY)
abline(m2, col='blue') # 가격 판매량  m2
plot(SALEDAY~QTY)
abline(m3, col='blue') # 판매일 판매량  m3
plot(RAIN_DAY~QTY)
abline(m4, col='blue') # 강수일수 판매량 m4

View(spk)
summary(m1)
summary(m2)
summary(m3) 
summary(m4)

m5 <- lm(QTY_CNT~MAXTEMP+PRICE+SALEDAY+RAIN_DAY, data=sparkle)
summary(m5)
# 의미는 있으나 그 신뢰도가 높지 않다. 

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
head(spk)

#상관분석&시각화
cor(spk[,0:6]) # 판매량 ~ 쉬는날
pairs(spk[,0:6])

#잔차분석 시각(가우시안) 
par(mfrow=c(2,2))
plot(m1)
plot(m2)
plot(m3)
plot(m4)
# 1번그림 : BMI 값이 증가할수록 폐기능 값이 선형관계라면 
# 예측값(predicted values)과 잔차(Residuals)가 일정한 관계를 
# 가지면 안된다. 즉 점이 산발적으로 찍혀야 한다. 빨간 줄이 거의 
# 직선 성향을 띠기에 선형관계에 있다고 한다.3

# 2번그림 : 잔차가 정규성을 갖기 위해서는 세로축의 Std deviance resid 
# 값이 가로축의 Qunantile이 증가하여도 일정하게 45도 각도로 증가하여야 한다. 
# Quantile 1 정도에서부터 약간 벗어낫지만 이정도는 정규성을 띠었다고 봐야 한다.

# 3번그림 : 분산이 일정하다면 값이 무작위로 찍혀야 한다.
# 선이 오른쪽으로 갈 수록 아래로 쳐졌지만(heteroscedastic) 
# 대략적으로 무작위로 찍혔다.

# 4번그림 : 개개 관측치의 이상치, 통계 모형 계수에 영향을 줄 수 
# 있는 관측치를 확인하기 위해 Cook's distance 가 빨간 점선으로 표시되었고
# 그림 안에는 빨간 점선이 보이지 않는다. 그러므로 특정 값이 통계 모형 계수에
# 영향을 주지 않는 것으로 보인다.

train_spk <- createDataPartition(spk$QTY, p=0.7, list=F)
spk_train <- spk[train_spk,]
spk_test <- spk[-train_spk,]
dim(spk_train)
dim(spk_test)

#다중회귀 분석
lm_fit <- lm(QTY_CNT~., data = spk_train)
summary(lm_fit)
# r 스퀘어드 20% 

lm_fit2 <- lm(lm_fit, method = "qr")
summary(lm_fit2)

# 예측값과 실제값 비교 산점도
lm_yhat2 <- predict(lm_fit2, newdata = spk_test)
sqrt(mean((lm_yhat2-spk_test$QTY_CNT)^2))
# 평균 제곱근 오차 56


# 의사결정트리
tree_fit <- tree(QTY_CNT~., data = spk_train)
summary(tree_fit)

# 시각화
par(mfrow=c(1,1))
plot(tree_fit)
text(tree_fit, pretty = 0)
# 가격이 가장 큰 요인으로 처음에 분할 요인으로 사용됨
# 1850원 이상일때 40개 정도가 팔림

# MSE 
tree_yhat <- predict(tree_fit, newdata = spk_test)
sqrt(mean(tree_yhat-spk_test$QTY_CNT)^2)
# 평균 제곱근 오차 7

#주성분 분석
pc1 <- princomp(spk, cor=T)
summary(pc1)
pc1$loadings

# 행렬도
biplot(pc1, cex=0.7)
# x 축 방향이 제1주성분 y 축 방향이 제2주성분  거리: 영향을 미치는 정도




