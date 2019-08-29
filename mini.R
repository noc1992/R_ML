library(dplyr)
library(arules)
library(tibble)
library(arulesViz)

raw <- read.csv("Data/sparkle.csv")
View(raw)


data <- raw[2:10]
write.csv(data, file = "Sparkle.csv")
View(data)

ym <- summarise(group_by(data, HOLIDAY, ITEM_CNT, PRICE, RAIN_DAY, MAXTEMP, YM , SALEDAY))
ymq <- summarise(group_by(data, HOLIDAY, ITEM_CNT, PRICE, RAIN_DAY, MAXTEMP, YM , SALEDAY, QTY))


pc1 <- princomp(ym, cor=T)
summary(pc1)
pc1$loadings

pc2 <- princomp(ymq, cor=T)
summary(pc2)
pc2$loadings


sparkle <- as.data.frame(data)
inspect(sparkle)
summary(data)

summary(sparkle)

apriori(sparkle)
result_data <- apriori(sparkle)   


summary(result_data)
inspect(result_data)

rules_lift <- sort(result_data,by='lift', decreasing = T)
inspect(rules_lift[1:7])






