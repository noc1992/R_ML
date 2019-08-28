library(dplyr)
library(arules)
library(arulesViz)

raw <- read.csv("Data/sparkle.csv")
View(raw)

data <- raw[2:10]
View(ym9)

ym <- summarise(group_by(data, MAXTEMP, RAIN_DAY,PRICE, YM,HOLIDAY))

pc1 <- princomp(ym, cor=T)
summary(pc1)
pc1$center
pc1$loadings

inspect(sparkle)
summary(data)
sparkle <- as.data.frame(data)

apriori(data)
result_data <- apriori(sparkle, parameter = list(support=0.005, confidence=0.5, minlen=2))

summary(result_data)
inspect(result_data)

rules_lift <- sort(result_data,by='lift', decreasing = T)
inspect(rules_lift[1:7])





