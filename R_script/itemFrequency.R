iris2 = iris[,1:4]
iris2

km.out.withness=c()
km.out.between=c()
ii=c()

for(i in 2:7){
  set.seed(1)
  km.out=kmeans(iris2,centers = i)
  km.out.withness[i-1]=km.out$tot.withinss
  km.out.between[i-1]=km.out$betweenss
  ii=c(ii,i)
}

data.frame(ii, km.out.withness, km.out.between)

install.packages("arules")
install.packages("arulesViz")
library(arules)
library(arulesViz)
data("Groceries")
data(package="arules")
Groceries
inspect(Groceries[1:10])

itemFrequency(Groceries, type='absolute')
round(sort(itemFrequency(Groceries, type='absolute'),decreasing=T),2)
itemFrequencyPlot(Groceries,topN=10, type='relative', col=rainbow(10))
apriori(Groceries)
rt_rules=apriori(Groceries, parameter = list(support=0.005, confidence=0.5, minlen=2))
summary(rt_rules)
inspect(rt_rules[1:5])

rules_lift=sort(rt_rules, by='lift',decreasing = T)
inspect(rules_lift[1:5])


plot(rules_lift[1:5], method = "graph", measure="lift", shading="confidence")
